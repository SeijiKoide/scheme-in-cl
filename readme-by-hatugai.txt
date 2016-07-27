* ファイル一覧

scheme-in-cl-dynamic.lisp				; dynamic scope version
scheme-in-cl-mixable.lisp				; lexical/dynamic mixable version
test-scheme-in-cl-dynamic.lisp			; test script for scheme-in-cl-dynamic.lisp
test-scheme-in-cl-mixable.lisp			; test script for scheme-in-cl-mixable.lisp
unit-test.lisp							; macros for unit test

各実装ファイルにおいて，セミコロンによる長い行のコメント以下が変更部分です．
それ以前はオリジナルと同じです．
（proc 構造体の printer だけ少し改変）

* 動的束縛版の解説
ベースはマクロ版です．
修正した関数は interp と init-scheme-proc の2つです

** 関数 interp

修正箇所は2箇所です．
特殊形式の場合分けを行う case の lambda の部分において
scheme の closure を表す意味オブジェクトとしての Common Lisp の closure に
環境を渡すための引数 env を追加しました．
これは定義時環境ではなく，実行時環境を渡せるようにするためです．

--- 8< -----------------------------------------------------------------
       (scheme:lambda (let ((parms (second x))
                            (code (maybe-add 'scheme:begin (rest2 x))))
                        #'(lambda (env &rest args) ; ***
                            (interp code (extend-env parms args env)))))
--- >8 -----------------------------------------------------------------

つづいて同じく case 文の関数適用の部分です．
closure に対して実行時環境を渡します．これで動的スコープになります．

--- 8< -----------------------------------------------------------------
       (otherwise ;; a procedure application
        (apply (interp (first x) env)
               env                      ; ***
               (mapcar #'(lambda (v) (interp v env)) (rest x))))
--- >8 -----------------------------------------------------------------

** 関数 init-scheme-proc

元の実装では組み込み手続きも Common Lisp の closure で表されていて，
lambda 式から生成される closure と統一的に扱えるようになっています．
したがって，組み込み関数を表す closure にも環境を受け取るパラメータ env を追加します．
これは継続渡し版と同じやり方でできます．

--- 8< -----------------------------------------------------------------
(defun init-scheme-proc (f)
  "Define a Scheme primitive procedure as a CL function."
  (if (listp f)
      (set-global-var! (first f)
                       #'(lambda (env &rest args)         ; ***
                           (apply (second f) args)))
      (init-scheme-proc (list f f))))
--- >8 -----------------------------------------------------------------

* 静的／動的スコープ混在可能版の解説

** 仕様
*** 動的変数の導入
動的変数の導入は SICP にならい dynamic で行います．
lambda パラメータのうち，dynamic で修飾されているものが動的変数になります．
<lambda-param> ::= <lexical-param> | <dynamic-param>
<lexical-param> ::= <symbol>
<dynamic-param> ::= (dynamic <symbol>)

例
(lambda (x y (dynamic ratio)) expr ...)
x, y は lexical 変数
ratio は dynamic 変数

*** 動的変数の参照
動的変数の参照となるケースは3つあります
- もっとも内側の束縛が dynamic であるとき
- 変数が自由な出現であるとき
- dynamic-reference によるとき

いずれの場合も局所的な動的束縛が存在しない場合にはグローバル束縛を参照し，
グローバル束縛が存在しない場合はエラーとなります．

**** もっとも内側の束縛が dynamic であるとき
lexical に見たときに，もっとも内側の束縛が dynamic 変数であればそれを参照します．
すなわち dynamic 変数は lexical 変数のスコープを遮蔽します．

**** 変数が自由な出現であるとき
自由変数は動的束縛の参照とみなします．

**** dynamic-reference によるとき
(dynamic-reference <var>) という式は，同名の lexical 変数の有無に関係なく
もっとも直近の動的束縛を参照します．

** 設計

基本的に元の実装を尊重し，最低限の修正でわかりやすいと思われるものを選びました．

*** 2つの環境

静的束縛と動的束縛それぞれのために環境を用意します．
これを lenv, denv と呼びます．

コードの上の任意の任意の場所に置いて，変数が出現した場合
それが lexical 変数であるか dynamic 変数であるかが判別できる必要があります．
そこで lenv には束縛としての役割だけでなく，lexical scope 上での変数の区分も
記録することにします．

具体的な構造は次のとおりです：
<lexical-env> ::= (<lexical-binding-info> ...)
<lexical-binding-info> ::= <lexical-binding> | <dynamic-binding-info>
<lexical-binding> ::= (<var> :lexical <value>)
<dynamic-binding-info> ::= (<var> :dynamic)

例
lenv = ((x :lexical 2) (y :lexical 1) (x :lexical 0) (x :dynamic))

lenv に現れない変数は自由変数ということになります．

動的変数のための環境 denv は，もとの実装と同じく変数名と値のリストのリストです．

※ lenv も元実装と同じ構造にすることは可能ですが，環境拡大の際に遮蔽のための
   大局的な改変が必要になります．効率もかなり悪くなります．
   一部は closing 処理に移すことで軽減できますが，焼け石に水のレベルです．

*** Closure の表現

Closure の表現は末尾再帰版と同様に構造体を使います．
定義も同じです．
ただし printer は環境が見えるように変えました．

*** 関数 interp

関数 interp は2つの環境 lenv と denv を持ち回る形にします．

--- 8< -----------------------------------------------------------------
(defun interp (x &optional lenv denv)
  ...
--- >8 -----------------------------------------------------------------

**** 変数の参照

変数の参照においては，まず lenv を調べてその変数が lexical であるか dynamic
であるかを判別します．
lexical の場合は同時に束縛も得られるので，値を取り出します．
dynamic の場合は元の実装と同じく get-var で値を得ます．
こちらは自由変数であった場合を含みます．

--- 8< -----------------------------------------------------------------
    ((symbolp x)
     (let ((p (assoc x lenv)))
       (if (and p (eq (second p) :lexical))
           (third p)
           (get-var x denv))))
--- >8 -----------------------------------------------------------------

dynamic-reference ではかならず denv を参照します．

--- 8< -----------------------------------------------------------------
       (dynamic-reference (get-var (second x) denv))
--- >8 -----------------------------------------------------------------

**** 変数の更新

変数の更新も参照と同様に lenv で種類を識別して対応する環境上の束縛を変更します．

--- 8< -----------------------------------------------------------------
       (scheme:set!
        (let ((var (second x))
              (val (interp (third x) lenv denv)))
          (let ((p (assoc var lenv)))
            (if (and p (eq (second p) :lexical))
                (setf (third p) val)
                (set-var! var val denv)))))
--- >8 -----------------------------------------------------------------

**** lambda 式

lambda 式は proc 構造体にします．lenv を close します．

--- 8< -----------------------------------------------------------------
       (scheme:lambda (make-proc :env lenv :parms (second x)
                       :code (maybe-add 'scheme:begin (rest2 x))))
--- >8 -----------------------------------------------------------------

**** 手続き適用

手続きが proc 構造体である場合は関数 interp-proc を呼び出します．
この時点での動的変数を参照できるように，実行時環境 denv を渡します．

--- 8< -----------------------------------------------------------------
       (otherwise ;; a procedure application
        (let ((proc (interp (first x) lenv denv))
              (args (mapcar #'(lambda (v) (interp v lenv denv)) (rest x))))
          (if (proc-p proc)
              (interp-proc proc args denv)
              (apply proc args))))))))
--- >8 -----------------------------------------------------------------

*** 関数 interp-proc

2つの環境を関数 extend-env によって拡大してから，その元で手続き本体を評価します．

--- 8< -----------------------------------------------------------------
(defun interp-proc (proc args denv)
  (let ((envs (extend-env (proc-parms proc) args
						  (proc-env proc) denv)))
	(interp (proc-code proc) (first envs) (second envs))))
--- >8 -----------------------------------------------------------------

関数 extend-env は2つの値を返しますが，ここでは多値ではなくリストを使いました．
先生の御本では Scheme が11章，多値を本格的に解説するのが19章なので，説明の順序から
その方がよいかと思ったからです．
継続渡しも考えましたが，わからない人がいるかもしれないのでやめました．

*** 関数 extend-env

2つの環境を lambda パラメータと引数で拡大します．
2つの累積変数があるので，末尾再帰のループにしました．

順にパラメータと引数をみて環境を拡大します．
- dynamic がついていたら denv に束縛を作り，同時に lenv に :dynamic の識別を加える
- そうでなければ lenv に :lexical 束縛を加える

--- 8< -----------------------------------------------------------------
(defun extend-env (parms args lenv denv)
  (if (null parms)
      (list lenv denv)
      (let ((p (first parms)))
        (cond ((symbolp p)
               (extend-env (rest parms) (rest args)
                           (cons (list p :lexical (first args)) lenv)
                           denv))
              ((and (consp p) (eq (first p) 'dynamic))
               (let ((var (second p)))
                 (extend-env (rest parms) (rest args)
                             (cons (list var :dynamic) lenv)
                             (cons (list var (first args)) denv))))
              (t
               (error "invalid parameter specifier" p))))))
--- >8 -----------------------------------------------------------------

* テストについて

それぞれの版について，スコープに関するテストを書きました．

SBCL の場合は 

sbcl --script test-scheme-in-cl-dynamic.lisp
sbcl --script test-scheme-in-cl-mixable.lisp

で一括テストできます．
