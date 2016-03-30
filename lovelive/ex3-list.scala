package ex3_list

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(y, ys) => ys
    case _ => Nil
  }

  def _tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, as) => Cons(a, as)
  } // 模範解答では引数２項の順序が逆だった
  
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (as, 0) => as
    case (Cons(_, t), n) => drop(t, n - 1) 
    case (Nil, _) => sys.error("dropping empty list")
  }
  
  // 前提条件が誤ってるとはいえないので、errorではなくNilが妥当だそうだ
  // 伝統的なdropのりよう方法を鑑みると、throwしてると面倒だとも
  // ところで@tailrec化されてないんですがそれは
  def _drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => _drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(a, t) if f(a) => dropWhile(t, f)
    case as => as // おしかった。模範解答では case _ => l たしかに
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(a, t) => Cons(a, init(t)) // aよりhが好きらしい
  } // クソほど効率悪いからリストバッファで作ってI/FだけFPにしようってさ
  // RT: Referential Transparency, Referen[t]ial なの注意だな
  
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  
  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  // メソッド引数名以外はOk てかテキストどうりやってんけど
  def length[A](as: List[A]): Int =
    foldRight(as, 0) { (_, acc) => acc + 1 }

  // 解けなかった。 foldLeftの展開後の姿がイメージできてない
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(l: List[A], zr: B): B = l match {
      case Nil => zr
      case Cons(x, xs) => loop(xs, f(zr, x)) // 解けなかった
    }
    loop(as, z)
  }

  @annotation.tailrec
  def _foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    // foldLeftは、自身のzを更新していく。再帰呼び出しに渡す引数zにfを適用していく。
    // 次の再帰呼び出しではzがf適用後の値になっているため、f(acc, x)のaccとして正しく
    // 機能する。
    // 一回目の評価は f(単位元, 先頭要素) であり、次の評価は
    // その結果値を新たな単位元として、残りの要素にたいして自身を呼び出すことである
    // 式で言えば foldLeft(残り要素, f(単位元, 先頭要素)) となる。
    // 展開して、 foldLeft(さらに残り要素, f(f(単位元, 先頭要素), ２番めの要素))
    // 次の評価は f(f(単位元, 先頭要素), ２番めの要素) である。
    // f(単位元, 先頭要素) は先程評価済みであるから
    // f(評価済みの値, ２番めの要素) が実際の式である。
    // foldLeft の再帰呼び出しの際に、fが評価されている必要があるため
    // fの評価は再帰呼出しよりも先に行われるのである。
    // つまり、foldLeftが再帰をやめた時には全ての評価が終了しているのである。
    // その時返すのは z 。冒頭で述べたとおり z は最新の結果値として更新されていくので
    // すなわち最新の結果地を返して foldLeft は終了するのだ。
    case Cons(h,t) => _foldLeft(t, f(z,h))(f)
    // 対してfoldRightはf(x, acc)のaccを更新していく。
    // fのaccに自身の再帰呼び出しの結果値を渡す。
    // まず、すべての f と foldRight 呼び出しが評価されようとする。これは、fの評価に
    // foldRightの結果値が不可欠だから、そして foldRight は f の評価値を返すから。
    // 最初に結果を返すのは foldRight(空リスト, 単位元)(f) で、これは単位元を返す
    // 次に結果を返すのは f(末尾要素, 単位元) で、accが単位元なのは上述の結果値である
    // このfの評価値は、１階層上の foldRight(Cons(末尾要素, Nil), 単位元)(f) の評価の
    // 結果値にほかならない。
    // よって次に結果を返すのは上述の一階層上の foldRight であり、
    // それは f(末尾要素, 単位元) を返す。ならば
    // その次に結果を返すのは f(末尾から2番目の要素, f(末尾要素, 単位元)) だ
    // かくして、末尾要素から順に f が連続適用される、とも言えるのである。
    // case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
    foldLeftは、 f(acc, x) の結果値を新たな zero とおいた、foldLeftの再帰呼び出し。
    foldRightは、foldRight(xs, z)(f) == f(_, _) | z を acc とおいた、fの再帰呼び出し。

    foldLeftがfoldLeftの再帰呼び出しなのは理解しやすい。
    foldRightがfの再帰呼び出しなのは、 foldRight がすなわち f か z値 のみを取ると
    理解しなければならないので難しい。
    */
  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0) { (acc, _) => acc + 1 }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) { (acc, x) => Cons(x, acc) }

  // ex3.13
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z) { (acc, x) => f(x, acc) } 

  // ex3.13
  // foldLeftを用いたfoldRight, foldRightを用いたfoldLeftどちらも可能
  // 前述 ex3.10 のfoldRight2はfoldLeftを使ったfoldRightの実装の一つ（scala.collection.immutable.ListのfoldRight実装はこれ）
  // それ以外の手法としてはアキュームレータを関数にしてゼロを恒等写像にした適用法で相互に置換できる
  def foldLeft3[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    val go: B => B = foldRight(l, (b: B) => b) { (a, acc) =>
      (bb: B) => acc(f(bb, a))
    }
    go(z)
  }
  def foldRight3[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    val go: B => B = foldLeft(l, (b: B) => b) { (acc, a) =>
      (bb: B) => acc(f(a, bb))
    }
    go(z)
  }
  // ただしこいつらはスタックセーフじゃない

  def tasuman(ns: List[Int]): List[Int] = 
    foldRight(ns, Nil: List[Int]) { (n, acc) => Cons(n + 1, acc) }
}

