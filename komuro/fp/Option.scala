import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {

    /** Exercise 4-4 */

    /** Optionに値が存在すればOptionに包んでfを実行する。fはBしか返さないのでエラーを起こす可能性があるからSomeでくるむ */
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    /** mapと似てるけどこっちはfがOption[B]を返すので、そのまま実行すればOK */
    def flatMap[B](f:A => Option[B]): Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }

    /** Optionに値が存在すればその値を返す。存在しなければdefaultの値を返す */
    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }

    /** こっちは中身の値を返すのではなくてOption[B]を返すのでNoneじゃなければ自身を返す。どういう時につかうんだこれ。 */
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case _ => this
    }

    /** f(a)が真のやつだけ返す */
    def filter(f:A => Boolean): Option[A] = this match {
        case None => None
        case Some(a) => {
            if (f(a)) {
                this
            } else {
                None
            }
        }
    }

    /** ExceptionをCatchしたらNoneに変換する */
    def Try[A](a: => A): Option[A] = 
        try Some(a)
        catch {case e: Exception => None}
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

    /** Exercise 4.3 Option2つをとって一つにまとめる */
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
        case (Some(a1), Some(b1)) => Some(f(a1, b1))
        case _ => None
    }

    /** Exercise 4.4 */
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {

        def loop(rest: List[Option[A]], result:Option[List[A]]): Option[List[A]] = {
            rest match {
                case Cons(Some(x), Nil) => result.map(hoge => Cons(x, hoge))             // ループ終了 
                case Cons(Some(x), xs) => loop(xs, result.map(head => Cons(x, head)))    // 残りをループさせる
                case _ => None  // 何もなければNone
            }
        }
        
        loop(a, Some(List()))
    }
}