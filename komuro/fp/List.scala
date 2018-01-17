sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    /** コンストラクタ */
    def apply[A](as: A*): List[A] = // Variadic function syntax
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    /** 足し算 */
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    /** 掛け算 */
    def product(ds: List[Double]): Double = ds match {
        case Nil => 1
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    /** Exercise 3-1 */
    val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
    }

    /** Exercise 3-2 */
    def tail[A](list: List[A]): List[A] = list match {
        case Nil => throw new RuntimeException("Input Must List")
        case Cons(_, xs) => xs
    }

    /** Exercise 3-3 */
    def setHead[A](head: A, list: List[A]): List[A] = list match {
        case Nil => throw new RuntimeException("Can not set empty list")
        case Cons(_, xs) => Cons(head, xs)
    }

    /** Exercise 3-4 */
    def drop[A](l: List[A], n:Int): List[A] = {
        def loop(rest: List[A], count: Int): List[A] = 
        if(count == 0) {
            rest
        } else { 
            rest match {
                case Nil => throw new RuntimeException("Index bound")
                case Cons(_, xs) => loop(xs, count - 1)
            }
        }
        loop(l, n)
    }

    /** Exercise 3-5 */
    def dropWhile[A](l:List[A], f: A => Boolean): List[A] = l match {
        case Nil => throw new RuntimeException("can not drop empty List")
        case Cons(x, xs) => {
            if(f(x)) {
                dropWhile(xs, f)
            } else {
                l
            }
        }
    }

    /** Exercise 3-5 模範解答 */
    // def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    //     l match {
    //         case Cons(h,t) if f(h) => dropWhile(t, f)
    //         case _ => l
    //     }

    /** List連結 */
    def append[A](a1: List[A], a2: List[A]): List[A] = 
        a1 match {
            case Nil => a2
            case Cons(h, t) => Cons(h, append(t, a2))
        }

    /** Exercise 3-6 */
    def init[A](l: List[A]):List[A] = {
        def loop(rest: List[A], result: List[A]): List[A] = {
            rest match {
                case Nil => result
                case Cons(x, Nil) => result
                case Cons(x, xs) => Cons(x, loop(xs, result))
            }
        }
        loop(l, Nil:List[A])
    }

    /** Curry化dropWhile */
    def curryDropWhile[A](as: List[A])(f: A => Boolean): List[A] = 
        as match {
            case Cons(h, t) if f(h) => curryDropWhile(t)(f)
            case _ => as
        }

    /** foldRight */
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        println(s"foldRight: ${as}")
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    }
        
    /** foldRightで足し算 */
    def sum2(ns: List[Int]) = 
        foldRight(ns, 0)((x, y) => x + y)

    /** foldRightで掛け算 */
    def product2(ns: List[Double]) = 
        foldRight(ns, 1.0)(_ * _)

    /** Exercise 3-8 */
    def ex3_8() = 
        foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)) // 順序そのままのCons(1, Cons(2, Cons(3, Nil))) が出力される
        // 1, foldRight(2, 3)
        // 1, 2, foldRight(3)
        // 1, 2, Cons(3, Nil:List[Int])
        // 1, Cons(2, Cons(3, Nil:List[Int]))
        // Cons(1, Cons(2, Cons(3, Nil:List[Int])))

    /** Exercise 3-9 */
    def length[A](as: List[A]): Int = 
        foldRight(as, 0)((head, result) => result + 1)

    /** Exercise 3-10 */
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
        println(s"foldLeft: ${as}, Output: ${z}")
        as match {
            case Nil => z
            case Cons(h, t) => foldLeft(t, f(z, h))(f)
        }
    }

    /** Exercise 3-11. 足し算 by foldLeft */
    def sum3(ns: List[Int]) = 
        foldLeft(ns, 0)((x, y) => y + x)

    /** Exercise 3-11. 掛け算 by foldLeft */
    def product3(ns: List[Int]) = 
        foldLeft(ns, 1.0)((x, y) => y * x)

    /** Exercise 3-11. 長さ計算 by foldLeft */
    def length2[A](ns: List[A]) = 
        foldLeft(ns, 0)((x, _) => x + 1)

    /** Exercise 3-12 */
    def reverse[A](ns: List[A]) = 
        foldLeft(ns, Nil:List[A])((x, y) => Cons(y, x))

    /** Exercise 3-13. 難問1 */
    def foldRightByfoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        foldLeft(reverse(as), z)((b, a) => f(a, b))
    }

    /** わからなかったので模範解答 */
    def foldRightByfoldLeft_1[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
        foldLeft(as, (b:B) => b)((g, a) => b => g(b))(z)
    }

    /** わからなかったので模範解答 */
    def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
        foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

    def apeendByfoldLeft[A](a1: List[A], a2: List[A]): List[A] = 
        foldLeft(a1, a2)((b, a) => Cons(a, b))  // 順番がおかしくなりますね。
        // (2, 4, 6, 8)
        // (1, 3, 5, 7) を左から処理してConsするため。tailにつければOK？

    /** foldRightを利用したAppend */
    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
        foldRight(l, r)(Cons(_,_))
    
    /** Exercise 3-16 */
    def add1(list: List[Int]): List[Int] = 
        foldRight(list, Nil:List[Int])((a, b) => Cons(a + 1, b))
    
    /** Exercise 3-17 */
    def listToString(list: List[Double]): List[String] = 
        foldRight(list, Nil:List[String])((a, b) => Cons(a.toString(), b))

    /** Exercise 3-18 */
    def map[A, B](as: List[A])(f:A => B): List[B] = 
        foldLeft(as, Nil:List[B])((result, item) => Cons(f(item), result))

    /** Exercise 3-19 */
    def filter[A](as: List[A])(f: A => Boolean): List[A] = 
        foldLeft(as, Nil:List[A])((result, item) => {
            if (f(item)) Cons(item, result) else result
        })

    /** Exercise 3-20 */
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = 
        
}