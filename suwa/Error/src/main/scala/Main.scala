/**
 * Created by suwa-yuki on 2017/02/09.
 */
object Main {

  def main(args: Array[String]) {

    // Option

    val o: Option[String] = Option("hoge")
    println(o.getOrElse(""))

    // Match 式

    val result = o match {
      case Some(str) => str
      case None => "not matched"
    }
    println(result)

    // map の適用

    val mul = Some(3).map(_ * 3)
    println(mul)

    // None の可能性があるとき

    val n: Option[Int] = None
    println(n.map(_ * 3))

    println(Some(3).fold(throw new RuntimeException)(_ * 3))

    // 入れ子の解消

    val v1: Option[Int] = Some(3)
    val v2: Option[Int] = Some(5)
    val nested = v1.map(i1 => v2.map(i2 => i1 * i2))
    println(nested)

    val flatten = v1.map(i1 => v2.map(i2 => i1 * i2)).flatten
    println(flatten)

    // 練習問題
    val answer = Some(2).map {
      v1 => Some(3).map {
        v2 => Some(5).map {
          v3 => Some(7).map {
            v4 => Some(11).map {
              v5 => v1 * v2 * v3 * v4 * v5
            }
          }.flatten
        }.flatten
      }.flatten
    }.flatten
    println(answer)

    val list = List(Some(2), Some(3), Some(5), Some(7), Some(11))
      .foldLeft[Option[Int]](Some(1)) {
        (x, y) => x.flatMap { v1 => y.map { v2 => v1 * v2 } }
      }
    println(list)

    // Either

    val either1: Either[String, Int] = Right(123)
    val either2: Either[String, Int] = Left("abc")
    either1 match {
      case Right(i) => println(i)
      case Left(s)  => println(s)
    }

    // ログイン機能

    sealed trait LoginError
    // パスワードが間違っている場合のエラー
    case object InvalidPassword extends LoginError
    // nameで指定されたユーザーが見つからない場合のエラー
    case object UserNotFound extends LoginError
    // パスワードがロックされている場合のエラー
    case object PasswordLocked extends LoginError

    case class User(id: Long, name: String, password: String)

    object LoginService {
      def login(name: String, password: String): Either[LoginError, User] = {
        Right(new User(1, name, password))
      }
    }

    LoginService.login(name = "dwango", password = "password") match {
      case Right(user) => println(s"id: ${user.id}")
      case Left(InvalidPassword) => println(s"Invalid Password!")
    }

  }

}
