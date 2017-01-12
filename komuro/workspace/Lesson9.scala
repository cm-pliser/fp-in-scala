object Lesson9 {
  def reverse[T](list: List[T]): List[T] = {
    // 初期値はList[T]の空配列。
    // yが入力値でxが途中の値なので、xにyを連結させる
    list.foldLeft(List[T]())((x, y) => y :: x)
  }

  // foldRightだとツラたん
  def reverse_r[T](list: List[T]): List[T] = list.foldRight(List[T]())((x, y) =>  x :: y)

  // fold-rightを使って全部足しあわせ
  def sum(list: List[Int]): Int = {
    list.foldRight(0)((x, y) => y + x)
  }

  // fold-rightを使って全部掛け合わせ
  def mul(list :List[Int]): Int = list.foldRight(1)((x, y) => y * x)

  // セパレータを指定して要素連結(あんまりScalaっぽくない)
  def mkString[T](list: List[T])(sep: String): String = {
    // 左から右への要素連結なのでfoldLeft
    list.foldLeft("")((x, y) => {
      if (x.length == 0) x + y // 途中の値のxが空の場合はセパレータいらない
      else x + sep + y  // 通常はセパレータつけて連結
    })
  }

  // 模範解答はこっち
  def mkString_c[T](list: List[T])(sep: String): String = list match {
    case Nil => ""
    case x :: xs => xs.foldLeft(x.toString)((x, y) => x + sep + y)
  }

}
