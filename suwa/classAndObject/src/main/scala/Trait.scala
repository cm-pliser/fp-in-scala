object Trait {
  def main(args: Array[String]): Unit = {

    // ミックスイン

    trait TraitA

    trait TraitB

    class ClassA

    class ClassB

    // コンパイルできる
    class ClassC extends ClassA with TraitA with TraitB

    // ダメ
//    class ClassD extends ClassA with ClassB


    // ---

    // ダメ
//    object ObjectA {
//      val a = new TraitA
//    }

    trait TraitD

    class ClassD extends TraitD

    // クラスにすればインスタンス化できる
    object ObjectD {
      val d = new ClassD
    }


    // ---

    // トレイトにコンストラクタ引数は付けられない
//    trait TraitA(name: String)

    trait TraitE {
      val name: String
      def printName(): Unit = println(name)
    }

    // クラスにして name を上書きする
    class ClassE(val name: String) extends TraitA

    object ObjectE {
      val e = new ClassE("classmethod")

      // name を上書きするような実装を与えてもよい
      val e2 = new TraitE { val name = "hoge" }
    }


    // ---

    // 菱形継承問題

    trait TraitF {
      def greet(): Unit
    }

    trait TraitG extends TraitF {
      def greet(): Unit = println("Good morning!")
    }

    trait TraitH extends TraitF {
      def greet(): Unit = println("Good evening!")
    }

    // TraitG と TraitH のどっちかコンパイラはわからない
//    class ClassF extends TraitG with TraitH

    // オーバーライドする
    class ClassG extends TraitG with TraitH {
      override def greet(): Unit = println("How are you?")
    }
    (new ClassG).greet()

    // オーバーライドしつつ親を特定して実行する
    class ClassH extends TraitG with TraitH {
      override def greet(): Unit = super[TraitG].greet()
    }
    (new ClassH).greet()

    // オーバーライドしつつ両方の親をまとめて実行する
    class ClassI extends TraitG with TraitH {
      override def greet(): Unit = {
        super[TraitG].greet()
        super[TraitH].greet()
      }
    }
    (new ClassI).greet()

    // ---

    // 線形型

    trait TraitJ {
      def greet(): Unit
    }

    trait TraitK extends TraitJ {
      override def greet(): Unit = println("Good morning!")
    }

    trait TraitL extends TraitJ {
      override def greet(): Unit = println("Good evening!")
    }

    class ClassM extends TraitK with TraitL

    // 一番最後にミックスインした TraitL が優先される
    (new ClassM).greet()

    // ---

    // 線形型 (親を使う場合)

    trait TraitN {
      def greet(): Unit = println("Hello!")
    }

    trait TraitO extends TraitN {
      override def greet(): Unit = {
        super.greet()
        println("My name is Terebi-chan.")
      }
    }

    trait TraitP extends TraitN {
      override def greet(): Unit = {
        super.greet()
        println("I like niconico.")
      }
    }

    // TraitN -> TraitP -> TraitO の順番で実行
    class ClassN extends TraitO with TraitP
    (new ClassN).greet()

    // TraitN -> TraitO -> TraitP の順番で実行
    class ClassO extends TraitP with TraitO
    (new ClassO).greet()

    // ---

    // 自分型

    trait Greeter {
      def greet(): Unit
    }

    trait Robot {
      // 自分型
      self: Greeter =>

      // Greeter のメソッドが呼べる
      def start(): Unit = greet()
      override final def toString = "Robot"
    }

    // greet() を実装する
    trait HelloGreeter extends Greeter {
      def greet(): Unit = println("Hello!")
    }
    (new Robot with HelloGreeter).start()

    // 普通にトレイトを継承しているのと似ている
    trait Robot2 extends Greeter {
      def start(): Unit = greet()
      override final def toString = "Robot2"
    }
    (new Robot2 with HelloGreeter).start()

    

  }
}
