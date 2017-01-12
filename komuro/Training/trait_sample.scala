trait TraitA {
  def greet(): Unit
}

trait TraitB extends TraitA {
  def greet(): Unit = println("Good Morning TraitB")
}

trait TraitC extends TraitA {
  def greet(): Unit = println("Good Evening TraitC")
}

class ClassA extends TraitB with TraitC {
  override def greet(): Unit = println("Good Afternoon ClassA")
}

class ClassB extends TraitB with TraitC {
  override def greet(): Unit = super[TraitB].greet()
}

class ClassC extends TraitB with TraitC {
  override def greet(): Unit = {
    super[TraitB].greet()
    super[TraitC].greet()
  }
}

val a = new ClassA
a.greet()

val b = new ClassB
b.greet()

val c = new ClassC
c.greet()

// Traitでも適切な実装するとインスタンス化できるらしい

trait Greeter {
  def greetTo(to: String): String
}

trait AbstractEnglishGreeter extends Greeter {
  def name: String
  def greet: String
  def greetTo(to: String): String = s"${name} says '${greet} ${to}!'"
}

trait AbstractJapaneseGreeter extends Greeter {
  def name: String
  def greet: String
  def greetTo(to: String): String = s"${name} は'${greet} ${to}!'と言いました"
}

class EnglishGreeter(val name: String) extends AbstractEnglishGreeter {
  val greet: String = "Hello"
}

class JapaneseGreeter(val name: String) extends AbstractJapaneseGreeter {
  val greet: String = "こんにちは"
}

 val byTrait = new AbstractEnglishGreeter {
   def name = "Alice"
   def greet = "Hi"
 }
 println(byTrait.greetTo("Ben"))

val english = new EnglishGreeter("John")
println(english.greetTo("Emilly"))

val japanese = new JapaneseGreeter("イチロー")
println(japanese.greetTo("サミー"))
