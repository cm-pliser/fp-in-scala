trait Greeter {
  def greet(): Unit
}

trait Robot {
  self: Greeter =>

  def start(): Unit = greet()
  override final def toString() = "Robot"
}

trait HelloGreeter extends Greeter {
  def greet(): Unit = println("Hello")
}

val r = new Robot with HelloGreeter
r.start()
