class APrinter {
  def print(): Any = {
    println("A")
  }
}

class BPrinter extends APrinter {
  override def print(): Unit = {
    println("B")
  }
}

class CPrinter extends APrinter {
  override def print(): String = {
    "C"
  }
}

def main():Unit = {
  // super class
  new APrinter().print()
  // sub class
  new BPrinter().print()
  // sub class not override
  new CPrinter().print()
}

main()
