object MyModule {
	def main(args: Array[String]): Unit = {
		@annotation.tailrec // 末尾再帰を強制する
		def fib(n: Int): Int =
			if (n == 0)
				0
			else if (n == 1)
				1
			else
				// +があるので末尾再帰になっていない
				fib(n - 2) + fib(n - 1)

		println(fib(10))
	}
}
