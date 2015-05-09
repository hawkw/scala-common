def f(mod: Int, result: String)(i: Int): Option[String] = i % mod match {
  case 0 => Some(result)
  case _ => None
}

val fizz = f(3,"Fizz")
val buzz = f(5, "Buzz")
val fizzbuzz = f(15, "FizzBuzz")

(1 until 100) map fizzbuzz getOrElse(fizz) getOrElse(buzz)
