package example

object UnsafeCalculator extends App {

	def calculator(operator: String): (Int, Int) => Int = operator match {
		case "multiply" => (a, b) => a * b
		case "divide" => (a, b) => a / b
		case "add" => (a, b) => a + b
		case "subtract" => (a, b) => a - b
		case other => throw new IllegalArgumentException(s"$other is not a valid operation")
	}

	println(calculator("multiply")(6, 2))
	println(calculator("divide")(6, 2))
	println(calculator("add")(6, 2))
	println(calculator("sabtract")(6, 2))

}
