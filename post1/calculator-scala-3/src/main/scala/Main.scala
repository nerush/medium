import scala.annotation.implicitNotFound

object SafeCalculator extends App {

	trait FunctionD[I, O] {
		type Input  = I
		type Output = O
		def apply: Input => Output
	}

	@implicitNotFound("${T} is not a valid operation")
	trait LiteralFunctionD[T <: String with Singleton] {
		type Input
		type Output
		val function: FunctionD[Input, Output]
	}

	type LDP[T <: String with Singleton, I, O] = LiteralFunctionD[T] {type Input = I; type Output = O}

	def define[T <: String with Singleton : ValueOf, I, O](f: I => O): LDP[T, I, O] =
		new LiteralFunctionD[T] {
			type Input = I
			type Output = O
			val function =
				new FunctionD[Input, Output] {
					val apply: Input => Output = (i: I) => f(i)
				}
		}

	def calculator[T <: String with Singleton](given p: LiteralFunctionD[T]): (it: p.Input) => p.Output = p.function.apply

	given add: LDP["add", (Int, Int), Int] = define(x => x._1 + x._2)
	given subtract: LDP["subtract", (Int, Int), Int] = define(x => x._1 - x._2)
	given multiply: LDP["multiply", (Int, Int), Int] = define(x => x._1 * x._2)
	given divide: LDP["divide", (Int, Int), Int] = define(x => x._1 / x._2)

	println(calculator["multiply"](6, 2))
	println(calculator["divide"](6, 2))
	println(calculator["add"](6, 2))
	println(calculator["subtract"](6, 2))


}

