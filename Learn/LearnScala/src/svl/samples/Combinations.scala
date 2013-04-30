package svl.samples

object Combinations extends App {
	val l = List(1,2,3, 4)
	val c = combs(l)

	println(c)

	def combs[T](items:List[T]):List[List[T]] = {
		def combine(item: T, res: List[List[T]]): List[List[T]] = {
			List(item) :: res ::: res.map(item :: _)
		}

		def iter (items:List[T], res:List[List[T]]):List[List[T]] = {
			items match {
				case List() => res
				case head::tail => iter(tail, combine(head, res))
			}
		}

		iter(items, List())
	}
}
