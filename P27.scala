object P27 {
  def main(args: Array[String]): Unit = {
    
		// Задача 27 Сгруппируйте элементы множества в непересекающиеся подмножества
    def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
      ls match {
        case Nil                   => Nil
        case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
      }

    def combinations[A](n: Int, ls: List[A]): List[List[A]] =
      if (n == 0) List(Nil)
      else
        flatMapSublists(ls) { sl =>
          combinations(n - 1, sl.tail) map { sl.head :: _ }
        }

    def group3[A](ls: List[A]): List[List[List[A]]] =
      for {
        a <- combinations(2, ls)
        noA = ls diff a
        b <- combinations(3, noA)
      } yield List(a, b, noA diff b)

    def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
      case Nil => List(Nil)
      case n :: ns =>
        combinations(n, ls) flatMap { c =>
          group(ns, ls diff c) map { c :: _ }
        }
    }

    // Проверка
    println(
      "Задача 27 Сгруппируйте элементы множества в непересекающиеся подмножества\n---"
    )
    println(
      "Проверка: group3(List(\"Aldo\", \"Beat\", \"Carla\", \"David\", \"Evi\", \"Flip\", \"Gary\", \"Hugo\", \"Ida\"))"
    )
    print("Результат: ")
    println(
      group3(
        List(
          "Aldo",
          "Beat",
          "Carla",
          "David",
          "Evi",
          "Flip",
          "Gary",
          "Hugo",
          "Ida"
        )
      ) + "\n"
    )
  }
}
