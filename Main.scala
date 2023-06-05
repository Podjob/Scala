object Main {
  def main(args: Array[String]): Unit = {
    
    // Задача 1 Найдите последний элемент списка
    // 1 способ
    def lastBuiltin[A](ls: List[A]): A = ls.last
    
    // 2 способ
    def lastRecursive[A](ls: List[A]): A = ls match {
      case h :: Nil  => h
      case _ :: tail => lastRecursive(tail)
      case _         => throw new NoSuchElementException
    }
    
    // Проверка
    println("\nЗадача 1 Найдите последний элемент списка\n---")
    println("Проверка: lastBuiltin(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(lastBuiltin(List(1, 1, 2, 3, 5, 8)))
    println("Проверка: lastRecursive(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(lastRecursive(List(1, 1, 2, 3, 5, 8)) + "\n")

    // Задача 2 Найдите предпоследний элемент списка
    // 1 способ
    def penultimateBuiltin[A](ls: List[A]): A =
      if (ls.isEmpty) throw new NoSuchElementException
      else ls.init.last

		// 2 способ
    def penultimateRecursive[A](ls: List[A]): A = ls match {
      case h :: _ :: Nil => h
      case _ :: tail     => penultimateRecursive(tail)
      case _             => throw new NoSuchElementException
    }
    
    // Проверка
    println("Задача 2 Найдите предпоследний элемент списка\n---")
    println("Проверка: penultimateBuiltin(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(penultimateBuiltin(List(1, 1, 2, 3, 5, 8)))
    println("Проверка: penultimateRecursive(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(penultimateRecursive(List(1, 1, 2, 3, 5, 8)) + "\n")

    // Задача 3 Найдите k-ый элемент списка
    // 1 способ
    def nthBuiltin[A](n: Int, ls: List[A]): A =
      if (n >= 0) ls(n)
      else throw new NoSuchElementException

		// 2 способ
    def nthRecursive[A](n: Int, ls: List[A]): A = (n, ls) match {
      case (0, h :: _)    => h
      case (n, _ :: tail) => nthRecursive(n - 1, tail)
      case (_, Nil)       => throw new NoSuchElementException
    }

    // Проверка
    println("Задача 3 Найдите k-ый элемент списка\n---")
    println("Проверка: nthBuiltin(2, List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(nthBuiltin(2, List(1, 1, 2, 3, 5, 8)))
    println("Проверка: nthRecursive(2, List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(nthRecursive(2, List(1, 1, 2, 3, 5, 8)) + "\n")

    // Задача 4 Найдите количество элементов списка
    // 1 способ
    def lengthBuiltin[A](ls: List[A]): Int = ls.length

		// 2 способ
    def lengthRecursive[A](ls: List[A]): Int = ls match {
      case Nil       => 0
      case _ :: tail => 1 + lengthRecursive(tail)
    }

		// 3 способ
    def lengthTailRecursive[A](ls: List[A]): Int = {
      def lengthR(result: Int, curList: List[A]): Int = curList match {
        case Nil       => result
        case _ :: tail => lengthR(result + 1, tail)
      }
      lengthR(0, ls)
    }

		// 4 способ
    def lengthFunctional[A](ls: List[A]): Int = ls.foldLeft(0) { (c, _) =>
      c + 1
    }

    // Проверка
    println("Задача 4 Найдите количество элементов списка\n---")
    println("Проверка: lengthBuiltin(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(lengthBuiltin(List(1, 1, 2, 3, 5, 8)))
    println("Проверка: lengthRecursive(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(lengthRecursive(List(1, 1, 2, 3, 5, 8)))
    println("Проверка: lengthTailRecursive(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(lengthTailRecursive(List(1, 1, 2, 3, 5, 8)))
    println("Проверка: lengthFunctional(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(lengthFunctional(List(1, 1, 2, 3, 5, 8)) + "\n")

    // Задача 5 Переверните список
    // 1 способ
    def reverseBuiltin[A](ls: List[A]): List[A] = ls.reverse

		// 2 способ O(n^2)
    def reverseRecursive[A](ls: List[A]): List[A] = ls match {
      case Nil       => Nil
      case h :: tail => reverseRecursive(tail) ::: List(h)
    }

		// 3 способ
    def reverseTailRecursive[A](ls: List[A]): List[A] = {
      def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
        case Nil       => result
        case h :: tail => reverseR(h :: result, tail)
      }
      reverseR(Nil, ls)
    }

		// 4 способ
    def reverseFunctional[A](ls: List[A]): List[A] =
      ls.foldLeft(List[A]()) { (r, h) => h :: r }

    // Проверка
    println("Задача 5 Переверните список\n---")
    println("Проверка: reverseBuiltin(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(reverseBuiltin(List(1, 1, 2, 3, 5, 8)))
    println("Проверка: reverseRecursive(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(reverseRecursive(List(1, 1, 2, 3, 5, 8)))
    println("Проверка: reverseTailRecursive(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(reverseTailRecursive(List(1, 1, 2, 3, 5, 8)))
    println("Проверка: reverseFunctional(List(1, 1, 2, 3, 5, 8))")
    print("Результат: ")
    println(reverseFunctional(List(1, 1, 2, 3, 5, 8)) + "\n")

    // Задача 6 Определите, является ли список палиндромом
    // 1 способ
    def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

    // 2 способ
    def isPalindromeSecond[A](l: List[A]): Boolean = l match {
      case Nil     => true
      case List(a) => true
      case list    => (list.head == list.last && isPalindrome(list.tail.init))
    }

    // Проверка
    println("Задача 6 Определите, является ли список палиндромом\n---")
    println("Проверка: isPalindrome(List(1, 2, 3, 2, 1))")
    print("Результат: ")
    println(isPalindrome(List(1, 2, 3, 2, 1)))
    println("Проверка: isPalindromeSecond(List(1, 2, 3, 2, 3))")
    print("Результат: ")
    println(isPalindromeSecond(List(1, 2, 3, 2, 3)) + "\n")

    // Задача 7 Преобразуйте многомерный список в одномерный
    def flatten(ls: List[Any]): List[Any] = ls flatMap {
      case ms: List[_] => flatten(ms)
      case e           => List(e)
    }

    // Проверка
    println("Задача 7 Преобразуйте многомерный список в одномерный\n---")
    println("Проверка: flatten(List(List(1, 1), 2, List(3, List(5, 8))))")
    print("Результат: ")
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) + "\n")

    // Задача 8 Замените серии одинаковых элементов списка на одиночный элемент
    // 1 способ
    def compressRecursive[A](ls: List[A]): List[A] = ls match {
      case Nil       => Nil
      case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
    }

		// 2 способ
    def compressTailRecursive[A](ls: List[A]): List[A] = {
      def compressR(result: List[A], curList: List[A]): List[A] =
        curList match {
          case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
          case Nil       => result.reverse
        }
      compressR(Nil, ls)
    }

		// 3 способ
    def compressFunctional[A](ls: List[A]): List[A] =
      ls.foldRight(List[A]()) { (h, r) =>
        if (r.isEmpty || r.head != h) h :: r
        else r
      }

    // Проверка
    println(
      "Задача 8 Замените серии одинаковых элементов списка на одиночный элемент\n---"
    )
    println(
      "Проверка: compressRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))"
    )
    print("Результат: ")
    println(
      compressRecursive(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      )
    )
    println(
      "Проверка: compressTailRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))"
    )
    print("Результат: ")
    println(
      compressTailRecursive(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      )
    )
    println(
      "Проверка: compressFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))"
    )
    print("Результат: ")
    println(
      compressFunctional(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) + "\n"
    )

    // Задача 9 Замените серию одинаковых элементов на список из этих элементов
    def pack[A](ls: List[A]): List[List[A]] = {
      if (ls.isEmpty) List(List())
      else {
        val (packed, next) = ls span { _ == ls.head }
        if (next == Nil) List(packed)
        else packed :: pack(next)
      }
    }

    // Проверка
    println(
      "Задача 9 Замените серию одинаковых элементов на список из этих элементов\n---"
    )
    println(
      "Проверка: pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))"
    )
    print("Результат: ")
    println(
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) + "\n"
    )

    // Задача 10 Дан список. Замените серию одинаковых элементов на кортеж из элемента серии и длину серии.
    def encode[A](ls: List[A]): List[(Int, A)] =
      pack(ls) map { e => (e.length, e.head) }

    // Проверка
    println(
      "Задача 10 Дан список. Замените серию одинаковых элементов на кортеж из элемента серии и длину серии.\n---"
    )
    println(
      "Проверка: encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))"
    )
    print("Результат: ")
    println(
      encode(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) + "\n"
    )

    // Задача 11 Дан список. Замените серию одинаковых элементов на кортеж из элемента серии и длину серии, но если серия состоит из одного элемента, то оставьте элемент без замены на кортеж.
    // 1 способ
    def encodeModified[A](ls: List[A]): List[Any] =
      encode(ls) map { t => if (t._1 == 1) t._2 else t }

		// 2 способ
    def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
      encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }

    // Проверка
    println(
      "Задача 11 Дан список. Замените серию одинаковых элементов на кортеж из элемента серии и длину серии, но если серия состоит из одного элемента, то оставьте элемент без замены на кортеж.\n---"
    )
    println(
      "Проверка: encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))"
    )
    print("Результат: ")
    println(
      encodeModified(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      )
    )
    println(
      "Проверка: encodeModified2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))"
    )
    print("Результат: ")
    println(
      encodeModified2(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) + "\n"
    )

    // Задача 12 Дан список из кортежей вида ('a, k). Получите список, полученный преобразований кортежей в серию элементов a указанной длины k.
    def decode[A](ls: List[(Int, A)]): List[A] =
      ls flatMap { e => List.fill(e._1)(e._2) }

    // Проверка
    println(
      "Задача 12 Дан список из кортежей вида ('a, k). Получите список, полученный преобразований кортежей в серию элементов a указанной длины k.\n---"
    )
    println(
      "Проверка: decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))"
    )
    print("Результат: ")
    println(
      decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) + "\n"
    )

    // Задача 13 Дан список. Замените серию одинаковых элементов на кортеж из элемента серии и длину серии
    def encodeDirect[A](ls: List[A]): List[(Int, A)] =
      if (ls.isEmpty) Nil
      else {
        val (packed, next) = ls span { _ == ls.head }
        (packed.length, packed.head) :: encodeDirect(next)
      }

    // Проверка
    println(
      "Задача 13 Дан список. Замените серию одинаковых элементов на кортеж из элемента серии и длину серии\n---"
    )
    println(
      "Проверка: encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))"
    )
    print("Результат: ")
    println(
      encodeDirect(
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      ) + "\n"
    )

    // Задача 14 Продублируйте каждый элемент в списке
    // 1 способ
    def duplicate[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }

    // 2 способ
    def dubl(l: List[Int]): List[Int] = l match {
      case Nil    => Nil
      case h :: t => h :: h :: dubl(t)
    }

    // Проверка
    println("Задача 14 Продублируйте каждый элемент в списке\n---")
    println("Проверка: duplicate(List('a, 'b, 'c, 'c, 'd))")
    print("Результат: ")
    println(duplicate(List('a, 'b, 'c, 'c, 'd)))
    println("Проверка: dubl(List('a, 'b, 'c, 'c, 'd))")
    print("Результат: ")
    println(duplicate(List('a, 'b, 'c, 'c, 'd)) + "\n")

    // Задача 15 Продублируйте каждый элемент в списке данное количество раз
    def dublN(n: Int, l: List[Symbol]): List[Symbol] =
      l.flatMap(e => List.fill(n)(e))

    // Проверка
    println(
      "Задача 15 Продублируйте каждый элемент в списке данное количество раз\n---"
    )
    println("Проверка: duplicateN(3, List('a, 'b, 'c, 'c, 'd))")
    print("Результат: ")
    println(dublN(3, List('a, 'b, 'c, 'c, 'd)) + "\n")

    // Задача 16 Удалите каждый n-ый элемент из списка
    // 1 способ
    def dropN(n: Int, l: List[Symbol]): List[Symbol] = {
      l.zipWithIndex.filter(pair => (1 + pair._2) % n != 0).map(_._1)
    }

    // 2 способ
    def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
      def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
        case (_, Nil)       => Nil
        case (1, _ :: tail) => dropR(n, tail)
        case (_, h :: tail) => h :: dropR(c - 1, tail)
      }
      dropR(n, ls)
    }

		// 3 способ
    def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
      def dropR(c: Int, curList: List[A], result: List[A]): List[A] =
        (c, curList) match {
          case (_, Nil)       => result.reverse
          case (1, _ :: tail) => dropR(n, tail, result)
          case (_, h :: tail) => dropR(c - 1, tail, h :: result)
        }
      dropR(n, ls, Nil)
    }

    // Проверка
    println("Задача 16 Удалите каждый n-ый элемент из списка\n---")
    println(
      "Проверка: dropN(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(dropN(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(
      "Проверка: dropRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(dropRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(
      "Проверка: dropTailRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(
      dropTailRecursive(
        3,
        List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      ) + "\n"
    )

    // Задача 17 Разделите список на две части (длина первой части задана)
    // 1 способ
    def splitBuiltin[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)

		// 2 способ
    def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) =
      (n, ls) match {
        case (_, Nil)  => (Nil, Nil)
        case (0, list) => (Nil, list)
        case (n, h :: tail) => {
          val (pre, post) = splitRecursive(n - 1, tail)
          (h :: pre, post)
        }
      }

		// 3 способ
    def splitTailRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = {
      def splitR(curN: Int, curL: List[A], pre: List[A]): (List[A], List[A]) =
        (curN, curL) match {
          case (_, Nil)       => (pre.reverse, Nil)
          case (0, list)      => (pre.reverse, list)
          case (n, h :: tail) => splitR(n - 1, tail, h :: pre)
        }
      splitR(n, ls, Nil)
    }

		//4 способ
    def splitFunctional[A](n: Int, ls: List[A]): (List[A], List[A]) =
      (ls.take(n), ls.drop(n))

    // Проверка
    println(
      "Задача 17 Разделите список на две части (длина первой части задана)\n---"
    )
    println(
      "Проверка: splitBuiltin(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(splitBuiltin(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(
      "Проверка: splitRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(splitRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(
      "Проверка: splitTailRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(
      splitTailRecursive(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
    println(
      "Проверка: splitFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(
      splitFunctional(
        3,
        List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      ) + "\n"
    )

    // Задача 18 Дан список и два индекса k и p. Получите фрагмент из списка с k-ого по p-ый (не включая p-ый)
    // 1 способ
    def sliceBuiltin[A](start: Int, end: Int, ls: List[A]): List[A] =
      ls.slice(start, end)

		// 2 способ
    def sliceRecursive[A](start: Int, end: Int, ls: List[A]): List[A] =
      (start, end, ls) match {
        case (_, _, Nil)                 => Nil
        case (_, e, _) if e <= 0         => Nil
        case (s, e, h :: tail) if s <= 0 => h :: sliceRecursive(0, e - 1, tail)
        case (s, e, h :: tail)           => sliceRecursive(s - 1, e - 1, tail)
      }

		// 3 способ
    def sliceTailRecursive[A](start: Int, end: Int, ls: List[A]): List[A] = {
      def sliceR(count: Int, curList: List[A], result: List[A]): List[A] =
        (count, curList) match {
          case (_, Nil)                     => result.reverse
          case (c, h :: tail) if end <= c   => result.reverse
          case (c, h :: tail) if start <= c => sliceR(c + 1, tail, h :: result)
          case (c, _ :: tail)               => sliceR(c + 1, tail, result)
        }
      sliceR(0, ls, Nil)
    }

		//4 способ
    def sliceTailRecursive2[A](start: Int, end: Int, ls: List[A]): List[A] = {
      def sliceR(count: Int, curList: List[A], result: List[A]): List[A] = {
        if (curList.isEmpty || count >= end) result.reverse
        else
          sliceR(
            count + 1,
            curList.tail,
            if (count >= start) curList.head :: result
            else result
          )
      }
      sliceR(0, ls, Nil)
    }

		//5 способ
    def sliceFunctional[A](s: Int, e: Int, ls: List[A]): List[A] =
      ls drop s take (e - (s max 0))

    // Проверка
    println(
      "Задача 18 Дан список и два индекса k и p. Получите фрагмент из списка с k-ого по p-ый (не включая p-ый)\n---"
    )
    println(
      "Проверка: sliceBuiltin(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(
      sliceBuiltin(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
    println(
      "Проверка: sliceRecursive(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(
      sliceRecursive(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
    println(
      "Проверка: sliceTailRecursive(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(
      sliceTailRecursive(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    )
    println(
      "Проверка: sliceTailRecursive2(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(
      sliceTailRecursive2(
        3,
        7,
        List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      )
    )
    println(
      "Проверка: sliceFunctional(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(
      sliceFunctional(
        3,
        7,
        List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      ) + "\n"
    )

    // Задача 19 Осуществите циклический сдвиг элементов списка на данное количество символов
    def rotate[A](n: Int, ls: List[A]): List[A] = {
      val nBounded = if (ls.isEmpty) 0 else n % ls.length
      if (nBounded < 0) rotate(nBounded + ls.length, ls)
      else (ls drop nBounded) ::: (ls take nBounded)
    }

    // Проверка
    println(
      "Задача 19 Осуществите циклический сдвиг элементов списка на данное количество символов\n---"
    )
    println(
      "Проверка: rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(
      "Проверка: rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))"
    )
    print("Результат: ")
    println(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) + "\n")

    // Задача 20 Удалите k-ый элемент  списка. Функция должна вернуть новый список и удаленный элемент
    // 1  способ
    def removeAt(index: Int, l: List[Any]): (List[Any], Any) = {
      if (index > l.length - 1)
        (l, Nil)
      else
        (
          l.take(index) ::: l.drop(index + 1),
          l(index)
        )
    }

    // 2 способ
    def removeAt2[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
      case (Nil, _) if n < 0 => throw new NoSuchElementException
      case (pre, e :: post)  => (pre ::: post, e)
      case (pre, Nil)        => throw new NoSuchElementException
    }

		// 3 способ
    def removeAt3[A](n: Int, ls: List[Any]): (List[Any], Any) =
      if (n < 0) throw new NoSuchElementException
      else
        (n, ls) match {
          case (_, Nil)       => throw new NoSuchElementException
          case (0, h :: tail) => (tail, h)
          case (_, h :: tail) => {
            val (t, e) = removeAt(n - 1, ls.tail)
            (ls.head :: t, e)
          }
        }

    // Проверка
    println(
      "Задача 20 Удалите k-ый элемент  списка. Функция должна вернуть новый список и удаленный элемент\n---"
    )
    println("Проверка: removeAt(1, List('a, 'b, 'c, 'd))")
    print("Результат: ")
    println(removeAt(1, List('a, 'b, 'c, 'd)))
    println("Проверка: removeAt2(1, List('a, 'b, 'c, 'd))")
    print("Результат: ")
    println(removeAt2(1, List('a, 'b, 'c, 'd)))
    println("Проверка: removeAt3(1, List('a, 'b, 'c, 'd))")
    print("Результат: ")
    println(removeAt3(1, List('a, 'b, 'c, 'd)) + "\n")

    // Задача 21 Вставьте элемент на указанное место в списке
    // 1 способ
    def insertN(element: Any, index: Int, l: List[Any]): List[Any] = {
      l.take(index) ::: List(element) ::: l.drop(index)
    }

    // 2 способ
    def insertN2(element: Any, index: Int, l: List[Any]): List[Any] = {
      def ins(k: Int, l: List[Any]): List[Any] = (k, l) match {
        case (0, _)         => List(element) ::: l
        case (k, h :: tail) => h :: ins(k - 1, tail)
        case (k, Nil)       => l
      }

      ins(index, l)
    }

		// 3 способ
    def insertN3[A](e: A, n: Int, ls: List[Any]): List[Any] =
      ls.splitAt(n) match {
        case (pre, post) => pre ::: e :: post
      }

    // Проверка
    println("Задача 21 Вставьте элемент на указанное место в списке\n---")
    println("Проверка: insertN('new, 1, List('a, 'b, 'c, 'd))")
    print("Результат: ")
    println(insertN('new, 1, List('a, 'b, 'c, 'd)))
    println("Проверка: insertN2('new, 1, List('a, 'b, 'c, 'd))")
    print("Результат: ")
    println(insertN2('new, 1, List('a, 'b, 'c, 'd)))
    println("Проверка: insertN3('new, 1, List('a, 'b, 'c, 'd))")
    print("Результат: ")
    println(insertN3('new, 1, List('a, 'b, 'c, 'd)) + "\n")

    // Задача 22 Создайте список с элементами по заданному диапазону
    // 1  способ
    def range(left: Int, right: Int): List[Int] = (left, right) match {
      case (x, y) if x > y  => Nil
      case (x, y) if x <= y => x :: range(x + 1, y)
    }

		// 2 способ
    def rangeBuiltin(start: Int, end: Int): List[Int] =
      List.range(start, end + 1)

		// 3 способ
    def rangeRecursive(start: Int, end: Int): List[Int] =
      if (end < start) Nil
      else start :: rangeRecursive(start + 1, end)

		// 4 способ
    def rangeTailRecursive(start: Int, end: Int): List[Int] = {
      def rangeR(end: Int, result: List[Int]): List[Int] = {
        if (end < start) result
        else rangeR(end - 1, end :: result)
      }
      rangeR(end, Nil)
    }

		//5 способ
    def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
      f(s) match {
        case None         => Nil
        case Some((r, n)) => r :: unfoldRight(n)(f)
      }
    def rangeFunctional(start: Int, end: Int): List[Int] =
      unfoldRight(start) { n =>
        if (n > end) None
        else Some((n, n + 1))
      }

    // Проверка
    println(
      "Задача 22 Создайте список с элементами по заданному диапазону\n---"
    )
    println("Проверка: range(4, 9)")
    print("Результат: ")
    println(range(4, 9))
    println("Проверка: rangeBuiltin(4, 9)")
    print("Результат: ")
    println(rangeBuiltin(4, 9))
    println("Проверка: rangeRecursive(4, 9)")
    print("Результат: ")
    println(rangeRecursive(4, 9))
    println("Проверка: rangeTailRecursive(4, 9)")
    print("Результат: ")
    println(rangeTailRecursive(4, 9))
    println("Проверка: rangeFunctional(4, 9)")
    print("Результат: ")
    println(rangeFunctional(4, 9) + "\n")

    // Задача 23 Дан список. Выберите из него k элементов в случайном порядке без повторов
    // 1 способ
    def randomSelect1[A](n: Int, ls: List[Any]): List[Any] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt2((new util.Random).nextInt(ls.length), ls)
        e :: randomSelect1(n - 1, rest)
      }

		// 2 способ
    def randomSelect2[A](n: Int, ls: List[Any]): List[Any] = {
      def randomSelectR(n: Int, ls: List[Any], r: util.Random): List[Any] =
        if (n <= 0) Nil
        else {
          val (rest, e) = removeAt(r.nextInt(ls.length), ls)
          e :: randomSelectR(n - 1, rest, r)
        }
      randomSelectR(n, ls, new util.Random)
    }

    // Проверка
    println(
      "Задача 23 Дан список. Выберите из него k элементов в случайном порядке без повторов\n---"
    )
    println("Проверка: randomSelect1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h))")
    print("Результат: ")
    println(randomSelect1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)))
    println("Проверка: randomSelect2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h))")
    print("Результат: ")
    println(randomSelect2(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)) + "\n")

    // Задача 24 Выберите k различных случайных чисел из чисел 1, 2, ... , n
    def lotto(count: Int, max: Int): List[Any] =
      randomSelect1(count, List.range(1, max + 1))

    // Проверка
    println(
      "Задача 24 Выберите k различных случайных чисел из чисел 1, 2, ... , n\n---"
    )
    println("Проверка: lotto(6, 49)")
    print("Результат: ")
    println(lotto(6, 49) + "\n")

    // Задача 25 Получите случайную перестановку элементов данного списка
    // 1 способ
    def randomPermute1[A](ls: List[Any]): List[Any] =
      randomSelect1(ls.length, ls)

		// 2 способ
    def randomPermute[A](ls: List[Any]): List[Any] = {
      val rand = new util.Random
      val a = ls.toArray
      for (i <- a.length - 1 to 1 by -1) {
        val i1 = rand.nextInt(i + 1)
        val t = a(i)
        a.update(i, a(i1))
        a.update(i1, t)
      }
      a.toList
    }

    // Проверка
    println(
      "Задача 25 Получите случайную перестановку элементов данного списка\n---"
    )
    println("Проверка: randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))")
    print("Результат: ")
    println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
    println("Проверка: randomPermute1(List('a, 'b, 'c, 'd, 'e, 'f))")
    print("Результат: ")
    println(randomPermute1(List('a, 'b, 'c, 'd, 'e, 'f)) + "\n")

    // Задача 26 Получите всевозможные наборы k различных чисел, выбранные случайным образом из чисел 1, 2, ..., n. Порядок чисел в наборе не важен.
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

    // Проверка
    println(
      "Задача 26 Получите всевозможные наборы k различных чисел, выбранные случайным образом из чисел 1, 2, ..., n. Порядок чисел в наборе не важен\n---"
    )
    println("Проверка: combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))")
    print("Результат: ")
    println(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) + "\n")

    // Задача 27 (из-за большого вывода перенесена в отдельный файл P27.scala)
    println("Задача 27 (из-за большого вывода перенесена в отдельный файл P27.scala)\n")

    // Задача 28 Дан двумерный список (список, элементами которого являются списки). Выполните сортировку списка по длине вложенных списков.
    def lsort[A](ls: List[List[A]]): List[List[A]] =
      ls sortWith { _.length < _.length }

    def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
      val freqs = Map(encode(ls map { _.length } sortWith { _ < _ }) map {
        _.swap
      }: _*)
      ls sortWith { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
    }

    // Проверка
    println(
      "Задача 28 Дан двумерный список (список, элементами которого являются списки). Выполните сортировку списка по длине вложенных списков.\n---"
    )
    println(
      "Проверка: lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))"
    )
    print("Результат: ")
    println(
      lsort(
        List(
          List('a, 'b, 'c),
          List('d, 'e),
          List('f, 'g, 'h),
          List('d, 'e),
          List('i, 'j, 'k, 'l),
          List('m, 'n),
          List('o)
        )
      ) + "\n"
    )
  }
}
