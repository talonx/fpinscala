sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def tail[A](xs: List[A]): List[A] = {
        xs match {
            case Nil => Nil
            case Cons(y, ys) => ys
        }
    }

    def setHead[A](xs: List[A], h: A): List[A] = {
        tail(xs) match  {
            case Nil => Nil
            case t => Cons(h, t)
        }
    }
    
    def drop[A](xs: List[A], n: Int): List[A] = {
        def loop(c: Int, xs: List[A]): List[A] = {
            c match {
                case _ if c == n => xs
                case _ => loop(c + 1, tail(xs))
            }
        }

        loop(0, xs)
    }
}

val l = List(1, 2, 3, 4, 5, 6)
println(List.drop(l, 2))
println(List.drop(l, 4))
