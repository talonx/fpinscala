import java.util.NoSuchElementException

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

    def head[A](xs: List[A]): A = {
        xs match {
            case Nil => throw new NoSuchElementException("head of empty list")
            case Cons(y: A, ys: List[A]) => y
        }
    }
    
    def setHead[A](xs: List[A], h: A): List[A] = {
        tail(xs) match  {
            case Nil => Nil
            case Cons(_, t) => Cons(h, t)
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

    def dropWhile[A](xs: List[A], p: A => Boolean): List[A] = {
        def loop(xs: List[A]): List[A] = {
            p(head(xs)) match {
                case true => loop(tail(xs))
                case false => xs
            }
        }
        loop(xs)
    }

    def init[A](xs: List[A]): List[A] = {
        xs match {
            case Cons(h, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))
        }
    }
}

val l = List(1, 2, 3, 4, 5, 6)
println(List.drop(l, 2))
println(List.drop(l, 4))

println(List.head(l))
println(List.head(List.tail(l)))

def even(i: Int): Boolean = i % 2 == 0


val l2 = List(2, 4, 6, 8, 9, 4, 7, 13, 15, 20)
println(List.dropWhile(l2, even))

println(List.init(l2))
