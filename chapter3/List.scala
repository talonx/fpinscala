import java.util.NoSuchElementException

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

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
            case Nil => Nil
            case Cons(h, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))
        }
    }

    //Length of a list using foldRight
    def lengthFR[A](xs: List[A]): Int = 
        foldRight(xs, 0)((_, acc) => acc + 1)
    
    @annotation.tailrec
    def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
        xs match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }
    }

    def sumFL[A](xs: List[Int]): Int = 
        foldLeft(xs, 0)(_ + _)

    def productFL[A](xs: List[Int]): Int = 
        foldLeft(xs, 1)(_ * _)

    def lengthFL[A](xs: List[A]): Int =
        foldLeft(xs, 0)((acc, _) => acc + 1)

    def reverseFL[A](xs: List[A]): List[A] =
        //Type decl for accummulator is required here
        foldLeft(xs, List[A]())((acc, e) => Cons(e, acc))

    def foldLeftFR[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
        foldRight(reverseFL(xs), z)((x, y) => f(y, x))

    def appendFL[A](xs: List[A], ys: List[A]): List[A] =
        foldRight(xs, ys)((e, acc) => Cons(e, acc))

    //Ex 3.16 without folding
    def transform(xs: List[Int]): List[Int] = 
        xs match {
            case Nil => Nil
            case Cons(y, ys) => Cons(y + 1, transform(ys))
        }

    //Ex 3.17 without folding
    def dToString(xs: List[Double]): List[String] =
        xs match {
            case Nil => Nil
            case Cons(y, ys) => Cons(y.toString, dToString(ys))
        }

    //Ex 3.18 map with foldRight
    def map[A, B](xs: List[A])(f: A => B): List[B] =
        foldRight(xs, List[B]())((e, acc) => Cons(f(e), acc))

    //Ex 3.19
    def filter[A](xs: List[A])(f: A => Boolean): List[A] =
        foldRight(xs, List[A]())((e, acc) => f(e) match { 
            case true => Cons(e, acc)
            case false => acc
        }
    )

    
}

val l = List(1, 2, 3, 4, 5, 6)
println(List.drop(l, 2))
println(List.drop(l, 4))

println(List.head(l))
println(List.head(List.tail(l)))

def even(i: Int): Boolean = i % 2 == 0


println("dropWhile")
val l2 = List(2, 4, 6, 8, 9, 4, 7, 13, 15, 20)
println(List.dropWhile(l2, even))

println("init")
println(List.init(l2))

println("Fold right")
println(List.lengthFR(l2))

println("Fold left")
println(List.foldLeft(l, 0)(_ + _))
println(List.sumFL(l))
println(List.productFL(l))
println(List.lengthFL(l))

println("reverse FL")
println(List.reverseFL(l))

println("FL/FR in terms of each other")
println(List.foldLeftFR(l, 0)(_ + _))

println("append")
val la = List(1, 3, 5)
val lb = List(2, 4, 6)
println(List.appendFL(la, lb))

println("transform")
val tl = List(1, 3, 10, 2, 1)
println(List.transform(tl))

println("Filter odd")
val tall = List(1, 2, 3, 4, 5, 6, 7, 8)
println(List.filter(tall)(_ % 2 == 0))
