def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
        n match {
            case _ if (as.length == n + 1) => true
            case _ if(ordered(as(n), as(n + 1))) => loop(n + 1)
            case _ => false
        }
    }

    loop(0)
}

def natural(x: Int, y: Int): Boolean = y > x

println(isSorted(Array(1, 2, 3, 4), natural))
println(isSorted(Array(1, 2, 5, 4), natural))
println(isSorted(Array(5, 2, 5, 4), natural))
println(isSorted(Array(1, 2, 3, 0), natural))

def reverse(x: Int, y: Int): Boolean = x > y
println(isSorted(Array(4, 3, 2, 1), reverse))
println(isSorted(Array(1, 2, 5, 4), reverse))
println(isSorted(Array(5, 2, 5, 4), reverse ))
println(isSorted(Array(1, 2, 3, 0), reverse))

