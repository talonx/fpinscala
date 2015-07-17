/**
 * n to start from 1, not 0
 */

def fib(n: Int): Int = {
    def loop(count: Int, f: Int, s: Int): Int = {
        if(count == n) f + s
        else loop(count + 1, s, f + s)
    }
    
    n match {
        case 0 => -1
        case 1 => 0
        case 2 => 1
        case _ => loop(3, 0, 1)
    }
}
