# Fibonacci sequence in Tcl
proc fibonacci {n} {
    if {$n <= 1} {
        return $n
    }
    return [expr {[fibonacci [expr {$n - 1}]] + [fibonacci [expr {$n - 2}]]}]
}

puts "Fibonacci sequence (0-10):"
for {set i 0} {$i <= 10} {incr i} {
    puts "fib($i) = [fibonacci $i]"
}
