# Procedures and recursion in Tcl
proc factorial {n} {
    if {$n <= 1} {
        return 1
    }
    return [expr {$n * [factorial [expr {$n - 1}]]}]
}

proc power {base exp} {
    if {$exp == 0} {
        return 1
    }
    return [expr {$base * [power $base [expr {$exp - 1}]]}]
}

puts "Factorials:"
for {set i 1} {$i <= 8} {incr i} {
    puts "  $i! = [factorial $i]"
}

puts "\nPowers of 2:"
for {set i 0} {$i <= 8} {incr i} {
    puts "  2^$i = [power 2 $i]"
}
