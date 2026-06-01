# Tcl Algorithms
# Sorting, searching, and classic algorithm demos

# ── Bubble sort ───────────────────────────────────────────────────────
proc bubbleSort {lst} {
    set n [llength $lst]
    for {set i 0} {$i < $n-1} {incr i} {
        set swapped 0
        for {set j 0} {$j < $n-$i-1} {incr j} {
            set a [lindex $lst $j]
            set b [lindex $lst [expr {$j+1}]]
            if {$a > $b} {
                lset lst $j $b
                lset lst [expr {$j+1}] $a
                set swapped 1
            }
        }
        if {!$swapped} break
    }
    return $lst
}

# ── Merge sort ────────────────────────────────────────────────────────
proc mergeSort {lst} {
    set n [llength $lst]
    if {$n <= 1} { return $lst }
    set mid [expr {$n / 2}]
    set left  [mergeSort [lrange $lst 0 [expr {$mid-1}]]]
    set right [mergeSort [lrange $lst $mid end]]
    return [merge $left $right]
}

proc merge {left right} {
    set result {}
    set i 0; set j 0
    set nl [llength $left]; set nr [llength $right]
    while {$i < $nl && $j < $nr} {
        if {[lindex $left $i] <= [lindex $right $j]} {
            lappend result [lindex $left $i]; incr i
        } else {
            lappend result [lindex $right $j]; incr j
        }
    }
    while {$i < $nl} { lappend result [lindex $left $i]; incr i }
    while {$j < $nr} { lappend result [lindex $right $j]; incr j }
    return $result
}

# ── Binary search ─────────────────────────────────────────────────────
proc binarySearch {lst target} {
    set lo 0
    set hi [expr {[llength $lst] - 1}]
    while {$lo <= $hi} {
        set mid [expr {($lo + $hi) / 2}]
        set val [lindex $lst $mid]
        if    {$val == $target} { return $mid }
        elseif {$val < $target} { set lo [expr {$mid + 1}] }
        else                    { set hi [expr {$mid - 1}] }
    }
    return -1
}

# ── GCD / LCM ─────────────────────────────────────────────────────────
proc gcd {a b} {
    while {$b != 0} {
        set t $b
        set b [expr {$a % $b}]
        set a $t
    }
    return $a
}

proc lcm {a b} {
    expr {$a / [gcd $a $b] * $b}
}

# ── Sieve of Eratosthenes ─────────────────────────────────────────────
proc sieve {limit} {
    set composite [lrepeat [expr {$limit+1}] 0]
    for {set i 2} {$i <= $limit} {incr i} {
        if {[lindex $composite $i] == 0} {
            for {set j [expr {$i*$i}]} {$j <= $limit} {incr j $i} {
                lset composite $j 1
            }
        }
    }
    set primes {}
    for {set i 2} {$i <= $limit} {incr i} {
        if {[lindex $composite $i] == 0} { lappend primes $i }
    }
    return $primes
}

# ── Main ──────────────────────────────────────────────────────────────
set data {64 25 12 90 3 77 44 18 55 37}
puts "=== Tcl Algorithms ==="
puts "Input: $data"
puts ""

puts "Bubble sort: [bubbleSort $data]"
puts "Merge sort:  [mergeSort  $data]"
puts "Tcl lsort:   [lsort -integer $data]"

puts ""
puts "=== Binary Search ==="
set sorted [lsort -integer $data]
puts "Sorted: $sorted"
foreach target {12 55 99} {
    set idx [binarySearch $sorted $target]
    if {$idx >= 0} {
        puts "Search $target: found at index $idx"
    } else {
        puts "Search $target: not found"
    }
}

puts ""
puts "=== GCD / LCM ==="
foreach pair {{48 18} {100 75} {17 13}} {
    lassign $pair a b
    puts "gcd($a,$b)=[gcd $a $b]  lcm($a,$b)=[lcm $a $b]"
}

puts ""
puts "=== Primes up to 50 (Sieve) ==="
puts [sieve 50]
