# sorting.tcl - Sorting algorithms in Tcl

# Bubble sort
proc bubble_sort {lst} {
    set n [llength $lst]
    for {set i 0} {$i < $n - 1} {incr i} {
        for {set j 0} {$j < $n - $i - 1} {incr j} {
            set a [lindex $lst $j]
            set b [lindex $lst [expr {$j + 1}]]
            if {$a > $b} {
                lset lst $j $b
                lset lst [expr {$j + 1}] $a
            }
        }
    }
    return $lst
}

# Selection sort
proc selection_sort {lst} {
    set n [llength $lst]
    for {set i 0} {$i < $n - 1} {incr i} {
        set min_idx $i
        for {set j [expr {$i + 1}]} {$j < $n} {incr j} {
            if {[lindex $lst $j] < [lindex $lst $min_idx]} {
                set min_idx $j
            }
        }
        if {$min_idx != $i} {
            set tmp [lindex $lst $i]
            lset lst $i [lindex $lst $min_idx]
            lset lst $min_idx $tmp
        }
    }
    return $lst
}

set data {64 34 25 12 22 11 90}
puts "Original: $data"

set sorted [bubble_sort $data]
puts "Bubble sorted: $sorted"

set sorted2 [selection_sort $data]
puts "Selection sorted: $sorted2"

# Built-in sort
puts "lsort: [lsort -integer $data]"
puts "Done."
