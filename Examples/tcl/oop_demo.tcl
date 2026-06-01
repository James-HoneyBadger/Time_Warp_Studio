# Tcl OOP Demo using TclOO
# Classes, inheritance, mixins, and introspection

package require TclOO

# ── Base class: Shape ─────────────────────────────────────────────────
oo::class create Shape {
    variable color

    constructor {c} {
        set color $c
    }

    method color {} { return $color }
    method color: {c} { set color $c }

    method area {} { return 0 }

    method describe {} {
        puts "[my type]: color=[my color], area=[format %.2f [my area]]"
    }

    method type {} { return [info object class [self]] }
}

# ── Subclass: Circle ──────────────────────────────────────────────────
oo::class create Circle {
    superclass Shape
    variable radius

    constructor {r c} {
        next $c
        set radius $r
    }

    method radius {} { return $radius }

    method area {} {
        set pi 3.14159265358979
        return [expr {$pi * $radius * $radius}]
    }

    method circumference {} {
        set pi 3.14159265358979
        return [expr {2 * $pi * $radius}]
    }
}

# ── Subclass: Rectangle ───────────────────────────────────────────────
oo::class create Rectangle {
    superclass Shape
    variable width height

    constructor {w h c} {
        next $c
        set width  $w
        set height $h
    }

    method width  {} { return $width  }
    method height {} { return $height }
    method area   {} { return [expr {$width * $height}] }
    method perimeter {} { return [expr {2 * ($width + $height)}] }
}

# ── Subclass: Square ──────────────────────────────────────────────────
oo::class create Square {
    superclass Rectangle

    constructor {side c} {
        next $side $side $c
    }
}

# ── Mixin: Printable ─────────────────────────────────────────────────
oo::class create Printable {
    method print {} {
        puts "  >> [my describe]"
    }
}

# Mix into Shape (all shapes get print)
oo::define Shape mixin Printable

# ── Demo ──────────────────────────────────────────────────────────────
puts "=== TclOO Shapes Demo ==="
puts ""

set shapes [list \
    [Circle   new 5    red   ] \
    [Circle   new 3    blue  ] \
    [Rectangle new 4 6 green ] \
    [Square   new 7    yellow] \
]

foreach s $shapes {
    $s describe
    $s print
    puts ""
}

# Polymorphism: total area
set total 0
foreach s $shapes {
    set total [expr {$total + [$s area]}]
}
puts [format "Total area: %.2f" $total]

puts ""
puts "=== Introspection ==="
set c [Circle new 10 purple]
puts "Object: $c"
puts "Class: [info object class $c]"
puts "Methods: [lsort [info object methods $c]]"
puts "Is Shape: [info object isa typeof $c Shape]"
