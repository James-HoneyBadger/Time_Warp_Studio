# Tcl Tutorial

## Introduction

Tcl (Tool Command Language, 1988) is a dynamic scripting language designed for easy embedding into applications. Everything in Tcl is a string; commands are lists; code is data.

**Key characteristics:**
- Simple, consistent syntax: every statement is `command arg1 arg2 ...`
- `[...]` for command substitution, `$var` for variable substitution
- `{...}` for quoting without substitution
- Excellent for glue scripting and GUI automation

## Hello World

```tcl
puts "Hello, World!"
```

## Variables

```tcl
set x 42
set name "Alice"
set greeting "Hello"

puts $name
puts "$greeting, $name!"        ;# variable substitution in ""
puts {$greeting, $name!}        ;# no substitution in {}
```

`set` assigns a value. `$` reads a variable. `;#` starts an inline comment.

## Arithmetic

```tcl
set result [expr {3 + 4}]       ;# 7
set product [expr {6 * 7}]      ;# 42
set power [expr {2 ** 10}]      ;# 1024
set pi [expr {acos(-1)}]        ;# 3.14159...

puts $result
puts [expr {17 % 5}]            ;# 2
puts [expr {sqrt(16)}]          ;# 4.0
puts [expr {abs(-7)}]           ;# 7
```

All math goes through `expr`. Curly braces prevent double-substitution.

## String Operations

```tcl
set s "Hello, World!"
puts [string length $s]          ;# 13
puts [string toupper $s]         ;# HELLO, WORLD!
puts [string tolower $s]         ;# hello, world!
puts [string range $s 7 11]      ;# World
puts [string index $s 0]         ;# H
puts [string first "World" $s]   ;# 7
puts [string replace $s 7 11 "Tcl"]  ;# Hello, Tcl!

# Concatenation with append
set msg "Hello"
append msg ", World!"
puts $msg
```

## Control Flow

```tcl
set x 42

# if / elseif / else
if {$x > 100} {
    puts "Very large"
} elseif {$x > 10} {
    puts "Large"
} else {
    puts "Small"
}

# switch
switch $x {
    0       { puts "zero" }
    42      { puts "the answer" }
    default { puts "something else" }
}
```

## Loops

```tcl
# for loop
for {set i 1} {$i <= 5} {incr i} {
    puts "i = $i"
}

# while loop
set n 10
while {$n > 0} {
    puts $n
    incr n -2
}

# foreach over list
foreach item {apple banana cherry} {
    puts $item
}

# foreach with index
set fruits {apple banana cherry}
for {set i 0} {$i < [llength $fruits]} {incr i} {
    puts "[expr {$i+1}]: [lindex $fruits $i]"
}
```

## Lists

```tcl
set lst {1 2 3 4 5}

puts [llength $lst]           ;# 5
puts [lindex $lst 0]          ;# 1
puts [lindex $lst end]        ;# 5
puts [lrange $lst 1 3]        ;# 2 3 4

# Build lists
set lst [list 10 20 30]
lappend lst 40 50
puts $lst                     ;# 10 20 30 40 50

# Search
puts [lsearch $lst 30]        ;# 2  (index)
puts [lsort {3 1 4 1 5 9}]    ;# 1 1 3 4 5 9
```

## Procedures

```tcl
proc greet {name} {
    puts "Hello, $name!"
}

greet "Alice"

proc factorial {n} {
    if {$n <= 1} { return 1 }
    return [expr {$n * [factorial [expr {$n - 1}]]}]
}

puts [factorial 6]   ;# 720

# Default argument values
proc connect {host {port 80}} {
    puts "Connecting to $host:$port"
}
connect "example.com"
connect "example.com" 443
```

## Dictionaries

```tcl
dict set person name "Alice"
dict set person age  30

puts [dict get $person name]       ;# Alice
puts [dict exists $person email]   ;# 0

# Iterate
dict for {key val} $person {
    puts "$key: $val"
}
```

## Regular Expressions

```tcl
set text "The quick brown fox"

if {[regexp {(\w+) fox} $text match adj]} {
    puts "Adjective: $adj"    ;# brown
}

regsub {fox} $text "cat" result
puts $result                  ;# The quick brown cat
```

## Quick Reference

| Command | Purpose |
|---------|---------|
| `set var val` | Assign variable |
| `puts text` | Print with newline |
| `expr {math}` | Arithmetic expression |
| `if {cond} { }` | Conditional |
| `for {init} {test} {step}` | For loop |
| `while {test} { }` | While loop |
| `foreach var list { }` | Iterate list |
| `proc name {args} { }` | Define procedure |
| `return val` | Return value |
| `lappend lst val` | Append to list |
| `llength lst` | List length |
| `lindex lst i` | List element at index |
| `string length s` | String length |
| `regexp pat str` | Regex match |
