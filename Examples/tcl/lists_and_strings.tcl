# List operations demo in Tcl
set fruits {apple banana cherry date elderberry}

puts "Fruit list: $fruits"
puts "Length: [llength $fruits]"
puts "First: [lindex $fruits 0]"
puts "Last: [lindex $fruits end]"

# Sort
set sorted [lsort $fruits]
puts "Sorted: $sorted"

# Iterate
puts "\nAll fruits:"
foreach fruit $fruits {
    puts "  - $fruit"
}

# String operations
set greeting "Hello, World!"
puts "\nString: $greeting"
puts "Length: [string length $greeting]"
puts "Upper: [string toupper $greeting]"
puts "Lower: [string tolower $greeting]"
