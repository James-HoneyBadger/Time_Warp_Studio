#!/usr/bin/perl
# Hello World in Perl 5
# Demonstrates basic output, variables, and control flow

use strict;
use warnings;

# Simple output
print "Hello, World!\n";

# Variables and string interpolation
my $name = "Time Warp Studio";
print "Welcome to $name!\n";

# Arithmetic
my $x = 42;
my $y = 7;
print "$x divided by $y is ", $x / $y, "\n";
print "$x modulo $y is ", $x % $y, "\n";

# Conditionals
if ($x > 10) {
    print "$x is greater than 10\n";
} else {
    print "$x is not greater than 10\n";
}

# Loops
print "Counting: ";
for my $i (1..5) {
    print "$i ";
}
print "\n";

# While loop with last/next
my $n = 1;
print "Odds under 10: ";
while ($n < 10) {
    print "$n " if $n % 2 != 0;
    $n++;
}
print "\n";

print "Done!\n";
