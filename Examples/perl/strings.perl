#!/usr/bin/perl
# Strings in Perl 5 — SHOWCASE
# String operations, interpolation, formatting, and builtins

use strict;
use warnings;

# String basics
my $greeting = "Hello, Perl!";
print "Original:   $greeting\n";
print "Uppercase:  ", uc($greeting), "\n";
print "Lowercase:  ", lc($greeting), "\n";
print "Length:     ", length($greeting), "\n";
print "Reversed:   ", scalar reverse($greeting), "\n";

# Substring and index
my $lang = "Perl 5 Programming";
print "\nSubstring:  ", substr($lang, 0, 6), "\n";
print "Index of 5: ", index($lang, "5"), "\n";
print "Last index: ", rindex($lang, "r"), "\n";

# String repetition
my $line = "-" x 30;
print "\n$line\n";

# String concatenation
my $first = "Time";
my $second = "Warp";
my $third = "Studio";
my $full = $first . " " . $second . " " . $third;
print "Joined: $full\n";

# String formatting with sprintf
my $pi = 3.14159265;
my $formatted = sprintf("Pi is approximately %.4f", $pi);
print "$formatted\n";

my $hex = sprintf("42 in hex is 0x%X", 42);
print "$hex\n";

# String split and join
my $csv = "apple,banana,cherry,date";
my @fruits = split(/,/, $csv);
print "\nFruits: ", scalar @fruits, " items\n";
for my $fruit (@fruits) {
    print "  - $fruit\n";
}
my $rejoined = join(" | ", @fruits);
print "Rejoined: $rejoined\n";

# Multiline strings
my $verse = "Roses are red,\nViolets are blue,\nPerl is awesome,\nAnd so are you!";
print "\n$verse\n";

print "\n$line\n";
print "String operations complete!\n";
