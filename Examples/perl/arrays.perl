#!/usr/bin/perl
# Arrays and Hashes in Perl 5 — SHOWCASE
# Demonstrates lists, array operations, and hash tables

use strict;
use warnings;

# --- Arrays ---
print "=== Arrays ===\n";

my @numbers = (3, 1, 4, 1, 5, 9, 2, 6, 5, 3);
print "Original: @numbers\n";
print "Length:   ", scalar @numbers, "\n";
print "First:    $numbers[0]\n";
print "Last:     $numbers[-1]\n";

# Sort and reverse
my @sorted   = sort { $a <=> $b } @numbers;
my @reversed = reverse @sorted;
print "Sorted:   @sorted\n";
print "Reversed: @reversed\n";

# Push, pop, shift, unshift
my @stack = (1, 2, 3);
push @stack, 4;
push @stack, 5;
print "\nStack after push: @stack\n";
my $top = pop @stack;
print "Popped: $top  Stack: @stack\n";

my @queue = (10, 20, 30);
my $front = shift @queue;
unshift @queue, 5;
print "Queue: @queue  (shifted off: $front)\n";

# Map and grep
my @evens = grep { $_ % 2 == 0 } @numbers;
my @doubled = map { $_ * 2 } (1..5);
print "\nEvens from list: @evens\n";
print "Doubled 1-5:     @doubled\n";

# Slice
my @slice = @numbers[0..2];
print "First 3: @slice\n";

# Join
my $joined = join(", ", @sorted);
print "Joined:  $joined\n";

# --- Hashes ---
print "\n=== Hashes ===\n";

my %capitals = (
    "France"  => "Paris",
    "Germany" => "Berlin",
    "Japan"   => "Tokyo",
    "Brazil"  => "Brasilia",
    "Egypt"   => "Cairo",
);

print "France capital: $capitals{France}\n";
print "Japan capital:  $capitals{Japan}\n";

# Keys and values
my @countries = sort keys %capitals;
print "\nCountries:\n";
for my $country (@countries) {
    print "  $country => $capitals{$country}\n";
}

# Exists and delete
if (exists $capitals{Germany}) {
    print "\nGermany is in the hash\n";
}
delete $capitals{Brazil};
print "After delete, count: ", scalar keys %capitals, "\n";

# Hash slice
my @some_capitals = @capitals{"France", "Japan"};
print "Paris & Tokyo: @some_capitals\n";

print "\nDone!\n";
