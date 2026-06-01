#!/usr/bin/perl
# Algorithms in Perl
# Sorting, searching, recursion

use strict;
use warnings;
use List::Util qw(min max sum);
use POSIX qw(floor);

# ── Sorting algorithms ────────────────────────────────────────────────
sub bubble_sort {
    my @arr = @_;
    my $n = scalar @arr;
    for my $i (0 .. $n - 2) {
        for my $j (0 .. $n - $i - 2) {
            if ($arr[$j] > $arr[$j+1]) {
                @arr[$j, $j+1] = @arr[$j+1, $j];
            }
        }
    }
    return @arr;
}

sub quicksort {
    my @arr = @_;
    return @arr if @arr <= 1;
    my $pivot   = $arr[int(@arr / 2)];
    my @less    = grep { $_ < $pivot } @arr;
    my @equal   = grep { $_ == $pivot } @arr;
    my @greater = grep { $_ > $pivot } @arr;
    return (quicksort(@less), @equal, quicksort(@greater));
}

# ── Binary search ─────────────────────────────────────────────────────
sub binary_search {
    my ($arr, $target) = @_;
    my ($lo, $hi) = (0, $#$arr);
    while ($lo <= $hi) {
        my $mid = int(($lo + $hi) / 2);
        if    ($arr->[$mid] == $target) { return $mid; }
        elsif ($arr->[$mid] < $target)  { $lo = $mid + 1; }
        else                            { $hi = $mid - 1; }
    }
    return -1;
}

# ── Recursive algorithms ──────────────────────────────────────────────
sub fibonacci {
    my ($n, %memo) = @_;
    return $n if $n <= 1;
    return $memo{$n} //= fibonacci($n-1) + fibonacci($n-2);
}

sub factorial {
    my ($n) = @_;
    return 1 if $n <= 1;
    return $n * factorial($n - 1);
}

# ── Statistics ────────────────────────────────────────────────────────
sub statistics {
    my @data = @_;
    my $n    = scalar @data;
    my $mean = sum(@data) / $n;
    my @sorted = sort { $a <=> $b } @data;
    my $median = $n % 2
        ? $sorted[int($n/2)]
        : ($sorted[$n/2 - 1] + $sorted[$n/2]) / 2;
    my $variance = sum(map { ($_ - $mean) ** 2 } @data) / $n;
    my $stddev   = sqrt($variance);
    return ($mean, $median, $stddev, min(@data), max(@data));
}

# ── Demo ──────────────────────────────────────────────────────────────
my @unsorted = (64, 25, 12, 22, 11, 90, 3, 77, 44);
print "=== Sorting ===\n";
print "  Unsorted:    @unsorted\n";
printf "  Bubble sort: %s\n", join(' ', bubble_sort(@unsorted));
printf "  Quick sort:  %s\n", join(' ', quicksort(@unsorted));

my @sorted = sort { $a <=> $b } @unsorted;
print "\n=== Binary Search ===\n";
print "  Array: @sorted\n";
for my $target (22, 64, 99) {
    my $idx = binary_search(\@sorted, $target);
    printf "  Search %2d: %s\n", $target,
        $idx >= 0 ? "found at index $idx" : "not found";
}

print "\n=== Fibonacci ===\n";
print "  ", join(', ', map { fibonacci($_) } 0..10), "\n";

print "\n=== Factorials ===\n";
print "  ", join(', ', map { factorial($_) } 1..10), "\n";

print "\n=== Statistics ===\n";
my @data = (4, 8, 15, 16, 23, 42, 7, 12, 19, 3);
my ($mean, $median, $stddev, $min, $max) = statistics(@data);
printf "  Data:   @data\n";
printf "  Mean:   %.2f\n", $mean;
printf "  Median: %.1f\n", $median;
printf "  StdDev: %.2f\n", $stddev;
printf "  Min/Max: %d / %d\n", $min, $max;
