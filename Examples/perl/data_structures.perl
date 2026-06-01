#!/usr/bin/perl
# Data Structures in Perl
# Arrays, hashes, references, nested structures

use strict;
use warnings;

# ── Arrays ────────────────────────────────────────────────────────────
my @fruits = qw(apple banana cherry date elderberry);
print "=== Arrays ===\n";
print "Fruits: @fruits\n";
print "Count: ", scalar(@fruits), "\n";
print "First: $fruits[0], Last: $fruits[-1]\n";

# Slice
my @slice = @fruits[1..3];
print "Slice [1..3]: @slice\n";

# Sort and reverse
my @sorted  = sort @fruits;
my @reversed = reverse @sorted;
print "Sorted:   @sorted\n";
print "Reversed: @reversed\n";

# grep (filter) and map
my @long = grep { length($_) > 5 } @fruits;
my @upper = map { uc($_) } @fruits;
print "Long names: @long\n";
print "Uppercase:  @upper\n";

# ── Hashes ────────────────────────────────────────────────────────────
print "\n=== Hashes ===\n";
my %capitals = (
    France    => 'Paris',
    Japan     => 'Tokyo',
    Brazil    => 'Brasilia',
    Australia => 'Canberra',
    Egypt     => 'Cairo',
);

for my $country (sort keys %capitals) {
    printf "  %-12s => %s\n", $country, $capitals{$country};
}

# Hash operations
$capitals{Germany} = 'Berlin';
delete $capitals{Egypt};
print "Keys after update: ", join(', ', sort keys %capitals), "\n";
print "Exists France: ", (exists $capitals{France} ? 'yes' : 'no'), "\n";

# ── References and complex structures ─────────────────────────────────
print "\n=== References & Nested Structures ===\n";

my @matrix = ([1,2,3], [4,5,6], [7,8,9]);
print "Matrix:\n";
for my $row (@matrix) {
    print "  ", join("  ", @$row), "\n";
}

# Array of hashes
my @people = (
    { name => 'Alice', age => 30, role => 'Engineer' },
    { name => 'Bob',   age => 25, role => 'Designer' },
    { name => 'Carol', age => 35, role => 'Manager'  },
);

print "\nPeople (sorted by age):\n";
for my $p (sort { $a->{age} <=> $b->{age} } @people) {
    printf "  %-8s age=%-3d role=%s\n", $p->{name}, $p->{age}, $p->{role};
}

# Hash of arrays
my %hobbies = (
    Alice => [qw(reading coding hiking)],
    Bob   => [qw(painting music cycling)],
);
print "\nHobbies:\n";
for my $person (sort keys %hobbies) {
    print "  $person: ", join(', ', @{$hobbies{$person}}), "\n";
}
