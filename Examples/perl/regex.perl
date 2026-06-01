#!/usr/bin/perl
# Regular Expressions in Perl 5 — SHOWCASE
# Perl's regex engine: matching, substitution, captures, and modifiers

use strict;
use warnings;

print "=== Regular Expressions in Perl 5 ===\n\n";

# --- Basic matching ---
my $text = "The quick brown fox jumps over the lazy dog";
print "Text: $text\n\n";

if ($text =~ /fox/) {
    print "Match: 'fox' found in text\n";
}

if ($text =~ /cat/) {
    print "Match: 'cat' found\n";
} else {
    print "No match: 'cat' not found\n";
}

# --- Case-insensitive matching ---
my $sentence = "Hello World from PERL";
if ($sentence =~ /perl/i) {
    print "Case-insensitive: 'perl' found\n";
}

# --- Capture groups ---
print "\n--- Captures ---\n";
my $date = "2024-06-15";
if ($date =~ /(\d{4})-(\d{2})-(\d{2})/) {
    print "Year:  $1\n";
    print "Month: $2\n";
    print "Day:   $3\n";
}

my $email = "user@example.com";
if ($email =~ /^(\w+)\@([\w.]+)$/) {
    print "\nEmail user:   $1\n";
    print "Email domain: $2\n";
}

# --- Substitution ---
print "\n--- Substitution ---\n";
my $str = "I love cats. Cats are great. My cat is named Whiskers.";
print "Before: $str\n";
(my $replaced = $str) =~ s/cat/dog/gi;
print "After:  $replaced\n";

# --- Global matching ---
print "\n--- Global Match ---\n";
my $code = "x = 1; y = 2; z = x + y; w = x * z";
my @vars;
while ($code =~ /\b([a-z])\b/g) {
    push @vars, $1;
}
my $unique = join(", ", @vars);
print "Variables found: $unique\n";

# --- Split with regex ---
print "\n--- Split with Regex ---\n";
my $csv = "one,,two , three,  four";
my @items = split(/\s*,\s*/, $csv);
for my $i (0..$#items) {
    printf "  [%d] '%s'\n", $i, $items[$i];
}

# --- Named patterns (simulated) ---
print "\n--- Validation ---\n";
my @inputs = ("hello123", "UPPER_CASE", "bad input!", "ValidName42");
for my $word (@inputs) {
    if ($word =~ /^[A-Za-z][A-Za-z0-9_]*$/) {
        print "  '$word' is a valid identifier\n";
    } else {
        print "  '$word' is NOT a valid identifier\n";
    }
}

print "\nRegex showcase complete!\n";
