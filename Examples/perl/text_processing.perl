#!/usr/bin/perl
# Text Processing in Perl
# Regular expressions, file parsing, transformations

use strict;
use warnings;

# ── Regular expressions ───────────────────────────────────────────────
print "=== Regular Expressions ===\n";

my @emails = (
    'alice@example.com',
    'bob.smith@company.org',
    'not-an-email',
    'carol@test.net',
    'invalid@',
    'dave@domain.co.uk',
);

my $email_re = qr/^[a-zA-Z0-9._%+\-]+@[a-zA-Z0-9.\-]+\.[a-zA-Z]{2,}$/;

for my $e (@emails) {
    my $valid = $e =~ $email_re ? '✓' : '✗';
    printf "  %s  %s\n", $valid, $e;
}

# ── Capture groups ────────────────────────────────────────────────────
print "\n=== Capture Groups ===\n";
my @dates = ('2026-06-01', '1999-12-31', '2000-01-15');
for my $d (@dates) {
    if ($d =~ /(\d{4})-(\d{2})-(\d{2})/) {
        printf "  Year: %s  Month: %s  Day: %s\n", $1, $2, $3;
    }
}

# ── Substitution ─────────────────────────────────────────────────────
print "\n=== Substitution ===\n";
my $text = "The quick brown fox jumps over the lazy dog.";
(my $vowel_count = $text) =~ s/[aeiou]//gi;
my $removed = length($text) - length($vowel_count);
print "  Original:   $text\n";
print "  No vowels:  $vowel_count\n";
print "  Vowels removed: $removed\n";

# Camel to snake_case
my $camelCase = "myVariableName";
(my $snake = $camelCase) =~ s/([A-Z])/'_' . lc($1)/ge;
print "  CamelCase: $camelCase => snake_case: $snake\n";

# ── Split and join ────────────────────────────────────────────────────
print "\n=== Split / Join ===\n";
my $csv = "Alice,30,Engineer,New York";
my @fields = split(/,/, $csv);
printf "  Name: %s  Age: %s  Role: %s  City: %s\n", @fields;

my @words = split(/\s+/, "  Hello   World   Perl  ");
print "  Words: ", join(" | ", @words), "\n";

# ── String functions ──────────────────────────────────────────────────
print "\n=== String Functions ===\n";
my $str = "  Hello, World!  ";
$str =~ s/^\s+|\s+$//g;  # trim
print "  Trimmed:    '$str'\n";
print "  Length:     ", length($str), "\n";
print "  Uppercase:  ", uc($str), "\n";
print "  Lowercase:  ", lc($str), "\n";
print "  Index of ,: ", index($str, ','), "\n";
print "  Substr 7-5: ", substr($str, 7, 5), "\n";

# ── Word frequency count ──────────────────────────────────────────────
print "\n=== Word Frequency ===\n";
my $passage = "to be or not to be that is the question whether to be";
my %freq;
$freq{$_}++ for split(/\s+/, $passage);
for my $word (sort { $freq{$b} <=> $freq{$a} } keys %freq) {
    printf "  %-12s %d\n", $word, $freq{$word};
}
