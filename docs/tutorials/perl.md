# Perl 5 Programming Tutorial

Perl 5 is a highly capable scripting language created by Larry Wall in 1987. It is renowned for its powerful text processing, regular expression engine, and the philosophy "There Is More Than One Way To Do It" (TIMTOWTDI).

## Hello World

```perl
print "Hello, World!\n";

my $name = "Time Warp Studio";
print "Welcome to $name!\n";
```

## Variables

Perl has three variable types, each with a different sigil:

```perl
# Scalars — single values ($)
my $number = 42;
my $pi     = 3.14159;
my $name   = "Perl";
my $flag   = 1;  # true

print "Number: $number\n";
print "Pi:     $pi\n";
print "Name:   $name\n";

# String interpolation inside double quotes
print "Hello, $name! Pi is about $pi.\n";
```

## Arrays

```perl
# Arrays — ordered lists (@)
my @fruits = ("apple", "banana", "cherry");
print "Count: ", scalar @fruits, "\n";
print "First: $fruits[0]\n";
print "Last:  $fruits[-1]\n";

# Push / pop
push @fruits, "date";
my $last = pop @fruits;
print "Popped: $last\n";

# Iterate
for my $fruit (@fruits) {
    print "  - $fruit\n";
}

# Map and grep
my @doubled = map { $_ * 2 } (1..5);
my @evens   = grep { $_ % 2 == 0 } (1..10);
print "Doubled: @doubled\n";
print "Evens:   @evens\n";

# Sort
my @sorted = sort { $a <=> $b } (5, 3, 1, 4, 2);
print "Sorted: @sorted\n";
```

## Hashes

```perl
# Hashes — key-value tables (%)
my %age = (
    Alice => 30,
    Bob   => 25,
    Carol => 35,
);

print "Alice's age: $age{Alice}\n";
$age{Dave} = 28;

# Iterate
for my $name (sort keys %age) {
    print "  $name => $age{$name}\n";
}

# Exists / delete
if (exists $age{Bob}) {
    print "Bob is in the hash\n";
}
delete $age{Bob};
print "After delete: ", scalar keys %age, " entries\n";
```

## Control Flow

```perl
# if / elsif / else / unless
my $x = 42;

if ($x > 100) {
    print "large\n";
} elsif ($x > 10) {
    print "medium\n";
} else {
    print "small\n";
}

unless ($x == 0) {
    print "x is non-zero\n";
}

# while / until
my $n = 1;
while ($n <= 5) {
    print "$n ";
    $n++;
}
print "\n";

# for range
for my $i (1..5) {
    print "$i ";
}
print "\n";

# C-style for
for (my $i = 0; $i < 3; $i++) {
    print "i=$i\n";
}

# last (break) / next (continue)
for my $i (1..10) {
    next if $i % 2 == 0;
    last if $i > 7;
    print "$i ";
}
print "\n";
```

## Statement Modifiers

Perl lets you put conditions and loops after statements:

```perl
print "positive\n" if $x > 0;
print "even\n"     unless $x % 2;

print "$_ " for (1..5);
print "\n";
```

## Subroutines

```perl
sub greet {
    my ($name) = @_;
    return "Hello, $name!";
}

print greet("Alice"), "\n";
print greet("Bob"),   "\n";

# Recursive subroutine
sub factorial {
    my ($n) = @_;
    return 1 if $n <= 1;
    return $n * factorial($n - 1);
}

print "5! = ", factorial(5), "\n";
print "10! = ", factorial(10), "\n";
```

## Regular Expressions

Regular expressions are a core strength of Perl:

```perl
my $text = "The quick brown fox";

# Match test
if ($text =~ /fox/) {
    print "Found fox!\n";
}

# Case-insensitive
if ($text =~ /QUICK/i) {
    print "Found QUICK (case-insensitive)\n";
}

# Capture groups
my $date = "2024-06-15";
if ($date =~ /(\d{4})-(\d{2})-(\d{2})/) {
    print "Year=$1, Month=$2, Day=$3\n";
}

# Substitution
my $str = "Hello, World!";
$str =~ s/World/Perl/;
print "$str\n";

# Global substitution
my $csv = "a,b,c,d";
$csv =~ s/,/ | /g;
print "$csv\n";

# Split and join
my @parts = split(/,/, "one,two,three");
my $joined = join(" + ", @parts);
print "$joined\n";
```

## String Operations

```perl
my $s = "Hello, Perl!";
print "uc:      ", uc($s), "\n";
print "lc:      ", lc($s), "\n";
print "length:  ", length($s), "\n";
print "substr:  ", substr($s, 7, 4), "\n";
print "index:   ", index($s, "Perl"), "\n";
print "reverse: ", scalar reverse($s), "\n";

# sprintf for formatting
printf "Pi = %.4f\n", 3.14159265;
my $hex = sprintf("0x%X", 255);
print "255 in hex: $hex\n";
```

## Turtle Graphics

Use built-in turtle commands for graphics:

```perl
# Draw a coloured square spiral
my $side = 10;
for my $step (1..40) {
    my @colors = ([255,80,80],[80,180,80],[80,120,220],[200,160,30]);
    my $c = $colors[$step % 4];
    color($c->[0], $c->[1], $c->[2]);
    forward($side);
    right(91);
    $side += 3;
}
```

Available turtle commands: `forward(n)`, `backward(n)`, `left(deg)`, `right(deg)`, `penup()`, `pendown()`, `color(r, g, b)`, `setheading(deg)`, `home()`.

## Next Steps

- Explore `Examples/perl/` for complete demo programs
- Try `regex.perl` for a comprehensive regex showcase
- Try `turtle_art.perl` for geometric pattern drawing
- Read `perldoc perlintro` (outside the IDE) for the full Perl manual
