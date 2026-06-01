#!/usr/bin/perl
# Turtle Art in Perl 5 — SHOWCASE
# Mathematical curves and geometric patterns with turtle graphics

use strict;
use warnings;

# --- Helper: draw a polygon ---
sub polygon {
    my ($sides, $size) = @_;
    my $angle = 360 / $sides;
    for my $i (1..$sides) {
        forward($size);
        right($angle);
    }
}

# --- Helper: draw a star ---
sub star {
    my ($points, $size) = @_;
    my $angle = 180 - 180 * ($points - 2) / $points;
    for my $i (1..$points) {
        forward($size);
        right(180 - $angle);
    }
}

# === Colour palette (r g b) ===
my @palette = (
    [220, 50,  50 ],  # red
    [50,  180, 80 ],  # green
    [60,  120, 220],  # blue
    [200, 150, 30 ],  # gold
    [160, 60,  200],  # purple
    [30,  200, 200],  # cyan
);

# --- Pattern 1: Nested hexagons ---
penup();
setheading(0);
pendown();

for my $ring (1..6) {
    my $c = $palette[($ring - 1) % 6];
    color($c->[0], $c->[1], $c->[2]);
    polygon(6, $ring * 12);
    left(10);
}

# --- Pattern 2: Spiral square ---
penup();
setheading(0);
pendown();

my $side = 5;
for my $step (1..60) {
    my $idx = int(($step - 1) / 10) % 6;
    my $c = $palette[$idx];
    color($c->[0], $c->[1], $c->[2]);
    forward($side);
    right(91);
    $side += 2;
}

# --- Pattern 3: Petal flower ---
penup();
setheading(0);
pendown();
color(220, 80, 120);

for my $petal (1..12) {
    for my $step (1..60) {
        forward(1);
        right(1);
    }
    for my $step (1..60) {
        forward(1);
        right(1);
    }
    right(30);
}

print "Turtle art complete!\n";
