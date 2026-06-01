# PostScript Tutorial

## Introduction

PostScript (1982, Adobe) is a stack-based, Turing-complete page-description language. It was the first widely adopted language to unify graphics and text in a device-independent way. Learning PostScript teaches fundamental stack-machine programming.

**Key characteristics:**
- Stack-based: operands are pushed before the operator
- Everything is an operation — numbers, strings, names, code blocks
- `{...}` are code arrays (procedures) — first-class values
- Coordinate system: origin (0,0) at bottom-left, points as units

## Hello World

```postscript
%!PS
/Helvetica findfont 24 scalefont setfont
100 700 moveto
(Hello, World!) show
showpage
```

`%!PS` is the required magic comment. `showpage` flushes the page.

## The Stack

```postscript
%!PS
% Push values onto the stack
3
4
% add pops two values, pushes their sum
add          % stack: 7
2
mul          % stack: 14
= pop and print
```

Use `=` to pop and print the top of stack during debugging. `pstack` prints all without popping.

## Arithmetic

```postscript
%!PS
3 4 add =        % 7
10 3 sub =       % 7
4 5 mul =        % 20
10 4 div =       % 2.5
17 5 mod =       % 2
2 8 exp =        % 256.0
16 sqrt =        % 4.0
-5 abs =         % 5
```

Operators pop their arguments from the stack and push the result.

## Variables

```postscript
%!PS
/x 42 def
/name (Alice) def

x =              % 42

% Arithmetic with variables
/result x 2 mul def
result =         % 84
```

`/name` is a name literal. `def` binds name to value in current dictionary.

## Stack Manipulation

```postscript
%!PS
% dup: duplicate top
5 dup mul =      % 25 (5*5)

% exch: swap top two
3 7 exch         % stack: 7 3
sub =            % 4

% pop: discard top
10 20 pop =      % 10

% copy: copy top N items
1 2 3 3 copy     % stack: 1 2 3 1 2 3
```

## Drawing Lines and Shapes

```postscript
%!PS
newpath
100 100 moveto
200 100 lineto
200 200 lineto
100 200 lineto
closepath
stroke
showpage
```

```postscript
%!PS
% Filled rectangle
newpath
50 50 moveto
150 50 lineto
150 150 lineto
50 150 lineto
closepath
0.5 0.7 1.0 setrgbcolor
fill
showpage
```

## Circles and Arcs

```postscript
%!PS
% arc: x y radius start-angle end-angle
newpath
200 300 80 0 360 arc
stroke
showpage

% Filled circle
newpath
200 300 50 0 360 arc
1 0 0 setrgbcolor
fill
showpage
```

## Text

```postscript
%!PS
/Times-Roman findfont 36 scalefont setfont
150 500 moveto
0 0 0 setrgbcolor
(Hello PostScript!) show
showpage
```

## Control Flow

```postscript
%!PS
% if
5 3 gt {
    (5 is greater) =
} if

% ifelse
42 0 gt {
    (positive) =
} {
    (non-positive) =
} ifelse
```

## Loops

```postscript
%!PS
% repeat
5 {
    (Hello) =
} repeat

% for: initial increment limit proc
1 1 5 {
    dup =   % print current value
} for

% forall: iterate over array
[10 20 30 40 50] {
    =
} forall
```

## Procedures

```postscript
%!PS
/square {
    dup mul
} def

5 square =    % 25
7 square =    % 49

/factorial {
    dup 1 le {
        pop 1
    } {
        dup 1 sub factorial mul
    } ifelse
} def

6 factorial =   % 720
```

## Quick Reference

| Operation | Stack effect | Meaning |
|-----------|-------------|---------|
| `add` | (a b → a+b) | Addition |
| `sub` | (a b → a-b) | Subtraction |
| `mul` | (a b → a*b) | Multiplication |
| `div` | (a b → a/b) | Division |
| `mod` | (a b → a%b) | Modulo |
| `dup` | (a → a a) | Duplicate |
| `pop` | (a →) | Discard |
| `exch` | (a b → b a) | Swap |
| `def` | (/name val →) | Define |
| `moveto` | (x y →) | Move pen |
| `lineto` | (x y →) | Draw line |
| `stroke` | () | Render path |
| `fill` | () | Fill path |
| `show` | (str →) | Draw text |
| `showpage` | () | Output page |
