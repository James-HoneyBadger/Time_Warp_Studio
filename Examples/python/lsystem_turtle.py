# ============================================================
# L-SYSTEM FRACTAL GENERATOR — Python Turtle Showcase
# Koch * Hilbert * Dragon Curve * Sierpinski * Barnsley Fern
# Time Warp Studio — Python Language Demo
# ============================================================

import math
import random

# ===== L-SYSTEM ENGINE =====

def lsystem_expand(axiom, rules, generations):
    """Expand an L-system axiom for N generations."""
    state = axiom
    for _ in range(generations):
        new_state = []
        for ch in state:
            new_state.append(rules.get(ch, ch))
        state = ''.join(new_state)
    return state

def lsystem_draw(instructions, angle_deg, step,
                 start_x=0, start_y=0, start_heading=0):
    """
    Interpret L-system string as turtle commands.
    F = forward, f = forward no draw, + = turn left, - = turn right
    [ = push state, ] = pop state, | = turn 180
    """
    stack = []
    x, y = start_x, start_y
    heading = start_heading

    turtle.penup()
    turtle.goto(x, y)
    turtle.setheading(heading)
    turtle.pendown()

    for ch in instructions:
        if ch in ('F', 'A', 'B', 'X', 'Y'):
            turtle.fd(step)
        elif ch == 'f':
            turtle.penup()
            turtle.fd(step)
            turtle.pendown()
        elif ch == '+':
            turtle.lt(angle_deg)
        elif ch == '-':
            turtle.rt(angle_deg)
        elif ch == '[':
            stack.append((turtle.xcor(), turtle.ycor(),
                          turtle.heading()))
        elif ch == ']':
            if stack:
                px, py, ph = stack.pop()
                turtle.penup()
                turtle.goto(px, py)
                turtle.setheading(ph)
                turtle.pendown()
        elif ch == '|':
            turtle.lt(180)

# ===== L-SYSTEM DEFINITIONS =====

LSYSTEMS = {
    'Koch Snowflake': {
        'axiom': 'F++F++F',
        'rules': {'F': 'F-F++F-F'},
        'angle': 60,
        'generations': 4,
        'step': 3,
        'start': (-120, -60),
        'heading': 0,
        'color': (0, 255, 220),
        'description': 'Infinite perimeter, finite area — classic paradox',
    },
    'Sierpinski Triangle': {
        'axiom': 'A-B-B',
        'rules': {'A': 'A-B+A+B-A', 'B': 'BB'},
        'angle': 120,
        'generations': 6,
        'step': 4,
        'start': (-120, -100),
        'heading': 0,
        'color': (255, 180, 0),
        'description': 'Self-similar at every scale — Hausdorff dim ≈ 1.585',
    },
    'Dragon Curve': {
        'axiom': 'FX',
        'rules': {'X': 'X+YF+', 'Y': '-FX-Y'},
        'angle': 90,
        'generations': 11,
        'step': 5,
        'start': (-20, 20),
        'heading': 0,
        'color': (255, 80, 160),
        'description': 'Never crosses itself — featured in Jurassic Park',
    },
    'Hilbert Curve': {
        'axiom': 'A',
        'rules': {'A': '+BF-AFA-FB+', 'B': '-AF+BFB+FA-'},
        'angle': 90,
        'generations': 5,
        'step': 6,
        'start': (-90, -90),
        'heading': 0,
        'color': (80, 200, 255),
        'description': 'Space-filling curve — visits every point in the square',
    },
    'Barnsley Fern Outline': {
        'axiom': 'X',
        'rules': {'X': 'F+[[X]-X]-F[-FX]+X', 'F': 'FF'},
        'angle': 25,
        'generations': 5,
        'step': 4,
        'start': (0, -130),
        'heading': 90,
        'color': (80, 220, 80),
        'description': 'Biologically realistic fern — discovered by Michael Barnsley',
    },
}

# ===== STATISTICS ON L-SYSTEM STRINGS =====

def lsystem_stats(name, system):
    axiom = system['axiom']
    rules = system['rules']
    gen = system['generations']

    print(f"\n  {name}")
    print(f"  {system['description']}")
    print(f"  Axiom: {axiom!r}  Angle: {system['angle']}°  Generations: {gen}")

    # Show growth of string length
    state = axiom
    for g in range(gen + 1):
        if g > 0:
            new_state = []
            for ch in state:
                new_state.append(rules.get(ch, ch))
            state = ''.join(new_state)
        draws = sum(1 for c in state if c in 'FABfXY')
        turns = sum(1 for c in state if c in '+-|')
        branches = state.count('[')
        print(f"  Gen {g}: length={len(state):6d} "
              f"draws={draws:5d} turns={turns:5d} branches={branches:4d}")

# ===== TURTLE UTILITIES =====

def draw_title(text):
    turtle.penup()
    turtle.goto(-140, 160)
    turtle.pendown()

def set_pen_color(rgb):
    """Set turtle color from (r,g,b) tuple 0-255."""
    r, g, b = rgb
    turtle.color(r/255, g/255, b/255)

# ===== MAIN PROGRAM =====

print("=" * 60)
print("  L-SYSTEM FRACTAL GENERATOR — Python & Turtle")
print("  Exploring mathematical beauty through recursive rules")
print("=" * 60)

print("\n  L-System Statistics (string growth per generation):")
print("  " + "-" * 56)

for name, system in LSYSTEMS.items():
    lsystem_stats(name, system)

print("\n" + "=" * 60)
print("  Drawing fractals — watch the canvas!")
print("=" * 60)

# Draw each fractal
for name, system in LSYSTEMS.items():
    print(f"\n  Drawing: {name}")
    turtle.clear()
    turtle.home()
    turtle.pensize(1)
    turtle.ht()

    r, g, b = system['color']
    turtle.color(r, g, b)
    turtle.pensize(1)

    instructions = lsystem_expand(
        system['axiom'],
        system['rules'],
        system['generations']
    )

    sx, sy = system['start']
    lsystem_draw(
        instructions,
        system['angle'],
        system['step'],
        start_x=sx,
        start_y=sy,
        start_heading=system['heading']
    )
    print(f"    Done! {len(instructions)} instructions executed.")

# ===== BONUS: RANDOM WALK COMPARISON =====
print("\n  Bonus: Random walk vs fractal — demonstrating order vs chaos")
turtle.clear()
turtle.home()
turtle.pensize(1)

# Random walk
turtle.color(255, 100, 100)
x, y = 0, 0
turtle.penup()
turtle.goto(0, 0)
turtle.pendown()
random.seed(42)
for _ in range(500):
    angle = random.randint(0, 3) * 90
    turtle.setheading(angle)
    turtle.fd(8)

# Dragon curve beside it
turtle.penup()
turtle.goto(-150, 0)
turtle.setheading(0)
turtle.pendown()
turtle.color(0, 220, 180)
instructions = lsystem_expand('FX', {'X': 'X+YF+', 'Y': '-FX-Y'}, 10)
lsystem_draw(instructions, 90, 4, -150, 0, 0)

print("\n" + "=" * 60)
print("  Python L-System Demo Complete!")
print("  Koch | Sierpinski | Dragon | Hilbert | Fern | Random Walk")
print("  L-Systems: Aristid Lindenmayer, 1968")
print("=" * 60)
