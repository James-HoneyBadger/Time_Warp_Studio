# Time Warp IDE - Example Programs

This directory contains sample programs demonstrating the three supported languages:

## PILOT Programs

PILOT (Programmed Inquiry, Learning, Or Teaching) - Simple command-based language

- `pilot_adventure.pilot` - Text adventure game
- `pilot_dragon_adventure.pilot` - Extended adventure with branching story
- `pilot_quiz.pilot` - Simple quiz program
- `pilot_quiz_competition.pilot` - Multi-question quiz
- `pilot_simple_calculator.pilot` - Basic calculator
- `pilot_story_builder.pilot` - Interactive story creation
- `pilot_screen_demo.pilot` - Screen control demonstration

**PILOT Commands:**
- `T:` - Type (print) text
- `A:` - Accept (input) to variable
- `M:` - Match input against pattern
- `Y:` - Yes (jump if match succeeded)
- `N:` - No (jump if match failed)
- `C:` - Compute expression
- `U:` - Use (print) variable
- `J:` - Jump to label
- `L:` - Label definition
- `E:` - End program

## BASIC Programs

Classic BASIC with line numbers and structured commands

- `basic_countdown.bas` - Simple countdown timer
- `basic_guess.bas` - Number guessing game
- `basic_multiplication_table.bas` - Print multiplication tables
- `basic_rock_paper_scissors.bas` - Rock, paper, scissors game
- `basic_hangman.bas` - Word guessing game
- `basic_cls_locate.bas` - Screen positioning demo
- `basic_screen_modes.bas` - Graphics mode demonstration
- `basic_inkey_demo.bas` - Keyboard input demo
- `basic_arrow_keys.bas` - Arrow key navigation
- `basic_graphics.bas` - Simple graphics commands

## Logo Programs

Logo turtle graphics for visual learning

### Simple Shapes
- `logo_square.logo` - Draw a square
- `logo_star.logo` - Draw a star
- `logo_polygons.logo` - Multiple polygon shapes

### Geometric Patterns
- `logo_flower.logo` - Flower pattern
- `logo_petal_rosette.logo` - Rosette with petals
- `logo_polygonal_rose.logo` - Rose curve pattern
- `logo_spirograph.logo` - Spirograph-like pattern
- `logo_starburst.logo` - Radiating starburst

### Spirals and Fractals
- `logo_spiral_walk.logo` - Simple spiral
- `logo_rainbow_spiral.logo` - Colorful spiral
- `logo_recursive_spiral.logo` - Recursive spiral pattern
- `logo_fractal_tree.logo` - Fractal tree structure
- `logo_koch_snowflake.logo` - Koch snowflake fractal

### Complex Drawings
- `logo_house.logo` - House with windows and door
- `logo_snowman.logo` - Snowman with details

## Running Examples

### Using Python API

```python
from time_warp.core.interpreter import Interpreter
from time_warp.graphics.turtle_state import TurtleState

# Load example program
with open('examples/logo_square.logo', 'r') as f:
    program = f.read()

# Execute
interp = Interpreter()
turtle = TurtleState()
interp.load_program(program)
output = interp.execute(turtle)

# Display results
print('\n'.join(output))
print(f"Lines drawn: {len(turtle.lines)}")
```

### CLI (when implemented)

```bash
time-warp examples/logo_square.logo
time-warp examples/basic_guess.bas
time-warp examples/pilot_quiz.pilot
```

## Learning Path

### Beginners
1. Start with `pilot_quiz.pilot` - Learn basic I/O
2. Try `logo_square.logo` - Introduction to turtle graphics
3. Move to `basic_countdown.bas` - Understand loops

### Intermediate
1. `pilot_adventure.pilot` - Conditional logic and branching
2. `logo_spirograph.logo` - Complex turtle patterns
3. `basic_rock_paper_scissors.bas` - Game logic

### Advanced
1. `logo_fractal_tree.logo` - Recursive patterns
2. `pilot_dragon_adventure.pilot` - Complex state management
3. `basic_hangman.bas` - String manipulation and game state

## Modifying Examples

All examples are simple text files. Feel free to:

- Change numbers to see different patterns
- Add your own commands
- Combine ideas from multiple examples
- Create your own variations

## Contributing Examples

Have a cool program to share? Contributions welcome!

1. Keep examples educational and well-commented
2. Test with the interpreter before submitting
3. Follow existing naming convention: `language_description.ext`
4. Add entry to this README

---

*Happy coding! 🚀*
