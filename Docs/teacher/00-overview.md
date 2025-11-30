# Teacher's Guide to Time Warp IDE

A comprehensive resource for educators using Time Warp IDE in the classroom.

---

## Table of Contents

1. [Introduction for Educators](#introduction-for-educators)
2. [Curriculum Integration](#curriculum-integration)
3. [Classroom Setup](#classroom-setup)
4. [Teaching Strategies](#teaching-strategies)
5. [Assessment Tools](#assessment-tools)
6. [Lesson Plans](#lesson-plans)
7. [Differentiation](#differentiation)
8. [Troubleshooting](#troubleshooting)
9. [Resources](#resources)

---

## Introduction for Educators

### Why Time Warp IDE for Education?

Time Warp IDE brings together six classic programming languages in one unified environment, offering unique pedagogical advantages:

**Multiple Learning Paths**: Students can explore different programming paradigms without learning different tools.

**Visual Learning**: Built-in turtle graphics make abstract concepts concrete and engaging.

**Immediate Feedback**: Real-time execution and clear error messages help students learn through experimentation.

**Historical Context**: Students see how programming evolved, understanding why modern languages work the way they do.

**Cross-Platform**: Works on school computers running Linux, macOS, or Windows.

### Educational Philosophy

Time Warp IDE embodies these principles:

1. **Learning by Doing**: Students write code from day one
2. **Progressive Complexity**: Start simple, add sophistication gradually
3. **Creative Expression**: Programming as a creative medium
4. **Clear Feedback**: Errors are learning opportunities, not failures
5. **Multiple Approaches**: Different languages for different thinking styles

---

## Curriculum Integration

### Alignment with Standards

Time Warp IDE supports common computer science education standards:

**CSTA K-12 Standards**:
- 1B-AP-08: Compare and refine multiple algorithms
- 1B-AP-10: Create programs including sequences, events, loops, conditionals
- 2-AP-11: Create procedures with parameters
- 2-AP-13: Decompose problems into sub-problems
- 2-AP-16: Include code comments for documentation

**ISTE Standards for Students**:
- Computational Thinker: Develop and test algorithms
- Creative Communicator: Create original works
- Innovative Designer: Solve problems through iterative design

### Scope and Sequence

**Level 1: Introduction (4-6 weeks)**
- Basic concepts: sequences, loops, variables
- Language: BASIC or PILOT
- Output: Simple text programs, guessing games
- Assessment: Can write programs with input/output and conditionals

**Level 2: Visual Programming (4-6 weeks)**
- Turtle graphics, procedures, parameters
- Language: Logo
- Output: Geometric patterns, artistic creations
- Assessment: Can create procedures with parameters

**Level 3: Structured Programming (6-8 weeks)**
- Strong typing, functions, data structures
- Language: Pascal
- Output: Data processing programs, grade calculators
- Assessment: Can design multi-function programs

**Level 4: Logic and AI (4-6 weeks)**
- Declarative programming, rules, queries
- Language: Prolog
- Output: Knowledge bases, logic puzzles
- Assessment: Can define facts and rules

**Level 5: Systems Concepts (6-8 weeks)**
- Low-level programming, pointers, memory
- Language: C
- Output: Utility programs, algorithms
- Assessment: Can write programs with arrays and functions

---

## Classroom Setup

### Hardware Requirements

**Minimum per Student Station**:
- Processor: Any modern CPU (1 GHz+)
- RAM: 512 MB minimum, 1 GB recommended
- Display: 1024×768 resolution
- Storage: 50 MB for installation

**Network**: Not required, but useful for:
- Sharing student work
- Accessing online examples
- Collaborative projects

### Software Installation

**Option 1: System-Wide Installation** (requires admin)
```bash
# See installation/01-linux.md for detailed steps
sudo apt install time-warp-ide
```

**Option 2: Portable Installation** (no admin needed)
```bash
# Extract to USB drive or network folder
./time-warp-ide
```

**Option 3: Virtual Environment**
- Use VirtualBox or similar
- Pre-configured images available
- Students can work anywhere

### Lab Layout Suggestions

**Traditional Rows**:
- Good for direct instruction
- Easy monitoring
- Individual assessment

**Pods/Groups**:
- Encourages collaboration
- Pair programming
- Peer learning

**Maker Space**:
- Flexible arrangement
- Project-based work
- Creative exploration

---

## Teaching Strategies

### First Day

**Hook**: Show an impressive turtle graphics program
```logo
; Spiral Flower - Show this running
TO FLOWER
  REPEAT 36 [
    REPEAT 4 [FORWARD 100 RIGHT 90]
    RIGHT 10
  ]
END
FLOWER
```

**Explain**: "You'll be able to create this by the end of the unit!"

**Start Simple**:
```basic
10 PRINT "Hello, [Your Name]!"
```

**Key Message**: Everyone can program. We'll start small and build up.

### Lesson Structure (50-minute period)

**Minutes 0-10: Review/Warm-up**
- Quick coding challenge from previous lesson
- Review key concepts
- Share interesting student work

**Minutes 10-25: Direct Instruction**
- Introduce new concept (show, don't just tell)
- Live coding demonstration
- Think-aloud while programming
- Common mistakes and how to fix them

**Minutes 25-40: Guided Practice**
- Students work on structured exercises
- Circulate and assist
- Ask guiding questions, don't give answers
- Encourage experimentation

**Minutes 40-50: Wrap-up**
- Share solutions (multiple approaches welcome)
- Preview next lesson
- Exit ticket: "One thing you learned, one question you have"

### Effective Demonstrations

**Live Coding Best Practices**:
1. **Think aloud**: Explain your reasoning
2. **Make mistakes**: Show debugging in action
3. **Involve students**: Ask for suggestions
4. **Build incrementally**: Test frequently
5. **Save examples**: Create a class library

**Common Pitfalls to Demonstrate**:
- Typos and syntax errors
- Logic errors (wrong operators)
- Off-by-one errors in loops
- Forgetting to reset variables
- Missing END or semicolons

### Encouraging Problem-Solving

**When Students Ask "Is this right?"**:
- "What happens when you run it?"
- "Does it do what you wanted?"
- "Try it and see!"

**When Students Say "It doesn't work!"**:
- "What were you expecting?"
- "What actually happened?"
- "What have you tried so far?"
- "Can you show me the error message?"

**When Students Are Stuck**:
- "Break it into smaller pieces"
- "What part works already?"
- "Can you explain your plan in English first?"
- "Look at a similar example"

### Pair Programming

**Driver/Navigator Roles**:
- Driver: Types code, focuses on syntax
- Navigator: Thinks strategically, spots errors
- Switch roles every 10-15 minutes

**Benefits**:
- Fewer frustrations
- Peer learning
- Better code quality
- Soft skills development

**Managing Pairs**:
- Assign pairs or let students choose
- Rotate partners periodically
- Monitor participation
- Address conflicts promptly

---

## Assessment Tools

### Formative Assessment

**Daily Check-ins**:
- Code review: Walk around, look at screens
- Quick questions: "Show me a loop" "Explain this line"
- Exit tickets: Written or digital

**Code Reading**:
- Give students code, ask what it does
- Find and fix bugs in example code
- Trace execution with specific inputs

**Concept Checks**:
```
True/False:
- A loop repeats code [TRUE]
- Variables can store numbers or text [TRUE]
- GOTO is required in modern programming [FALSE]

Fill in the blank:
- To display output in BASIC, use _____ [PRINT]
- Logo's turtle draws when pen is _____ [DOWN]
```

### Summative Assessment

**Programming Projects**:

*Beginner Project: Guessing Game*
- Requirements: Use variables, loops, conditionals, input/output
- Rubric:
  - Correct logic (30%)
  - Code organization (20%)
  - Comments (20%)
  - Creative enhancements (15%)
  - Testing (15%)

*Intermediate Project: Turtle Art Gallery*
- Requirements: Multiple procedures, parameters, colors
- Rubric:
  - Technical implementation (35%)
  - Creativity and design (25%)
  - Code reusability (20%)
  - Documentation (20%)

*Advanced Project: Choose Your Adventure*
- Requirements: Functions, data structures, user interface
- Rubric:
  - Program complexity (30%)
  - Code organization (25%)
  - User experience (20%)
  - Documentation (15%)
  - Testing and debugging (10%)

**Written Assessments**:
- Trace code execution on paper
- Design algorithms in pseudocode
- Explain programming concepts
- Compare different approaches

### Rubric Example: Basic Programming Project

| Criterion | Excellent (4) | Good (3) | Satisfactory (2) | Needs Work (1) |
|-----------|--------------|----------|------------------|----------------|
| **Functionality** | Program works perfectly, handles edge cases | Works with typical input, minor bugs | Mostly works, some features incomplete | Many errors, doesn't run |
| **Code Quality** | Well-organized, elegant solutions | Generally clear and organized | Some organization, hard to follow | Disorganized, difficult to understand |
| **Documentation** | Comprehensive comments, clear explanations | Good comments on key sections | Some comments, incomplete | Few or no comments |
| **Creativity** | Innovative features beyond requirements | Some creative additions | Meets basic requirements | Minimal effort |

### Portfolio Assessment

Have students maintain a programming portfolio:

**Contents**:
- Best programs from each unit
- Reflections on learning
- Screenshots of visual programs
- Descriptions of challenges overcome

**Benefits**:
- Track growth over time
- Evidence for standards alignment
- Student ownership of learning
- Share with parents/administrators

---

## Lesson Plans

### Unit 1, Lesson 1: Introduction to Programming

**Objectives**:
- Students can write a simple BASIC program
- Students understand sequence and output
- Students can run programs and interpret results

**Materials**:
- Time Warp IDE installed
- Example programs
- Exit ticket handout

**Procedure**:

*Engage (10 min)*:
1. Show classic video game or program
2. Ask: "How do you think this was created?"
3. Discuss: Programming tells computers what to do

*Explore (15 min)*:
1. Open Time Warp IDE
2. Type together:
   ```basic
   10 PRINT "Hello, Class!"
   20 PRINT "Today is our first programming lesson"
   30 END
   ```
3. Run and observe output
4. Modify: Change text, add line 25 with their name

*Explain (10 min)*:
1. Line numbers organize program
2. PRINT displays output
3. Quotes around text (strings)
4. END terminates program
5. Programs run top to bottom (sequence)

*Elaborate (10 min)*:
Students create personalized program:
- Name, age, favorite hobby
- At least 5 lines
- Creative content encouraged

*Evaluate (5 min)*:
- Share programs (volunteers)
- Exit ticket: "Programming is..."
- Preview next lesson: Input and variables

### Unit 2, Lesson 5: Turtle Graphics Introduction

**Objectives**:
- Students can control turtle movement
- Students understand basic Logo commands
- Students create simple geometric shapes

**Materials**:
- Time Warp IDE (Logo mode)
- Shape reference cards
- Graph paper (optional)

**Procedure**:

*Engage (5 min)*:
1. Show complex turtle graphic
2. Ask: "How many lines of code do you think this took?"
3. Reveal: Often just a few commands with loops!

*Explore (15 min)*:
1. Switch to Logo
2. Try commands together:
   ```logo
   FORWARD 100
   RIGHT 90
   FORWARD 100
   ```
3. Predict: What shape are we drawing?
4. Complete the square:
   ```logo
   RIGHT 90
   FORWARD 100
   RIGHT 90
   FORWARD 100
   ```

*Explain (10 min)*:
1. Turtle starts at center, facing up
2. FORWARD/BACK: Distance in steps
3. RIGHT/LEFT: Angle in degrees
4. 90° = square corner, 360° = full circle
5. Pen draws as turtle moves

*Elaborate (15 min)*:
Challenge: Draw these shapes
1. Triangle (hint: 120° turns)
2. Pentagon (hint: 72° turns)
3. Your initials

*Evaluate (5 min)*:
- Gallery walk: View each other's work
- Discuss: How did you figure out the angles?
- Preview: Next time, we'll use REPEAT for efficiency

---

## Differentiation

### For Students Who Need Extra Support

**Scaffolding Strategies**:
- Provide code templates with blanks to fill
- Use color-coded syntax highlighting prominently
- Create step-by-step checklists
- Pair with supportive peer
- Offer sentence starters for comments
- Break assignments into smaller pieces

**Modified Assignments**:
- Fewer features required
- Provided starter code
- More structured guidance
- Extended deadlines

**Example: Scaffolded Loop Exercise**
```basic
10 REM Count to 10
20 FOR I = ___ TO ___
30   PRINT ___
40 NEXT ___
```

### For Advanced Students

**Enrichment Opportunities**:
- Challenge problems with multiple solutions
- Explore additional languages early
- Mentor other students
- Create programs for younger classes
- Research programming history
- Build complex projects

**Extension Activities**:
- "How many ways can you solve this?"
- Add features beyond requirements
- Optimize for speed or elegance
- Create teaching materials for peers

**Example: Advanced Challenge**
"Create a program that draws your name in block letters using turtle graphics. Requirements: Use procedures with parameters, at least 3 colors, movable anywhere on screen."

### For English Language Learners

**Strategies**:
- Visual programming (Logo) reduces language barrier
- Provide bilingual terminology lists
- Use diagrams and flowcharts
- Allow code comments in native language
- Pair with bilingual buddy

**Supports**:
- Keyword glossary with examples
- Visual command reference
- Error message explanations
- Step-by-step visual guides

---

## Troubleshooting

### Common Technical Issues

**"Time Warp won't start"**:
- Check system requirements (especially CPU features)
- Verify installation completed
- Look for error logs in user directory
- Try portable version

**"My program won't run"**:
- Check for syntax errors (red underlines)
- Verify language mode matches code
- Look at error message carefully
- Try example program to test IDE

**"Turtle graphics don't show"**:
- Ensure View → Turtle Canvas is enabled
- Try Run → Reset Turtle
- Check that commands are Logo or Logo-enabled
- Restart IDE if needed

### Common Student Coding Errors

**BASIC**:
```basic
10 PRINT Hello          ❌ Missing quotes
10 PRINT "Hello"        ✅ Correct

10 LET X = 5 + Y        ❌ Y not defined yet
10 LET Y = 3            ✅ Define first
20 LET X = 5 + Y        ✅ Then use

10 FOR I = 1 TO 10      ❌ Missing NEXT
10 FOR I = 1 TO 10      ✅ Complete loop
20   PRINT I
30 NEXT I
```

**Logo**:
```logo
FORWARD                 ❌ Missing parameter
FORWARD 100             ✅ Correct

REPEAT 4 FORWARD 100    ❌ Missing brackets
REPEAT 4 [FORWARD 100]  ✅ Correct

SETCOLOR 1              ❌ Wrong command name
SETPENCOLOR 1           ✅ Correct
```

**Pascal**:
```pascal
age = 25;               ❌ Wrong assignment
age := 25;              ✅ Use :=

writeln('Hello')        ❌ Missing semicolon
writeln('Hello');       ✅ Correct

if age > 18 then        ❌ Inconsistent indentation
writeln('Adult')
else
  writeln('Minor');

if age > 18 then        ✅ Consistent style
  writeln('Adult')
else
  writeln('Minor');
```

### Classroom Management Issues

**Students Racing Ahead**:
- Provide extension challenges
- Ask them to help others
- Introduce advanced topics early

**Students Copying Code**:
- Emphasize learning over completion
- Require code explanations
- Use varied assessments
- Teach collaboration vs. cheating

**Unequal Participation in Pairs**:
- Enforce role switching
- Individual check-ins
- Assign specific responsibilities
- Allow partner feedback

---

## Resources

### For Teachers

**Professional Development**:
- CSTA (Computer Science Teachers Association)
- Code.org professional learning
- Local CS teacher meetups
- Online communities (Reddit r/CSEducation)

**Curriculum Resources**:
- CS Unplugged (offline activities)
- Scratch (visual programming bridge)
- Hour of Code activities
- Khan Academy CS courses

### For Students

**Tutorials and Practice**:
- Time Warp Examples folder
- Codecademy (supplementary)
- Programming books for kids
- Online coding challenges

**Inspiration**:
- Classic BASIC games
- Turtle art galleries
- Programming competition sites
- Student showcase websites

### Sample Class Files

Create a shared folder with:
- Starter code templates
- Example programs
- Common error references
- Project requirements
- Rubrics and checklists

---

## Year-Long Planning

### Semester 1: Foundations
- Weeks 1-6: BASIC (sequence, variables, conditionals, loops)
- Weeks 7-12: Logo (turtle graphics, procedures, parameters)
- Weeks 13-16: PILOT (teaching-focused programming)
- Week 17-18: Review and semester project

### Semester 2: Advanced Topics
- Weeks 1-8: Pascal (structured programming, functions, data types)
- Weeks 9-12: Prolog (logic programming) OR C (systems concepts)
- Weeks 13-16: Final project (student choice of language)
- Weeks 17-18: Presentations and portfolio completion

---

## Success Strategies

**Build a Programming Culture**:
- Display student work prominently
- Celebrate creative solutions
- Share "bug of the week" learning moments
- Host coding club or after-school sessions

**Parent Communication**:
- Send home program printouts
- Explain what students are learning
- Suggest programming activities at home
- Showcase work at parent nights

**Assessment Balance**:
- Mix individual and collaborative work
- Include projects and traditional tests
- Value process and product equally
- Allow revisions and resubmissions

**Your Own Learning**:
- Program alongside students
- Try languages you're less familiar with
- Make mistakes publicly
- Model lifelong learning

---

Time Warp IDE is more than a tool—it's a gateway to computational thinking, creative expression, and problem-solving skills. Your role as educator is to guide students through this exciting journey.

**Questions?** Check the [FAQ](../user/03-faq.md) or join the [teacher community](https://github.com/honey-badger-org/Time_Warp/discussions/teachers).

Good luck, and happy teaching!
