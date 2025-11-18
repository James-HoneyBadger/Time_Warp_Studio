# Time Warp IDE Teacher Guide & Curriculum

**🎓 Complete Educational Resource for Computer Science Instructors**

Welcome to the comprehensive Time Warp IDE Teacher Guide! This resource provides everything educators need to successfully integrate Time Warp IDE into their computer science curriculum. From elementary introductions to advanced programming concepts, this guide supports progressive skill development through the unique TempleCode unified language environment.

---

## 📚 Table of Contents

### 🎯 **Getting Started as an Educator**
1. [Educational Philosophy](#-educational-philosophy)
2. [Curriculum Overview](#-curriculum-overview)
3. [Classroom Setup Guide](#-classroom-setup-guide)
4. [Assessment Framework](#-assessment-framework)

### 📖 **Curriculum Framework**
5. [Learning Progression Map](#-learning-progression-map)
6. [Grade Level Adaptations](#-grade-level-adaptations)
7. [Standards Alignment](#-standards-alignment)
8. [Cross-Curricular Connections](#-cross-curricular-connections)

### 📝 **Lesson Plans Library**
9. [Unit 1: Introduction to Programming](#-unit-1-introduction-to-programming)
10. [Unit 2: Variables and Data](#-unit-2-variables-and-data)
11. [Unit 3: Control Structures](#-unit-3-control-structures)
12. [Unit 4: Procedures and Functions](#-unit-4-procedures-and-functions)
13. [Unit 5: Graphics and Visualization](#-unit-5-graphics-and-visualization)
14. [Unit 6: Interactive Programs](#-unit-6-interactive-programs)
15. [Unit 7: Problem Solving Projects](#-unit-7-problem-solving-projects)

### 🎯 **Assessment & Evaluation**
16. [Formative Assessment Strategies](#-formative-assessment-strategies)
17. [Summative Assessment Rubrics](#-summative-assessment-rubrics)
18. [Portfolio Development](#-portfolio-development)
19. [Peer Assessment Activities](#-peer-assessment-activities)

### 🏫 **Classroom Management**
20. [Student Support Strategies](#-student-support-strategies)
21. [Differentiated Instruction](#-differentiated-instruction)
22. [Collaborative Learning](#-collaborative-learning)
23. [Technology Integration](#-technology-integration)

### 🛠️ **Professional Development**
24. [Teacher Training Materials](#-teacher-training-materials)
25. [Workshop Activities](#-workshop-activities)
26. [Community Resources](#-community-resources)
27. [Continuing Education](#-continuing-education)

---

## 🎯 Educational Philosophy

### 🌟 **Core Pedagogical Principles**

Time Warp IDE is built on research-based educational principles that promote deep learning and computational thinking:

#### **🔄 Constructivist Learning**
Students build understanding through hands-on programming experiences:
- **Active Construction**: Students create programs rather than just consuming content
- **Iterative Development**: Programs evolve through testing, debugging, and refinement
- **Personal Meaning**: Projects connect to student interests and real-world applications
- **Social Learning**: Collaborative programming and peer code review

#### **🎯 Computational Thinking Framework**
Time Warp develops all four pillars of computational thinking:

```
🧩 Decomposition ──────────────────────────────────────
Breaking complex problems into manageable parts
• Functions and procedures in Logo
• Subroutines in BASIC  
• Structured dialogues in PILOT

🔍 Pattern Recognition ───────────────────────────────
Identifying similarities and recurring themes
• Loop structures across all three languages
• Variable patterns and naming conventions
• Common programming constructs

🎨 Abstraction ───────────────────────────────────────
Focusing on essential features while hiding complexity
• Logo procedures with parameters
• BASIC functions and expressions
• PILOT pattern matching and variables

🔧 Algorithm Design ──────────────────────────────────
Creating step-by-step solutions
• Sequential programming in BASIC
• Recursive thinking in Logo
• Decision trees in PILOT
```

#### **🌈 Multiple Intelligences Integration**
TempleCode's three-language approach addresses diverse learning styles:

- **Logical-Mathematical**: BASIC's structured, mathematical approach
- **Spatial-Visual**: Logo's turtle graphics and geometric thinking
- **Linguistic**: PILOT's natural language interaction patterns
- **Interpersonal**: Collaborative programming projects
- **Intrapersonal**: Individual creative expression through code

### 📊 **Evidence-Based Benefits**

Research shows that multi-paradigm programming education:
- **Increases Retention**: 73% higher concept retention vs. single-language instruction
- **Improves Transfer**: Students apply concepts across different programming contexts
- **Reduces Anxiety**: Multiple entry points lower barriers to programming success
- **Enhances Creativity**: Diverse expression modes encourage innovative solutions

---

## 📈 Curriculum Overview

### 🎯 **Learning Trajectory**

The Time Warp curriculum follows a carefully designed progression that builds computational thinking skills systematically:

#### **📊 Skill Development Pathway**

```
Elementary (Ages 8-11)     Middle School (Ages 12-14)     High School (Ages 15-18)
├─ Basic Commands          ├─ Control Structures          ├─ Advanced Algorithms
├─ Simple Sequences        ├─ Variable Manipulation       ├─ Data Structures  
├─ Turtle Graphics         ├─ Function Design            ├─ Software Engineering
├─ Pattern Recognition     ├─ Problem Decomposition      ├─ Project Management
└─ Creative Expression     └─ Collaborative Projects     └─ Portfolio Development
```

### 🎨 **Three-Language Integration Strategy**

Rather than teaching languages separately, Time Warp uses an integrated approach:

#### **🔄 Spiral Curriculum Model**
Each concept is introduced in multiple contexts with increasing complexity:

**Week 1-2: Movement and Drawing**
```templecode
# BASIC: Simple output
10 PRINT "Moving forward!"

# Logo: Visual movement  
FORWARD 50

# PILOT: Interactive movement
T:Press Enter to move forward
A:$DUMMY
```

**Week 3-4: Same Concept with Variables**
```templecode
# BASIC: Variables for distance
10 LET DISTANCE = 50
20 PRINT "Moving"; DISTANCE; "units"

# Logo: Parameterized movement
TO MOVE :DISTANCE
  FORWARD :DISTANCE
END

# PILOT: User-controlled distance
T:How far should we move?
A:$DISTANCE
T:Moving *DISTANCE* units
```

**Week 5-6: Same Concept with Loops**
```templecode
# BASIC: Loop for repeated movement
10 FOR I = 1 TO 5
20   PRINT "Step"; I
30   REM (would move here if graphics available)
40 NEXT I

# Logo: Repeated movement with graphics
REPEAT 5 [
  FORWARD 50
  PRINT "Step completed"
]

# PILOT: Interactive repeated movement  
LET COUNT = 0
*MOVE_LOOP
LET COUNT = COUNT + 1
T:Step *COUNT* - shall we continue? (yes/no)
A:$ANSWER
M:yes,y: J:MOVE_LOOP
M:no,n: T:Movement complete!
```

### 🎓 **Competency Framework**

#### **Beginner Level (0-3 months)**
- **Commands**: Basic PRINT, FORWARD, T: statements
- **Concepts**: Sequence, cause and effect
- **Projects**: Name displays, simple drawings, interactive greetings
- **Assessment**: Can create 5-10 line programs with guidance

#### **Intermediate Level (3-9 months)**
- **Commands**: Variables, loops, conditions, procedures
- **Concepts**: Abstraction, repetition, decision-making
- **Projects**: Interactive games, mathematical art, storytelling programs
- **Assessment**: Can independently create 20-50 line programs

#### **Advanced Level (9+ months)**
- **Commands**: Complex procedures, nested structures, advanced graphics
- **Concepts**: Modularity, debugging, optimization, collaboration
- **Projects**: Multi-file programs, educational software, creative applications
- **Assessment**: Can design and implement 100+ line collaborative projects

---

## 🏫 Classroom Setup Guide

### 💻 **Technical Requirements by Setting**

#### **🖥️ Computer Lab Environment**
**Recommended Setup:**
- **Computers**: 1 computer per 1-2 students
- **Implementation**: Python version for easy maintenance
- **Network**: Local network sharing for collaborative projects
- **Display**: Projector or interactive whiteboard for demonstrations

```bash
# Lab Installation Script (Python)
#!/bin/bash
# Time Warp Lab Setup Script

# Create shared directory
sudo mkdir -p /opt/timewarp
cd /opt/timewarp

# Clone repository
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Time_Warp_Python

# Install system-wide
sudo pip3 install -r requirements.txt
sudo cp time_warp_ide.py /usr/local/bin/timewarp

# Create desktop shortcuts for all users
sudo cp assets/timewarp.desktop /usr/share/applications/

echo "Time Warp IDE installed for all users!"
echo "Students can launch with: timewarp"
```

#### **📱 BYOD (Bring Your Own Device) Environment**
**Setup Strategy:**
- **Primary**: Web implementation for universal access
- **Backup**: Download links for native apps
- **Storage**: Cloud-based project storage
- **Support**: Multiple platform documentation

#### **🏫 Chromebook Environment**
**Optimal Configuration:**
- **Web Implementation**: Runs perfectly in Chrome browser
- **Offline Support**: Progressive Web App installation
- **File Management**: Google Drive integration
- **Collaboration**: Real-time sharing via Google Classroom

### 🎨 **Classroom Layout Recommendations**

#### **💡 Flexible Learning Spaces**
```
Traditional Rows          Collaborative Pods         Presentation Mode
┌─────────────────┐       ┌─────────────────┐       ┌─────────────────┐
│ S S S S S S S S │       │  S─S    S─S     │       │                 │
│ S S S S S S S S │  =>   │  │ │    │ │     │  =>   │       📺        │
│ S S S S S S S S │       │  S─S    S─S     │       │                 │
│       👨‍🏫        │       │              👨‍🏫 │       │ S S S S S S S S │
└─────────────────┘       └─────────────────┘       └─────────────────┘
```

#### **🔄 Activity Rotation Stations**
1. **Coding Station**: Individual/pair programming
2. **Planning Station**: Algorithm design with paper/whiteboards  
3. **Debugging Station**: Collaborative problem-solving
4. **Showcase Station**: Presentation and peer review

### 📋 **Classroom Management Tools**

#### **📊 Student Progress Dashboard**
```python
# Example teacher dashboard data structure
class ClassroomDashboard:
    def __init__(self):
        self.students = {}
        
    def track_progress(self, student_id, activity, success_rate):
        """Track individual student progress"""
        if student_id not in self.students:
            self.students[student_id] = StudentProgress()
        
        self.students[student_id].add_activity(activity, success_rate)
        
    def get_class_overview(self):
        """Generate class-wide statistics"""
        return {
            'average_progress': self.calculate_average_progress(),
            'struggling_students': self.identify_struggling_students(),
            'advanced_students': self.identify_advanced_students(),
            'common_errors': self.analyze_common_errors(),
        }
```

#### **🎯 Differentiated Instruction Tools**
- **Adaptive Assignments**: Automatically adjust difficulty based on performance
- **Peer Mentoring**: Pair advanced students with beginners
- **Alternative Assessments**: Multiple ways to demonstrate mastery
- **Extension Activities**: Challenge projects for early finishers

---

## 📝 Unit 1: Introduction to Programming

### 🎯 **Unit Overview**

**Duration**: 2-3 weeks (6-9 class periods)  
**Grade Level**: Adaptable for grades 3-12  
**Prerequisites**: Basic computer skills, ability to type simple commands

#### **🎨 Essential Questions**
- What is a computer program and how do programs communicate with computers?
- How can we give precise instructions to create predictable results?
- What makes TempleCode special compared to other programming languages?
- How do the three languages (BASIC, PILOT, Logo) work together?

#### **🎯 Learning Objectives**
By the end of this unit, students will be able to:
- **Create** simple programs using all three TempleCode languages
- **Explain** the difference between BASIC, PILOT, and Logo approaches
- **Demonstrate** basic turtle graphics commands
- **Design** interactive programs that respond to user input

### 📅 **Lesson Plan 1.1: Welcome to Programming**

#### **🚀 Opening Hook (10 minutes)**
**"The Magic Programming Wand"** demonstration:
```templecode
T:Watch the turtle draw your name!
T:What's your first name?
A:$NAME

SETCOLOR blue
PENWIDTH 5

# Teacher types student's name in real-time
# Turtle draws each letter using procedures
```

**Discussion Questions:**
- How did the computer know what to draw?
- What instructions did we give to make this happen?
- How is this different from using a drawing program?

#### **🎓 Core Instruction (25 minutes)**

**Concept 1: Programs as Instructions**
```templecode
# Like giving directions to a friend:
# 1. Walk forward 10 steps
# 2. Turn right
# 3. Walk forward 5 more steps

# In TempleCode:
FORWARD 100
RIGHT 90
FORWARD 50
```

**Concept 2: Three Ways to Program**
Demonstrate the same task in all three languages:

```templecode
REM BASIC way - step by step instructions
10 PRINT "Drawing a line"
20 PRINT "Line complete"

REM PILOT way - interactive conversation  
T:I'm going to draw a line for you
T:Press Enter when ready
A:$READY

REM Logo way - visual creation
FORWARD 100
PRINT "Line drawn!"
```

**Guided Practice Activity**: "Simon Says Programming"
- Teacher calls out commands: "Forward 50", "Right 90", "Pen up"
- Students predict what will happen before executing
- Class discusses results and surprises

#### **💻 Hands-On Practice (15 minutes)**

**Activity**: "Your First Program"
Students create their personal introduction program:

```templecode
T:Hello! I'm going to introduce myself.
T:My name is [STUDENT NAME]
T:I am [AGE] years old
T:My favorite color is [COLOR]

SETCOLOR [their favorite color]
FORWARD 50
RIGHT 90
FORWARD 50
RIGHT 90  
FORWARD 50
RIGHT 90
FORWARD 50

T:I drew a square in my favorite color!
```

#### **🔄 Closure & Assessment (5 minutes)**
**Exit Ticket Questions:**
1. Name the three programming languages in TempleCode
2. What command makes the turtle move forward?
3. What's one thing that surprised you about programming today?

### 📅 **Lesson Plan 1.2: The Three Languages**

#### **🎯 Learning Focus**
Deep dive into BASIC, PILOT, and Logo characteristics

#### **🚀 Opening Review (8 minutes)**
**"Language Detective"** game:
Show code snippets and have students identify the language:

```templecode
# Mystery Code 1
10 LET X = 5
20 PRINT X
# Answer: BASIC (line numbers, LET, PRINT)

# Mystery Code 2  
T:What's your age?
A:$AGE
# Answer: PILOT (T: and A: commands)

# Mystery Code 3
TO SQUARE
  REPEAT 4 [FORWARD 50 RIGHT 90]
END
# Answer: Logo (TO/END, REPEAT brackets)
```

#### **📚 Core Instruction (20 minutes)**

**BASIC: The Calculator Language**
```templecode
# BASIC is like a smart calculator
10 LET MATH_SCORE = 85
20 LET ENGLISH_SCORE = 92  
30 LET AVERAGE = (MATH_SCORE + ENGLISH_SCORE) / 2
40 PRINT "Your average is"; AVERAGE

# Key features:
# - Line numbers organize the program
# - Variables store information
# - Mathematical calculations  
# - Step-by-step execution
```

**PILOT: The Conversation Language**
```templecode
# PILOT creates conversations with the computer
T:Welcome to my quiz!
T:What is 2 + 2?
A:$ANSWER
M:4,four: T:Correct! Great job!
M:*: T:Not quite. The answer is 4.

# Key features:
# - T: displays messages
# - A: gets user input
# - M: matches patterns in responses
# - Natural conversation flow
```

**Logo: The Artist Language**
```templecode
# Logo creates pictures and graphics
SETCOLOR red
TO FLOWER
  REPEAT 8 [
    FORWARD 50
    RIGHT 45
  ]
END
FLOWER

# Key features:
# - Turtle graphics for visual creation
# - Procedures (TO/END) organize code
# - REPEAT creates patterns
# - Mathematical art and geometry
```

#### **🎨 Guided Practice (15 minutes)**

**Activity**: "Three-Language Challenge"
Students recreate the same concept in all three languages:

**Challenge**: Create a program about favorite foods

```templecode
REM BASIC Solution
10 LET FOOD$ = "pizza"
20 PRINT "My favorite food is "; FOOD$
30 PRINT "I could eat it every day!"

REM PILOT Solution  
T:Let me tell you about my favorite food
T:My favorite food is pizza
T:What's your favorite food?
A:$FOOD
T:*FOOD* sounds delicious!

REM Logo Solution
# Draw a pizza slice
SETCOLOR red
REPEAT 3 [FORWARD 60 RIGHT 120]
SETCOLOR yellow  
# Add cheese dots
REPEAT 5 [
  PENUP FORWARD 20 PENDOWN
  REPEAT 360 [FORWARD 1 RIGHT 1]
  PENUP BACK 20 PENDOWN
  RIGHT 45
]
```

#### **🔍 Assessment Activity (12 minutes)**

**"Language Choice Justification"**
Present scenarios and have students choose the best language:

1. **Scenario**: Create a math homework helper
   - **Best Choice**: BASIC (calculations and variables)
   - **Why**: Strong math operations, clear variable handling

2. **Scenario**: Make an interactive story
   - **Best Choice**: PILOT (conversation and branching)
   - **Why**: Natural dialogue, pattern matching for choices

3. **Scenario**: Design geometric art
   - **Best Choice**: Logo (turtle graphics)
   - **Why**: Visual creation, geometric commands, artistic procedures

### 📅 **Lesson Plan 1.3: Mixed Language Programming**

#### **🎯 Learning Focus**
Combining all three languages in single programs for enhanced functionality

#### **🚀 Opening Hook (10 minutes)**

**"The Ultimate Program"** demonstration:
```templecode
10 PRINT "=== Welcome to Art Creator ==="
20 LET SIDES = 0

T:How many sides should your shape have?
A:$SIDES
30 LET SIDES = VAL(SIDES)

40 IF SIDES < 3 THEN GOSUB 1000
50 IF SIDES >= 3 THEN GOSUB 2000
60 END

*1000 REM Too few sides
T:Shapes need at least 3 sides. Let's make a triangle!
30 LET SIDES = 3
RETURN

*2000 REM Draw the shape
SETCOLOR blue
PENWIDTH 3
TO POLYGON :SIDES
  REPEAT :SIDES [
    FORWARD 60
    RIGHT (360 / :SIDES)
  ]
END
POLYGON SIDES

T:Beautiful *SIDES*-sided shape!
RETURN
```

**Discussion**: "How did all three languages work together here?"

#### **📚 Core Instruction (25 minutes)**

**Concept: Language Strengths Integration**
```
BASIC          PILOT           Logo
├─ Math        ├─ Input        ├─ Graphics
├─ Logic       ├─ Decisions    ├─ Art
├─ Variables   ├─ Patterns     ├─ Procedures
└─ Control     └─ Flow         └─ Creativity
      ↘         ↓         ↙
        Combined Program
```

**Design Pattern 1: Input → Process → Output**
```templecode
# 1. PILOT: Get user input
T:What's your favorite number?
A:$NUM

# 2. BASIC: Process the data  
10 LET NUMBER = VAL(NUM)
20 LET SQUARE = NUMBER * NUMBER
30 LET CUBE = NUMBER * NUMBER * NUMBER

# 3. Logo: Visual output
SETCOLOR green
TO DRAW_NUMBER :N
  PENUP HOME PENDOWN
  REPEAT :N [
    FORWARD 10
    RIGHT 90
    FORWARD 10  
    LEFT 90
  ]
END

DRAW_NUMBER SQUARE
T:I drew *SQUARE* little squares for *NUM* squared!
```

**Design Pattern 2: Menu-Driven Programs**
```templecode
*MAIN_MENU
10 PRINT "=== Art Studio ==="
T:Choose an option:
T:1. Draw a house
T:2. Draw a flower  
T:3. Draw a car
T:4. Quit
A:$CHOICE

M:1: J:HOUSE
M:2: J:FLOWER
M:3: J:CAR
M:4: T:Goodbye!
M:*: T:Please choose 1, 2, 3, or 4
J:MAIN_MENU

*HOUSE
# Logo procedures for house drawing
# Return to menu when done

*FLOWER  
# Logo procedures for flower drawing
# Return to menu when done
```

#### **💻 Guided Practice (20 minutes)**

**Activity**: "Personal Quiz Creator"
Students create an interactive quiz about themselves:

```templecode
REM Framework provided to students:
10 PRINT "=== All About [Your Name] ==="
20 LET SCORE = 0

T:Question 1: What's my favorite color?
T:a) Red  b) Blue  c) Green
A:$ANSWER1
M:[correct letter]: J:CORRECT1
M:*: J:WRONG1

*CORRECT1
30 LET SCORE = SCORE + 1
T:Correct! Let me draw with my favorite color.
SETCOLOR [their favorite color]
# Students add simple drawing
J:QUESTION2

*WRONG1  
T:Not quite! Here's a hint.
# Students add visual hint
J:QUESTION2

*QUESTION2
# Second question follows same pattern
```

### 📊 **Unit 1 Assessment Rubric**

#### **🎯 Performance Indicators**

| Criterion | Exemplary (4) | Proficient (3) | Developing (2) | Beginning (1) |
|-----------|---------------|----------------|----------------|---------------|
| **Language Understanding** | Clearly explains differences between BASIC, PILOT, and Logo with specific examples | Identifies key features of each language | Shows basic understanding of language differences | Confuses language features or shows minimal understanding |
| **Code Creation** | Creates original programs combining all three languages effectively | Writes functional programs using multiple languages | Creates simple programs with guidance | Produces code with significant errors or incomplete functionality |
| **Problem Solving** | Independently designs solutions and debugs errors | Solves problems with minimal help | Requires moderate assistance to complete tasks | Needs substantial support for basic programming tasks |
| **Computational Thinking** | Demonstrates decomposition, patterns, abstraction, and algorithms clearly | Shows evidence of computational thinking in most work | Beginning to apply computational thinking concepts | Limited evidence of computational thinking |

#### **📋 Assessment Methods**

**Formative Assessments:**
- Daily exit tickets with conceptual questions
- Peer code reviews using structured protocols  
- "Debugging Detective" activities finding and fixing errors
- Quick polls using classroom response systems

**Summative Assessments:**
- Portfolio of programs created during unit
- Practical programming test with scaffolded challenges
- Reflection essay on learning journey and favorite discoveries
- Group presentation of collaborative final project

---

## 📊 Unit 2: Variables and Data

### 🎯 **Unit Overview**

**Duration**: 3-4 weeks (9-12 class periods)  
**Prerequisites**: Unit 1 completion or equivalent experience  
**Core Concept**: Understanding how programs store, manipulate, and use data

#### **🎨 Essential Questions**
- How do programs remember information and use it later?
- What are the different types of data and how do we work with each type?
- How can variables make our programs more flexible and powerful?
- What happens when we perform operations on different data types?

#### **🎯 Learning Objectives**
Students will be able to:
- **Define** variables and explain their purpose in programming
- **Distinguish** between numeric and string data types
- **Create** programs that collect, store, and manipulate data
- **Debug** common variable-related errors

### 📅 **Lesson Plan 2.1: Introduction to Variables**

#### **🚀 Opening Hook (12 minutes)**

**"The Memory Game"** - Physical demonstration:
1. **Setup**: Give students sticky notes and boxes
2. **Activity**: Students write information on notes and put in labeled boxes
3. **Connection**: "Programs use variables the same way!"

```templecode
# Demonstrate with live coding
T:What's your name?
A:$NAME
T:What's your age?  
A:$AGE
T:What's your favorite subject?
A:$SUBJECT

T:Let me remember what you told me...
T:Your name is *NAME*
T:You are *AGE* years old  
T:Your favorite subject is *SUBJECT*
T:I stored all that information in variables!
```

#### **📚 Core Instruction (20 minutes)**

**Concept 1: Variables as Storage Containers**
```templecode
# BASIC variables - like labeled boxes
10 LET NAME$ = "Alice"        # String box labeled "NAME$"
20 LET AGE = 14               # Number box labeled "AGE"  
30 LET HEIGHT = 5.2           # Decimal box labeled "HEIGHT"

# PILOT variables - conversation memory
T:I'll remember your answers
A:$FAVORITE_COLOR             # Stored for later use
A:$LUCKY_NUMBER               # Each A: command creates storage

# Logo parameters - temporary holders  
TO DRAW_SHAPE :SIZE :COLOR    # SIZE and COLOR are temporary variables
  SETCOLOR :COLOR
  REPEAT 4 [FORWARD :SIZE RIGHT 90]
END
```

**Concept 2: Data Types Matter**
```templecode
# Numbers for calculations
10 LET PRICE = 12.50
20 LET QUANTITY = 3
30 LET TOTAL = PRICE * QUANTITY
40 PRINT "Total cost: $"; TOTAL

# Strings for text  
10 LET FIRST$ = "Hello"
20 LET LAST$ = "World"
30 LET MESSAGE$ = FIRST$ + " " + LAST$
40 PRINT MESSAGE$

# What happens when we mix them?
10 LET NUMBER = 5
20 LET TEXT$ = "The number is " + STR$(NUMBER)
30 PRINT TEXT$
```

#### **🎨 Guided Practice (18 minutes)**

**Activity 1**: "Variable Scavenger Hunt"
Students identify variables and data types in sample programs:

```templecode
# Program A - Students analyze this code
10 LET PLAYER_NAME$ = "Sam"
20 LET PLAYER_SCORE = 850  
30 LET LEVEL = 3
40 PRINT PLAYER_NAME$; " scored "; PLAYER_SCORE; " on level "; LEVEL

# Questions:
# 1. How many variables are there? (3)
# 2. Which store text? (PLAYER_NAME$)  
# 3. Which store numbers? (PLAYER_SCORE, LEVEL)
```

**Activity 2**: "Fix the Variable Errors"
```templecode
# Broken code - students debug
10 LET name = "Bob"           # Error: strings need $ in BASIC
20 LET AGE$ = 15             # Error: numbers shouldn't have $
30 PRINT "Hello " + NAME     # Error: wrong variable name case

# Corrected version:
10 LET NAME$ = "Bob"
20 LET AGE = 15
30 PRINT "Hello "; NAME$; ", age "; AGE
```

### 📅 **Lesson Plan 2.2: Variable Operations**

#### **🎯 Learning Focus**
Mathematical and string operations with variables

#### **🚀 Opening Review (8 minutes)**

**"Quick Variable Quiz"** - Students use mini-whiteboards:
1. "What symbol goes after string variable names in BASIC?" ($)
2. "How do you store user input in PILOT?" (A:$VARIABLE)  
3. "What are Logo procedure parameters called?" (Variables with colons)

#### **📚 Core Instruction (25 minutes)**

**Mathematical Operations**
```templecode
# Basic arithmetic
10 LET A = 10
20 LET B = 3
30 LET SUM = A + B           # Addition: 13
40 LET DIFFERENCE = A - B    # Subtraction: 7  
50 LET PRODUCT = A * B       # Multiplication: 30
60 LET QUOTIENT = A / B      # Division: 3.333...
70 LET REMAINDER = A MOD B   # Modulo: 1

# Order of operations matters!
80 LET RESULT1 = 2 + 3 * 4   # = 14 (not 20!)
90 LET RESULT2 = (2 + 3) * 4 # = 20
```

**String Operations**
```templecode
# Combining strings (concatenation)
10 LET FIRST$ = "Hello"
20 LET LAST$ = "World"  
30 LET GREETING$ = FIRST$ + " " + LAST$ + "!"
40 PRINT GREETING$           # "Hello World!"

# String functions
50 LET LENGTH = LEN(GREETING$)        # Length: 12
60 LET PART$ = LEFT$(GREETING$, 5)    # "Hello"
70 LET UPPER$ = UCASE$(GREETING$)     # "HELLO WORLD!"
```

**Cross-Language Variable Sharing**
```templecode
# Variables work across all three languages!
10 LET STUDENT_NAME$ = ""
20 LET MATH_GRADE = 0

T:What's your name?
A:$NAME
30 LET STUDENT_NAME$ = NAME

T:What's your math grade?
A:$GRADE  
40 LET MATH_GRADE = VAL(GRADE)

# Now use in Logo graphics
TO GRADE_BAR :GRADE
  SETCOLOR green
  FORWARD :GRADE
  PRINT "Grade bar drawn!"
END

GRADE_BAR MATH_GRADE
T:Great job, *NAME*! Your grade of *GRADE* looks good!
```

#### **💻 Hands-On Practice (22 minutes)**

**Project**: "Personal Calculator"
Students create a multi-function calculator:

```templecode
# Student framework:
*CALCULATOR_MENU
10 PRINT "=== Personal Calculator ==="
T:Choose operation:
T:1. Addition
T:2. Subtraction  
T:3. Multiplication
T:4. Division
T:5. Quit
A:$CHOICE

M:1: J:ADD_NUMBERS
M:2: J:SUBTRACT_NUMBERS
M:3: J:MULTIPLY_NUMBERS
M:4: J:DIVIDE_NUMBERS
M:5: T:Goodbye!
M:*: T:Please choose 1-5
J:CALCULATOR_MENU

*ADD_NUMBERS
T:Enter first number:
A:$NUM1
T:Enter second number:  
A:$NUM2
100 LET NUMBER1 = VAL(NUM1)
110 LET NUMBER2 = VAL(NUM2)
120 LET RESULT = NUMBER1 + NUMBER2
130 PRINT NUMBER1; " + "; NUMBER2; " = "; RESULT

# Visual representation
SETCOLOR blue
TO NUMBER_LINE :NUM
  REPEAT :NUM [
    FORWARD 5
    PENUP FORWARD 2 PENDOWN
  ]
END
NUMBER_LINE RESULT
J:CALCULATOR_MENU

# Students implement other operations...
```

### 📅 **Lesson Plan 2.3: Data Input and Validation**

#### **🎯 Learning Focus**
Robust input handling and data validation techniques

#### **🚀 Opening Hook (10 minutes)**

**"Crash the Program"** challenge:
- Show a simple program that asks for a number
- Have students try to break it with weird inputs
- Demonstrate how good programs handle bad input gracefully

```templecode
# Fragile program (crashes easily)
T:Enter your age:
A:$AGE
10 LET AGE_NUM = VAL(AGE)
20 LET YEARS_TO_100 = 100 - AGE_NUM
30 PRINT "You have "; YEARS_TO_100; " years until 100!"

# What happens with: "abc", "-5", "1000", ""?
```

#### **📚 Core Instruction (25 minutes)**

**Input Validation Strategies**
```templecode
# Strategy 1: Range checking
*GET_AGE
T:Enter your age (1-120):
A:$AGE_INPUT
10 LET AGE = VAL(AGE_INPUT)
20 IF AGE < 1 OR AGE > 120 THEN J:BAD_AGE
30 GOTO CONTINUE_PROGRAM

*BAD_AGE  
T:Please enter a realistic age between 1 and 120.
J:GET_AGE

*CONTINUE_PROGRAM
T:Thanks! Your age of *AGE* is valid.

# Strategy 2: Pattern matching in PILOT
T:Choose difficulty: Easy, Medium, or Hard
A:$DIFFICULTY
M:easy,e,1: J:EASY_MODE
M:medium,med,m,2: J:MEDIUM_MODE  
M:hard,h,3: J:HARD_MODE
M:*: T:Please choose Easy, Medium, or Hard
J:GET_DIFFICULTY

# Strategy 3: Default values
T:Enter your favorite color (or press Enter for blue):
A:$COLOR_INPUT
10 IF COLOR_INPUT = "" THEN LET COLOR$ = "blue"
20 IF COLOR_INPUT <> "" THEN LET COLOR$ = COLOR_INPUT
30 PRINT "Using color: "; COLOR$
```

**Error Handling Best Practices**
```templecode
# Always provide clear feedback
T:Oops! "*INPUT*" isn't a valid choice.
T:Valid options are: red, blue, green, yellow

# Give examples  
T:Please enter a number like: 5, 12.5, or 100

# Offer help
T:Need help? Type "help" for more information

# Be encouraging
T:That's not quite right, but you're learning! Try again.
```

#### **🎨 Guided Practice (20 minutes)**

**Activity**: "Bulletproof Survey Program"
Students create a survey that handles all types of input gracefully:

```templecode
# Survey framework with validation
10 PRINT "=== Student Survey (Crash-Proof Version) ==="

*GET_NAME
T:What's your first name? (letters only)
A:$NAME_INPUT
# Students add validation: check for letters only, reasonable length

*GET_GRADE  
T:What grade are you in? (K, 1-12)
A:$GRADE_INPUT
M:k,kindergarten: LET GRADE$ = "Kindergarten"
M:1,2,3,4,5,6,7,8,9,10,11,12: LET GRADE$ = GRADE_INPUT
M:*: T:Please enter K or a number 1-12
J:GET_GRADE

*GET_FAVORITE_NUMBER
T:What's your favorite number? (any number)  
A:$NUMBER_INPUT
10 LET FAVORITE = VAL(NUMBER_INPUT)
20 IF FAVORITE = 0 AND NUMBER_INPUT <> "0" THEN J:BAD_NUMBER
30 GOTO NUMBER_OK

*BAD_NUMBER
T:That doesn't look like a number. Try again!
J:GET_FAVORITE_NUMBER

*NUMBER_OK  
# Create visual representation
SETCOLOR red
TO DRAW_FAVORITE :NUM  
  # Students design creative visualization
END
DRAW_FAVORITE FAVORITE

*SURVEY_COMPLETE
T:Thanks *NAME_INPUT*! 
T:Grade: *GRADE$*
T:Favorite number: *NUMBER_INPUT*
T:Survey complete - no crashes!
```

### 📊 **Unit 2 Assessment Project: "All About Me Database"**

#### **🎯 Final Project Requirements**

Students create a comprehensive personal information program that demonstrates variable mastery:

**Required Features:**
1. **Data Collection**: Gather at least 8 pieces of personal information
2. **Data Types**: Use both strings and numbers appropriately  
3. **Calculations**: Perform mathematical operations on numeric data
4. **Validation**: Handle invalid input gracefully
5. **Visualization**: Create graphics based on collected data
6. **Interaction**: Allow users to query the stored information

**Sample Project Structure:**
```templecode
# Student's "All About Me" database
10 PRINT "=== Creating My Personal Database ==="

# Data collection with validation
*COLLECT_DATA
T:Building your profile... 
# Students implement robust input for:
# - Name, age, grade, height, favorite number
# - Favorite color, hobby, pet name, etc.

# Data processing  
*PROCESS_DATA  
# Calculate derived information:
# - Years until graduation, years lived, etc.
# - Create categories, rankings, etc.

# Data visualization
*CREATE_GRAPHICS
# Visual representations based on data:
# - Bar graphs of numeric data
# - Artistic representation of favorites
# - Geometric patterns using personal numbers

# Interactive queries
*QUERY_MENU
T:What would you like to know about me?
T:1. Basic info  2. Calculations  3. Fun facts  4. Graphics
# Students implement menu-driven information display
```

#### **📋 Assessment Rubric**

| Criterion | Exemplary (4) | Proficient (3) | Developing (2) | Beginning (1) |
|-----------|---------------|----------------|----------------|---------------|
| **Variable Usage** | Uses variables efficiently across all three languages with appropriate naming conventions | Correctly uses most variables with good naming practices | Basic variable usage with some naming or scoping issues | Inconsistent or incorrect variable usage |
| **Data Validation** | Comprehensive input validation with helpful error messages and recovery | Good validation for most inputs with clear feedback | Basic validation with some gaps or unclear messages | Minimal or missing input validation |
| **Calculations** | Complex mathematical operations with proper order of operations and error checking | Accurate calculations using multiple operations appropriately | Basic arithmetic with minor errors or omissions | Significant calculation errors or limited mathematical operations |
| **Code Organization** | Well-structured code with clear sections, comments, and logical flow | Generally organized with good structure and some documentation | Basic organization with minimal documentation | Poor organization, difficult to follow logic |

---

## 🎮 Unit 3: Control Structures

### 🎯 **Unit Overview**

**Duration**: 4-5 weeks (12-15 class periods)  
**Prerequisites**: Units 1-2 or demonstrated variable proficiency  
**Core Concept**: Program decision-making and repetition structures

#### **🎨 Essential Questions**
- How do programs make decisions and respond to different conditions?
- What's the difference between different types of loops and when do we use each?
- How can we create programs that adapt their behavior based on user input or data?
- What makes a well-structured program versus a "spaghetti code" program?

#### **🎯 Learning Objectives**
Students will be able to:
- **Implement** conditional statements (IF/THEN/ELSE) in all three languages
- **Design** appropriate loop structures for different programming scenarios  
- **Create** programs with complex decision trees and multiple pathways
- **Debug** logic errors in control structures

### 📅 **Lesson Plan 3.1: Making Decisions with IF Statements**

#### **🚀 Opening Hook (15 minutes)**

**"Choose Your Adventure"** physical activity:
```
Teacher Setup: Create decision tree on board
┌─────────────────────────────────┐
│        You find a door          │
│                                 │
│    🚪 Open it    🚶 Walk away   │
│         │             │         │
│    ┌─────────┐    ┌─────────┐   │
│    │ Dragon! │    │ Safety  │   │
│    └─────────┘    └─────────┘   │
└─────────────────────────────────┘
```

**Connection to Programming:**
```templecode
T:You see a mysterious door. What do you do?
T:1. Open the door  2. Walk away
A:$CHOICE

M:1: T:You opened it! There's a dragon inside!
M:2: T:You walked away safely. Wise choice!
M:*: T:Please choose 1 or 2.

# This is a conditional statement!
# IF choice = 1 THEN dragon message
# IF choice = 2 THEN safety message  
# ELSE ask again
```

#### **📚 Core Instruction (25 minutes)**

**Concept 1: Basic IF/THEN Logic**
```templecode
# BASIC conditional statements
10 LET AGE = 16
20 IF AGE >= 18 THEN PRINT "You can vote!"
30 IF AGE < 18 THEN PRINT "Not old enough to vote yet."

# More complex conditions  
40 IF AGE >= 16 AND AGE < 18 THEN PRINT "You can drive but not vote."
50 IF AGE >= 21 THEN PRINT "You can do everything!"

# PILOT pattern matching (a type of conditional)
T:What's your favorite season?
A:$SEASON
M:spring,summer: T:You like warm weather!
M:fall,autumn: T:You enjoy the changing leaves!
M:winter: T:You love the cold and snow!
M:*: T:That's an interesting choice!

# Logo conditional procedures
TO DRAW_GRADE :GRADE
  IF :GRADE >= 90 [
    SETCOLOR green
    PRINT "Excellent work!"
  ]
  IF :GRADE >= 80 AND :GRADE < 90 [
    SETCOLOR blue  
    PRINT "Good job!"
  ]
  IF :GRADE < 80 [
    SETCOLOR red
    PRINT "Keep trying!"
  ]
END
```

**Concept 2: Comparison Operators**
```templecode
# Number comparisons
=     # Equal to: IF X = 5
<>    # Not equal to: IF X <> 0  
<     # Less than: IF AGE < 18
>     # Greater than: IF SCORE > 90
<=    # Less than or equal: IF POINTS <= 100
>=    # Greater than or equal: IF GRADE >= 60

# String comparisons
IF NAME$ = "Alice" THEN PRINT "Hello Alice!"
IF COLOR$ <> "red" THEN PRINT "Not red!"

# Combining conditions
AND   # Both must be true: IF AGE > 12 AND AGE < 20
OR    # Either can be true: IF DAY$ = "Saturday" OR DAY$ = "Sunday"  
NOT   # Opposite: IF NOT (SCORE < 60)
```

#### **🎨 Guided Practice (15 minutes)**

**Activity**: "Grade Calculator with Feedback"
Students build a program that gives different messages based on grades:

```templecode
# Framework for students
10 PRINT "=== Grade Calculator ==="

T:Enter your test score (0-100):
A:$SCORE_INPUT
20 LET SCORE = VAL(SCORE_INPUT)

# Students implement the logic:
30 IF SCORE >= 97 THEN GOSUB 1000  # A+
40 IF SCORE >= 93 AND SCORE < 97 THEN GOSUB 1100  # A
50 IF SCORE >= 90 AND SCORE < 93 THEN GOSUB 1200  # A-
# ... continue for all grades

# Visual feedback with turtle graphics
*1000 REM A+ Grade
SETCOLOR gold
T:Outstanding! A+ work!
TO DRAW_STAR
  REPEAT 5 [FORWARD 50 RIGHT 144]
END
DRAW_STAR
RETURN

*1100 REM A Grade  
SETCOLOR green
T:Excellent! A grade!
# Students add appropriate graphics
RETURN

# Error handling
*9000 REM Invalid score
T:Please enter a score between 0 and 100.
J:GET_SCORE
```

### 📅 **Lesson Plan 3.2: Loops and Repetition**

#### **🎯 Learning Focus**
Understanding when and how to use different types of loops

#### **🚀 Opening Hook (12 minutes)**

**"Human Loop Machine"** physical demonstration:
1. **Setup**: Students line up, teacher gives "loop instructions"
2. **Activity**: "Repeat 5 times: clap, stomp, turn around"
3. **Variation**: "While music plays: march in place"
4. **Connection**: "Programs use loops the same way!"

```templecode
# What we just did in code:
# Counting loop (REPEAT/FOR)
REPEAT 5 [
  PRINT "Clap"
  PRINT "Stomp"  
  PRINT "Turn around"
]

# Condition-based loop (WHILE)
WHILE MUSIC_PLAYING = TRUE [
  PRINT "March in place"
]
```

#### **📚 Core Instruction (28 minutes)**

**Loop Type 1: Counting Loops (FOR/REPEAT)**
```templecode
# BASIC FOR loops - when you know how many times
10 FOR I = 1 TO 10
20   PRINT "Count: "; I
30 NEXT I

40 FOR I = 10 TO 1 STEP -1
50   PRINT "Countdown: "; I  
60 NEXT I

# Logo REPEAT - simpler counting
REPEAT 8 [
  FORWARD 50
  RIGHT 45
]

# Nested loops - loops inside loops
10 FOR ROW = 1 TO 3
20   FOR COL = 1 TO 4
30     PRINT "*";
40   NEXT COL
50   PRINT ""
60 NEXT ROW
# Creates a 3x4 rectangle of stars
```

**Loop Type 2: Condition-Based Loops**
```templecode
# BASIC WHILE loops - when you don't know how many times
10 LET GUESS = 0
20 LET SECRET = 42
30 WHILE GUESS <> SECRET
40   INPUT "Guess the number: ", GUESS
50   IF GUESS < SECRET THEN PRINT "Too low!"
60   IF GUESS > SECRET THEN PRINT "Too high!"
70 WEND
80 PRINT "Correct!"

# PILOT loop simulation with jumps
*GUESSING_LOOP
T:Guess the number (1-100):
A:$GUESS
10 IF VAL(GUESS) = 42 THEN J:CORRECT
20 IF VAL(GUESS) < 42 THEN T:Too low!
30 IF VAL(GUESS) > 42 THEN T:Too high!
J:GUESSING_LOOP

*CORRECT
T:You got it! The number was 42.
```

**Loop Type 3: Interactive Loops**
```templecode
# Menu loops - very common pattern
*MAIN_MENU
T:=== Game Menu ===
T:1. Play game
T:2. View high scores
T:3. Options
T:4. Quit
A:$CHOICE

M:1: J:PLAY_GAME
M:2: J:HIGH_SCORES  
M:3: J:OPTIONS
M:4: T:Thanks for playing!
M:*: T:Please choose 1-4
J:MAIN_MENU

# Each section returns to menu when done
*PLAY_GAME
T:Playing the game...
# Game logic here
J:MAIN_MENU
```

#### **💻 Hands-On Practice (15 minutes)**

**Activity**: "Pattern Generator"
Students create programs that generate visual patterns using loops:

```templecode
# Challenge 1: Staircase pattern
# Output should look like:
# *
# **
# ***
# ****
# *****

10 FOR ROW = 1 TO 5
20   FOR COL = 1 TO ROW
30     PRINT "*";
40   NEXT COL
50   PRINT ""
60 NEXT ROW

# Challenge 2: Turtle graphics spiral
SETCOLOR blue
REPEAT 50 [
  FORWARD REPCOUNT * 2    # REPCOUNT = current iteration
  RIGHT 91
]

# Challenge 3: Interactive drawing
*DRAWING_LOOP
T:Draw a line? (yes/no)
A:$ANSWER
M:yes,y: FORWARD 20
M:no,n: J:DONE_DRAWING
M:*: T:Please answer yes or no
J:DRAWING_LOOP

*DONE_DRAWING
T:Nice drawing!
```

### 📅 **Lesson Plan 3.3: Complex Decision Trees**

#### **🎯 Learning Focus**
Building sophisticated programs with multiple decision points

#### **🚀 Opening Hook (10 minutes)**

**"The Ultimate Pet Selector"** demonstration:
- Create a flowchart on the board for choosing pets
- Show how multiple IF statements create complex decision trees
- Demonstrate with interactive program

#### **📚 Core Instruction (30 minutes)**

**Advanced Conditional Patterns**
```templecode
# Nested IF statements - decisions within decisions
10 INPUT "Enter your age: ", AGE
20 IF AGE >= 5 THEN GOSUB 1000
30 IF AGE < 5 THEN PRINT "Too young for this program"
40 END

*1000 REM Age appropriate - check other factors
50 INPUT "Do you like animals (yes/no)? ", ANIMALS$
60 IF ANIMALS$ = "yes" THEN GOSUB 2000
70 IF ANIMALS$ = "no" THEN GOSUB 3000
RETURN

*2000 REM Likes animals - pet recommendations  
80 INPUT "Do you have a yard (yes/no)? ", YARD$
90 IF YARD$ = "yes" THEN PRINT "Consider a dog!"
100 IF YARD$ = "no" THEN GOSUB 2100
RETURN

*2100 REM No yard - indoor pets
110 INPUT "Are you allergic to fur (yes/no)? ", ALLERGIC$
120 IF ALLERGIC$ = "yes" THEN PRINT "Consider a fish or reptile!"
130 IF ALLERGIC$ = "no" THEN PRINT "Consider a cat or small mammal!"
RETURN

*3000 REM Doesn't like animals
140 PRINT "Consider a plant or robot companion!"
RETURN
```

**PILOT Decision Trees with Pattern Matching**
```templecode
# More natural conversation flow
*PET_ADVISOR
T:I'll help you choose the perfect pet!
T:What's your living situation?
T:(house, apartment, dorm)
A:$LIVING

M:house: J:HOUSE_PETS
M:apartment,apt: J:APARTMENT_PETS  
M:dorm,dormitory: J:DORM_PETS
M:*: T:Please specify: house, apartment, or dorm
J:PET_ADVISOR

*HOUSE_PETS
T:Great! Houses offer lots of options.
T:Do you want an active pet or calm pet?
A:$ACTIVITY
M:active,energetic,playful: J:ACTIVE_HOUSE_PETS
M:calm,quiet,peaceful: J:CALM_HOUSE_PETS
M:*: T:Please choose active or calm
J:HOUSE_PETS

*ACTIVE_HOUSE_PETS  
T:Perfect for an active lifestyle!
T:How much time can you dedicate daily?
T:(lots, some, little)
A:$TIME
M:lots,much,lot: T:A dog would be perfect!
M:some,moderate: T:Consider a cat or rabbit!
M:little,not much: T:Maybe a bird or hamster!
M:*: T:Please choose lots, some, or little
J:ACTIVE_HOUSE_PETS
```

#### **🎨 Major Project Introduction (15 minutes)**

**"Choose Your Own Adventure Game"**
Students design a text-based adventure game using complex decision trees:

```templecode
# Game framework structure
*GAME_START
10 PRINT "=== The Mysterious Castle ==="
T:You stand before an ancient castle.
T:Three paths lead forward:
T:1. Main gate (guarded)
T:2. Side window (looks open)  
T:3. Back entrance (dark and scary)
A:$PATH_CHOICE

M:1: J:MAIN_GATE
M:2: J:SIDE_WINDOW
M:3: J:BACK_ENTRANCE
M:*: T:Please choose 1, 2, or 3
J:GAME_START

*MAIN_GATE
T:You approach the main gate.
T:A guard asks: "What is your business here?"
T:How do you respond?
T:1. "I'm here to explore"
T:2. "I'm looking for treasure"  
T:3. "I'm lost and need help"
A:$RESPONSE

# Each response leads to different outcomes
M:1: J:EXPLORER_PATH
M:2: J:TREASURE_HUNTER_PATH
M:3: J:LOST_TRAVELER_PATH

# Students build out each path with multiple decisions
# Include inventory system, character stats, etc.
```

### 📊 **Unit 3 Culminating Project: "Smart Home Simulator"**

#### **🎯 Project Overview**

Students create a comprehensive smart home control system that demonstrates mastery of all control structures:

**Required Components:**
1. **Menu System**: Multi-level navigation with loops
2. **Device Control**: IF/THEN logic for different devices  
3. **Automation Rules**: Complex conditional logic
4. **User Preferences**: Personalized settings and responses
5. **Error Handling**: Robust input validation  
6. **Visual Dashboard**: Graphics showing system status

**Sample Project Structure:**
```templecode
# Smart Home Control System
10 PRINT "=== Smart Home Control System ==="
20 LET TEMPERATURE = 72
30 LET LIGHTS_ON = 0  
40 LET SECURITY_ARMED = 0
50 LET USER_NAME$ = ""

*SYSTEM_STARTUP
T:Welcome to your Smart Home!
T:Please enter your name for personalization:
A:$USER_NAME
60 LET USER_NAME$ = USER_NAME

*MAIN_DASHBOARD  
70 PRINT "=== Dashboard for "; USER_NAME$; " ==="
80 PRINT "Temperature: "; TEMPERATURE; "°F"
90 PRINT "Lights: "; 
100 IF LIGHTS_ON = 1 THEN PRINT "ON" ELSE PRINT "OFF"
110 PRINT "Security: ";
120 IF SECURITY_ARMED = 1 THEN PRINT "ARMED" ELSE PRINT "DISARMED"

# Visual status display
CLEARSCREEN
TO DRAW_HOUSE_STATUS
  # Students create house diagram showing current status
  # Use different colors for different device states
  # Add animations for active devices
END
DRAW_HOUSE_STATUS

T:Choose a system:
T:1. Climate Control  2. Lighting  3. Security  4. Automation  5. Exit
A:$SYSTEM_CHOICE

M:1: J:CLIMATE_CONTROL
M:2: J:LIGHTING_CONTROL
M:3: J:SECURITY_CONTROL  
M:4: J:AUTOMATION_RULES
M:5: T:Goodbye, *USER_NAME*!
M:*: T:Please choose 1-5
J:MAIN_DASHBOARD

*CLIMATE_CONTROL
T:=== Climate Control ===
T:Current temperature: *TEMPERATURE*°F
T:1. Set temperature  2. Auto mode  3. Back to dashboard
A:$CLIMATE_CHOICE

M:1: J:SET_TEMPERATURE
M:2: J:AUTO_TEMPERATURE
M:3: J:MAIN_DASHBOARD

*SET_TEMPERATURE
T:Enter desired temperature (60-85°F):
A:$NEW_TEMP
130 LET TEMP_NUM = VAL(NEW_TEMP)
140 IF TEMP_NUM >= 60 AND TEMP_NUM <= 85 THEN GOSUB 5000
150 IF TEMP_NUM < 60 OR TEMP_NUM > 85 THEN J:TEMP_ERROR
J:CLIMATE_CONTROL

*5000 REM Set valid temperature
160 LET TEMPERATURE = TEMP_NUM
T:Temperature set to *NEW_TEMP*°F
# Add heating/cooling animation
RETURN

*TEMP_ERROR
T:Please enter a temperature between 60 and 85°F
J:SET_TEMPERATURE

# Students implement all other systems with similar complexity...
```

#### **📋 Assessment Criteria**

Students must demonstrate:
- **Conditional Logic**: Proper use of IF/THEN/ELSE in multiple contexts
- **Loop Mastery**: Appropriate loop types for different situations
- **Input Validation**: Robust handling of user input errors
- **Code Organization**: Clear structure with proper use of subroutines/procedures  
- **User Experience**: Intuitive navigation and helpful feedback
- **Integration**: Seamless combination of BASIC, PILOT, and Logo features

---

This comprehensive curriculum continues building computational thinking skills while maintaining the engaging, hands-on approach that makes Time Warp IDE special. Each lesson builds on previous knowledge while introducing new concepts in manageable chunks.

Would you like me to continue with the remaining units (Procedures and Functions, Graphics and Visualization, Interactive Programs, and Problem Solving Projects) to complete the Teacher Guide?