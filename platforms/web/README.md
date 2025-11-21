# 🌐 Time Warp IDE - Web Implementation

**🚀 Zero-Install Browser-Based Educational Programming Platform**

[![HTML5](https://img.shields.io/badge/HTML5-Ready-orange.svg)](https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/HTML5)
[![JavaScript](https://img.shields.io/badge/JavaScript-ES6+-yellow.svg)](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](../LICENSE)

> **🎯 Part of the Time Warp Educational Platform** — See [main documentation](../docs/) for complete guides and curriculum materials.

The **Web implementation** of Time Warp IDE brings TempleCode programming directly to any modern web browser. Perfect for **Chromebooks**, **1:1 device programs**, and **remote learning**, this version requires no installation and works on any device with internet access.

## Features

## 🎯 Why Choose the Web Version?

- **🚫 Zero Installation**: Works instantly in any modern web browser
- **📱 Universal Access**: Perfect for Chromebooks, tablets, and mobile devices
- **🏫 School-Friendly**: No admin rights needed, works behind firewalls
- **☁️ Always Updated**: Latest features without manual updates
- **🤝 Easy Sharing**: Share programs with simple URLs
- **💾 Auto-Save**: Never lose your work with automatic local storage

## ✨ Features

### 🎓 **Educational Excellence**
- **Complete TempleCode Support**: BASIC, PILOT, and Logo in one unified language
- **Interactive Turtle Graphics**: Full HTML5 Canvas implementation with smooth animations
- **Professional IDE Features**: Syntax highlighting, line numbers, and error detection
- **Mobile-Responsive Design**: Perfect for tablets and Chromebooks

### 🛠️ **Advanced Development Tools**
- **Interactive Debugging**: Step-through execution with breakpoints
- **Variable Inspector**: Real-time variable monitoring and watches
- **Performance Metrics**: Execution timing and optimization insights
- **Code Snippets Library**: Pre-built examples for quick learning
- **Export Capabilities**: Save graphics and share programs easily

## Getting Started

### Running Locally

1. Clone or download the Time Warp project
2. Navigate to the `Time_Warp_Web` directory
3. Open `index.html` in a modern web browser
4. Start programming!

### Online Deployment

Simply upload all files to any web server or hosting platform. The IDE runs entirely in the browser with no server-side requirements.

## Usage

1. **Choose Language Mode**: Select PILOT, BASIC, Logo, or Auto-Detect from the dropdown
2. **Write Your Program**: Use the code editor with line numbers and syntax support
3. **Run or Debug**: Click Run for normal execution or Debug for step-by-step
4. **Explore Tabs**: 
   - Output: Program messages and results
   - Variables: Live variable values and watches
   - Graphics: Turtle drawing canvas
   - Performance: Execution metrics and timing
   - Timeline: Step-by-step execution history
   - Snippets: Pre-built code examples
   - Help: Language reference and examples

## Example Programs

### PILOT Example
```
T:Welcome to PILOT!
A:name
T:Hello *name*!
R:5 * 10 -> result
T:5 times 10 equals *result*
E:
```

### BASIC Example
```
10 PRINT "Counting to 10"
20 FOR I = 1 TO 10
30   PRINT "Count: "; I
40 NEXT I
50 END
```

### Logo Example
```
CLEARSCREEN
REPEAT 4 [
  FORWARD 100
  RIGHT 90
]
```

## File Structure

```
Time_Warp_Web/
├── index.html          # Main IDE interface
├── styles.css          # Professional styling
├── js/
│   ├── interpreter.js  # Multi-language interpreter
│   ├── graphics.js     # Turtle graphics engine
│   ├── ui.js          # User interface controller
│   └── app.js         # Application initialization
└── README.md          # This file
```

## Browser Compatibility

- **Recommended**: Chrome 60+, Firefox 55+, Safari 12+, Edge 79+
- **Requirements**: ES6 support, HTML5 Canvas, Local Storage
- **Mobile**: Responsive design supports tablets and smartphones

## Educational Use

Time Warp Web is designed for:
- Programming education at all levels
- Computer science classrooms
- Self-directed learning
- Historical programming language exploration
- Turtle graphics and computational thinking

## 📚 Learning & Documentation

### 🎓 **For Students & Beginners**
- **[📖 Student Lesson Book](../docs/STUDENT_LESSON_BOOK.md)** — Progressive 24-lesson curriculum with hands-on projects
- **[🎯 User Guide](../docs/USER_GUIDE.md)** — Complete installation and usage guide for all platforms  
- **[🌐 Web-Specific Guide](../docs/USER_GUIDE.md#web-version)** — Browser setup and mobile optimization

### 👨‍🏫 **For Educators**  
- **[🍎 Teacher Guide & Curriculum](../docs/TEACHER_GUIDE.md)** — Complete educational framework with lesson plans
- **[🏫 Classroom Setup](../docs/TEACHER_GUIDE.md#classroom-technology)** — Chromebook and 1:1 device deployment
- **[📱 Mobile Learning](../docs/TEACHER_GUIDE.md#mobile-considerations)** — Tablet and phone optimization

### 🔧 **For Developers**
- **[⚙️ Technical Reference](../docs/TECHNICAL_REFERENCE.md)** — Architecture, APIs, and implementation details
- **[🌐 Web Architecture](../docs/TECHNICAL_REFERENCE.md#web-implementation)** — Client-side design and performance
- **[🏗️ Contributing Guide](../docs/CONTRIBUTING.md)** — How to extend and improve Time Warp

## 🛠️ Technical Architecture

### **Client-Side Excellence**
- **Pure JavaScript ES6+**: No frameworks, maximum compatibility
- **HTML5 Canvas Graphics**: Smooth, hardware-accelerated turtle graphics
- **Responsive CSS3**: Perfect scaling from phones to desktops
- **Local Storage**: Persistent program saves without servers

### **Performance & Reliability**
- **Efficient Interpreter**: Optimized for mobile processors
- **Memory Management**: Safe execution with configurable limits
- **Error Handling**: Graceful degradation and helpful messages
- **Progressive Enhancement**: Works on older devices too

## 🔗 Quick Links

- **🚀 [Try Online](index.html)** — Open directly in your browser
- **📱 [Mobile Setup](../docs/USER_GUIDE.md#mobile-setup)** — Optimize for tablets and phones
- **🏫 [Classroom Deployment](../docs/TEACHER_GUIDE.md#web-deployment)** — School network guidelines
- **🧪 [Examples Library](examples/)** — Ready-to-run educational programs

---

<div align="center">

**🌐 Time Warp IDE - Web Implementation**

*Programming education that works everywhere*

📱 **Mobile Ready** • 🏫 **School Friendly** • ⚡ **Instant Access**

Made with ❤️ for digital learning and accessibility

</div>
- **2.0.x**: Desktop versions with native implementations
- **1.x**: Original PILOT interpreter versions

---

Start programming and explore the fascinating world of educational programming languages with Time Warp Web Edition!