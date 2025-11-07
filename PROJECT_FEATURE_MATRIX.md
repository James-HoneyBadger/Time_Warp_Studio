# Time Warp IDE - Cross-Platform Feature Parity Matrix

**Generated:** November 7, 2025  
**Status:** Post-comprehensive expansion audit

This document tracks feature implementation across all Time Warp IDE platforms, ensuring consistency and completeness.

---

## 🎯 Platform Overview

| Platform | Language | Status | GUI Framework | Target OS |
|----------|----------|--------|---------------|-----------|
| **Rust** | Rust | ✅ Reference | egui | Linux/Windows/macOS |
| **Python** | Python 3 | ✅ Complete | tkinter/PySide6 | All platforms |
| **Go** | Go 1.18+ | ✅ Expanded | Fyne/Terminal | All platforms |
| **Win2000** | C (Win32) | ✅ Expanded | MDI/GDI | Windows 2000+ |
| **OS/2** | C (PM) | 🔶 Scaffolded | Presentation Manager | OS/2 Warp 4 |
| **DOS** | C89 | ✅ Complete | Text mode | MS-DOS/FreeDOS |
| **Amiga** | C (SAS/GCC) | 🔶 Minimal | Intuition | AmigaOS 2.0+ |
| **Web** | JavaScript | ✅ Complete | HTML5 Canvas | Browsers |
| **Apple** | Swift | 📄 README only | SwiftUI | macOS/iOS |
| **Windows** | C#/.NET | 📄 README only | WPF/WinUI3 | Windows 10+ |

**Legend:**  
✅ Complete  
🔶 Partial  
📄 Documentation only  
❌ Not started

---

## 📊 BASIC Language Feature Matrix

### Core Commands

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **PRINT** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **LET** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **INPUT** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **GOTO** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **IF/THEN** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **FOR/NEXT** | ✅ | ✅ | ✅ | 🔶 | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **GOSUB/RETURN** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **REM** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **END** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **CLS** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **LOCATE** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |

### Graphics Commands

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **LINE** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **CIRCLE** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **SCREEN** | ✅ | ✅ | ❌ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **PSET** | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ❌ | ✅ | ❌ | ❌ |

### Advanced Features

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **Arrays (DIM)** | 🔶 | ✅ | ❌ | ❌ | ❌ | ✅ | ❌ | ✅ | ❌ | ❌ |
| **DATA/READ** | 🔶 | ✅ | ❌ | ❌ | ❌ | ✅ | ❌ | ✅ | ❌ | ❌ |
| **Functions (ABS, INT, SQR)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **String Functions** | 🔶 | ✅ | ❌ | ❌ | ❌ | ✅ | ❌ | ✅ | ❌ | ❌ |
| **INKEY$** | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |

---

## 🐢 Logo Language Feature Matrix

### Movement Commands

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **FORWARD/FD** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **BACK/BK** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **LEFT/LT** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **RIGHT/RT** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **HOME** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **SETXY** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **SETHEADING/SETH** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |

### Pen Control

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **PENUP/PU** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **PENDOWN/PD** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **SETCOLOR** | ✅ | ✅ | ✅ | ✅ | 🔶 | 🔶 | 🔶 | ✅ | ❌ | ❌ |
| **PENWIDTH** | ✅ | ✅ | ✅ | ✅ | 🔶 | 🔶 | 🔶 | ✅ | ❌ | ❌ |
| **SETBGCOLOR** | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |

### Advanced Features

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **CLEARSCREEN/CS** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **HIDETURTLE/HT** | ✅ | ✅ | ✅ | ✅ | 🔶 | ❌ | ❌ | ✅ | ❌ | ❌ |
| **SHOWTURTLE/ST** | ✅ | ✅ | ✅ | ✅ | 🔶 | ❌ | ❌ | ✅ | ❌ | ❌ |
| **TO/END (Procedures)** | ✅ | ✅ | 🔶 | ✅ | 🔶 | ❌ | ❌ | ✅ | ❌ | ❌ |
| **REPEAT** | ✅ | ✅ | 🔶 | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |

---

## 🗣️ PILOT Language Feature Matrix

### Core Commands

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **T: (Type)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **A: (Accept)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **U: (Use)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **C: (Compute)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **M: (Match)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **Y: (Yes)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **N: (No)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **J: (Jump)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **L: (Label)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **E: (End)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **R: (Remark)** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |

### Advanced Features

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **Variable Interpolation** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **Pattern Matching** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **Condition Evaluation** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **Numeric Variables** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |
| **String Variables** | ✅ | ✅ | ✅ | ✅ | 🔶 | ✅ | 🔶 | ✅ | ❌ | ❌ |

---

## 🎨 IDE Features Matrix

### Editor Features

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **Syntax Highlighting** | ✅ | ✅ | ❌ | ✅ | 🔶 | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Line Numbers** | ✅ | ✅ | ❌ | ✅ | 🔶 | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Code Folding** | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| **Auto-Indent** | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Find/Replace** | ✅ | ✅ | ❌ | ✅ | 🔶 | ❌ | ❌ | ✅ | ❌ | ❌ |

### Debug Features

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **Breakpoints** | 🔶 | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Step Execution** | 🔶 | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Watch Variables** | 🔶 | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Call Stack** | 🔶 | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Error Highlighting** | ✅ | ✅ | ❌ | ✅ | 🔶 | ❌ | ❌ | ✅ | ❌ | ❌ |

### File Operations

| Feature | Rust | Python | Go | Win2000 | OS/2 | DOS | Amiga | Web | Apple | Windows |
|---------|------|--------|----|---------| -----|-----|-------|-----|-------|---------|
| **Open/Save** | ✅ | ✅ | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ |
| **Recent Files** | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Auto-Save** | ✅ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |
| **Export (HTML/PDF)** | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ | ❌ | ❌ |

---

## 📦 Build & Distribution

### Build Systems

| Platform | Build Tool | Dependencies | Package Format |
|----------|------------|--------------|----------------|
| **Rust** | Cargo | egui, anyhow | Binary executable |
| **Python** | pip/poetry | tkinter/PySide6 | Wheel/EXE |
| **Go** | go build | Fyne (optional) | Binary executable |
| **Win2000** | MSVC/MinGW | Win32 SDK | .EXE |
| **OS/2** | OpenWatcom/EMX | PM Toolkit | .EXE |
| **DOS** | OpenWatcom/DJGPP | None | .EXE |
| **Amiga** | SAS/C or GCC | Intuition libs | Binary |
| **Web** | webpack/vite | None (pure JS) | HTML/JS bundle |
| **Apple** | Xcode/Swift PM | SwiftUI | .app bundle |
| **Windows** | VS/dotnet | WPF/.NET | .exe/MSIX |

---

## 🚀 Priority Action Items

### Immediate (Next Session)

1. **OS/2 Port Completion**
   - Copy Win2000 interpreters (basic, logo, pilot)
   - Replace TCHAR → char, _tcs → str functions
   - Adapt Win32 API → PM API (SendMessage → WinSendMsg, etc.)
   - Test with OpenWatcom wcl386

2. **Amiga Port Expansion**
   - Copy DOS text-mode interpreters
   - Add Intuition GUI wrapper
   - Implement IFF graphics for turtle
   - Test with SAS/C or m68k-amigaos-gcc

3. **Apple SwiftUI Implementation**
   - Create Xcode project structure
   - Implement Swift interpreters (BASIC/Logo/PILOT)
   - SwiftUI editor + canvas views
   - Universal binary for Mac/iPad/iPhone

4. **Generic Windows Port**
   - Create WPF/WinUI3 project
   - C# interpreter implementations
   - XAML UI with MVVM pattern
   - MSIX packaging for Windows Store

### Medium Term

5. **Cross-Platform Test Suite**
   - Create standard test programs (.bas, .logo, .pilot)
   - Automated test runner for C/Python/Rust/Go
   - Expected output verification
   - Performance benchmarks

6. **Documentation Updates**
   - Platform-specific installation guides
   - Feature comparison charts
   - Migration guides between platforms
   - Video tutorials for each platform

### Long Term

7. **Feature Parity Completion**
   - Arrays (DIM) in all BASIC implementations
   - DATA/READ/RESTORE in all platforms
   - String functions uniformly
   - REPEAT blocks in all Logo variants

8. **Advanced Features**
   - Network capabilities (TCP/IP)
   - File I/O (OPEN/CLOSE/READ/WRITE)
   - Sound/music commands
   - IoT hardware integration (Arduino/RPi)

---

## 📝 Notes

- **Reference Implementation**: Rust version is the canonical reference for all commands
- **Minimum Viable**: PRINT, LET, IF, GOTO, FORWARD, RIGHT, T:, J: are minimum for any platform
- **Testing**: DOS and Web versions are most extensively tested
- **Performance**: Native C implementations (Win2000, OS/2, DOS, Amiga) are fastest
- **Portability**: Python and Go versions are most portable across modern systems

---

**Maintained by:** James Temple <james@honey-badger.org>  
**Repository:** <https://github.com/James-HoneyBadger/Time_Warp>  
**License:** MIT
