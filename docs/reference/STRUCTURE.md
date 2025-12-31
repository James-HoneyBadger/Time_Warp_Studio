# Time Warp Studio - Project Structure

```
Time_Warp_Studio/
├── Platforms/
│   └── Python/              ← MAIN IMPLEMENTATION (PySide6)
│       ├── time_warp_ide.py ← Entry point
│       ├── time_warp/       ← Core package
│       ├── requirements.txt
│       └── .venv/           ← Virtual environment
│
├── Examples/                ← Sample programs
│   ├── basic/
│   ├── logo/
│   ├── pilot/
│   ├── pascal/
│   ├── prolog/
│   ├── forth/
│   └── c/
│
├── Scripts/                 ← Build & utility scripts
├── Config/                  ← Configuration files
├── packaging/               ← Distribution packaging
├── docs/                    ← Documentation
│   ├── README.md
│   └── archive/             ← Historical docs
│
├── .github/                 ← GitHub configuration
│   └── copilot-instructions.md
│
├── README.md                ← Main documentation
├── INSTALL_NATIVE.md        ← Native build instructions
├── LICENSE
└── .gitignore
```

## Key Files

### Documentation
- **README.md** - Overview, quick start, feature list
- **INSTALL_NATIVE.md** - Building native versions

### Main Implementation
- **Platforms/Python/time_warp_ide.py** - Official IDE entry point
- **Platforms/Python/time_warp/** - Core interpreter and UI code

### Examples
Sample programs demonstrating all supported languages:
- BASIC: `Examples/basic/`
- Logo: `Examples/logo/`
- PILOT: `Examples/pilot/`
- Pascal: `Examples/pascal/`
- Prolog: `Examples/prolog/`
- Forth: `Examples/forth/`
- C: `Examples/c/`

## Development

### Python (Official)
```bash
cd Platforms/Python
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
python time_warp_ide.py
```
