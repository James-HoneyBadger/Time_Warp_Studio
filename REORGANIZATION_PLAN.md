# File Structure Reorganization Plan

## Current Issues
1. Too many root-level documentation files
2. Duplicate platform folders (Time_Warp_* and platforms/)
3. Backup files cluttering root
4. Unclear separation of concerns
5. Mixed installation scripts and docs

## Proposed Structure

```
Time_Warp/
├── .github/                    # GitHub-specific files
├── docs/                       # ALL documentation
│   ├── architecture/          # Architecture docs
│   ├── development/           # Developer guides
│   ├── installation/          # Installation guides
│   ├── user/                  # User guides
│   └── misc/                  # Project summaries, etc.
├── platforms/                 # Platform implementations
│   ├── rust/                  # Primary (from Time_Warp_Rust)
│   ├── python/                # From Time_Warp_Python
│   ├── go/                    # From Time_Warp_Go
│   ├── web/                   # From Time_Warp_Web
│   ├── dos/                   # From Time_Warp_DOS
│   ├── windows/               # From Time_Warp_Windows
│   ├── apple/                 # From Time_Warp_Apple
│   └── experimental/          # Haiku, OS2, Amiga, Win2000
├── core-spec/                 # Language specification
├── examples/                  # Example programs
├── scripts/                   # Build/utility scripts
├── tests/                     # Test suites
├── packaging/                 # Package/release files
├── .archive/                  # Archived/backup files
├── LICENSE                    # Main license
├── README.md                  # Main readme
└── CONTRIBUTING.md            # How to contribute
```

## Migration Steps
1. Create new directory structure
2. Move documentation to docs/
3. Consolidate platform folders
4. Move backups to .archive/
5. Clean up root directory
6. Update all path references
