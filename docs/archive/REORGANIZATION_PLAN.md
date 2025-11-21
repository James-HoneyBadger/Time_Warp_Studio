# File Structure Reorganization Plan

## Current Issues
1. Too many root-level documentation files
2. Duplicate platform folders (legacy Time Warp platform directories and the new platforms/ tree)
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
│   ├── rust/                  # Primary (formerly the Time Warp Rust folder)
│   ├── python/                # Formerly the Time Warp Python folder
│   ├── go/                    # Formerly the Time Warp Go folder
│   ├── web/                   # Formerly the Time Warp Web folder
│   ├── dos/                   # Formerly the Time Warp DOS folder
│   ├── windows/               # Formerly the Time Warp Windows folder
│   ├── apple/                 # Formerly the Time Warp Apple folder
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
