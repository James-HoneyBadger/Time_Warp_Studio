# ⚠️ EXPERIMENTAL RUST PORT

This directory contains an **experimental** native Rust port of Time Warp IDE.

## Status
- **Production Ready**: ❌ No - early-stage implementation
- **Feature Complete**: ❌ No - lacks features in Python version
- **Recommended For**: Advanced users, performance testing, porting contributions
- **Not Recommended For**: Learning, production use, feature-critical workflows

## Primary Implementation
The **official, feature-complete, production-ready version is in `Platforms/Python`**.

Users should default to the Python version unless they specifically need:
1. Native binary without Python runtime
2. Improved performance for specific use cases
3. To contribute to the Rust port

## Known Limitations
- Graphics implementation incomplete
- Missing advanced IDE features (breakpoints, collaboration, CRT effects, etc.)
- Limited language support compared to Python version
- UI still under development

## Building & Running
```bash
cargo build
./target/debug/tw
```

## Contributing
If you wish to help complete the Rust port, see the main repository issues for ongoing work items.
