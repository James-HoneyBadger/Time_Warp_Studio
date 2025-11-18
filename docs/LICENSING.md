# Licensing Information

**Time Warp IDE - License Documentation**

**Last Updated:** November 18, 2025

---

## Primary License

Time Warp IDE is licensed under the **MIT License**.

**SPDX License Identifier:** `MIT`

**License File:** [LICENSE](../LICENSE)

### MIT License Summary

The MIT License is a permissive free software license that allows you to:
- ✅ Use the software commercially
- ✅ Modify the software
- ✅ Distribute the software
- ✅ Sublicense the software
- ✅ Use the software privately

**Requirements:**
- Include the copyright notice and permission notice in all copies
- The software is provided "as is" without warranty

**Full Text:** See [LICENSE](../LICENSE)

---

## Historical Context

Some components of Time Warp IDE were previously distributed under the **Apache License 2.0**. This was changed to MIT for simplicity and broader compatibility.

**Apache 2.0 License:** Retained in `LICENSES/Apache-2.0.txt` for historical reference and compatibility.

### Why MIT?

The MIT License was chosen for Time Warp IDE because:
1. **Simplicity** - Easy to understand and apply
2. **Permissiveness** - Minimal restrictions on use
3. **Compatibility** - Works well with other open source licenses
4. **Education** - Appropriate for educational software
5. **Community** - Popular in the Rust and educational software communities

---

## Third-Party Dependencies

Time Warp IDE uses several third-party libraries, each with their own licenses:

### Rust Implementation

| Dependency | License | Purpose |
|------------|---------|---------|
| eframe | MIT OR Apache-2.0 | UI framework |
| egui | MIT OR Apache-2.0 | Immediate mode GUI |
| rfd | MIT | File dialogs |
| anyhow | MIT OR Apache-2.0 | Error handling |
| serde | MIT OR Apache-2.0 | Serialization |
| regex | MIT OR Apache-2.0 | Pattern matching |
| tokio | MIT | Async runtime |
| image | MIT OR Apache-2.0 | Image processing |

**Note:** Most Rust dependencies are dual-licensed MIT OR Apache-2.0, providing maximum compatibility.

### Python Implementation

| Dependency | License | Purpose |
|------------|---------|---------|
| PySide6 | LGPL-3.0 | Qt6 bindings |
| Pillow | HPND | Image processing |
| pytest | MIT | Testing framework |

**Note:** PySide6 is licensed under LGPL-3.0. When distributing Time Warp Python, comply with LGPL requirements (allow relinking, provide source, etc.).

### Go Implementation

| Dependency | License | Purpose |
|------------|---------|---------|
| Fyne | BSD-3-Clause | UI toolkit |

---

## License Compliance

### For Users

**Using Time Warp IDE:**
- No restrictions - use freely for education, personal, or commercial purposes
- No need to release your TempleCode programs under any specific license
- Your code is yours

**Redistributing Time Warp IDE:**
- Include the LICENSE file
- Include the copyright notice
- That's it!

### For Contributors

**Contributing to Time Warp IDE:**
- By contributing, you agree to license your contributions under MIT
- Include copyright header in new files:
  ```rust
  // SPDX-License-Identifier: MIT
  // Copyright (c) 2025 James Temple <james@honey-badger.org>
  ```
- Ensure third-party code is MIT-compatible

**Adding Dependencies:**
- Verify license compatibility
- Update this document
- Add to LICENSES/ directory if needed

### For Distributors

**Binary Distributions:**
- Include LICENSE file in package
- Include copyright notices for dependencies
- For Python: Comply with LGPL requirements for PySide6

**Modified Versions:**
- You may modify and distribute
- Retain original copyright notices
- Add your own copyright for modifications
- Consider contributing changes upstream

---

## Copyright Notices

### Time Warp IDE

```
Copyright (c) 2025 James Temple <james@honey-badger.org>
```

### Example Programs

Example programs in `examples/` are also MIT licensed and may be used freely in educational settings.

### Documentation

All documentation is MIT licensed unless otherwise noted.

---

## SPDX Headers

Source files should include SPDX license identifiers:

**Rust:**
```rust
// SPDX-License-Identifier: MIT
```

**Python:**
```python
# SPDX-License-Identifier: MIT
```

**C:**
```c
/* SPDX-License-Identifier: MIT */
```

**TempleCode:**
```templecode
REM SPDX-License-Identifier: MIT
```

---

## License Directory

The `LICENSES/` directory contains full text of all licenses:

```
LICENSES/
├── MIT.txt           # Primary license (MIT)
└── Apache-2.0.txt    # Historical license (reference)
```

---

## Frequently Asked Questions

### Can I use Time Warp IDE in my classroom?

**Yes!** Time Warp IDE is designed for education. Use it freely in any educational setting.

### Can I sell software built with Time Warp IDE?

**Yes!** Your TempleCode programs are yours. You own the copyright and can do whatever you want with them.

### Can I include Time Warp IDE in my Linux distribution?

**Yes!** The MIT license is distribution-friendly. Just include the LICENSE file.

### Can I fork and modify Time Warp IDE?

**Yes!** Fork away. Modifications are encouraged. Consider contributing improvements back.

### Do I need to open source my modifications?

**No.** The MIT license does not require you to release source code. However, contributing back helps the community.

### Can I use Time Warp IDE commercially?

**Yes!** Commercial use is permitted. No royalties or fees required.

### What if I want to relicense my fork?

**You cannot.** MIT-licensed software must remain MIT-licensed. You can add additional permissions but not remove the MIT terms.

### Can I remove the copyright notice?

**No.** You must retain the copyright notice and permission notice in all copies.

---

## Contact

For licensing questions:
- **Email:** James Temple <james@honey-badger.org>
- **GitHub:** [github.com/James-HoneyBadger/Time_Warp](https://github.com/James-HoneyBadger/Time_Warp)

---

## Additional Resources

- **MIT License:** [https://opensource.org/licenses/MIT](https://opensource.org/licenses/MIT)
- **SPDX:** [https://spdx.org/](https://spdx.org/)
- **Choose a License:** [https://choosealicense.com/](https://choosealicense.com/)
- **Open Source Initiative:** [https://opensource.org/](https://opensource.org/)

---

**Disclaimer:** This document provides general information about licensing. It is not legal advice. Consult an attorney for specific legal questions.

---

**Last Updated:** November 18, 2025  
**License:** MIT  
**SPDX-License-Identifier:** MIT
