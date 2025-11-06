# Security Policy

## Supported Versions

We actively support and provide security updates for the following versions of Time Warp IDE:

| Version | Supported          |
| ------- | ------------------ |
| 2.0.x   | :white_check_mark: |
| < 2.0   | :x:                |

## Security Features

Time Warp IDE includes several built-in security features to protect users, especially students in educational environments:

### Execution Safety

- **No Code Execution**: No use of `eval()` or `exec()` in Python implementation
- **Iteration Limits**: Maximum 100,000 iterations prevents infinite loops
- **Timeout Protection**: 10-second hard limit on program execution
- **Token Limits**: Expression evaluator limited to 1,000 tokens
- **Safe Expression Evaluation**: Custom parser prevents code injection

### Input Validation

- **Sanitized Input**: All user input is validated and sanitized
- **Type Checking**: Strict type validation prevents type confusion attacks
- **Bounds Checking**: Array and buffer access is bounds-checked
- **File I/O Restrictions**: Limited to program loading/saving only

### Memory Safety

- **Rust Implementation**: Memory-safe by design (no buffer overflows, use-after-free)
- **Python Implementation**: Managed memory with garbage collection
- **Stack Protection**: Limited recursion depth prevents stack overflow

## Reporting a Vulnerability

We take security seriously. If you discover a security vulnerability in Time Warp IDE, please report it responsibly:

### How to Report

**DO:**
- Email: james@honey-badger.org
- Subject line: "[SECURITY] Vulnerability Report"
- Include detailed description
- Provide steps to reproduce
- Suggest a fix if possible

**DO NOT:**
- Publicly disclose the vulnerability before it is fixed
- Create public GitHub issues for security vulnerabilities
- Exploit the vulnerability for malicious purposes

### What to Include

Please provide as much information as possible:

```
1. Vulnerability Type (e.g., code injection, DoS, memory leak)
2. Affected Component (Python/Rust, specific module)
3. Affected Version(s)
4. Steps to Reproduce (detailed)
5. Proof of Concept (if applicable)
6. Potential Impact Assessment
7. Suggested Remediation (if known)
8. Your Contact Information
```

### Response Timeline

- **Initial Response**: Within 48 hours
- **Vulnerability Assessment**: Within 1 week
- **Fix Development**: 1-4 weeks (depending on severity)
- **Release**: As soon as fix is tested
- **Public Disclosure**: After fix is released and users notified

### Severity Levels

We classify vulnerabilities using these severity levels:

**Critical (CVSS 9.0-10.0)**
- Remote code execution
- Arbitrary file access
- System compromise
- Response: Immediate action, emergency release

**High (CVSS 7.0-8.9)**
- Privilege escalation
- Information disclosure
- Denial of service (easily exploitable)
- Response: Fix within 7 days

**Medium (CVSS 4.0-6.9)**
- Limited information disclosure
- DoS requiring specific conditions
- Input validation bypass
- Response: Fix within 30 days

**Low (CVSS 0.1-3.9)**
- Minor information leakage
- Theoretical vulnerabilities
- Best practice violations
- Response: Fix in next regular release

## Security Updates

Security updates are released as patch versions (e.g., 2.0.1, 2.0.2) and include:

- **Security Advisories**: Published on GitHub
- **CHANGELOG Updates**: Documented in CHANGELOG.md
- **User Notifications**: Announced in release notes
- **CVE Assignment**: For critical/high vulnerabilities

## Known Security Considerations

### Educational Context

Time Warp IDE is designed for educational use in controlled environments:

- **Sandboxing**: No built-in sandboxing; should be run in controlled environment
- **Network Access**: No network I/O support (intentional design)
- **File System**: Limited to program files only (no arbitrary file access)
- **Permissions**: Runs with user's permissions (no elevation)

### Deployment Recommendations

For classroom/lab deployment:

1. **User Permissions**: Run as standard user (not administrator/root)
2. **Network Isolation**: Consider network restrictions if needed
3. **File Access**: Place in user-writable directory
4. **Updates**: Keep software up-to-date
5. **Monitoring**: Monitor for unusual activity in multi-user environments

### Platform-Specific Considerations

**Python Implementation:**
- Requires Python 3.8+ (ensure Python is up-to-date)
- PySide6 dependency (keep updated)
- No shell command execution

**Rust Implementation:**
- Compiled binary (no interpreter vulnerabilities)
- Memory-safe by design
- Static linking recommended for distribution

## Security Best Practices for Users

### Students and Teachers

- Keep Time Warp IDE updated to the latest version
- Only run programs from trusted sources
- Report suspicious behavior to instructors/maintainers
- Don't share personal information in program code
- Use in supervised educational settings

### Developers and Contributors

- Follow secure coding practices
- Validate all input
- Use safe APIs (no `eval`, `exec`, `system`)
- Run security checks (`cargo clippy`, linters)
- Test with malicious input samples
- Review dependencies for vulnerabilities

## Security Audit History

| Date       | Type               | Findings | Status   |
|------------|-------------------|----------|----------|
| 2025-10-28 | Internal Review   | 0 issues | Complete |

## Compliance and Standards

Time Warp IDE follows industry security standards:

- **OWASP**: Follows OWASP secure coding practices
- **CWE**: Addresses common weakness enumeration items
- **Safe Rust**: Uses safe Rust patterns (no unsafe blocks without justification)
- **Type Safety**: Strict type checking in both implementations

## Security-Related Configuration

No security-related configuration required. Time Warp IDE is secure by default.

## Acknowledgments

We appreciate the security research community and thank those who responsibly disclose vulnerabilities. Security researchers who report valid vulnerabilities will be:

- Credited in security advisories (with permission)
- Listed in CHANGELOG.md
- Thanked in release notes
- Awarded bragging rights 🏆

## Contact

For security concerns:

**Email**: james@honey-badger.org  
**GitHub**: https://github.com/James-HoneyBadger/Time_Warp  
**PGP Key**: Available upon request

## Additional Resources

- [Contributing Guidelines](CONTRIBUTING.md)
- [Code of Conduct](CODE_OF_CONDUCT.md)
- [CHANGELOG](CHANGELOG.md)
- [Architecture Documentation](Time_Warp_Rust/ARCHITECTURE.md)

---

**Last Updated**: October 28, 2025  
**Version**: 2.0.0
