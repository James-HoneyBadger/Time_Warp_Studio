# Security Policy

## Supported Versions

| Version | Supported          |
|---------|--------------------|
| 7.x     | :white_check_mark: |
| < 7.0   | :x:                |

## Reporting a Vulnerability

If you discover a security vulnerability in Time Warp Studio, please report it responsibly.

**Do NOT open a public GitHub issue for security vulnerabilities.**

### How to Report

1. **GitHub Security Advisories (Preferred):**  
   Use [GitHub's private vulnerability reporting](https://github.com/James-HoneyBadger/Time_Warp_Studio/security/advisories/new) to submit a confidential report directly through the repository.

2. **Email:**  
   Send details to **james@honey-badger.org** with the subject line `[SECURITY] Time Warp Studio`.

### What to Include

- A description of the vulnerability and its potential impact
- Steps to reproduce the issue
- Affected version(s)
- Any suggested fixes or mitigations (if available)

### Response Timeline

- **Acknowledgement:** Within 48 hours of receipt
- **Initial assessment:** Within 5 business days
- **Fix or mitigation:** Targeted within 30 days, depending on severity

### What to Expect

- You will receive a confirmation that your report was received.
- We will work with you to understand and validate the issue.
- A fix will be developed and released as a patch version.
- You will be credited in the release notes (unless you prefer to remain anonymous).

## Security Considerations

Time Warp Studio is an **educational desktop application**. It is designed for local use and does not expose network services by default. Key security notes:

- **Safe expression evaluation:** All math/expression evaluation uses a sandboxed evaluator (`safe_eval`) — `eval()` is never called directly on user input.
- **No network exposure:** The application runs locally. The optional Docker/collaboration features are not enabled by default.
- **Hardware simulation:** Arduino and Raspberry Pi integrations default to simulation mode and do not interact with real hardware unless explicitly configured.
- **Dependencies:** We use `PySide6` (Qt6) and `Pillow` as core dependencies. Keep them updated to their latest stable versions.

## Best Practices for Users

- Run Time Warp Studio in a standard user account (no elevated privileges required).
- Keep Python and all dependencies updated.
- Only load programs and plugins from trusted sources.
- If using Docker, review `docker-compose.yml` and network settings before deploying.

## Acknowledgements

We appreciate the security research community's efforts in helping keep Time Warp Studio safe. Contributors who responsibly disclose vulnerabilities will be acknowledged here.

---

*This policy follows [GitHub's recommended security policy format](https://docs.github.com/en/code-security/getting-started/adding-a-security-policy-to-your-repository).*
