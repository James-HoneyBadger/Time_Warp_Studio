# Security Policy

## Supported Versions

| Version | Supported           |
| ------- | ------------------- |
| 14.x    | :white_check_mark:  |
| < 14.0  | :x:                 |

## Reporting a Vulnerability

If you discover a security vulnerability in Time Warp Studio, please report it responsibly.

**Do NOT open a public GitHub issue for security vulnerabilities.**

### How to Report

1. **GitHub Security Advisories (Preferred):**  
   Use [GitHub's private vulnerability reporting](https://github.com/James-HoneyBadger/Time_Warp_Studio/security/advisories/new) to submit a confidential report directly through the repository.

2. **Email:**  
   Send details to **<james@honey-badger.org>** with the subject line `[SECURITY] Time Warp Studio`.

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

- **Safe expression evaluation:** All math/expression evaluation uses a hand-written parser — native C++ (`timewarp_expr`) and, in the bridge engine, `ExpressionEvaluator`. `eval()` is never called on user input.
- **No network exposure:** The application runs locally and does not open network ports or ship any server components.
- **Student programs are not a security boundary.** A program you run executes with the privileges of the IDE: BASIC's file commands can read and write files the user can, and a program that never returns has to be stopped by the user. Run programs you trust.
- **Optional subprocess sandbox:** setting `TWS_SANDBOX=1` runs whole-program executors in a separate process with CPU/memory limits and a timeout, for running untrusted student code. It is opt-in and off by default.
- **Dependencies:** the application is C++20 with Qt6. The temporary bridge engine for the three unported languages is Python 3.10+ and standard-library only — no third-party packages are required (`pyttsx3`, `pygame` and `inputs` are optional and imported defensively). Keep Qt and Python updated to their latest stable versions.
- **Secrets stay out of the repository.** A `detect-secrets` pre-commit hook checks staged files against a committed baseline (`.secrets.baseline`), so a credential pasted into a file fails the commit instead of being pushed. If a secret does reach a commit, treat it as compromised: rotate it, then rewrite the history that contains it — removing the file in a later commit does not remove it from the repository.

## Best Practices for Users

- Run Time Warp Studio in a standard user account (no elevated privileges required).
- Keep Qt, Python and all dependencies updated.
- Only load programs from trusted sources.

## Acknowledgements

We appreciate the security research community's efforts in helping keep Time Warp Studio safe. Contributors who responsibly disclose vulnerabilities will be acknowledged here.

---

*This policy follows [GitHub's recommended security policy format](https://docs.github.com/en/code-security/getting-started/adding-a-security-policy-to-your-repository).*
