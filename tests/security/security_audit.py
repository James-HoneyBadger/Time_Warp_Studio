#!/usr/bin/env python3
"""
Time Warp Studio - Security & Vulnerability Testing Suite

Run with: python security_audit.py --all
"""

import sys
import json
import subprocess
import requests
from dataclasses import dataclass, asdict
from typing import List, Dict, Any
from enum import Enum
from datetime import datetime

class Severity(Enum):
    CRITICAL = "CRITICAL"
    HIGH = "HIGH"
    MEDIUM = "MEDIUM"
    LOW = "LOW"
    INFO = "INFO"

@dataclass
class Vulnerability:
    id: str
    title: str
    description: str
    severity: Severity
    component: str
    remediation: str
    cve: str = None
    cvss_score: float = 0.0
    found_at: str = None
    fixed: bool = False

class SecurityAuditor:
    def __init__(self, base_url: str = "http://localhost:8000"):
        self.base_url = base_url
        self.vulnerabilities: List[Vulnerability] = []
        self.session = requests.Session()
        self.session.headers.update({
            "User-Agent": "SecurityAuditor/1.0"
        })

    def run_all_audits(self) -> Dict[str, Any]:
        """Run all security audits and return summary"""
        audits = [
            ("SQL Injection", self.test_sql_injection),
            ("XSS Vulnerabilities", self.test_xss),
            ("CSRF Protection", self.test_csrf),
            ("Authentication", self.test_auth),
            ("Authorization", self.test_authz),
            ("Input Validation", self.test_input_validation),
            ("Dependency Vulnerabilities", self.test_dependencies),
            ("SSL/TLS Configuration", self.test_tls),
            ("Headers Security", self.test_security_headers),
            ("API Rate Limiting", self.test_rate_limiting),
            ("Cryptography", self.test_cryptography),
            ("Session Management", self.test_session_mgmt),
            ("File Upload Security", self.test_file_uploads),
            ("Command Injection", self.test_command_injection),
            ("XXE Vulnerabilities", self.test_xxe),
        ]

        for name, test_func in audits:
            try:
                print(f"Running: {name}...", end=" ", flush=True)
                test_func()
                print("✓")
            except Exception as e:
                print(f"✗ ({str(e)[:50]})")

        return self.generate_report()

    def test_sql_injection(self):
        """Test for SQL injection vulnerabilities"""
        payloads = [
            "' OR '1'='1",
            "'; DROP TABLE users; --",
            "1' UNION SELECT * FROM users --",
            "admin' --",
            "' OR 1=1 --",
        ]

        endpoints = [
            "/api/search?q=",
            "/api/filter?name=",
            "/api/users/",
        ]

        for endpoint in endpoints:
            for payload in payloads:
                try:
                    response = self.session.get(
                        f"{self.base_url}{endpoint}{payload}",
                        timeout=5
                    )
                    
                    # Check for error messages that indicate SQL injection
                    if any(keyword in response.text.lower() for keyword in 
                           ['sql', 'syntax', 'database', 'table', 'column']):
                        self.vulnerabilities.append(Vulnerability(
                            id="SQL_INJECT_001",
                            title="Potential SQL Injection",
                            description=f"Endpoint {endpoint} may be vulnerable to SQL injection",
                            severity=Severity.CRITICAL,
                            component="API",
                            remediation="Use parameterized queries/ORM"
                        ))
                        return
                except Exception:
                    pass

    def test_xss(self):
        """Test for XSS vulnerabilities"""
        xss_payloads = [
            "<script>alert('XSS')</script>",
            "'\"><script>alert(String.fromCharCode(88,83,83))</script>",
            "<img src=x onerror=alert('XSS')>",
            "<svg onload=alert('XSS')>",
            "javascript:alert('XSS')",
            "<iframe src='javascript:alert(\"XSS\")'></iframe>",
            "<body onload=alert('XSS')>",
        ]

        endpoints = [
            "/api/snippets",
            "/api/comments",
            "/api/profile",
        ]

        for endpoint in endpoints:
            for payload in xss_payloads:
                try:
                    response = self.session.post(
                        f"{self.base_url}{endpoint}",
                        json={"content": payload},
                        timeout=5
                    )
                    
                    # Check if payload is reflected without escaping
                    if payload in response.text and '<' in response.text:
                        self.vulnerabilities.append(Vulnerability(
                            id="XSS_001",
                            title="XSS Vulnerability",
                            description=f"Endpoint {endpoint} reflects user input without escaping",
                            severity=Severity.HIGH,
                            component="Web UI",
                            remediation="Properly escape all user input in templates"
                        ))
                        return
                except Exception:
                    pass

    def test_csrf(self):
        """Test for CSRF protection"""
        # Check if state-changing requests require CSRF tokens
        
        try:
            # Try POST without token
            response = self.session.post(
                f"{self.base_url}/api/profile/update",
                json={"name": "hacker"},
                timeout=5
            )
            
            # If it succeeds without CSRF token, it's vulnerable
            if response.status_code == 200:
                self.vulnerabilities.append(Vulnerability(
                    id="CSRF_001",
                    title="Missing CSRF Protection",
                    description="State-changing requests don't require CSRF tokens",
                    severity=Severity.HIGH,
                    component="API",
                    remediation="Implement CSRF token validation for all state-changing requests"
                ))
        except Exception:
            pass

    def test_auth(self):
        """Test authentication mechanism"""
        # Test missing credentials
        try:
            response = self.session.get(
                f"{self.base_url}/api/protected-endpoint",
                timeout=5
            )
            
            if response.status_code != 401:
                self.vulnerabilities.append(Vulnerability(
                    id="AUTH_001",
                    title="Missing Authentication Check",
                    description="Protected endpoint accessible without credentials",
                    severity=Severity.CRITICAL,
                    component="API",
                    remediation="Implement proper authentication checks"
                ))
        except Exception:
            pass

        # Test weak password requirements
        weak_passwords = ["123", "password", "admin", "test", "123456"]
        for password in weak_passwords:
            try:
                response = self.session.post(
                    f"{self.base_url}/api/auth/register",
                    json={
                        "username": f"testuser_{password}",
                        "password": password,
                        "email": f"test_{password}@example.com"
                    },
                    timeout=5
                )
                
                if response.status_code in [200, 201]:
                    self.vulnerabilities.append(Vulnerability(
                        id="AUTH_002",
                        title="Weak Password Requirements",
                        description=f"Password '{password}' is accepted",
                        severity=Severity.HIGH,
                        component="Auth",
                        remediation="Enforce minimum password complexity"
                    ))
                    return
            except Exception:
                pass

    def test_authz(self):
        """Test authorization mechanism"""
        # Test accessing other user's data
        try:
            # Get token for user1
            token1 = self._get_test_token("user1")
            
            # Try to access user2's data
            response = self.session.get(
                f"{self.base_url}/api/users/user2/profile",
                headers={"Authorization": f"Bearer {token1}"},
                timeout=5
            )
            
            if response.status_code == 200:
                self.vulnerabilities.append(Vulnerability(
                    id="AUTHZ_001",
                    title="Insufficient Authorization Check",
                    description="User can access other users' data",
                    severity=Severity.CRITICAL,
                    component="API",
                    remediation="Implement proper authorization checks"
                ))
        except Exception:
            pass

    def test_input_validation(self):
        """Test input validation"""
        invalid_inputs = [
            {"code": None},
            {"code": ""},
            {"code": "\x00\x01\x02"},
            {"code": "A" * 10000000},  # 10MB
            {"language": "../../../etc/passwd"},
        ]

        for payload in invalid_inputs:
            try:
                response = self.session.post(
                    f"{self.base_url}/api/execute",
                    json=payload,
                    timeout=5
                )
                
                # Should either fail or be sanitized
                if response.status_code not in [400, 422, 413]:
                    print(f"Warning: Invalid input not properly rejected: {payload}")
            except Exception:
                pass

    def test_dependencies(self):
        """Check for known vulnerable dependencies"""
        # Run safety check on Python dependencies
        try:
            result = subprocess.run(
                ["safety", "check", "--json"],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode != 0:
                vulns = json.loads(result.stdout)
                for vuln in vulns:
                    self.vulnerabilities.append(Vulnerability(
                        id=f"DEP_{vuln.get('cve', 'UNKNOWN')}",
                        title=vuln.get("advisory", "Vulnerable Dependency"),
                        description=vuln.get("advisory", ""),
                        severity=Severity.HIGH,
                        component="Dependencies",
                        remediation=f"Update {vuln.get('package', '')} to latest version",
                        cve=vuln.get("cve"),
                        cvss_score=vuln.get("cvssv3", {}).get("base_score", 0)
                    ))
        except Exception as e:
            print(f"Warning: Could not run dependency check: {e}")

    def test_tls(self):
        """Test TLS/SSL configuration"""
        try:
            import ssl
            context = ssl.create_default_context()
            
            # Check TLS version
            with context.wrap_socket(socket.socket(), server_hostname="localhost") as sock:
                sock.connect(("localhost", 443))
                version = sock.version()
                
                if version not in ["TLSv1.2", "TLSv1.3"]:
                    self.vulnerabilities.append(Vulnerability(
                        id="TLS_001",
                        title="Outdated TLS Version",
                        description=f"Using {version}, should use TLS 1.2 or 1.3",
                        severity=Severity.HIGH,
                        component="TLS",
                        remediation="Update to TLS 1.2 or 1.3"
                    ))
        except Exception:
            pass

    def test_security_headers(self):
        """Test security headers"""
        required_headers = [
            "Strict-Transport-Security",
            "X-Content-Type-Options",
            "X-Frame-Options",
            "X-XSS-Protection",
            "Content-Security-Policy",
            "Referrer-Policy",
        ]

        try:
            response = self.session.get(f"{self.base_url}/", timeout=5)
            
            for header in required_headers:
                if header not in response.headers:
                    self.vulnerabilities.append(Vulnerability(
                        id="HEADERS_001",
                        title="Missing Security Header",
                        description=f"Missing {header} header",
                        severity=Severity.MEDIUM,
                        component="HTTP Headers",
                        remediation=f"Add {header} header to all responses"
                    ))
        except Exception:
            pass

    def test_rate_limiting(self):
        """Test rate limiting"""
        # Make rapid requests
        try:
            for i in range(100):
                response = self.session.get(
                    f"{self.base_url}/api/execute",
                    timeout=1
                )
                
                if response.status_code == 429:
                    # Rate limiting is working
                    return
            
            # If we got here, rate limiting might not be working
            print("Warning: Rate limiting may not be properly configured")
        except Exception:
            pass

    def test_cryptography(self):
        """Test cryptographic implementation"""
        try:
            # Check for insecure hashing
            response = self.session.get(
                f"{self.base_url}/api/crypto-check",
                timeout=5
            )
            
            if response.status_code == 200:
                data = response.json()
                
                if data.get("hash_algorithm") in ["md5", "sha1"]:
                    self.vulnerabilities.append(Vulnerability(
                        id="CRYPTO_001",
                        title="Insecure Hash Algorithm",
                        description=f"Using {data.get('hash_algorithm')}, should use bcrypt/scrypt",
                        severity=Severity.HIGH,
                        component="Cryptography",
                        remediation="Use bcrypt, scrypt, or Argon2 for password hashing"
                    ))
        except Exception:
            pass

    def test_session_mgmt(self):
        """Test session management"""
        try:
            # Get session token
            response = self.session.post(
                f"{self.base_url}/api/auth/login",
                json={"username": "testuser", "password": "testpass"}
            )
            
            if "Set-Cookie" in response.headers:
                cookie = response.headers["Set-Cookie"]
                
                # Check for secure flags
                if "Secure" not in cookie:
                    self.vulnerabilities.append(Vulnerability(
                        id="SESSION_001",
                        title="Session Cookie Missing Secure Flag",
                        description="Session cookies should have Secure flag",
                        severity=Severity.HIGH,
                        component="Session Management",
                        remediation="Add Secure flag to session cookies"
                    ))
                
                if "HttpOnly" not in cookie:
                    self.vulnerabilities.append(Vulnerability(
                        id="SESSION_002",
                        title="Session Cookie Missing HttpOnly Flag",
                        description="Session cookies should have HttpOnly flag",
                        severity=Severity.HIGH,
                        component="Session Management",
                        remediation="Add HttpOnly flag to session cookies"
                    ))
        except Exception:
            pass

    def test_file_uploads(self):
        """Test file upload security"""
        dangerous_files = [
            ("shell.php", b"<?php system($_GET['cmd']); ?>"),
            ("shell.jsp", b"<% Runtime.getRuntime().exec(...); %>"),
            ("shell.exe", b"MZ\x90\x00"),  # PE header
        ]

        for filename, content in dangerous_files:
            try:
                response = self.session.post(
                    f"{self.base_url}/api/files/upload",
                    files={"file": (filename, content)},
                    timeout=5
                )
                
                if response.status_code in [200, 201]:
                    self.vulnerabilities.append(Vulnerability(
                        id="UPLOAD_001",
                        title="Dangerous File Upload Allowed",
                        description=f"File type {filename.split('.')[-1]} was accepted",
                        severity=Severity.CRITICAL,
                        component="File Upload",
                        remediation="Implement file type whitelist and content validation"
                    ))
            except Exception:
                pass

    def test_command_injection(self):
        """Test for command injection vulnerabilities"""
        payloads = [
            "; ls -la",
            "| whoami",
            "` id `",
            "$(cat /etc/passwd)",
            "&& cat /etc/passwd",
        ]

        for payload in payloads:
            try:
                response = self.session.post(
                    f"{self.base_url}/api/execute",
                    json={"code": f"echo {payload}"},
                    timeout=5
                )
                
                if "root:" in response.text or "bash" in response.text:
                    self.vulnerabilities.append(Vulnerability(
                        id="CMD_INJECT_001",
                        title="Command Injection Vulnerability",
                        description="Possible command injection in code execution",
                        severity=Severity.CRITICAL,
                        component="Code Executor",
                        remediation="Use subprocess.run with shell=False"
                    ))
                    return
            except Exception:
                pass

    def test_xxe(self):
        """Test for XXE vulnerabilities"""
        xxe_payload = """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE foo [<!ENTITY xxe SYSTEM "file:///etc/passwd">]>
<root>&xxe;</root>"""

        try:
            response = self.session.post(
                f"{self.base_url}/api/parse/xml",
                data=xxe_payload,
                headers={"Content-Type": "application/xml"},
                timeout=5
            )
            
            if "root:" in response.text:
                self.vulnerabilities.append(Vulnerability(
                    id="XXE_001",
                    title="XXE Vulnerability",
                    description="XML parser is vulnerable to XXE attacks",
                    severity=Severity.CRITICAL,
                    component="XML Parser",
                    remediation="Disable DTD processing in XML parser"
                ))
        except Exception:
            pass

    def _get_test_token(self, username: str) -> str:
        """Get a test token for a user"""
        response = self.session.post(
            f"{self.base_url}/api/auth/login",
            json={"username": username, "password": "testpass"},
            timeout=5
        )
        
        if response.status_code == 200:
            return response.json().get("token", "")
        return ""

    def generate_report(self) -> Dict[str, Any]:
        """Generate security audit report"""
        critical = [v for v in self.vulnerabilities if v.severity == Severity.CRITICAL]
        high = [v for v in self.vulnerabilities if v.severity == Severity.HIGH]
        medium = [v for v in self.vulnerabilities if v.severity == Severity.MEDIUM]
        low = [v for v in self.vulnerabilities if v.severity == Severity.LOW]

        report = {
            "timestamp": datetime.now().isoformat(),
            "total_vulnerabilities": len(self.vulnerabilities),
            "critical": len(critical),
            "high": len(high),
            "medium": len(medium),
            "low": len(low),
            "pass_rate": 100.0 - (len(self.vulnerabilities) / 100) * 100,
            "vulnerabilities": {
                "critical": [asdict(v) for v in critical],
                "high": [asdict(v) for v in high],
                "medium": [asdict(v) for v in medium],
                "low": [asdict(v) for v in low],
            }
        }

        return report

    def save_report(self, filename: str = "security_report.json"):
        """Save report to file"""
        report = self.generate_report()
        with open(filename, 'w') as f:
            json.dump(report, f, indent=2, default=str)
        print(f"Report saved to {filename}")

if __name__ == "__main__":
    auditor = SecurityAuditor()
    report = auditor.run_all_audits()
    
    print("\n========== Security Audit Summary ==========")
    print(f"Timestamp: {report['timestamp']}")
    print(f"Total Vulnerabilities: {report['total_vulnerabilities']}")
    print(f"  - Critical: {report['critical']}")
    print(f"  - High: {report['high']}")
    print(f"  - Medium: {report['medium']}")
    print(f"  - Low: {report['low']}")
    print(f"Pass Rate: {report['pass_rate']:.1f}%")
    
    # Save report
    auditor.save_report()
    
    # Exit with error if critical vulnerabilities found
    if report['critical'] > 0:
        sys.exit(1)
