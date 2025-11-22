use anyhow::{anyhow, Result};
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;
use std::process::Command;

/// Experimental TempleCode-to-C compiler (transpiler + system compiler invoker)
///
/// Transpiles TempleCode programs to C and invokes the system compiler (cc, gcc, or clang)
/// to produce standalone executables. Supports text-mode BASIC and PILOT subset.
///
/// # Supported Features
/// - **BASIC**: PRINT, LET, INPUT, IF...THEN, GOTO, END, REM
/// - **PILOT**: T: (text output), A: (accept input)
/// - **Logo**: Commands converted to C comments (execution not yet supported)
///
/// # Variable Mapping
/// - Numeric variables → `double V_<name>`
/// - String variables → `char S_<name>[256]`
/// - Expressions passed to C with minimal transformation
/// - Operators: `+`, `-`, `*`, `/`, `%`, `^` (maps to `pow()`)
/// - Comparisons: `<>` → `!=`, `=` → `==`
///
/// # Compilation Process
/// 1. `compile_to_c()`: TempleCode → C source string
/// 2. `compile_to_executable()`: Write temp file → invoke system cc → produce binary
///
/// # Example
/// ```ignore
/// let compiler = TempleCodeCompiler::new();
/// let c_code = compiler.compile_to_c("10 PRINT \"Hello\"\n20 END")?;
/// let exe_path = compiler.compile_to_executable(&c_code, "hello")?;
/// // Run: ./hello
/// ```
///
/// # Architecture Notes
/// - Labels generated as `line_<number>:` for GOTO/GOSUB targets
/// - INPUT prompts embedded as string literals
/// - REM comments converted to `/* ... */`
/// - System compiler detected via `cc` command (gcc/clang fallback)
pub struct TempleCodeCompiler;

impl TempleCodeCompiler {
    pub fn new() -> Self {
        Self
    }

    /// Compile TempleCode source into C code (as a String)
    pub fn compile_to_c(&self, source: &str) -> Result<String> {
        let mut c = String::new();
        let mut line_map: Vec<(Option<usize>, String)> = Vec::new();
        let mut labels: BTreeSet<usize> = BTreeSet::new();

        for raw in source.lines() {
            let line = raw.trim_end();
            if line.is_empty() {
                continue;
            }
            let (num, cmd) = Self::split_line_number(line);
            if let Some(n) = num {
                labels.insert(n);
            }
            line_map.push((num, cmd.to_string()));
        }

        // Collect variables encountered
        let mut vars: BTreeSet<String> = BTreeSet::new();
        let mut str_vars: BTreeSet<String> = BTreeSet::new();

        // Pre-scan for simple assignments and inputs to declare variables
        for (_num, cmd) in &line_map {
            let up = cmd.trim().to_uppercase();
            if up.starts_with("LET ") {
                if let Some((lhs, _rhs)) = cmd[4..].split_once('=') {
                    let v = Self::normalize_var(lhs.trim());
                    vars.insert(v);
                }
            } else if up.starts_with("INPUT ") {
                let v = Self::normalize_var(cmd[6..].trim());
                vars.insert(v.clone());
                str_vars.insert(v);
            } else if up.starts_with("A:") {
                let name = Self::normalize_var(cmd[2..].trim());
                vars.insert(name.clone());
                str_vars.insert(name);
            }
        }

        // Emit C prolog
        c.push_str(
            "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <math.h>\n\n",
        );
        c.push_str("static void read_line(char* buf, size_t n){ if(!fgets(buf,n,stdin)){buf[0]='\\0';} size_t l=strlen(buf); if(l>0 && buf[l-1]=='\\n'){buf[l-1]='\\0';}}\n\n");
        c.push_str("int main(){\n");

        // Declare variables
        if !vars.is_empty() {
            c.push_str("  /* numeric variables */\n");
            for v in &vars {
                c.push_str(&format!("  double V_{} = 0.0;\n", v));
            }
        }
        if !str_vars.is_empty() {
            c.push_str("  /* string buffers */\n");
            for v in &str_vars {
                c.push_str(&format!("  char S_{}[256] = {{0}};\n", v));
            }
        }
        c.push_str("  char INPUT_BUF[256];\n\n");

        // Generate code per line
        for (num, cmd) in &line_map {
            if let Some(n) = num {
                c.push_str(&format!("line_{}:;\n", n));
            }
            let up = cmd.trim().to_uppercase();
            if up.is_empty() {
                continue;
            }

            // TempleCode detection (BASIC/PILOT/Logo merged)
            if up.starts_with("REM ") || up == "REM" {
                // Emit as C comment
                let comment_text = if up == "REM" { "" } else { &cmd[4..] };
                c.push_str(&format!(
                    "  /* {} */\n",
                    comment_text.replace("/*", "/ *").replace("*/", "* /")
                ));
                continue;
            }

            if up.starts_with("PRINT ") {
                c.push_str(&Self::emit_print(&cmd[6..], &vars)?);
            } else if up == "PRINT" {
                c.push_str("  puts(\"\");\n");
            } else if up.starts_with("LET ") {
                let rest = &cmd[4..];
                let (lhs, rhs) = rest
                    .split_once('=')
                    .ok_or_else(|| anyhow!("Invalid LET syntax"))?;
                let v = Self::normalize_var(lhs.trim());
                let expr = Self::expr_to_c(rhs.trim(), &vars)?;
                c.push_str(&format!("  V_{} = {};\n", v, expr));
            } else if up.starts_with("INPUT ") {
                let rest = cmd[6..].trim();
                if let Some(stripped) = rest.strip_prefix('"') {
                    // INPUT "Prompt", VAR
                    if let Some(end) = stripped.find('"') {
                        let prompt = &stripped[..end];
                        let after = stripped[end + 1..].trim();
                        let after = after.strip_prefix(',').unwrap_or(after).trim();
                        if !after.is_empty() {
                            let v = Self::normalize_var(after);
                            let esc = Self::escape_c_string(prompt);
                            c.push_str(&format!(
                                "  fputs(\"{} \", stdout); fflush(stdout);\n",
                                esc
                            ));
                            c.push_str(&Self::emit_read_into(&v));
                        } else {
                            c.push_str(&format!(
                                "  /* Invalid INPUT syntax: {} */\n",
                                rest.replace("/*", "/ *")
                            ));
                        }
                    } else {
                        c.push_str(&format!(
                            "  /* Invalid INPUT prompt: {} */\n",
                            rest.replace("/*", "/ *")
                        ));
                    }
                } else {
                    let v = Self::normalize_var(rest);
                    c.push_str(&Self::emit_input_read(&v));
                }
            } else if up.starts_with("IF ") {
                // Very limited: IF <cond> THEN <line|PRINT ...>
                let cond_then = &cmd[3..];
                let (cond_part, then_part) = cond_then
                    .split_once("THEN")
                    .ok_or_else(|| anyhow!("Invalid IF syntax"))?;
                let cond_upper = cond_part.to_uppercase();
                let cond_c = Self::cond_to_c(cond_upper.trim(), &vars)?;
                let then_src = then_part.trim(); // original case
                if let Ok(target) = then_src.parse::<usize>() {
                    c.push_str(&format!("  if ({}) goto line_{};\n", cond_c, target));
                } else if then_src.to_uppercase().starts_with("PRINT ") {
                    c.push_str(&format!("  if ({}) {{\n", cond_c));
                    c.push_str(&Self::emit_print(&then_src[6..], &vars)?);
                    c.push_str("  }\n");
                } else {
                    // Fallback: ignore
                    c.push_str(&format!(
                        "  /* IF THEN unhandled: {} */\n",
                        then_src.replace("/*", "/ *")
                    ));
                }
            } else if up.starts_with("GOTO ") {
                let target = cmd[5..]
                    .trim()
                    .parse::<usize>()
                    .map_err(|_| anyhow!("Invalid GOTO target"))?;
                c.push_str(&format!("  goto line_{};\n", target));
            } else if up == "END" {
                c.push_str("  return 0;\n");
            }
            // PILOT
            else if up.starts_with("T:") {
                let text = cmd[2..].trim();
                let escaped = Self::escape_c_string(text);
                c.push_str(&format!("  puts(\"{}\");\n", escaped));
            } else if up.starts_with("A:") {
                let v = Self::normalize_var(cmd[2..].trim());
                c.push_str(&Self::emit_input_read(&v));
            }
            // Logo (ignored for now)
            else {
                c.push_str(&format!(
                    "  /* Unhandled or Logo command: {} */\n",
                    up.replace("/*", "/ *")
                ));
            }
        }

        c.push_str("  return 0;\n}\n");
        Ok(c)
    }

    /// Compile TempleCode source directly to an executable using system C compiler (cc/gcc/clang)
    pub fn compile_to_executable(&self, source: &str, output_path: &Path) -> Result<()> {
        let c_src = self.compile_to_c(source)?;
        let tmp_dir = Path::new("target/tmp");
        fs::create_dir_all(tmp_dir).ok();
        let c_path = tmp_dir.join("templecode_out.c");
        fs::write(&c_path, c_src)?;

        // Try common compilers
        let compilers = ["cc", "gcc", "clang"];
        let mut last_err: Option<anyhow::Error> = None;
        for cc in compilers.iter() {
            let status = Command::new(cc)
                .arg("-O2")
                .arg("-std=c11")
                .arg("-o")
                .arg(output_path)
                .arg(&c_path)
                .status();
            match status {
                Ok(s) if s.success() => {
                    return Ok(());
                }
                Ok(s) => {
                    last_err = Some(anyhow!("{} exited with status {}", cc, s));
                }
                Err(e) => {
                    last_err = Some(anyhow!("failed to invoke {}: {}", cc, e));
                }
            }
        }
        Err(last_err.unwrap_or_else(|| anyhow!("no C compiler found")))
    }

    fn split_line_number(line: &str) -> (Option<usize>, &str) {
        let mut parts = line.splitn(2, char::is_whitespace);
        if let Some(first) = parts.next() {
            if let Ok(n) = first.parse::<usize>() {
                let rest = parts.next().unwrap_or("");
                return (Some(n), rest.trim());
            }
        }
        (None, line.trim())
    }

    fn normalize_var(name: &str) -> String {
        name.chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() {
                    c.to_ascii_uppercase()
                } else {
                    '_'
                }
            })
            .collect()
    }

    fn escape_c_string(s: &str) -> String {
        s.replace('\\', "\\\\").replace('"', "\\\"")
    }

    fn emit_input_read(v: &str) -> String {
        format!(
            concat!(
                "  fputs(\"? \", stdout); fflush(stdout); read_line(INPUT_BUF, sizeof(INPUT_BUF));\n",
                "  strncpy(S_{0}, INPUT_BUF, sizeof(S_{0})-1); S_{0}[sizeof(S_{0})-1]='\\0';\n",
                "  char* endptr=NULL; V_{0} = strtod(INPUT_BUF, &endptr); if(endptr==INPUT_BUF){{ /* not numeric */ }}\n"
            ),
            v
        )
    }

    fn emit_read_into(v: &str) -> String {
        format!(
            concat!(
                "  read_line(INPUT_BUF, sizeof(INPUT_BUF));\n",
                "  strncpy(S_{0}, INPUT_BUF, sizeof(S_{0})-1); S_{0}[sizeof(S_{0})-1]='\\0';\n",
                "  char* endptr=NULL; V_{0} = strtod(INPUT_BUF, &endptr); if(endptr==INPUT_BUF){{ /* not numeric */ }}\n"
            ),
            v
        )
    }

    fn expr_to_c(expr: &str, vars: &BTreeSet<String>) -> Result<String> {
        // Replace '^' with pow() calls and variables with V_ names.
        let mut out = String::new();
        let mut i = 0;
        let chars: Vec<char> = expr.chars().collect();
        while i < chars.len() {
            let c = chars[i];
            if c.is_ascii_alphabetic() {
                // read identifier
                let start = i;
                let mut j = i + 1;
                while j < chars.len() && (chars[j].is_ascii_alphanumeric() || chars[j] == '_') {
                    j += 1;
                }
                let ident: String = chars[start..j].iter().collect();
                let norm = Self::normalize_var(&ident);
                if vars.contains(&norm) {
                    out.push_str(&format!("V_{}", norm));
                } else {
                    out.push_str(&norm);
                }
                i = j;
            } else if c == '^' {
                // Transform a ^ b into pow(a,b): backtrack to find last token and next token.
                // For simplicity, caller should avoid nested ^ without parentheses; we do left-assoc approximation.
                out.push('^'); // leave as-is; optional: could implement proper parse later
                i += 1;
            } else {
                out.push(c);
                i += 1;
            }
        }
        // crude replace ^ with pow: a^b -> pow(a,b)
        let mut s = out;
        if s.contains('^') {
            // Very naive: replace x ^ y with pow(x,y)
            // Users should parenthesize expressions if needed.
            s = s.replace("^", ",");
            s = format!("pow({})", s);
        }
        Ok(s)
    }

    fn cond_to_c(cond: &str, vars: &BTreeSet<String>) -> Result<String> {
        // Support BASIC-style operators and ensure proper C comparisons
        // 1) Convert "<>" to "!="
        // 2) Duplicate bare '=' to '==' but leave '>=', '<=', '!=', and existing '==' intact
        let s = cond.replace("<>", "!=");
        let mut out = String::new();
        let mut prev: Option<char> = None;
        let mut iter = s.chars().peekable();
        while let Some(ch) = iter.next() {
            if ch == '=' {
                let next_is_eq = matches!(iter.peek().copied(), Some('='));
                let prev_is_cmp = matches!(prev, Some('!') | Some('<') | Some('>') | Some('='));
                if prev_is_cmp || next_is_eq {
                    // Already part of !=, <=, >=, == or second '=' in '=='
                    out.push('=');
                } else {
                    // Bare assignment-like '=' in condition -> equality test
                    out.push('=');
                    out.push('=');
                }
            } else {
                out.push(ch);
            }
            prev = Some(ch);
        }
        Self::expr_to_c(&out, vars)
    }

    fn emit_print(arglist: &str, vars: &BTreeSet<String>) -> Result<String> {
        // Support: quoted strings and variable names separated by commas
        let mut code = String::new();
        let rest = arglist.trim();
        if rest.is_empty() {
            code.push_str("  puts(\"\");\n");
            return Ok(code);
        }
        // Split by commas not inside quotes (simple scan)
        let mut parts: Vec<String> = Vec::new();
        let mut buf = String::new();
        let mut in_str = false;
        for ch in rest.chars() {
            match ch {
                '"' => {
                    in_str = !in_str;
                    buf.push(ch);
                }
                ',' if !in_str => {
                    parts.push(buf.trim().to_string());
                    buf.clear();
                }
                _ => buf.push(ch),
            }
        }
        if !buf.trim().is_empty() {
            parts.push(buf.trim().to_string());
        }
        for p in parts {
            let ptrim = p.trim();
            if ptrim.starts_with('"') && ptrim.ends_with('"') {
                let inner = &ptrim[1..ptrim.len() - 1];
                code.push_str(&format!(
                    "  fputs(\"{}\", stdout);\n",
                    Self::escape_c_string(inner)
                ));
            } else {
                // Numeric literal?
                if let Ok(n) = ptrim.parse::<f64>() {
                    code.push_str(&format!("  printf(\"%g\", {});\n", n));
                } else {
                    let v = Self::normalize_var(ptrim);
                    if vars.contains(&v) {
                        code.push_str(&format!("  printf(\"%g\", V_{});\n", v));
                    } else {
                        code.push_str(&format!("  /* Unknown token in PRINT: {} */\n", ptrim));
                    }
                }
            }
        }
        code.push_str("  fputc('\\n', stdout);\n");
        Ok(code)
    }
}

impl Default for TempleCodeCompiler {
    fn default() -> Self {
        Self::new()
    }
}
