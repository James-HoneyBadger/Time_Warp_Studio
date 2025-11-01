use once_cell::sync::Lazy;
use std::collections::HashMap;

/// Common command typos and their corrections
static TYPO_SUGGESTIONS: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    let mut m = HashMap::new();
    // BASIC typos
    m.insert("PRITN", "PRINT");
    m.insert("PIRNT", "PRINT");
    m.insert("PRNIT", "PRINT");
    m.insert("IMPUT", "INPUT");
    m.insert("INOUT", "INPUT");
    m.insert("GОТО", "GOTO"); // Cyrillic O
    m.insert("GTOO", "GOTO");
    m.insert("GOТО", "GOTO");
    m.insert("PSINT", "PRINT");
    m.insert("PRNT", "PRINT");
    m.insert("INOPUT", "INPUT");
    m.insert("INPUTT", "INPUT");

    // Logo typos
    m.insert("FORWAD", "FORWARD");
    m.insert("FORWAR", "FORWARD");
    m.insert("FOWARD", "FORWARD");
    m.insert("FORWRD", "FORWARD");
    m.insert("BAKC", "BACK");
    m.insert("BSCK", "BACK");
    m.insert("RIHGT", "RIGHT");
    m.insert("RITGH", "RIGHT");
    m.insert("RIGTH", "RIGHT");
    m.insert("LAFT", "LEFT");
    m.insert("LEFFT", "LEFT");
    m.insert("PENUP", "PENUP");
    m.insert("PENDWON", "PENDOWN");
    m.insert("PENDONW", "PENDOWN");
    m.insert("PNDOWN", "PENDOWN");
    m.insert("REPAAT", "REPEAT");
    m.insert("REPET", "REPEAT");
    m.insert("REPAET", "REPEAT");

    // PILOT typos
    m.insert("TXT:", "T:");
    m.insert("TEXT:", "T:");
    m.insert("ACCEPT:", "A:");
    m.insert("ACC:", "A:");
    m.insert("JUMP:", "J:");
    m.insert("JMP:", "J:");
    m.insert("MATCH:", "M:");

    m
});

/// Suggest correction for unknown commands
pub fn suggest_command(cmd: &str) -> Option<String> {
    let cmd_upper = cmd.trim().to_uppercase();

    // Check exact match in typo table
    if let Some(&correction) = TYPO_SUGGESTIONS.get(cmd_upper.as_str()) {
        return Some(format!("Did you mean '{}'?", correction));
    }

    // Check for similar-looking commands (Levenshtein distance ≤ 2)
    let common_commands = [
        "PRINT",
        "INPUT",
        "GOTO",
        "IF",
        "THEN",
        "FOR",
        "NEXT",
        "END",
        "FORWARD",
        "BACK",
        "LEFT",
        "RIGHT",
        "PENUP",
        "PENDOWN",
        "HOME",
        "REPEAT",
        "TO",
        "CLEARSCREEN",
    ];

    for &correct_cmd in &common_commands {
        if levenshtein_distance(&cmd_upper, correct_cmd) <= 2 {
            return Some(format!("Did you mean '{}'?", correct_cmd));
        }
    }

    None
}

/// Check for common syntax mistakes
pub fn check_syntax_mistakes(line: &str) -> Vec<String> {
    let mut suggestions = Vec::new();

    // Check for unclosed quotes
    let quote_count = line.chars().filter(|&c| c == '"').count();
    if quote_count % 2 != 0 {
        suggestions.push("Unclosed string quote (\")".to_string());
    }

    // Check for unmatched parentheses
    let open_paren = line.chars().filter(|&c| c == '(').count();
    let close_paren = line.chars().filter(|&c| c == ')').count();
    if open_paren > close_paren {
        suggestions.push(format!(
            "Missing {} closing parenthesis",
            open_paren - close_paren
        ));
    } else if close_paren > open_paren {
        suggestions.push(format!(
            "Missing {} opening parenthesis",
            close_paren - open_paren
        ));
    }

    // Check for missing variable in LET statement
    if line.trim().to_uppercase().starts_with("LET") && !line.contains('=') {
        suggestions.push("LET statement requires '=' (e.g., LET X = 10)".to_string());
    }

    // Check for missing THEN in IF statement
    let line_upper = line.trim().to_uppercase();
    if line_upper.starts_with("IF") && !line_upper.contains("THEN") {
        suggestions
            .push("IF statement requires THEN (e.g., IF X > 10 THEN PRINT \"Hi\")".to_string());
    }

    // Check for PRINT without quotes for strings
    if line_upper.starts_with("PRINT") && !line.contains('"') && !line.contains(',') {
        let after_print = line[5..].trim();
        if !after_print.is_empty()
            && !after_print.chars().next().unwrap().is_ascii_digit()
            && !after_print.contains('+')
            && !after_print.contains('-')
            && !after_print.contains('*')
        {
            suggestions.push(
                "String literals in PRINT should be quoted (e.g., PRINT \"Hello\")".to_string(),
            );
        }
    }

    suggestions
}

/// Compute Levenshtein distance between two strings
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for (i, row) in matrix.iter_mut().enumerate().take(len1 + 1) {
        row[0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let cost = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = *[
                matrix[i][j + 1] + 1, // deletion
                matrix[i + 1][j] + 1, // insertion
                matrix[i][j] + cost,  // substitution
            ]
            .iter()
            .min()
            .unwrap();
        }
    }

    matrix[len1][len2]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typo_suggestions() {
        assert_eq!(
            suggest_command("PRITN"),
            Some("Did you mean 'PRINT'?".to_string())
        );
        assert_eq!(
            suggest_command("FORWAD"),
            Some("Did you mean 'FORWARD'?".to_string())
        );
        assert_eq!(
            suggest_command("IMPUT"),
            Some("Did you mean 'INPUT'?".to_string())
        );
    }

    #[test]
    fn test_levenshtein() {
        assert_eq!(levenshtein_distance("PRITN", "PRINT"), 2);
        assert_eq!(levenshtein_distance("FORWAD", "FORWARD"), 1);
        assert_eq!(levenshtein_distance("HELLO", "WORLD"), 4);
    }

    #[test]
    fn test_syntax_mistakes() {
        let errs = check_syntax_mistakes("PRINT \"Hello");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].contains("quote"));

        let errs = check_syntax_mistakes("LET X");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].contains("="));

        let errs = check_syntax_mistakes("IF X > 10");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].contains("THEN"));
    }
}
