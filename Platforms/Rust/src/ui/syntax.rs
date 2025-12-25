use eframe::egui::{text::LayoutJob, Color32, FontId, TextFormat};

pub fn highlight(theme: &eframe::egui::Style, code: &str, language: &str) -> LayoutJob {
    let mut job = LayoutJob::default();
    let is_dark = theme.visuals.dark_mode;
    
    // Colors
    let color_keyword = if is_dark { Color32::from_rgb(255, 121, 198) } else { Color32::from_rgb(200, 0, 200) }; // Pink/Purple
    let color_string = if is_dark { Color32::from_rgb(241, 250, 140) } else { Color32::from_rgb(150, 150, 0) }; // Yellow/Dark Yellow
    let color_number = if is_dark { Color32::from_rgb(189, 147, 249) } else { Color32::from_rgb(100, 0, 200) }; // Purple/Dark Purple
    let color_text = if is_dark { Color32::from_rgb(248, 248, 242) } else { Color32::BLACK }; // White/Black
    
    // Simple keyword list based on language
    let keywords = match language {
        "BASIC" => vec!["PRINT", "IF", "THEN", "GOTO", "INPUT", "LET", "END", "FOR", "NEXT", "TO", "STEP"],
        "PILOT" => vec!["T:", "A:", "M:", "J:", "U:", "E:", "R:", "TY:", "TN:"],
        "Logo" => vec!["FORWARD", "FD", "BACK", "BK", "RIGHT", "RT", "LEFT", "LT", "PENUP", "PU", "PENDOWN", "PD", "CLEAR", "CS", "HOME", "PRINT", "PR", "REPEAT"],
        "Pascal" => vec!["PROGRAM", "BEGIN", "END", "VAR", "INTEGER", "REAL", "STRING", "IF", "THEN", "ELSE", "WHILE", "DO", "FOR", "TO", "WRITELN", "WRITE", "READLN"],
        "Prolog" => vec!["write", "nl", "assert", "retract"], 
        "Forth" => vec!["DUP", "DROP", "SWAP", "OVER", "ROT", ".", "CR", ":", ";", "IF", "THEN", "ELSE", "DO", "LOOP"],
        "C" => vec!["int", "float", "char", "void", "return", "if", "else", "while", "for", "do", "switch", "case", "break", "continue", "#include"],
        _ => vec![],
    };

    let font_id = FontId::monospace(14.0);
    
    let mut current_word = String::new();
    let mut in_string = false;
    
    // Very naive tokenizer
    for c in code.chars() {
        if in_string {
            job.append(&c.to_string(), 0.0, TextFormat {
                font_id: font_id.clone(),
                color: color_string,
                ..Default::default()
            });
            if c == '"' {
                in_string = false;
            }
            continue;
        }

        if c == '"' {
            if !current_word.is_empty() {
                let color = if keywords.contains(&current_word.as_str()) || keywords.contains(&current_word.to_uppercase().as_str()) {
                    color_keyword
                } else if current_word.chars().all(|c| c.is_numeric()) {
                    color_number
                } else {
                    color_text
                };
                
                job.append(&current_word, 0.0, TextFormat {
                    font_id: font_id.clone(),
                    color,
                    ..Default::default()
                });
                current_word.clear();
            }
            
            in_string = true;
            job.append(&c.to_string(), 0.0, TextFormat {
                font_id: font_id.clone(),
                color: color_string,
                ..Default::default()
            });
            continue;
        }

        if c.is_alphanumeric() || c == ':' || c == '#' || c == '_' {
            current_word.push(c);
        } else {
            if !current_word.is_empty() {
                let color = if keywords.contains(&current_word.as_str()) || keywords.contains(&current_word.to_uppercase().as_str()) {
                    color_keyword
                } else if current_word.chars().all(|c| c.is_numeric()) {
                    color_number
                } else {
                    color_text
                };
                
                job.append(&current_word, 0.0, TextFormat {
                    font_id: font_id.clone(),
                    color,
                    ..Default::default()
                });
                current_word.clear();
            }
            
            job.append(&c.to_string(), 0.0, TextFormat {
                font_id: font_id.clone(),
                color: color_text,
                ..Default::default()
            });
        }
    }
    
    if !current_word.is_empty() {
         let color = if keywords.contains(&current_word.as_str()) || keywords.contains(&current_word.to_uppercase().as_str()) {
            color_keyword
        } else if current_word.chars().all(|c| c.is_numeric()) {
            color_number
        } else {
            color_text
        };
        
        job.append(&current_word, 0.0, TextFormat {
            font_id: font_id.clone(),
            color,
            ..Default::default()
        });
    }

    job
}
