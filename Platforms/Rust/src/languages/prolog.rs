use crate::core::interpreter::{Interpreter, ExecutionState};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
enum Term {
    Atom(String),
    Variable(String),
}

#[derive(Clone, Debug)]
struct Predicate {
    name: String,
    args: Vec<Term>,
}

#[derive(Clone, Debug)]
struct Rule {
    head: Predicate,
    body: Vec<Predicate>,
}

pub struct PrologInterpreter {
    rules: Vec<Rule>,
}

impl PrologInterpreter {
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
        }
    }

    fn parse_term(s: &str) -> Term {
        let s = s.trim();
        if s.chars().next().unwrap().is_uppercase() {
            Term::Variable(s.to_string())
        } else {
            Term::Atom(s.to_string())
        }
    }

    fn parse_predicate(s: &str) -> Option<Predicate> {
        let s = s.trim();
        if let Some(start) = s.find('(') {
            if let Some(end) = s.rfind(')') {
                let name = s[..start].trim().to_string();
                let args_str = &s[start+1..end];
                let args: Vec<Term> = args_str.split(',')
                    .map(|arg| Self::parse_term(arg))
                    .collect();
                return Some(Predicate { name, args });
            }
        }
        // Atom as predicate (e.g. "true")
        if !s.is_empty() {
             return Some(Predicate { name: s.to_string(), args: Vec::new() });
        }
        None
    }

    fn parse_rule(s: &str) -> Option<Rule> {
        let s = s.trim().trim_end_matches('.');
        if let Some(idx) = s.find(":-") {
            let head_str = &s[..idx];
            let body_str = &s[idx+2..];
            let head = Self::parse_predicate(head_str)?;
            
            // Split body by comma, but be careful about nested parens (not handling nested yet)
            let body: Vec<Predicate> = body_str.split("),")
                .map(|part| {
                    let p = if !part.ends_with(')') && part.contains('(') {
                        format!("{})", part) // Re-add closing paren if split removed it (hacky)
                    } else {
                        part.to_string()
                    };
                    // Actually, simple split by "), " is dangerous.
                    // Let's assume simple structure for now.
                    Self::parse_predicate(&p).unwrap()
                })
                .collect();
            
            Some(Rule { head, body })
        } else {
            // Fact
            let head = Self::parse_predicate(s)?;
            Some(Rule { head, body: Vec::new() })
        }
    }

    fn unify(&self, t1: &Term, t2: &Term, bindings: &mut HashMap<String, Term>) -> bool {
        let t1 = self.resolve(t1, bindings);
        let t2 = self.resolve(t2, bindings);

        match (t1, t2) {
            (Term::Atom(a), Term::Atom(b)) => a == b,
            (Term::Variable(v), t) => {
                bindings.insert(v, t);
                true
            },
            (t, Term::Variable(v)) => {
                bindings.insert(v, t);
                true
            }
        }
    }

    fn resolve(&self, term: &Term, bindings: &HashMap<String, Term>) -> Term {
        match term {
            Term::Variable(v) => {
                if let Some(val) = bindings.get(v) {
                    self.resolve(val, bindings)
                } else {
                    term.clone()
                }
            },
            _ => term.clone()
        }
    }

    fn solve(&self, goals: &[Predicate], bindings: HashMap<String, Term>) -> Vec<HashMap<String, Term>> {
        if goals.is_empty() {
            return vec![bindings];
        }

        let goal = &goals[0];
        let remaining_goals = &goals[1..];
        let mut solutions = Vec::new();

        for rule in &self.rules {
            if rule.head.name != goal.name || rule.head.args.len() != goal.args.len() {
                continue;
            }

            let mut new_bindings = bindings.clone();
            let mut unified = true;
            for (a1, a2) in goal.args.iter().zip(rule.head.args.iter()) {
                if !self.unify(a1, a2, &mut new_bindings) {
                    unified = false;
                    break;
                }
            }

            if unified {
                // Add rule body to goals
                let mut new_goals = rule.body.clone();
                new_goals.extend_from_slice(remaining_goals);
                
                let sub_solutions = self.solve(&new_goals, new_bindings);
                solutions.extend(sub_solutions);
            }
        }

        solutions
    }

    fn process_code(&mut self, code: &str) -> String {
        let mut output = String::new();
        
        for line in code.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() { continue; }
            
            if trimmed.starts_with("?-") {
                // Query
                let query_str = trimmed[2..].trim().trim_end_matches('.');
                if let Some(goal) = Self::parse_predicate(query_str) {
                    // Collect variables from goal
                    let query_vars: Vec<String> = goal.args.iter().filter_map(|t| {
                        if let Term::Variable(v) = t { Some(v.clone()) } else { None }
                    }).collect();

                    let solutions = self.solve(&[goal], HashMap::new());
                    if solutions.is_empty() {
                        output.push_str("false.\n");
                    } else {
                        for sol in solutions {
                            let mut parts = Vec::new();
                            for (k, v) in sol {
                                if query_vars.contains(&k) {
                                    if let Term::Atom(s) = v {
                                        parts.push(format!("{} = {}", k, s));
                                    }
                                }
                            }
                            if parts.is_empty() {
                                output.push_str("true.\n");
                            } else {
                                parts.sort(); // Deterministic output
                                output.push_str(&parts.join(", "));
                                output.push_str(".\n");
                            }
                        }
                    }
                } else {
                    output.push_str("Error: Invalid query\n");
                }
            } else {
                // Rule or Fact
                if let Some(rule) = Self::parse_rule(trimmed) {
                    self.rules.push(rule);
                }
            }
        }
        output
    }
}

impl Interpreter for PrologInterpreter {
    fn execute(&mut self, command: &str) -> String {
        self.process_code(command)
    }

    fn run(&mut self, code: &str) -> String {
        self.rules.clear();
        self.process_code(code)
    }

    fn start_execution(&mut self, code: &str) -> (String, ExecutionState) {
        self.rules.clear();
        let output = self.process_code(code);
        (output, ExecutionState::Finished)
    }

    fn continue_execution(&mut self) -> (String, ExecutionState) {
        (String::new(), ExecutionState::Finished)
    }

    fn get_variables(&self) -> HashMap<String, String> {
        let mut vars = HashMap::new();
        vars.insert("RULES_COUNT".to_string(), self.rules.len().to_string());
        vars
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prolog_fact() {
        let mut interpreter = PrologInterpreter::new();
        interpreter.execute("parent(john, mary).");
        let output = interpreter.execute("?- parent(john, mary).");
        assert!(output.contains("true"));
        
        let output2 = interpreter.execute("?- parent(john, bob).");
        assert!(output2.contains("false"));
    }

    #[test]
    fn test_prolog_variable() {
        let mut interpreter = PrologInterpreter::new();
        interpreter.execute("parent(john, mary).");
        let output = interpreter.execute("?- parent(john, X).");
        assert!(output.contains("X = mary"));
    }

    #[test]
    fn test_prolog_rule() {
        let mut interpreter = PrologInterpreter::new();
        interpreter.execute("parent(john, mary).");
        interpreter.execute("parent(mary, ann).");
        // grandparent(X,Y) :- parent(X,Z),parent(Z,Y).
        interpreter.execute("grandparent(X,Y) :- parent(X,Z),parent(Z,Y).");
        
        let output = interpreter.execute("?- grandparent(john, ann).");
        assert!(output.contains("true"));
    }
}
