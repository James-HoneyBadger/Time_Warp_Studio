use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use evalexpr::{
    self, eval_with_context, ContextWithMutableFunctions, ContextWithMutableVariables, Function,
    HashMapContext,
};
use image::{Rgb, RgbImage};
#[cfg(feature = "mysql")]
use mysql::{prelude::Queryable, Pool};
use rand::Rng;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Str(String),
    Bool(bool),
    #[allow(dead_code)]
    Array(Vec<Value>),
    #[allow(dead_code)]
    Map(HashMap<String, Value>),
    Null,
}

impl Value {
    pub fn to_f64(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            Value::Bool(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
            _ => 0.0,
        }
    }
    pub fn to_string_lossy(&self) -> String {
        match self {
            Value::Number(n) => {
                if n.fract() == 0.0 {
                    format!("{}", *n as i64)
                } else {
                    format!("{}", n)
                }
            }
            Value::Bool(b) => b.to_string(),
            Value::Str(s) => s.clone(),
            Value::Array(v) => {
                let inner: Vec<String> = v.iter().map(|x| x.to_string_lossy()).collect();
                format!("[{}]", inner.join(", "))
            }
            Value::Map(m) => {
                let mut parts: Vec<String> = Vec::new();
                for (k, v) in m.iter() {
                    parts.push(format!("\"{}\": {}", k, v.to_string_lossy()));
                }
                format!("{{{}}}", parts.join(", "))
            }
            Value::Null => "null".into(),
        }
    }
}

pub trait Console: Send + Sync {
    fn print(&self, s: &str);
}

#[derive(Default)]
pub struct BufferConsole {
    pub buf: Arc<Mutex<String>>,
}
impl BufferConsole {
    pub fn new(buf: Arc<Mutex<String>>) -> Self {
        Self { buf }
    }
}
impl Console for BufferConsole {
    fn print(&self, s: &str) {
        if let Ok(mut b) = self.buf.lock() {
            b.push_str(s);
            if !s.ends_with('\n') {
                b.push('\n');
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Color(pub u8, pub u8, pub u8);

#[derive(Clone, Debug, Default)]
pub struct LineSeg {
    pub x1: f32,
    pub y1: f32,
    pub x2: f32,
    pub y2: f32,
    pub color: Color,
}

#[derive(Default, Clone)]
pub struct TurtleModel {
    pub pen_down: bool,
    pub heading_deg: f32,
    pub x: f32,
    pub y: f32,
    pub color: Color,
    pub lines: Vec<LineSeg>,
    pub width: f32,
    pub height: f32,
}

impl TurtleModel {
    pub fn new(w: f32, h: f32) -> Self {
        Self {
            pen_down: true,
            heading_deg: 0.0,
            x: w / 2.0,
            y: h / 2.0,
            color: Color(255, 255, 255),
            lines: Vec::new(),
            width: w,
            height: h,
        }
    }
    pub fn clear(&mut self) {
        self.lines.clear();
    }
}

pub trait Turtle: Send + Sync {
    fn clear(&self);
    fn pen_up(&self);
    fn pen_down(&self);
    fn left(&self, deg: f32);
    fn right(&self, deg: f32);
    fn forward(&self, dist: f32);
    fn set_xy(&self, x: f32, y: f32);
    fn color(&self, r: u8, g: u8, b: u8);
    fn snapshot(&self) -> TurtleModel;
}

#[derive(Clone)]
pub struct SharedTurtle(pub Arc<Mutex<TurtleModel>>);
impl SharedTurtle {
    pub fn new(model: Arc<Mutex<TurtleModel>>) -> Self {
        Self(model)
    }
}
impl Turtle for SharedTurtle {
    fn clear(&self) {
        if let Ok(mut m) = self.0.lock() {
            m.clear();
        }
    }
    fn pen_up(&self) {
        if let Ok(mut m) = self.0.lock() {
            m.pen_down = false;
        }
    }
    fn pen_down(&self) {
        if let Ok(mut m) = self.0.lock() {
            m.pen_down = true;
        }
    }
    fn left(&self, deg: f32) {
        if let Ok(mut m) = self.0.lock() {
            m.heading_deg += deg;
        }
    }
    fn right(&self, deg: f32) {
        if let Ok(mut m) = self.0.lock() {
            m.heading_deg -= deg;
        }
    }
    fn forward(&self, dist: f32) {
        if let Ok(mut m) = self.0.lock() {
            let rad = m.heading_deg.to_radians();
            let nx = m.x + dist * rad.cos();
            let ny = m.y + dist * rad.sin();
            if m.pen_down {
                let (x1, y1, color) = (m.x, m.y, m.color);
                m.lines.push(LineSeg {
                    x1,
                    y1,
                    x2: nx,
                    y2: ny,
                    color,
                });
            }
            m.x = nx;
            m.y = ny;
        }
    }
    fn set_xy(&self, x: f32, y: f32) {
        if let Ok(mut m) = self.0.lock() {
            if m.pen_down {
                let (x1, y1, color) = (m.x, m.y, m.color);
                m.lines.push(LineSeg {
                    x1,
                    y1,
                    x2: x,
                    y2: y,
                    color,
                });
            }
            m.x = x;
            m.y = y;
        }
    }
    fn color(&self, r: u8, g: u8, b: u8) {
        if let Ok(mut m) = self.0.lock() {
            m.color = Color(r, g, b);
        }
    }
    fn snapshot(&self) -> TurtleModel {
        if let Ok(m) = self.0.lock() {
            m.clone()
        } else {
            TurtleModel::new(500.0, 400.0)
        }
    }
}

pub struct Interpreter<C: Console, T: Turtle> {
    pub console: C,
    pub turtle: T,
    pub vars: HashMap<String, crate::engine::Value>,
    pub trace: bool,
    #[cfg(feature = "mysql")]
    pub db: Option<Pool>,
}

impl<C: Console, T: Turtle> Interpreter<C, T> {
    pub fn new(console: C, turtle: T) -> Self {
        let mut vars = HashMap::new();
        vars.insert("pi".into(), Value::Number(std::f64::consts::PI));
        Self {
            console,
            turtle,
            vars,
            trace: false,
            #[cfg(feature = "mysql")]
            db: None,
        }
    }

    fn eval(&mut self, expr: &str) -> Result<Value, String> {
        // Build context from vars and custom functions
        let mut ctx = HashMapContext::new();
        for (k, v) in self.vars.clone() {
            match v {
                Value::Number(n) => {
                    let _ = ctx.set_value(k, evalexpr::Value::Float(n));
                }
                Value::Str(s) => {
                    let _ = ctx.set_value(k, evalexpr::Value::String(s));
                }
                Value::Bool(b) => {
                    let _ = ctx.set_value(k, evalexpr::Value::Boolean(b));
                }
                _ => {} // complex types not in evalexpr context
            }
        }
        // Register functions
        ctx.set_function(
            "sin".into(),
            Function::new(|arg| Ok(evalexpr::Value::Float(arg.as_number()?.sin()))),
        )
        .ok();
        ctx.set_function(
            "cos".into(),
            Function::new(|arg| Ok(evalexpr::Value::Float(arg.as_number()?.cos()))),
        )
        .ok();
        ctx.set_function(
            "int".into(),
            Function::new(|arg| Ok(evalexpr::Value::Float(arg.as_number()?.floor()))),
        )
        .ok();
        ctx.set_function(
            "abs".into(),
            Function::new(|arg| Ok(evalexpr::Value::Float(arg.as_number()?.abs()))),
        )
        .ok();
        ctx.set_function(
            "str".into(),
            Function::new(|arg| Ok(evalexpr::Value::String(arg.to_string()))),
        )
        .ok();
        ctx.set_function(
            "randint".into(),
            Function::new(|arg| {
                let t = arg.as_tuple()?;
                if t.len() != 2 {
                    return Err(evalexpr::EvalexprError::wrong_function_argument_amount(
                        t.len(),
                        2,
                    ));
                }
                let a = t[0].as_int()?;
                let b = t[1].as_int()?;
                let mut rng = rand::thread_rng();
                let n: i64 = rng.gen_range(a..=b);
                Ok(evalexpr::Value::Int(n))
            }),
        )
        .ok();
        // Evaluate
        let r = eval_with_context(expr, &ctx).map_err(|e| e.to_string())?;
        Ok(match r {
            evalexpr::Value::Int(i) => Value::Number(i as f64),
            evalexpr::Value::Float(f) => Value::Number(f),
            evalexpr::Value::String(s) => Value::Str(s),
            evalexpr::Value::Boolean(b) => Value::Bool(b),
            _ => Value::Null,
        })
    }

    fn set_var(&mut self, name: &str, val: Value) {
        self.vars.insert(name.to_string(), val);
    }

    fn get_var(&self, name: &str) -> Value {
        self.vars.get(name).cloned().unwrap_or(Value::Null)
    }

    fn parse_assign(&mut self, rest: &str) -> Result<(), String> {
        // Supports: name = expr  or  LET name = expr
        let parts: Vec<&str> = rest.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err("Invalid assignment".into());
        }
        let name = parts[0].trim();
        let expr = parts[1].trim();
        let v = self.eval(expr)?;
        self.set_var(name, v);
        Ok(())
    }

    pub fn run(&mut self, code: &str) -> Result<(), String> {
        let mut lines: Vec<String> = Vec::new();
        for line in code.lines() {
            let t = line.trim();
            if t.is_empty() {
                continue;
            }
            if t.starts_with("REM") || t.starts_with('#') {
                continue;
            }
            lines.push(t.to_string());
        }
        let mut pc: usize = 0;
        // Label map
        let mut labels: HashMap<String, usize> = HashMap::new();
        for (i, l) in lines.iter().enumerate() {
            if let Some(idx) = l.find(':') {
                if idx + 1 == l.len() {
                    labels.insert(l[..idx].to_string(), i);
                }
            }
        }

        // Procedure map (PROC name [params...] ... ENDPROC)
        let mut procs: HashMap<String, (usize, Vec<String>)> = HashMap::new();
        for (i, l) in lines.iter().enumerate() {
            let up = l.to_uppercase();
            if up.starts_with("PROC ") {
                let rest = l[4..].trim();
                // Split by whitespace or commas
                let mut toks: Vec<String> = rest
                    .split(|c: char| c.is_whitespace() || c == ',')
                    .filter(|s| !s.is_empty())
                    .map(|s| s.to_string())
                    .collect();
                if !toks.is_empty() {
                    let name = toks.remove(0);
                    let params = toks;
                    procs.insert(name, (i + 1, params));
                }
            }
        }

        // Simple block stacks for loops and procedures (minimal impl)
        let mut for_stack: Vec<(String, f64, f64, usize)> = Vec::new();
        let mut repeat_stack: Vec<(i64, usize)> = Vec::new();
        let mut while_stack: Vec<(usize, String)> = Vec::new();
        #[derive(Clone)]
        struct CallFrame {
            ret_pc: usize,
            saved: Vec<(String, Option<Value>)>,
        }
        let mut call_stack: Vec<CallFrame> = Vec::new();

        while pc < lines.len() {
            let line = lines[pc].clone();
            if self.trace {
                self.console.print(&format!("[{}] {}", pc + 1, line));
            }
            // Skip pure label lines
            if line.ends_with(':') {
                pc += 1;
                continue;
            }
            // Split keyword and rest
            let mut parts = line.splitn(2, ' ');
            let kw = parts.next().unwrap().to_uppercase();
            let rest = parts.next().unwrap_or("").trim();

            match kw.as_str() {
                "PROC" => {
                    // Skip procedure body during linear execution
                    let mut idx = pc + 1;
                    while idx < lines.len() {
                        if lines[idx].to_uppercase() == "ENDPROC" {
                            break;
                        }
                        idx += 1;
                    }
                    pc = idx + 1;
                }
                "ENDPROC" | "RETURN" => {
                    if let Some(frame) = call_stack.pop() {
                        // restore saved param vars
                        for (name, old) in frame.saved.into_iter() {
                            match old {
                                Some(v) => {
                                    self.set_var(&name, v);
                                }
                                None => {
                                    self.vars.remove(&name);
                                }
                            }
                        }
                        pc = frame.ret_pc;
                    } else {
                        pc += 1;
                    }
                }
                "CALL" => {
                    // CALL name arg1, arg2, ...
                    let rest_trim = rest.trim();
                    if rest_trim.is_empty() {
                        return Err("CALL missing name".into());
                    }
                    // split off name (first token)
                    let mut parts_call = rest_trim.splitn(2, char::is_whitespace);
                    let name = parts_call.next().unwrap().to_string();
                    let args_str = parts_call.next().unwrap_or("").trim();

                    // split args by commas at depth 0 (respect parentheses)
                    let mut args_list: Vec<String> = Vec::new();
                    if !args_str.is_empty() {
                        let mut buf = String::new();
                        let mut depth = 0i32;
                        for ch in args_str.chars() {
                            match ch {
                                '(' => {
                                    depth += 1;
                                    buf.push(ch);
                                }
                                ')' => {
                                    depth -= 1;
                                    buf.push(ch);
                                }
                                ',' if depth == 0 => {
                                    let s = buf.trim();
                                    if !s.is_empty() {
                                        args_list.push(s.to_string());
                                    }
                                    buf.clear();
                                }
                                _ => buf.push(ch),
                            }
                        }
                        let s = buf.trim();
                        if !s.is_empty() {
                            args_list.push(s.to_string());
                        }
                    }

                    if let Some(&(start, ref params)) = procs.get(&name) {
                        // evaluate args
                        let mut args: Vec<Value> = Vec::new();
                        for a in args_list.iter() {
                            args.push(self.eval(a)?);
                        }
                        // save previous param vars and set new
                        let mut saved: Vec<(String, Option<Value>)> = Vec::new();
                        for (i, p) in params.iter().enumerate() {
                            let prev = self.vars.get(p).cloned();
                            saved.push((p.clone(), prev));
                            let val = args.get(i).cloned().unwrap_or(Value::Null);
                            self.set_var(p, val);
                        }
                        call_stack.push(CallFrame {
                            ret_pc: pc + 1,
                            saved,
                        });
                        pc = start;
                    } else {
                        return Err(format!("Unknown procedure {}", name));
                    }
                }
                "PRINT" | "TYPE" => {
                    let v = self.eval(rest)?;
                    self.console.print(&v.to_string_lossy());
                    pc += 1;
                }
                "LET" => {
                    self.parse_assign(rest)?;
                    pc += 1;
                }
                "INPUT" | "ACCEPT" => {
                    // no interactive input yet; set empty string
                    let var = rest.split_whitespace().next().ok_or("INPUT missing var")?;
                    self.set_var(var, Value::Str(String::new()));
                    pc += 1;
                }
                "IF" => {
                    // IF expr THEN statement
                    if let Some(pos) = rest.to_uppercase().find("THEN") {
                        let cond_expr = &rest[..pos].trim();
                        let stmt = rest[pos + 4..].trim();
                        let cond = self.eval(cond_expr)?.to_f64() != 0.0;
                        if cond {
                            // Execute one-line statement by recursion on a tiny program
                            let one = stmt.to_string();
                            self.run(&one)?;
                        }
                        pc += 1;
                    } else {
                        return Err("IF missing THEN".into());
                    }
                }
                "GOTO" | "JUMP" => {
                    let label = rest;
                    if let Some(&dest) = labels.get(label) {
                        pc = dest + 1;
                    } else {
                        return Err(format!("Unknown label {}", label));
                    }
                }
                "REPEAT" => {
                    let n = self.eval(rest)?.to_f64().floor() as i64;
                    repeat_stack.push((n, pc + 1));
                    pc += 1;
                }
                "ENDREPEAT" => {
                    if let Some((ref mut count, start_pc)) = repeat_stack.last_mut() {
                        *count -= 1;
                        if *count > 0 {
                            pc = *start_pc;
                        } else {
                            repeat_stack.pop();
                            pc += 1;
                        }
                    } else {
                        return Err("ENDREPEAT without REPEAT".into());
                    }
                }
                "FOR" => {
                    // FOR i = a TO b [STEP s]
                    let mut seg = rest.split("TO");
                    let left = seg.next().ok_or("FOR missing TO")?.trim();
                    let right = seg.next().ok_or("FOR missing rhs")?.trim();
                    let mut leftp = left.split('=');
                    let var = leftp.next().ok_or("FOR missing var")?.trim().to_string();
                    let start_expr = leftp.next().ok_or("FOR missing start expr")?.trim();
                    let mut end_expr = right;
                    let mut step_expr = "1";
                    if let Some(pos) = right.to_uppercase().find("STEP") {
                        end_expr = right[..pos].trim();
                        step_expr = right[pos + 4..].trim();
                    }
                    let start = self.eval(start_expr)?.to_f64();
                    let endv = self.eval(end_expr)?.to_f64();
                    let step = self.eval(step_expr)?.to_f64();
                    self.set_var(&var, Value::Number(start));
                    for_stack.push((var, endv, step, pc + 1));
                    pc += 1;
                }
                "NEXT" => {
                    if let Some((var, endv, step, start_pc)) = for_stack.last().cloned() {
                        let cur = self.get_var(&var).to_f64();
                        let nextv = cur + step;
                        self.set_var(&var, Value::Number(nextv));
                        let done = if step >= 0.0 {
                            nextv > endv
                        } else {
                            nextv < endv
                        };
                        if done {
                            for_stack.pop();
                            pc += 1;
                        } else {
                            pc = start_pc;
                        }
                    } else {
                        return Err("NEXT without FOR".into());
                    }
                }
                "WHILE" => {
                    let cond = rest.to_string();
                    let ok = self.eval(&cond)?.to_f64() != 0.0;
                    if ok {
                        while_stack.push((pc, cond));
                        pc += 1;
                    } else {
                        // skip to matching ENDWHILE
                        let mut depth = 1;
                        let mut idx = pc + 1;
                        while idx < lines.len() {
                            let l = lines[idx].to_uppercase();
                            if l.starts_with("WHILE ") {
                                depth += 1;
                            }
                            if l == "ENDWHILE" {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            idx += 1;
                        }
                        pc = idx + 1;
                    }
                }
                "ENDWHILE" => {
                    if let Some((wstart, cond)) = while_stack.last().cloned() {
                        if self.eval(&cond)?.to_f64() != 0.0 {
                            pc = wstart + 1;
                        } else {
                            while_stack.pop();
                            pc += 1;
                        }
                    } else {
                        return Err("ENDWHILE without WHILE".into());
                    }
                }
                "TRACE" => {
                    self.trace = !self.trace;
                    pc += 1;
                }
                "DUMPVARS" => {
                    let mut keys: Vec<_> = self.vars.keys().cloned().collect();
                    keys.sort();
                    for k in keys {
                        if !k.eq("pi") {
                            if let Some(v) = self.vars.get(&k) {
                                self.console
                                    .print(&format!("{} = {}", k, v.to_string_lossy()));
                            }
                        }
                    }
                    pc += 1;
                }
                "ASSERT" => {
                    if self.eval(rest)?.to_f64() == 0.0 {
                        return Err(format!("ASSERT failed: {}", rest));
                    }
                    pc += 1;
                }
                "PAUSE" => {
                    std::thread::sleep(Duration::from_millis(400));
                    pc += 1;
                }
                // Turtle
                "CLS" => {
                    self.turtle.clear();
                    pc += 1;
                }
                "FD" | "FORWARD" => {
                    let d = self.eval(rest)?.to_f64() as f32;
                    self.turtle.forward(d);
                    pc += 1;
                }
                "LT" | "LEFT" => {
                    let d = self.eval(rest)?.to_f64() as f32;
                    self.turtle.left(d);
                    pc += 1;
                }
                "RT" | "RIGHT" => {
                    let d = self.eval(rest)?.to_f64() as f32;
                    self.turtle.right(d);
                    pc += 1;
                }
                "PU" | "PENUP" => {
                    self.turtle.pen_up();
                    pc += 1;
                }
                "PD" | "PENDOWN" => {
                    self.turtle.pen_down();
                    pc += 1;
                }
                "SETXY" => {
                    let parts: Vec<&str> =
                        rest.split([' ', ',']).filter(|s| !s.is_empty()).collect();
                    if parts.len() == 2 {
                        let x = self.eval(parts[0])?.to_f64() as f32;
                        let y = self.eval(parts[1])?.to_f64() as f32;
                        self.turtle.set_xy(x, y);
                        pc += 1;
                    } else {
                        return Err("SETXY expects two arguments".into());
                    }
                }
                "COLOR" => {
                    // Accept either: COLOR r g b  or  COLOR r, g, b
                    let parts: Vec<&str> =
                        rest.split([',', ' ']).filter(|s| !s.is_empty()).collect();
                    if parts.len() != 3 {
                        return Err("COLOR r g b".into());
                    }
                    let r = self.eval(parts[0])?.to_f64().clamp(0.0, 255.0) as u8;
                    let g = self.eval(parts[1])?.to_f64().clamp(0.0, 255.0) as u8;
                    let b = self.eval(parts[2])?.to_f64().clamp(0.0, 255.0) as u8;
                    self.turtle.color(r, g, b);
                    pc += 1;
                }
                "PEN" => {
                    // Compatibility: PEN RGB(r,g,b) or PEN r g b or PEN r, g, b
                    let trimmed = rest.trim();
                    let inner = if let Some(s) = trimmed.strip_prefix("RGB(") {
                        s.strip_suffix(')').unwrap_or(s)
                    } else {
                        trimmed
                    };
                    let parts: Vec<&str> =
                        inner.split([',', ' ']).filter(|s| !s.is_empty()).collect();
                    if parts.len() != 3 {
                        return Err("PEN expects RGB(r,g,b) or r g b".into());
                    }
                    let r = self.eval(parts[0])?.to_f64().clamp(0.0, 255.0) as u8;
                    let g = self.eval(parts[1])?.to_f64().clamp(0.0, 255.0) as u8;
                    let b = self.eval(parts[2])?.to_f64().clamp(0.0, 255.0) as u8;
                    self.turtle.color(r, g, b);
                    pc += 1;
                }
                "SLEEP" => {
                    let s = self.eval(rest)?.to_f64();
                    std::thread::sleep(Duration::from_secs_f64(s));
                    pc += 1;
                }
                "EXPORTPNG" => {
                    let path = rest.trim().trim_matches('"');
                    let model = self.turtle.snapshot();
                    let (w, h) = (model.width as u32, model.height as u32);
                    let mut img = RgbImage::from_pixel(w.max(1), h.max(1), Rgb([0u8, 0u8, 0u8]));
                    for seg in &model.lines {
                        let (x0, y0, x1, y1) =
                            (seg.x1 as i32, seg.y1 as i32, seg.x2 as i32, seg.y2 as i32);
                        let col = Rgb([seg.color.0, seg.color.1, seg.color.2]);
                        let dx = (x1 - x0).abs();
                        let sx = if x0 < x1 { 1 } else { -1 };
                        let dy = -(y1 - y0).abs();
                        let sy = if y0 < y1 { 1 } else { -1 };
                        let mut err = dx + dy;
                        let (mut x, mut y) = (x0, y0);
                        loop {
                            if x >= 0 && y >= 0 && (x as u32) < w && (y as u32) < h {
                                img.put_pixel(x as u32, y as u32, col);
                            }
                            if x == x1 && y == y1 {
                                break;
                            }
                            let e2 = 2 * err;
                            if e2 >= dy {
                                err += dy;
                                x += sx;
                            }
                            if e2 <= dx {
                                err += dx;
                                y += sy;
                            }
                        }
                    }
                    if let Err(e) = img.save(path) {
                        return Err(format!("EXPORTPNG error: {}", e));
                    }
                    self.console.print(&format!("Saved {}", path));
                    pc += 1;
                }

                // DB commands
                #[cfg(not(feature = "mysql"))]
                "DBCONNECT" | "DBDISCONNECT" | "DBEXEC" | "DBEXECPARAM" | "DBQUERY"
                | "DBQUERYPARAM" => {
                    self.console
                        .print("[DB] MySQL feature not enabled in Rust build");
                    pc += 1;
                }
                #[cfg(feature = "mysql")]
                "DBCONNECT" => {
                    let dsn = rest.trim().trim_matches('"');
                    match Pool::new(dsn) {
                        Ok(pool) => {
                            self.db = Some(pool);
                            self.console.print(&format!("[DB] Connected: {}", dsn));
                        }
                        Err(e) => return Err(format!("DBCONNECT error: {}", e)),
                    }
                    pc += 1;
                }
                #[cfg(feature = "mysql")]
                "DBDISCONNECT" => {
                    self.db = None;
                    self.console.print("[DB] Disconnected");
                    pc += 1;
                }
                #[cfg(feature = "mysql")]
                "DBEXEC" => {
                    let sql = rest;
                    if let Some(pool) = &self.db {
                        match pool.get_conn() {
                            Ok(mut conn) => {
                                if let Err(e) = conn.exec_drop(sql, ()) {
                                    return Err(format!("DBEXEC error: {}", e));
                                }
                                self.console.print("[DB] OK");
                            }
                            Err(e) => return Err(format!("DB connection error: {}", e)),
                        }
                    } else {
                        return Err("No DB connection".into());
                    }
                    pc += 1;
                }
                #[cfg(feature = "mysql")]
                "DBQUERY" => {
                    let sql = rest;
                    if let Some(pool) = &self.db {
                        match pool.get_conn() {
                            Ok(mut conn) => match conn.query_iter(sql) {
                                Ok(mut result_set) => {
                                    let mut printed = 0;
                                    while let Some(row_res) = result_set.next() {
                                        let row = row_res.map_err(|e| e.to_string())?;
                                        let vals: Vec<String> = row
                                            .unwrap()
                                            .unwrap()
                                            .unwrap()
                                            .iter()
                                            .map(|v| format!("{}", v))
                                            .collect();
                                        self.console.print(&format!("[DB] {}", vals.join(", ")));
                                        printed += 1;
                                        if printed >= 5 {
                                            break;
                                        }
                                    }
                                }
                                Err(e) => return Err(format!("DBQUERY error: {}", e)),
                            },
                            Err(e) => return Err(format!("DB connection error: {}", e)),
                        }
                    } else {
                        return Err("No DB connection".into());
                    }
                    pc += 1;
                }
                // Assignment without LET
                _ if line.contains('=') && !kw.ends_with(':') => {
                    self.parse_assign(&line)?;
                    pc += 1;
                }
                _ => {
                    return Err(format!("Unknown command: {}", kw));
                }
            }
        }
        Ok(())
    }
}
