import Foundation

class BasicExecutor {
    private var vars: [String: Double] = [:]
    private struct ForFrame { let varName: String; let end: Double; let step: Double; let loopStart: Int }
    private var forStack: [ForFrame] = []
    private var gosubStack: [Int] = []

    func execute(code: String) -> String {
        var output = ""
    vars = [:]
    forStack.removeAll()
    gosubStack.removeAll()

        // Build program with optional line numbers
        var program: [(num: Int?, text: String)] = []
        for raw in code.components(separatedBy: .newlines) {
            let line = raw.trimmingCharacters(in: .whitespaces)
            if line.isEmpty { continue }
            if let space = line.firstIndex(of: " "), let n = Int(line[..<space]) {
                let rest = line[line.index(after: space)...].trimmingCharacters(in: .whitespaces)
                program.append((n, String(rest)))
            } else {
                program.append((nil, line))
            }
        }
        if program.isEmpty { return "" }

        // Map line numbers to indices
        var numToIndex: [Int: Int] = [:]
        for (i, entry) in program.enumerated() {
            if let n = entry.num { numToIndex[n] = i }
        }

        var pc = 0
        var steps = 0
        let maxSteps = 10000

        while pc < program.count && steps < maxSteps {
            steps += 1
            let raw = program[pc].text
            let cmd = raw.trimmingCharacters(in: .whitespaces)
            if cmd.isEmpty { pc += 1; continue }

            let upper = cmd.uppercased()
            if upper.hasPrefix("REM") { pc += 1; continue }
            if upper == "END" { break }

            if upper == "CLS" {
                output.removeAll()
                pc += 1
                continue
            }

            if upper.hasPrefix("PRINT") {
                let arg = String(cmd.dropFirst(5)).trimmingCharacters(in: .whitespaces)
                output += evaluatePrint(arg) + "\n"
                pc += 1
                continue
            }

            if upper.hasPrefix("INPUT ") {
                // INPUT A  -> stub to 0; show prompt line
                let name = String(cmd.dropFirst(6)).trimmingCharacters(in: .whitespaces).uppercased()
                output += "📝 \(name)? => 0\n"
                vars[name] = 0
                pc += 1
                continue
            }

            if upper.hasPrefix("FOR ") {
                // FOR I = a TO b [STEP s]
                let body = String(cmd.dropFirst(4))
                // Split at '='
                guard let eq = body.firstIndex(of: "=") else { output += "❌ Bad FOR\n"; pc += 1; continue }
                let vname = body[..<eq].trimmingCharacters(in: .whitespaces).uppercased()
                var rest = body[body.index(after: eq)...].trimmingCharacters(in: .whitespaces)
                // Expect a TO b
                guard let toRange = rest.range(of: "TO", options: .caseInsensitive) else { output += "❌ Bad FOR (TO)\n"; pc += 1; continue }
                let startExpr = String(rest[..<toRange.lowerBound]).trimmingCharacters(in: .whitespaces)
                rest = rest[toRange.upperBound...].trimmingCharacters(in: .whitespaces)
                var stepVal = 1.0
                var endExpr = rest
                if let stepRange = rest.range(of: "STEP", options: .caseInsensitive) {
                    endExpr = String(rest[..<stepRange.lowerBound]).trimmingCharacters(in: .whitespaces)
                    let stepExpr = String(rest[stepRange.upperBound...]).trimmingCharacters(in: .whitespaces)
                    stepVal = evalExpr(stepExpr) ?? 1.0
                }
                let startVal = evalExpr(startExpr) ?? 0
                let endVal = evalExpr(endExpr) ?? startVal
                vars[vname] = startVal
                let frame = ForFrame(varName: vname, end: endVal, step: stepVal, loopStart: pc + 1)
                forStack.append(frame)
                pc += 1
                continue
            }

            if upper.hasPrefix("NEXT") {
                // NEXT [I]
                if forStack.isEmpty { pc += 1; continue }
                var frame = forStack[forStack.count - 1]
                // Optional variable name enforcement
                let parts = cmd.split(separator: " ")
                if parts.count > 1 {
                    let name = parts[1].trimmingCharacters(in: .whitespaces).uppercased()
                    if name != frame.varName { pc += 1; continue }
                }
                let cur = (vars[frame.varName] ?? 0) + frame.step
                vars[frame.varName] = cur
                let cont = frame.step >= 0 ? (cur <= frame.end + 1e-12) : (cur >= frame.end - 1e-12)
                if cont {
                    pc = frame.loopStart
                } else {
                    _ = forStack.popLast()
                    pc += 1
                }
                continue
            }

            if upper.hasPrefix("GOSUB ") {
                let target = String(cmd.dropFirst(6)).trimmingCharacters(in: .whitespaces)
                if let n = Int(target), let idx = numToIndex[n] {
                    gosubStack.append(pc + 1)
                    pc = idx
                } else {
                    output += "❌ Unknown line \(target)\n"
                    pc += 1
                }
                continue
            }

            if upper == "RETURN" {
                if let ret = gosubStack.popLast() {
                    pc = ret
                } else {
                    pc += 1
                }
                continue
            }

            if upper.hasPrefix("IF ") {
                // IF expr THEN line
                let rest = cmd.dropFirst(3)
                if let thenRange = rest.range(of: "THEN", options: .caseInsensitive) {
                    let condExpr = String(rest[..<thenRange.lowerBound]).trimmingCharacters(in: .whitespaces)
                    let targetStr = String(rest[thenRange.upperBound...]).trimmingCharacters(in: .whitespaces)
                    let cond = (evalExpr(condExpr) ?? 0) != 0
                    if cond {
                        if let n = Int(targetStr), let idx = numToIndex[n] {
                            pc = idx
                        } else {
                            output += "❌ Unknown line \(targetStr)\n"
                            pc += 1
                        }
                    } else {
                        pc += 1
                    }
                    continue
                }
            }

            if upper.hasPrefix("GOTO") {
                let target = String(cmd.dropFirst(4)).trimmingCharacters(in: .whitespaces)
                if let n = Int(target), let idx = numToIndex[n] {
                    pc = idx
                } else {
                    output += "❌ Unknown line \(target)\n"
                    pc += 1
                }
                continue
            }

            if let eq = cmd.firstIndex(of: "=") {
                // LET A = expr  or  A = expr
                let left = cmd[..<eq].replacingOccurrences(of: "LET", with: "", options: .caseInsensitive).trimmingCharacters(in: .whitespaces)
                let right = String(cmd[cmd.index(after: eq)...]).trimmingCharacters(in: .whitespaces)
                if let val = evalExpr(right) {
                    vars[left.uppercased()] = val
                } else {
                    output += "❌ Bad expression: \(right)\n"
                }
                pc += 1
                continue
            }

            // Unknown
            output += "ℹ️ Unknown: \(cmd)\n"
            pc += 1
        }

        if steps >= maxSteps {
            output += "❌ Stopped: too many steps\n"
        }
        return output
    }

    private func evaluatePrint(_ s: String) -> String {
        var str = s.trimmingCharacters(in: .whitespaces)
        if (str.hasPrefix("\"") && str.hasSuffix("\"")) || (str.hasPrefix("'") && str.hasSuffix("'")) {
            str.removeFirst(); str.removeLast()
            return str
        }
        if let v = evalExpr(str) {
            if floor(v) == v { return String(Int(v)) }
            return String(v)
        }
        return "❌ Bad expression: \(s)"
    }

    private func evalExpr(_ expr: String) -> Double? {
        // Replace variables with numeric values to keep NSExpression safe
        let pattern = "[A-Za-z][A-Za-z0-9_]*"
        guard let regex = try? NSRegularExpression(pattern: pattern) else { return nil }
        let ns = expr as NSString
        var replaced = expr
        var offset = 0
        let matches = regex.matches(in: expr, range: NSRange(location: 0, length: ns.length))
        for m in matches {
            let r = NSRange(location: m.range.location + offset, length: m.range.length)
            let start = replaced.index(replaced.startIndex, offsetBy: r.location)
            let end = replaced.index(start, offsetBy: r.length)
            let token = String(replaced[start..<end])
            // Skip keywords like THEN, GOTO etc. (not expected in arithmetic), and numbers disguised as words
            if Double(token) != nil { continue }
            let key = token.uppercased()
            if let val = vars[key] {
                let valStr = (floor(val) == val) ? String(Int(val)) : String(val)
                replaced.replaceSubrange(start..<end, with: valStr)
                offset += valStr.count - r.length
            }
        }
        // Evaluate numeric expression
        let exp = NSExpression(format: replaced)
        let result = exp.expressionValue(with: nil, context: nil)
        if let n = result as? NSNumber { return n.doubleValue }
        if let s = result as? String, let d = Double(s) { return d }
        return nil
    }
}
