import Foundation

class PilotExecutor {
    struct Label { let name: String; let index: Int }
    private var labels: [String: Int] = [:]
    private var vars: [String: String] = [:]
    private var lastCondition: Bool = false

    func execute(code: String) -> String {
        let rawLines = code.components(separatedBy: .newlines)
        var lines: [String] = []
        for l in rawLines { let t = l.trimmingCharacters(in: .whitespaces); if !t.isEmpty { lines.append(t) } }
        // First pass: collect labels L:NAME
        for (idx, line) in lines.enumerated() {
            if line.uppercased().hasPrefix("L:") {
                let name = String(line.dropFirst(2)).trimmingCharacters(in: .whitespaces)
                if !name.isEmpty { labels[name.uppercased()] = idx }
            }
        }
    var pc = 0
        var output = ""
        var steps = 0
        let maxSteps = 10000

        func substituteVars(_ s: String) -> String {
            var result = s
            for (k,v) in vars { result = result.replacingOccurrences(of: "{\(k)}", with: v) }
            return result
        }

        while pc < lines.count && steps < maxSteps {
            steps += 1
            let line = lines[pc]
            pc += 1
            let up = line.uppercased()
            if up.hasPrefix("L:") { continue }
            if up.hasPrefix("R:") { break }
            if up.hasPrefix("T:") {
                let text = String(line.dropFirst(2)).trimmingCharacters(in: .whitespaces)
                output += substituteVars(text) + "\n"
                continue
            }
            if up.hasPrefix("A:") {
                // A:NAME=VALUE
                let body = String(line.dropFirst(2))
                if let eq = body.firstIndex(of: "=") {
                    let name = body[..<eq].trimmingCharacters(in: .whitespaces).uppercased()
                    let value = body[body.index(after: eq)...].trimmingCharacters(in: .whitespaces)
                    vars[name] = substituteVars(value)
                }
                continue
            }
            if up.hasPrefix("U:") {
                // U:NAME?PROMPT  (store user input)
                let body = String(line.dropFirst(2))
                if let q = body.firstIndex(of: "?") {
                    let name = body[..<q].trimmingCharacters(in: .whitespaces).uppercased()
                    let prompt = body[body.index(after: q)...].trimmingCharacters(in: .whitespaces)
                    // No actual interactive input in this environment; stub.
                    vars[name] = "(input)"
                    output += "📝 \(prompt) => (input)\n"
                }
                continue
            }
            if up.hasPrefix("C:") {
                // C: pattern ; text-if-match
                let body = String(line.dropFirst(2))
                let parts = body.split(separator: ";", maxSplits: 1).map { String($0).trimmingCharacters(in: .whitespaces) }
                if parts.count == 2 {
                    let pattern = substituteVars(parts[0])
                    let text = substituteVars(parts[1])
                    lastCondition = (pattern == text)
                    if lastCondition { output += text + "\n" }
                }
                continue
            }
            if up.hasPrefix("M:") {
                // M: a = b   OR   M: a ; b
                let body = String(line.dropFirst(2))
                if let eq = body.firstIndex(of: "=") {
                    let left = substituteVars(String(body[..<eq]).trimmingCharacters(in: .whitespaces))
                    let right = substituteVars(String(body[body.index(after: eq)...]).trimmingCharacters(in: .whitespaces))
                    lastCondition = (left == right)
                } else {
                    let parts = body.split(separator: ";", maxSplits: 1).map { String($0).trimmingCharacters(in: .whitespaces) }
                    if parts.count == 2 {
                        lastCondition = (substituteVars(parts[0]) == substituteVars(parts[1]))
                    } else {
                        lastCondition = false
                    }
                }
                continue
            }
            if up.hasPrefix("Y:") {
                if lastCondition {
                    let inner = String(line.dropFirst(2)).trimmingCharacters(in: .whitespaces)
                    // Execute a single embedded command like T:/J:/E:/A:
                    let res = executeEmbedded(inner, labels: labels, pc: &pc)
                    output += res.output
                    if res.didJump { continue }
                }
                continue
            }
            if up.hasPrefix("N:") {
                if !lastCondition {
                    let inner = String(line.dropFirst(2)).trimmingCharacters(in: .whitespaces)
                    let res = executeEmbedded(inner, labels: labels, pc: &pc)
                    output += res.output
                    if res.didJump { continue }
                }
                continue
            }
            if up.hasPrefix("J:") {
                let target = String(line.dropFirst(2)).trimmingCharacters(in: .whitespaces).uppercased()
                if let dest = labels[target] { pc = dest } else { output += "❌ Unknown label: \(target)\n" }
                continue
            }
            if up.hasPrefix("E:") {
                let text = String(line.dropFirst(2)).trimmingCharacters(in: .whitespaces)
                output += "❌ \(substituteVars(text))\n"
                continue
            }
            if up.hasPrefix("Y:") || up.hasPrefix("N:") || up.hasPrefix("M:") {
                // handled above
                continue
            }
            output += "ℹ️ Unknown: \(line)\n"
        }
        if steps >= maxSteps { output += "❌ Stopped: too many steps\n" }
        return output
    }

    private func executeEmbedded(_ cmd: String, labels: [String:Int], pc: inout Int) -> (output: String, didJump: Bool) {
        let up = cmd.uppercased()
        if up.hasPrefix("T:") {
            let text = String(cmd.dropFirst(2)).trimmingCharacters(in: .whitespaces)
            return (substituteVars(text) + "\n", false)
        }
        if up.hasPrefix("E:") {
            let text = String(cmd.dropFirst(2)).trimmingCharacters(in: .whitespaces)
            return ("❌ " + substituteVars(text) + "\n", false)
        }
        if up.hasPrefix("J:") {
            let target = String(cmd.dropFirst(2)).trimmingCharacters(in: .whitespaces).uppercased()
            if let dest = labels[target] { pc = dest; return ("", true) }
            return ("❌ Unknown label: \(target)\n", false)
        }
        if up.hasPrefix("A:") {
            let body = String(cmd.dropFirst(2))
            if let eq = body.firstIndex(of: "=") {
                let name = body[..<eq].trimmingCharacters(in: .whitespaces).uppercased()
                let value = body[body.index(after: eq)...].trimmingCharacters(in: .whitespaces)
                vars[name] = substituteVars(value)
            }
            return ("", false)
        }
        return ("ℹ️ Unknown: \(cmd)\n", false)
    }
}
