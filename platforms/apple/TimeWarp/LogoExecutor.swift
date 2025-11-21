import Foundation
import CoreGraphics

struct TurtleState {
    var x: Double = 256
    var y: Double = 256
    var heading: Double = 0 // degrees, 0 = east
    var penDown: Bool = true
}

class LogoExecutor {
    func execute(code: String) -> (String, [CGPoint]) {
        var out = ""
        var state = TurtleState()
        var points: [CGPoint] = [CGPoint(x: state.x, y: state.y)]
        var newSegment = false

        let lines = code.components(separatedBy: .newlines)
        var i = 0
        var steps = 0
        let maxSteps = 10000

        func forward(_ dist: Double) {
            let rad = state.heading * .pi / 180.0
            state.x += cos(rad) * dist
            state.y += sin(rad) * dist
            if state.penDown {
                if newSegment { points.append(CGPoint(x: state.x, y: state.y)); newSegment = false }
                else { points.append(CGPoint(x: state.x, y: state.y)) }
            } else {
                // With pen up, mark that next draw should begin a new segment
                newSegment = true
            }
        }
        func left(_ deg: Double) { state.heading -= deg }
        func right(_ deg: Double) { state.heading += deg }
        func home() {
            state.x = 256; state.y = 256; state.heading = 0
            points.append(CGPoint(x: state.x, y: state.y))
        }

        while i < lines.count && steps < maxSteps {
            steps += 1
            let raw = lines[i].trimmingCharacters(in: .whitespaces)
            i += 1
            if raw.isEmpty { continue }
            let up = raw.uppercased()
            if up.hasPrefix(";") { continue }

            let parts = raw.split(whereSeparator: { $0 == " " || $0 == "\t" })
            guard let cmd = parts.first?.uppercased() else { continue }
            let arg = parts.dropFirst().joined(separator: " ")

            switch cmd {
            case "FD", "FORWARD":
                let d = Double(arg) ?? 0
                forward(d)
            case "BK", "BACK":
                let d = Double(arg) ?? 0
                forward(-d)
            case "LT", "LEFT":
                let d = Double(arg) ?? 0
                left(d)
            case "RT", "RIGHT":
                let d = Double(arg) ?? 0
                right(d)
            case "PU", "PENUP":
                state.penDown = false
                newSegment = true
            case "PD", "PENDOWN":
                state.penDown = true
            case "HOME":
                home()
            case "CS", "CLEARSCREEN":
                points.removeAll()
                state = TurtleState()
                points.append(CGPoint(x: state.x, y: state.y))
            case "SETH", "SETHEADING":
                let d = Double(arg) ?? 0
                state.heading = d
            case "SETXY":
                let comps = arg.split(separator: " ")
                if comps.count >= 2, let x = Double(comps[0]), let y = Double(comps[1]) {
                    state.x = x; state.y = y
                    points.append(CGPoint(x: x, y: y))
                }
            case "REPEAT":
                // REPEAT n [ ... ] (very basic)
                // Parse count and bracket block
                let trimmed = arg.trimmingCharacters(in: .whitespaces)
                if let spaceIdx = trimmed.firstIndex(of: " "),
                   let count = Int(trimmed[..<spaceIdx]) {
                    let rest = trimmed[trimmed.index(after: spaceIdx)...].trimmingCharacters(in: .whitespaces)
                    if rest.hasPrefix("[") && rest.hasSuffix("]") {
                        let inner = String(rest.dropFirst().dropLast())
                        for _ in 0..<count {
                            let (subOut, subPts) = LogoExecutor().execute(code: inner)
                            out += subOut
                            // Merge points from sub-exec by appending last position step-by-step
                            for p in subPts { points.append(p) }
                        }
                    } else {
                        out += "❌ REPEAT missing [ ]\n"
                    }
                } else {
                    out += "❌ REPEAT bad count\n"
                }
            default:
                out += "ℹ️ Unknown: \(raw)\n"
            }
        }
        if steps >= maxSteps { out += "❌ Stopped: too many steps\n" }
        return (out, points)
    }
}
