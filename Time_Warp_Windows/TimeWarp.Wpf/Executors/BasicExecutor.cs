using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;

namespace TimeWarp.Wpf
{
    public class BasicExecutor
    {
        private readonly Dictionary<string, double> _vars = new(StringComparer.OrdinalIgnoreCase);
        private readonly Stack<(string varName, double end, double step, int loopStart)> _forStack = new();
        private readonly Stack<int> _gosubStack = new();

        public string Execute(string code)
        {
            _vars.Clear();
            _forStack.Clear();
            _gosubStack.Clear();
            var lines = new List<(int? num, string text)>();
            foreach (var raw in code.Replace("\r", string.Empty).Split('\n'))
            {
                var line = raw.Trim();
                if (line.Length == 0) continue;
                var m = Regex.Match(line, "^(?<n>\\d+)\\s+(?<t>.*)$");
                if (m.Success)
                    lines.Add((int.Parse(m.Groups["n"].Value, CultureInfo.InvariantCulture), m.Groups["t"].Value.Trim()));
                else
                    lines.Add((null, line));
            }
            if (lines.Count == 0) return string.Empty;

            var numToIndex = new Dictionary<int, int>();
            for (int i = 0; i < lines.Count; i++) if (lines[i].num.HasValue) numToIndex[lines[i].num!.Value] = i;

            int pc = 0, steps = 0, max = 10000;
            var outBuf = new System.Text.StringBuilder();
            while (pc < lines.Count && steps++ < max)
            {
                var cmd = lines[pc].text.Trim();
                var up = cmd.ToUpperInvariant();
                if (up.Length == 0 || up.StartsWith("REM")) { pc++; continue; }
                if (up == "END") break;
                if (up == "CLS") { outBuf.Clear(); pc++; continue; }
                if (up.StartsWith("INPUT ")) {
                    var name = cmd.Substring(6).Trim().ToUpperInvariant();
                    _vars[name] = 0; outBuf.AppendLine($"📝 {name}? => 0"); pc++; continue;
                }
                if (up.StartsWith("FOR ")) {
                    // FOR I = a TO b [STEP s]
                    var body = cmd.Substring(4).Trim();
                    var eq = body.IndexOf('=');
                    if (eq < 0) { outBuf.AppendLine("❌ Bad FOR (missing =)"); pc++; continue; }
                    var vname = body.Substring(0, eq).Trim().ToUpperInvariant();
                    var rest = body.Substring(eq + 1).Trim();
                    var toIdx = rest.ToUpperInvariant().IndexOf("TO");
                    if (toIdx < 0) { outBuf.AppendLine("❌ Bad FOR (missing TO)"); pc++; continue; }
                    var startExpr = rest.Substring(0, toIdx).Trim();
                    rest = rest.Substring(toIdx + 2).Trim();
                    double step = 1.0; string endExpr = rest;
                    var stepIdx = rest.ToUpperInvariant().IndexOf("STEP");
                    if (stepIdx >= 0) {
                        endExpr = rest.Substring(0, stepIdx).Trim();
                        var stepExpr = rest.Substring(stepIdx + 4).Trim();
                        step = EvalExpr(stepExpr);
                    }
                    var startVal = EvalExpr(startExpr);
                    var endVal = EvalExpr(endExpr);
                    _vars[vname] = startVal;
                    _forStack.Push((vname, endVal, step, pc + 1));
                    pc++; continue;
                }
                if (up.StartsWith("NEXT")) {
                    if (_forStack.Count == 0) { outBuf.AppendLine("❌ NEXT without FOR"); pc++; continue; }
                    var frame = _forStack.Peek();
                    var parts = cmd.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                    if (parts.Length > 1 && !parts[1].Equals(frame.varName, StringComparison.OrdinalIgnoreCase)) { outBuf.AppendLine("❌ NEXT wrong variable"); pc++; continue; }
                    var cur = (_vars.TryGetValue(frame.varName, out var v) ? v : 0) + frame.step;
                    _vars[frame.varName] = cur;
                    bool cont = frame.step >= 0 ? cur <= frame.end + 1e-12 : cur >= frame.end - 1e-12;
                    if (cont) { pc = frame.loopStart; continue; }
                    _forStack.Pop(); pc++; continue;
                }
                if (up.StartsWith("GOSUB ")) {
                    var target = cmd.Substring(6).Trim();
                    if (int.TryParse(target, out var ln) && numToIndex.TryGetValue(ln, out var idx)) { _gosubStack.Push(pc + 1); pc = idx; continue; }
                    outBuf.AppendLine($"❌ Unknown line {target}"); pc++; continue;
                }
                if (up == "RETURN") {
                    if (_gosubStack.Count == 0) { outBuf.AppendLine("❌ RETURN without GOSUB"); pc++; continue; }
                    pc = _gosubStack.Pop(); continue;
                }
                if (up.StartsWith("PRINT")) { outBuf.AppendLine(EvalPrint(cmd.Substring(5))); pc++; continue; }
                if (up.StartsWith("IF "))
                {
                    var rest = cmd.Substring(3);
                    var idx = rest.ToUpperInvariant().IndexOf("THEN");
                    if (idx >= 0)
                    {
                        var cond = rest.Substring(0, idx).Trim();
                        var target = rest.Substring(idx + 4).Trim();
                        var condVal = EvalExpr(cond) != 0;
                        if (condVal && int.TryParse(target, out var ln) && numToIndex.TryGetValue(ln, out var jump)) pc = jump; else pc++;
                        continue;
                    }
                }
                if (up.StartsWith("GOTO")) { var t = cmd.Substring(4).Trim(); if (int.TryParse(t, out var ln) && numToIndex.TryGetValue(ln, out var j)) pc = j; else pc++; continue; }

                var eq = cmd.IndexOf('=');
                if (eq >= 0)
                {
                    var left = cmd.Substring(0, eq).Replace("LET", string.Empty, StringComparison.OrdinalIgnoreCase).Trim();
                    var right = cmd.Substring(eq + 1).Trim();
                    _vars[left.ToUpperInvariant()] = EvalExpr(right);
                    pc++; continue;
                }

                outBuf.AppendLine($"ℹ️ Unknown: {cmd}"); pc++;
            }
            if (steps >= max) outBuf.AppendLine("❌ Stopped: too many steps");
            return outBuf.ToString();
        }

        private string EvalPrint(string s)
        {
            var str = s.Trim();
            if ((str.StartsWith("\"") && str.EndsWith("\"")) || (str.StartsWith("'") && str.EndsWith("'")))
                return str.Substring(1, str.Length - 2);
            var v = EvalExpr(str);
            return Math.Abs(v - Math.Round(v)) < 1e-9 ? Math.Round(v).ToString(CultureInfo.InvariantCulture) : v.ToString(CultureInfo.InvariantCulture);
        }

        private double EvalExpr(string expr)
        {
            // trivial evaluator: variables or double; extend later with shunting-yard
            if (_vars.TryGetValue(expr.Trim().ToUpperInvariant(), out var vv)) return vv;
            if (double.TryParse(expr, NumberStyles.Float, CultureInfo.InvariantCulture, out var d)) return d;
            // simple + support
            var parts = expr.Split('+');
            double sum = 0; foreach (var p in parts) sum += EvalExpr(p);
            return sum;
        }
    }
}
