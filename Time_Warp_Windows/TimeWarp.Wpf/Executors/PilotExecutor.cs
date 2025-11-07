using System;
using System.Collections.Generic;
using System.Text;

namespace TimeWarp.Wpf
{
    public class PilotExecutor
    {
        private readonly Dictionary<string, int> _labels = new(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, string> _vars = new(StringComparer.OrdinalIgnoreCase);
        private bool _lastCondition;

        public string Execute(string code)
        {
            _labels.Clear(); _vars.Clear(); _lastCondition = false;
            var lines = new List<string>();
            foreach (var raw in code.Replace("\r", string.Empty).Split('\n'))
            {
                var t = raw.Trim(); if (t.Length > 0) lines.Add(t);
            }
            // First pass: labels L:NAME
            for (int i = 0; i < lines.Count; i++)
            {
                var up = lines[i].ToUpperInvariant();
                if (up.StartsWith("L:"))
                {
                    var name = lines[i].Substring(2).Trim();
                    if (name.Length > 0) _labels[name.ToUpperInvariant()] = i;
                }
            }

            int pc = 0; int steps = 0; var outBuf = new StringBuilder();
            while (pc < lines.Count && steps++ < 10000)
            {
                var line = lines[pc]; pc++;
                var up = line.ToUpperInvariant();
                if (up.StartsWith("L:")) continue;
                if (up.StartsWith("R:")) break;
                if (up.StartsWith("T:"))
                {
                    var text = line.Substring(2).Trim();
                    outBuf.AppendLine(SubVars(text));
                    continue;
                }
                if (up.StartsWith("A:"))
                {
                    var body = line.Substring(2);
                    var eq = body.IndexOf('=');
                    if (eq >= 0)
                    {
                        var name = body.Substring(0, eq).Trim().ToUpperInvariant();
                        var value = body.Substring(eq + 1).Trim();
                        _vars[name] = SubVars(value);
                    }
                    continue;
                }
                if (up.StartsWith("U:"))
                {
                    // U:NAME?PROMPT stubbed
                    var body = line.Substring(2);
                    var q = body.IndexOf('?');
                    if (q >= 0)
                    {
                        var name = body.Substring(0, q).Trim().ToUpperInvariant();
                        var prompt = body.Substring(q + 1).Trim();
                        _vars[name] = "(input)";
                        outBuf.AppendLine($"📝 {prompt} => (input)");
                    }
                    continue;
                }
                if (up.StartsWith("C:"))
                {
                    var body = line.Substring(2);
                    var parts = body.Split(';');
                    if (parts.Length == 2)
                    {
                        var pattern = SubVars(parts[0].Trim());
                        var text = SubVars(parts[1].Trim());
                        _lastCondition = pattern == text;
                        if (_lastCondition) outBuf.AppendLine(text);
                    }
                    continue;
                }
                if (up.StartsWith("M:"))
                {
                    var body = line.Substring(2);
                    var eq = body.IndexOf('=');
                    if (eq >= 0)
                    {
                        var left = SubVars(body.Substring(0, eq).Trim());
                        var right = SubVars(body.Substring(eq + 1).Trim());
                        _lastCondition = string.Equals(left, right, StringComparison.OrdinalIgnoreCase);
                    }
                    else
                    {
                        var parts2 = body.Split(';');
                        if (parts2.Length == 2)
                        {
                            _lastCondition = string.Equals(SubVars(parts2[0].Trim()), SubVars(parts2[1].Trim()), StringComparison.OrdinalIgnoreCase);
                        }
                        else _lastCondition = false;
                    }
                    continue;
                }
                if (up.StartsWith("Y:"))
                {
                    if (_lastCondition)
                    {
                        var inner = line.Substring(2).Trim();
                        var (embeddedOut, didJump) = ExecuteEmbedded(inner, ref pc);
                        outBuf.Append(embeddedOut);
                        if (didJump) continue;
                    }
                    continue;
                }
                if (up.StartsWith("N:"))
                {
                    if (!_lastCondition)
                    {
                        var inner = line.Substring(2).Trim();
                        var (embeddedOut, didJump) = ExecuteEmbedded(inner, ref pc);
                        outBuf.Append(embeddedOut);
                        if (didJump) continue;
                    }
                    continue;
                }
                if (up.StartsWith("J:"))
                {
                    var target = line.Substring(2).Trim().ToUpperInvariant();
                    if (_labels.TryGetValue(target, out var dest)) pc = dest; else outBuf.AppendLine($"❌ Unknown label: {target}");
                    continue;
                }
                if (up.StartsWith("E:"))
                {
                    var text = line.Substring(2).Trim();
                    outBuf.AppendLine($"❌ {SubVars(text)}");
                    continue;
                }
                if (up.StartsWith("Y:") || up.StartsWith("N:") || up.StartsWith("M:")) { continue; }
                outBuf.AppendLine($"ℹ️ Unknown: {line}");
            }
            if (steps >= 10000) outBuf.AppendLine("❌ Stopped: too many steps");
            return outBuf.ToString();
        }

        private string SubVars(string s)
        {
            foreach (var kv in _vars)
            {
                s = s.Replace($"{{{kv.Key}}}", kv.Value, StringComparison.OrdinalIgnoreCase);
                s = s.Replace($"${kv.Key}", kv.Value, StringComparison.OrdinalIgnoreCase);
            }
            return s;
        }

        private (string output, bool didJump) ExecuteEmbedded(string cmd, ref int pc)
        {
            var up = cmd.ToUpperInvariant();
            if (up.StartsWith("T:"))
            {
                var text = cmd.Substring(2).Trim();
                return (SubVars(text) + "\n", false);
            }
            if (up.StartsWith("E:"))
            {
                var text = cmd.Substring(2).Trim();
                return ($"❌ {SubVars(text)}\n", false);
            }
            if (up.StartsWith("A:"))
            {
                var body = cmd.Substring(2);
                var eq = body.IndexOf('=');
                if (eq >= 0)
                {
                    var name = body.Substring(0, eq).Trim().ToUpperInvariant();
                    var value = body.Substring(eq + 1).Trim();
                    _vars[name] = SubVars(value);
                }
                return (string.Empty, false);
            }
            if (up.StartsWith("J:"))
            {
                var target = cmd.Substring(2).Trim().ToUpperInvariant();
                if (_labels.TryGetValue(target, out var dest)) { pc = dest; return (string.Empty, true); }
                return ($"❌ Unknown label: {target}\n", false);
            }
            return ($"ℹ️ Unknown: {cmd}\n", false);
        }
    }
}
