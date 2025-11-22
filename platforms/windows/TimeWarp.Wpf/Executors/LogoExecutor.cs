using System;
using System.Collections.Generic;

namespace TimeWarp.Wpf
{
    public class LogoExecutor
    {
        public (string output, (double x, double y)[] points) Execute(string code)
        {
            var outBuf = new System.Text.StringBuilder();
            var pts = new List<(double x, double y)>();
            double x = 256, y = 256, heading = 0; bool penDown = true;
            pts.Add((x, y));

            var lines = code.Replace("\r", string.Empty).Split('\n');
            int steps = 0;
            foreach (var raw in lines)
            {
                if (++steps > 10000) { outBuf.AppendLine("❌ Stopped: too many steps"); break; }
                var line = raw.Trim(); if (line.Length == 0 || line.StartsWith(";")) continue;
                var parts = line.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                var cmd = parts[0].ToUpperInvariant();
                string arg = parts.Length > 1 ? string.Join(" ", parts, 1, parts.Length - 1) : string.Empty;

                switch (cmd)
                {
                    case "FD": case "FORWARD":
                        var d = ParseDouble(arg);
                        var rad = heading * Math.PI / 180.0;
                        x += Math.Cos(rad) * d; y += Math.Sin(rad) * d; pts.Add((x, y));
                        break;
                    case "BK": case "BACK":
                        d = ParseDouble(arg);
                        rad = heading * Math.PI / 180.0;
                        x -= Math.Cos(rad) * d; y -= Math.Sin(rad) * d; pts.Add((x, y));
                        break;
                    case "LT": case "LEFT": heading -= ParseDouble(arg); break;
                    case "RT": case "RIGHT": heading += ParseDouble(arg); break;
                    case "PU": case "PENUP": penDown = false; break;
                    case "PD": case "PENDOWN": penDown = true; break;
                    case "CS": case "CLEARSCREEN": pts.Clear(); x = 256; y = 256; heading = 0; penDown = true; pts.Add((x, y)); break;
                    case "HOME": x = 256; y = 256; heading = 0; pts.Add((x, y)); break;
                    case "SETH": case "SETHEADING": heading = ParseDouble(arg); break;
                    case "SETXY":
                        var a = arg.Split(new[] { ' ', ',' }, StringSplitOptions.RemoveEmptyEntries);
                        if (a.Length >= 2 && double.TryParse(a[0], out var nx) && double.TryParse(a[1], out var ny)) { x = nx; y = ny; pts.Add((x, y)); }
                        break;
                    default:
                        outBuf.AppendLine($"ℹ️ Unknown: {line}");
                        break;
                }
            }
            return (outBuf.ToString(), pts.ToArray());
        }

        private static double ParseDouble(string s) => double.TryParse(s, out var d) ? d : 0;
    }
}
