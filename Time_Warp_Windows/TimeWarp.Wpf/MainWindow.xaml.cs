using System;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace TimeWarp.Wpf
{
    public partial class MainWindow : Window
    {
        private readonly BasicExecutor _basic = new();
        private readonly LogoExecutor _logo = new();
        private readonly PilotExecutor _pilot = new();

        public MainWindow()
        {
            InitializeComponent();
        }

        private void Clear_Click(object sender, RoutedEventArgs e)
        {
            OutputText.Text = string.Empty;
            TurtleCanvas.Children.Clear();
        }

        private void Run_Click(object sender, RoutedEventArgs e)
        {
            OutputText.Text = string.Empty;
            TurtleCanvas.Children.Clear();

            string lang = ((ComboBoxItem)LanguageCombo.SelectedItem).Content?.ToString() ?? "BASIC";
            string code = CodeBox.Text;

            switch (lang)
            {
                case "BASIC":
                    OutputText.Text = _basic.Execute(code);
                    break;
                case "Logo":
                    var (outText, points) = _logo.Execute(code);
                    OutputText.Text = outText;
                    DrawPoints(points);
                    break;
                case "PILOT":
                    OutputText.Text = _pilot.Execute(code);
                    break;
            }
        }

        private void DrawPoints((double x, double y)[] pts)
        {
            if (pts.Length < 2) return;
            var geom = new StreamGeometry();
            using (var ctx = geom.Open())
            {
                ctx.BeginFigure(new Point(pts[0].x, pts[0].y), false, false);
                for (int i = 1; i < pts.Length; i++)
                    ctx.LineTo(new Point(pts[i].x, pts[i].y), true, false);
            }
            var path = new System.Windows.Shapes.Path
            {
                Data = geom,
                Stroke = Brushes.Blue,
                StrokeThickness = 2
            };
            TurtleCanvas.Children.Add(path);
        }
    }
}
