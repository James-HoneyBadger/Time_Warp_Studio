import SwiftUI

struct ContentView: View {
    @State private var codeText = ""
    @State private var outputText = ""
    @State private var selectedLanguage = "BASIC"
    @State private var turtlePoints: [CGPoint] = []
    
    let languages = ["BASIC", "Logo", "PILOT"]
    
    private let basicExecutor = BasicExecutor()
    private let logoExecutor = LogoExecutor()
    private let pilotExecutor = PilotExecutor()
    
    var body: some View {
        VStack(spacing: 0) {
            // Menu bar
            HStack {
                Text("Time Warp IDE")
                    .font(.headline)
                    .padding(.leading)
                
                Spacer()
                
                Picker("Language", selection: $selectedLanguage) {
                    ForEach(languages, id: \.self) { lang in
                        Text(lang).tag(lang)
                    }
                }
                .pickerStyle(.segmented)
                .frame(width: 300)
                
                Button("Run") {
                    runCode()
                }
                .buttonStyle(.borderedProminent)
                .padding(.horizontal)
                
                Button("Clear") {
                    outputText = ""
                    turtlePoints.removeAll()
                }
                .padding(.trailing)
            }
            .padding(.vertical, 8)
            .background(Color.gray.opacity(0.2))
            
            // Main content area
            HSplitView {
                // Code editor
                VStack(alignment: .leading) {
                    Text("Code Editor")
                        .font(.caption)
                        .padding(.horizontal)
                    
                    TextEditor(text: $codeText)
                        .font(.system(.body, design: .monospaced))
                        .padding(4)
                        .background(Color(nsColor: .textBackgroundColor))
                        .border(Color.gray.opacity(0.3))
                }
                .frame(minWidth: 300)
                
                // Output and Canvas
                VStack {
                    // Text output
                    VStack(alignment: .leading) {
                        Text("Output")
                            .font(.caption)
                            .padding(.horizontal)
                        
                        ScrollView {
                            Text(outputText)
                                .font(.system(.body, design: .monospaced))
                                .frame(maxWidth: .infinity, alignment: .leading)
                                .padding(8)
                        }
                        .frame(height: 200)
                        .background(Color(nsColor: .textBackgroundColor))
                        .border(Color.gray.opacity(0.3))
                    }
                    
                    // Turtle canvas
                    if selectedLanguage == "Logo" {
                        VStack(alignment: .leading) {
                            Text("Canvas")
                                .font(.caption)
                                .padding(.horizontal)
                            
                            Canvas { context, size in
                                // Background
                                context.fill(
                                    Path(CGRect(origin: .zero, size: size)),
                                    with: .color(.white)
                                )
                                
                                // Turtle path
                                if turtlePoints.count > 1 {
                                    var path = Path()
                                    path.move(to: turtlePoints[0])
                                    for i in 1..<turtlePoints.count {
                                        path.addLine(to: turtlePoints[i])
                                    }
                                    context.stroke(path, with: .color(.blue), lineWidth: 2)
                                }
                            }
                            .frame(height: 400)
                            .border(Color.gray)
                        }
                    }
                }
                .frame(minWidth: 300)
            }
        }
        .frame(minWidth: 800, minHeight: 600)
    }
    
    private func runCode() {
        outputText = ""
        turtlePoints.removeAll()
        
        switch selectedLanguage {
        case "BASIC":
            outputText = basicExecutor.execute(code: codeText)
        case "Logo":
            let (output, points) = logoExecutor.execute(code: codeText)
            outputText = output
            turtlePoints = points
        case "PILOT":
            outputText = pilotExecutor.execute(code: codeText)
        default:
            outputText = "Unknown language"
        }
    }
}

#if os(macOS)
#Preview {
    ContentView()
        .frame(width: 1024, height: 768)
}
#endif
