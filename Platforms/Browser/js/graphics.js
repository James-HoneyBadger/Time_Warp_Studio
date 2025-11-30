/**
 * Time Warp Web IDE - Graphics Engine
 * Handles turtle graphics and canvas operations
 */

class TurtleGraphics {
    constructor(canvas) {
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');

        // Get dimensions with multiple fallback strategies
        const rect = canvas.getBoundingClientRect();
        let width = rect.width;
        let height = rect.height;

        // Fallback 1: Use canvas attributes
        if (width <= 0 || height <= 0) {
            width = canvas.width || 400;
            height = canvas.height || 400;
            console.log('Using canvas attribute fallback:', width, 'x', height);
        }

        // Fallback 2: Use parent container dimensions
        if (width <= 0 || height <= 0) {
            const parent = canvas.parentElement;
            if (parent) {
                const parentRect = parent.getBoundingClientRect();
                width = parentRect.width > 0 ? parentRect.width - 20 : 400; // Leave some margin
                height = parentRect.height > 0 ? parentRect.height - 20 : 400;
                console.log('Using parent container fallback:', width, 'x', height);
            }
        }

        // Final fallback: Fixed dimensions
        if (width <= 0 || height <= 0) {
            width = 400;
            height = 400;
            console.log('Using fixed fallback:', width, 'x', height);
        }

        this.width = width;
        this.height = height;

        console.log('TurtleGraphics constructor - final dimensions:', this.width, 'x', this.height);

        // Turtle state
        this.x = this.width / 2;
        this.y = this.height / 2;
        this.heading = 90; // degrees, 90 = up
        this.isPenDown = true;
        this.penColor = 'blue';
        this.penWidth = 2;

        // Drawing state
        this.showGrid = true;
        this.paths = [];
        this.currentPath = [];

        // Animation
        this.animationSpeed = 1;
        this.isAnimating = false;

        // Always setup canvas
        this.setupCanvas();
        this.draw();
    }

    setupCanvas() {
        console.log('Setting up canvas with dimensions:', this.width, 'x', this.height);

        // Set canvas context properties
        this.ctx.lineCap = 'round';
        this.ctx.lineJoin = 'round';

        // High DPI support
        const dpr = window.devicePixelRatio || 1;
        console.log('Device pixel ratio:', dpr);

        // Set canvas internal dimensions (for drawing)
        this.canvas.width = this.width * dpr;
        this.canvas.height = this.height * dpr;

        // Scale context for high DPI
        this.ctx.scale(dpr, dpr);

        // Set canvas display size (CSS)
        this.canvas.style.width = this.width + 'px';
        this.canvas.style.height = this.height + 'px';

        // Center the turtle
        this.x = this.width / 2;
        this.y = this.height / 2;

        console.log('Canvas setup complete:', {
            displaySize: this.width + 'x' + this.height,
            internalSize: this.canvas.width + 'x' + this.canvas.height,
            turtlePos: '(' + this.x + ', ' + this.y + ')'
        });
    }

    // Coordinate system conversion
    toCanvasX(x) {
        return this.width / 2 + x;
    }

    toCanvasY(y) {
        return this.height / 2 - y; // Flip Y axis
    }

    fromCanvasX(x) {
        return x - this.width / 2;
    }

    fromCanvasY(y) {
        return this.height / 2 - y; // Flip Y axis
    }

    // Turtle movement
    forward(distance) {
        const radians = (this.heading - 90) * Math.PI / 180; // Convert to math coordinates
        const newX = this.x + Math.cos(radians) * distance;
        const newY = this.y + Math.sin(radians) * distance;

        if (this.isPenDown) {
            this.drawLine(this.x, this.y, newX, newY);
            this.currentPath.push({
                type: 'line',
                x1: this.x,
                y1: this.y,
                x2: newX,
                y2: newY,
                color: this.penColor,
                width: this.penWidth
            });
        }

        this.x = newX;
        this.y = newY;
        this.draw();

        this.updateTurtleInfo();
    }

    backward(distance) {
        this.forward(-distance);
    }

    right(angle) {
        this.heading = (this.heading - angle) % 360;
        if (this.heading < 0) this.heading += 360;
        this.draw();
        this.updateTurtleInfo();
    }

    left(angle) {
        this.heading = (this.heading + angle) % 360;
        this.draw();
        this.updateTurtleInfo();
    }

    penUp() {
        this.isPenDown = false;
        if (this.currentPath.length > 0) {
            this.paths.push([...this.currentPath]);
            this.currentPath = [];
        }
        this.updateTurtleInfo();
    }

    penDown() {
        this.isPenDown = true;
        this.updateTurtleInfo();
    }

    setColor(color) {
        // Handle both color names and numbers
        if (typeof color === 'number') {
            const colors = [
                'black', 'blue', 'red', 'green', 'yellow',
                'magenta', 'cyan', 'white', 'gray', 'orange',
                'purple', 'brown', 'pink', 'lightblue', 'lightgreen'
            ];
            this.penColor = colors[Math.floor(color) % colors.length] || 'black';
        } else {
            this.penColor = color || 'black';
        }
    }

    setPenWidth(width) {
        this.penWidth = Math.max(1, parseInt(width) || 1);
    }

    // Screen operations
    clearScreen() {
        this.paths = [];
        this.currentPath = [];
        this.draw();
    }

    home() {
        this.x = this.width / 2;
        this.y = this.height / 2;
        this.heading = 90;
        this.draw();
        this.updateTurtleInfo();
    }

    // Drawing methods
    drawLine(x1, y1, x2, y2) {
        this.ctx.beginPath();
        this.ctx.strokeStyle = this.penColor;
        this.ctx.lineWidth = this.penWidth;
        this.ctx.moveTo(x1, y1);
        this.ctx.lineTo(x2, y2);
        this.ctx.stroke();
    }

    drawGrid() {
        if (!this.showGrid) return;

        this.ctx.save();
        this.ctx.strokeStyle = '#f0f0f0';
        this.ctx.lineWidth = 1;
        this.ctx.globalAlpha = 0.5;

        const gridSize = 20;

        // Vertical lines
        for (let x = 0; x <= this.width; x += gridSize) {
            this.ctx.beginPath();
            this.ctx.moveTo(x, 0);
            this.ctx.lineTo(x, this.height);
            this.ctx.stroke();
        }

        // Horizontal lines
        for (let y = 0; y <= this.height; y += gridSize) {
            this.ctx.beginPath();
            this.ctx.moveTo(0, y);
            this.ctx.lineTo(this.width, y);
            this.ctx.stroke();
        }

        // Center cross
        this.ctx.strokeStyle = '#ddd';
        this.ctx.lineWidth = 2;
        this.ctx.globalAlpha = 0.7;

        // Vertical center line
        this.ctx.beginPath();
        this.ctx.moveTo(this.width / 2, 0);
        this.ctx.lineTo(this.width / 2, this.height);
        this.ctx.stroke();

        // Horizontal center line
        this.ctx.beginPath();
        this.ctx.moveTo(0, this.height / 2);
        this.ctx.lineTo(this.width, this.height / 2);
        this.ctx.stroke();

        this.ctx.restore();
    }

    drawTurtle() {
        this.ctx.save();

        // Move to turtle position
        this.ctx.translate(this.x, this.y);
        this.ctx.rotate((this.heading - 90) * Math.PI / 180);

        // Draw turtle body (triangle)
        this.ctx.beginPath();
        this.ctx.moveTo(12, 0);
        this.ctx.lineTo(-6, -8);
        this.ctx.lineTo(-6, 8);
        this.ctx.closePath();

        // Fill turtle
        this.ctx.fillStyle = this.isPenDown ? '#e74c3c' : '#95a5a6';
        this.ctx.fill();

        // Stroke turtle
        this.ctx.strokeStyle = '#2c3e50';
        this.ctx.lineWidth = 2;
        this.ctx.stroke();

        // Draw direction indicator
        this.ctx.beginPath();
        this.ctx.moveTo(8, 0);
        this.ctx.lineTo(16, 0);
        this.ctx.strokeStyle = this.isPenDown ? '#c0392b' : '#7f8c8d';
        this.ctx.lineWidth = 3;
        this.ctx.stroke();

        this.ctx.restore();
    }

    redrawPaths() {
        // Draw all saved paths
        for (const path of this.paths) {
            for (const segment of path) {
                if (segment.type === 'line') {
                    this.ctx.beginPath();
                    this.ctx.strokeStyle = segment.color;
                    this.ctx.lineWidth = segment.width;
                    this.ctx.moveTo(segment.x1, segment.y1);
                    this.ctx.lineTo(segment.x2, segment.y2);
                    this.ctx.stroke();
                }
            }
        }

        // Draw current path
        for (const segment of this.currentPath) {
            if (segment.type === 'line') {
                this.ctx.beginPath();
                this.ctx.strokeStyle = segment.color;
                this.ctx.lineWidth = segment.width;
                this.ctx.moveTo(segment.x1, segment.y1);
                this.ctx.lineTo(segment.x2, segment.y2);
                this.ctx.stroke();
            }
        }
    }

    draw() {
        console.log('Drawing canvas...', this.width, 'x', this.height);

        // Clear canvas
        this.ctx.clearRect(0, 0, this.width, this.height);

        // Set background
        this.ctx.fillStyle = 'white';
        this.ctx.fillRect(0, 0, this.width, this.height);

        // Draw a test border to ensure canvas is visible
        this.ctx.strokeStyle = 'red';
        this.ctx.lineWidth = 2;
        this.ctx.strokeRect(1, 1, this.width - 2, this.height - 2);

        // Draw grid
        this.drawGrid();

        // Draw all paths
        this.redrawPaths();

        // Draw turtle
        this.drawTurtle();

        console.log('Canvas drawing completed');
    }

    // UI Updates
    updateTurtleInfo() {
        const logicalX = Math.round(this.fromCanvasX(this.x));
        const logicalY = Math.round(this.fromCanvasY(this.y));

        const turtleXEl = document.getElementById('turtleX');
        const turtleYEl = document.getElementById('turtleY');
        const turtleHeadingEl = document.getElementById('turtleHeading');
        const penStatusEl = document.getElementById('penStatus');

        if (turtleXEl) turtleXEl.textContent = logicalX;
        if (turtleYEl) turtleYEl.textContent = logicalY;
        if (turtleHeadingEl) turtleHeadingEl.textContent = Math.round(this.heading);
        if (penStatusEl) penStatusEl.textContent = this.isPenDown ? 'Down' : 'Up';
    }

    // Configuration
    setShowGrid(show) {
        this.showGrid = show;
        this.draw();
    }

    setAnimationSpeed(speed) {
        this.animationSpeed = Math.max(0.1, Math.min(5, speed));
    }

    // Export/Import
    exportImage() {
        // Create a new canvas without the turtle for export
        const exportCanvas = document.createElement('canvas');
        exportCanvas.width = this.width;
        exportCanvas.height = this.height;
        const exportCtx = exportCanvas.getContext('2d');

        // Set background
        exportCtx.fillStyle = 'white';
        exportCtx.fillRect(0, 0, this.width, this.height);

        // Draw paths only
        exportCtx.lineCap = 'round';
        exportCtx.lineJoin = 'round';

        for (const path of this.paths) {
            for (const segment of path) {
                if (segment.type === 'line') {
                    exportCtx.beginPath();
                    exportCtx.strokeStyle = segment.color;
                    exportCtx.lineWidth = segment.width;
                    exportCtx.moveTo(segment.x1, segment.y1);
                    exportCtx.lineTo(segment.x2, segment.y2);
                    exportCtx.stroke();
                }
            }
        }

        for (const segment of this.currentPath) {
            if (segment.type === 'line') {
                exportCtx.beginPath();
                exportCtx.strokeStyle = segment.color;
                exportCtx.lineWidth = segment.width;
                exportCtx.moveTo(segment.x1, segment.y1);
                exportCtx.lineTo(segment.x2, segment.y2);
                exportCtx.stroke();
            }
        }

        return exportCanvas.toDataURL('image/png');
    }

    // Advanced drawing methods
    circle(radius) {
        const circumference = 2 * Math.PI * Math.abs(radius);
        const steps = Math.max(36, Math.floor(circumference / 5));
        const angle = 360 / steps;

        for (let i = 0; i < steps; i++) {
            this.forward(circumference / steps);
            if (radius > 0) {
                this.left(angle);
            } else {
                this.right(angle);
            }
        }
    }

    arc(radius, extent) {
        const circumference = 2 * Math.PI * Math.abs(radius) * (Math.abs(extent) / 360);
        const steps = Math.max(4, Math.floor(circumference / 2));
        const angleStep = extent / steps;
        const moveStep = circumference / steps;

        for (let i = 0; i < steps; i++) {
            this.forward(moveStep);
            if ((radius > 0 && extent > 0) || (radius < 0 && extent < 0)) {
                this.left(angleStep);
            } else {
                this.right(Math.abs(angleStep));
            }
        }
    }

    // Utility methods
    getPosition() {
        return {
            x: this.fromCanvasX(this.x),
            y: this.fromCanvasY(this.y)
        };
    }

    getHeading() {
        return this.heading;
    }

    setPosition(x, y) {
        this.x = this.toCanvasX(x);
        this.y = this.toCanvasY(y);
        this.draw();
        this.updateTurtleInfo();
    }

    setHeading(angle) {
        this.heading = angle % 360;
        if (this.heading < 0) this.heading += 360;
        this.draw();
        this.updateTurtleInfo();
    }

    // Event handling for interactive features
    setupInteraction() {
        this.canvas.addEventListener('click', (e) => {
            const rect = this.canvas.getBoundingClientRect();
            const x = e.clientX - rect.left;
            const y = e.clientY - rect.top;

            // You could add click-to-move functionality here
            console.log(`Clicked at canvas (${x}, ${y}), logical (${this.fromCanvasX(x)}, ${this.fromCanvasY(y)})`);
        });
    }

    // Resize handling
    resize() {
        this.setupCanvas();
        this.draw();
    }
}