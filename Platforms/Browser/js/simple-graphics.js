/**
 * Simplified TurtleGraphics class - bypasses all the complex initialization
 */

class SimpleTurtleGraphics {
    constructor(canvas) {
        console.log('SimpleTurtleGraphics: Initializing with canvas:', canvas);

        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');

        // Use simple, fixed dimensions
        this.width = 400;
        this.height = 400;

        // Ensure canvas has these dimensions
        canvas.width = this.width;
        canvas.height = this.height;
        canvas.style.width = this.width + 'px';
        canvas.style.height = this.height + 'px';

        console.log('SimpleTurtleGraphics: Canvas set to', this.width, 'x', this.height);

        // Turtle state
        this.x = this.width / 2;
        this.y = this.height / 2;
        this.heading = 90; // degrees, 90 = up
        this.isPenDown = true;
        this.penColor = 'blue';
        this.penWidth = 2;

        // Drawing state
        this.showGrid = true;

        console.log('SimpleTurtleGraphics: Turtle positioned at', this.x, this.y);

        // Immediately draw initial state
        this.clearScreen();

        console.log('SimpleTurtleGraphics: Initialization complete');
    }

    clearScreen() {
        console.log('SimpleTurtleGraphics: Clearing screen');

        // Clear canvas
        this.ctx.fillStyle = 'white';
        this.ctx.fillRect(0, 0, this.width, this.height);

        // Draw a visible border so we know the canvas is working
        this.ctx.strokeStyle = 'red';
        this.ctx.lineWidth = 2;
        this.ctx.strokeRect(1, 1, this.width - 2, this.height - 2);

        // Reset turtle position
        this.x = this.width / 2;
        this.y = this.height / 2;
        this.heading = 90;

        // Draw turtle
        this.drawTurtle();

        console.log('SimpleTurtleGraphics: Screen cleared and turtle drawn');
    }

    forward(distance) {
        console.log('SimpleTurtleGraphics: Moving forward', distance);

        const radians = (this.heading - 90) * Math.PI / 180;
        const newX = this.x + Math.cos(radians) * distance;
        const newY = this.y + Math.sin(radians) * distance;

        if (this.isPenDown) {
            this.ctx.beginPath();
            this.ctx.moveTo(this.x, this.y);
            this.ctx.lineTo(newX, newY);
            this.ctx.strokeStyle = this.penColor;
            this.ctx.lineWidth = this.penWidth;
            this.ctx.stroke();
            console.log('SimpleTurtleGraphics: Drew line from', this.x.toFixed(1), this.y.toFixed(1), 'to', newX.toFixed(1), newY.toFixed(1));
        }

        this.x = newX;
        this.y = newY;
        this.drawTurtle();
    }

    right(angle) {
        console.log('SimpleTurtleGraphics: Turning right', angle, 'degrees');
        this.heading = (this.heading + angle) % 360;
        this.drawTurtle();
    }

    left(angle) {
        console.log('SimpleTurtleGraphics: Turning left', angle, 'degrees');
        this.heading = (this.heading - angle + 360) % 360;
        this.drawTurtle();
    }

    penUp() {
        console.log('SimpleTurtleGraphics: Pen up');
        this.isPenDown = false;
        this.drawTurtle();
    }

    penDown() {
        console.log('SimpleTurtleGraphics: Pen down');
        this.isPenDown = true;
        this.drawTurtle();
    }

    home() {
        console.log('SimpleTurtleGraphics: Going home');
        this.x = this.width / 2;
        this.y = this.height / 2;
        this.heading = 90;
        this.drawTurtle();
    }

    setColor(color) {
        console.log('SimpleTurtleGraphics: Setting color to', color);
        this.penColor = color || 'blue';
    }

    setShowGrid(show) {
        console.log('SimpleTurtleGraphics: Grid', show ? 'enabled' : 'disabled');
        this.showGrid = show;
    }

    drawTurtle() {
        // Clear old turtle by redrawing the border area
        this.ctx.fillStyle = 'white';
        this.ctx.fillRect(this.x - 15, this.y - 15, 30, 30);

        // Redraw border in that area if needed
        if (this.x < 20 || this.x > this.width - 20 || this.y < 20 || this.y > this.height - 20) {
            this.ctx.strokeStyle = 'red';
            this.ctx.lineWidth = 2;
            this.ctx.strokeRect(1, 1, this.width - 2, this.height - 2);
        }

        // Draw turtle
        this.ctx.save();
        this.ctx.translate(this.x, this.y);
        this.ctx.rotate((this.heading - 90) * Math.PI / 180);

        this.ctx.beginPath();
        this.ctx.moveTo(10, 0);
        this.ctx.lineTo(-5, -8);
        this.ctx.lineTo(-5, 8);
        this.ctx.closePath();

        this.ctx.fillStyle = this.isPenDown ? '#e74c3c' : '#95a5a6';
        this.ctx.fill();
        this.ctx.strokeStyle = 'black';
        this.ctx.lineWidth = 1;
        this.ctx.stroke();

        this.ctx.restore();
    }
}

// Make it available globally
window.SimpleTurtleGraphics = SimpleTurtleGraphics;