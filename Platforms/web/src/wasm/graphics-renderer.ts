/**
 * WASM Graphics Rendering Integration
 * Converts WASM graphics commands to canvas drawing operations
 * Phase 5.3: WASM Runtime & Integration
 */

export interface GraphicsCommand {
  type: number;
  data: Record<string, any>;
}

export interface RenderContext {
  canvas: HTMLCanvasElement;
  ctx: CanvasRenderingContext2D;
  scale: number;
  offsetX: number;
  offsetY: number;
  width: number;
  height: number;
  backgroundColor: string;
}

export enum GraphicsCommandType {
  // Turtle graphics
  TURTLE_MOVE = 1,
  TURTLE_ROTATE = 2,
  TURTLE_PEN_UP = 3,
  TURTLE_PEN_DOWN = 4,
  TURTLE_SET_COLOR = 5,
  TURTLE_SET_WIDTH = 6,
  TURTLE_HOME = 7,
  CLEAR_SCREEN = 8,

  // Drawing primitives
  DRAW_LINE = 10,
  DRAW_CIRCLE = 11,
  DRAW_RECT = 12,
  DRAW_POLYGON = 13,
  FILL_SHAPE = 14,

  // Text
  DRAW_TEXT = 20,

  // Canvas control
  SET_BACKGROUND = 30,
  SAVE_STATE = 31,
  RESTORE_STATE = 32,
}

/**
 * TurtleGraphicsRenderer: Renders turtle graphics commands
 */
export class TurtleGraphicsRenderer {
  private canvas: HTMLCanvasElement;
  private ctx: CanvasRenderingContext2D;
  private turtleX: number = 0;
  private turtleY: number = 0;
  private turtleAngle: number = 0; // degrees, 0 = right
  private penDown: boolean = true;
  private penColor: string = 'black';
  private penWidth: number = 1;
  private scale: number = 1;
  private centerX: number = 0;
  private centerY: number = 0;
  private saveStack: Array<any> = [];

  constructor(canvas: HTMLCanvasElement, scale: number = 1) {
    this.canvas = canvas;
    const ctx = canvas.getContext('2d');
    if (!ctx) {
      throw new Error('Could not get 2D context from canvas');
    }
    this.ctx = ctx;
    this.scale = scale;
    this.centerX = canvas.width / 2;
    this.centerY = canvas.height / 2;
    this.turtleX = this.centerX;
    this.turtleY = this.centerY;
  }

  /**
   * Clear canvas
   */
  clear(backgroundColor: string = 'white'): void {
    this.ctx.fillStyle = backgroundColor;
    this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
    this.resetTurtleState();
  }

  /**
   * Reset turtle state
   */
  resetTurtleState(): void {
    this.turtleX = this.centerX;
    this.turtleY = this.centerY;
    this.turtleAngle = 0;
    this.penDown = true;
    this.penColor = 'black';
    this.penWidth = 1;
  }

  /**
   * Move turtle forward
   */
  forward(distance: number): void {
    const radians = (this.turtleAngle * Math.PI) / 180;
    const newX = this.turtleX + distance * this.scale * Math.cos(radians);
    const newY = this.turtleY + distance * this.scale * Math.sin(radians);

    if (this.penDown) {
      this.drawLine(this.turtleX, this.turtleY, newX, newY);
    }

    this.turtleX = newX;
    this.turtleY = newY;
  }

  /**
   * Move turtle backward
   */
  backward(distance: number): void {
    this.forward(-distance);
  }

  /**
   * Rotate turtle right
   */
  right(angle: number): void {
    this.turtleAngle = (this.turtleAngle + angle) % 360;
  }

  /**
   * Rotate turtle left
   */
  left(angle: number): void {
    this.turtleAngle = (this.turtleAngle - angle + 360) % 360;
  }

  /**
   * Pen up
   */
  penUp(): void {
    this.penDown = false;
  }

  /**
   * Pen down
   */
  penDown(): void {
    this.penDown = true;
  }

  /**
   * Set pen color
   */
  setColor(color: string | number): void {
    if (typeof color === 'number') {
      this.penColor = this.numberToColor(color);
    } else {
      this.penColor = color;
    }
    this.ctx.strokeStyle = this.penColor;
    this.ctx.fillStyle = this.penColor;
  }

  /**
   * Set pen width
   */
  setWidth(width: number): void {
    this.penWidth = width;
    this.ctx.lineWidth = width;
  }

  /**
   * Draw line
   */
  private drawLine(x1: number, y1: number, x2: number, y2: number): void {
    this.ctx.beginPath();
    this.ctx.moveTo(x1, y1);
    this.ctx.lineTo(x2, y2);
    this.ctx.stroke();
  }

  /**
   * Draw circle
   */
  drawCircle(radius: number, filled: boolean = false): void {
    this.ctx.beginPath();
    this.ctx.arc(
      this.turtleX,
      this.turtleY,
      radius * this.scale,
      0,
      2 * Math.PI
    );
    if (filled) {
      this.ctx.fill();
    } else {
      this.ctx.stroke();
    }
  }

  /**
   * Draw rectangle
   */
  drawRect(width: number, height: number, filled: boolean = false): void {
    const x = this.turtleX - (width * this.scale) / 2;
    const y = this.turtleY - (height * this.scale) / 2;

    if (filled) {
      this.ctx.fillRect(x, y, width * this.scale, height * this.scale);
    } else {
      this.ctx.strokeRect(x, y, width * this.scale, height * this.scale);
    }
  }

  /**
   * Draw polygon
   */
  drawPolygon(points: Array<{ x: number; y: number }>, filled: boolean = false): void {
    if (points.length < 2) return;

    this.ctx.beginPath();
    this.ctx.moveTo(
      points[0].x * this.scale + this.turtleX,
      points[0].y * this.scale + this.turtleY
    );

    for (let i = 1; i < points.length; i++) {
      this.ctx.lineTo(
        points[i].x * this.scale + this.turtleX,
        points[i].y * this.scale + this.turtleY
      );
    }

    this.ctx.closePath();

    if (filled) {
      this.ctx.fill();
    } else {
      this.ctx.stroke();
    }
  }

  /**
   * Draw text
   */
  drawText(text: string, fontSize: number = 12): void {
    this.ctx.font = `${fontSize}px Arial`;
    this.ctx.fillStyle = this.penColor;
    this.ctx.fillText(text, this.turtleX, this.turtleY);
  }

  /**
   * Save state
   */
  saveState(): void {
    this.saveStack.push({
      x: this.turtleX,
      y: this.turtleY,
      angle: this.turtleAngle,
      penDown: this.penDown,
      color: this.penColor,
      width: this.penWidth,
    });
  }

  /**
   * Restore state
   */
  restoreState(): void {
    const state = this.saveStack.pop();
    if (state) {
      this.turtleX = state.x;
      this.turtleY = state.y;
      this.turtleAngle = state.angle;
      this.penDown = state.penDown;
      this.penColor = state.color;
      this.penWidth = state.width;
      this.ctx.strokeStyle = this.penColor;
      this.ctx.fillStyle = this.penColor;
      this.ctx.lineWidth = this.penWidth;
    }
  }

  /**
   * Get turtle state
   */
  getState() {
    return {
      x: this.turtleX,
      y: this.turtleY,
      angle: this.turtleAngle,
      penDown: this.penDown,
      color: this.penColor,
      width: this.penWidth,
    };
  }

  /**
   * Set home position
   */
  home(): void {
    this.turtleX = this.centerX;
    this.turtleY = this.centerY;
    this.turtleAngle = 0;
  }

  /**
   * Convert number to color
   */
  private numberToColor(num: number): string {
    const colors: Record<number, string> = {
      0: 'black',
      1: 'red',
      2: 'green',
      3: 'blue',
      4: 'yellow',
      5: 'cyan',
      6: 'magenta',
      7: 'white',
    };
    return colors[num] || `rgb(${(num >> 16) & 255}, ${(num >> 8) & 255}, ${num & 255})`;
  }
}

/**
 * GraphicsRenderer: Main graphics rendering engine
 */
export class GraphicsRenderer {
  private turtleRenderer: TurtleGraphicsRenderer;
  private canvas: HTMLCanvasElement;
  private ctx: CanvasRenderingContext2D;
  private renderContext: RenderContext;

  constructor(canvas: HTMLCanvasElement, scale: number = 1) {
    this.canvas = canvas;
    const ctx = canvas.getContext('2d');
    if (!ctx) {
      throw new Error('Could not get 2D context from canvas');
    }
    this.ctx = ctx;
    this.turtleRenderer = new TurtleGraphicsRenderer(canvas, scale);
    this.renderContext = {
      canvas,
      ctx,
      scale,
      offsetX: 0,
      offsetY: 0,
      width: canvas.width,
      height: canvas.height,
      backgroundColor: 'white',
    };
  }

  /**
   * Render graphics commands
   */
  render(commands: GraphicsCommand[]): void {
    for (const command of commands) {
      this.executeCommand(command);
    }
  }

  /**
   * Execute single graphics command
   */
  private executeCommand(command: GraphicsCommand): void {
    const { type, data } = command;

    switch (type) {
      // Turtle movement
      case GraphicsCommandType.TURTLE_MOVE:
        if (data.direction === 0) {
          // Forward
          this.turtleRenderer.forward(data.distance || 0);
        } else if (data.direction === 1) {
          // Backward
          this.turtleRenderer.backward(data.distance || 0);
        }
        break;

      case GraphicsCommandType.TURTLE_ROTATE:
        if (data.direction === 0) {
          // Right
          this.turtleRenderer.right(data.angle || 0);
        } else if (data.direction === 1) {
          // Left
          this.turtleRenderer.left(data.angle || 0);
        }
        break;

      case GraphicsCommandType.TURTLE_PEN_UP:
        this.turtleRenderer.penUp();
        break;

      case GraphicsCommandType.TURTLE_PEN_DOWN:
        this.turtleRenderer.penDown();
        break;

      case GraphicsCommandType.TURTLE_SET_COLOR:
        this.turtleRenderer.setColor(data.color || 'black');
        break;

      case GraphicsCommandType.TURTLE_SET_WIDTH:
        this.turtleRenderer.setWidth(data.width || 1);
        break;

      case GraphicsCommandType.TURTLE_HOME:
        this.turtleRenderer.home();
        break;

      case GraphicsCommandType.CLEAR_SCREEN:
        this.turtleRenderer.clear(data.backgroundColor || 'white');
        break;

      // Drawing primitives
      case GraphicsCommandType.DRAW_CIRCLE:
        this.turtleRenderer.drawCircle(data.radius || 0, data.filled);
        break;

      case GraphicsCommandType.DRAW_RECT:
        this.turtleRenderer.drawRect(data.width || 0, data.height || 0, data.filled);
        break;

      case GraphicsCommandType.DRAW_TEXT:
        this.turtleRenderer.drawText(data.text || '', data.fontSize || 12);
        break;

      case GraphicsCommandType.SAVE_STATE:
        this.turtleRenderer.saveState();
        break;

      case GraphicsCommandType.RESTORE_STATE:
        this.turtleRenderer.restoreState();
        break;
    }
  }

  /**
   * Clear canvas
   */
  clear(backgroundColor: string = 'white'): void {
    this.turtleRenderer.clear(backgroundColor);
    this.renderContext.backgroundColor = backgroundColor;
  }

  /**
   * Get turtle state for debugging
   */
  getTurtleState() {
    return this.turtleRenderer.getState();
  }

  /**
   * Set canvas size
   */
  setSize(width: number, height: number): void {
    this.canvas.width = width;
    this.canvas.height = height;
    this.renderContext.width = width;
    this.renderContext.height = height;
  }

  /**
   * Export canvas as image
   */
  exportImage(format: 'png' | 'jpeg' = 'png'): string {
    const mimeType = format === 'png' ? 'image/png' : 'image/jpeg';
    return this.canvas.toDataURL(mimeType);
  }

  /**
   * Download canvas as file
   */
  downloadImage(filename: string = 'graphics.png'): void {
    const link = document.createElement('a');
    link.href = this.exportImage();
    link.download = filename;
    link.click();
  }
}

/**
 * CanvasManager: Manages multiple canvas instances
 */
export class CanvasManager {
  private canvases: Map<string, { canvas: HTMLCanvasElement; renderer: GraphicsRenderer }> =
    new Map();

  /**
   * Register canvas
   */
  register(name: string, canvas: HTMLCanvasElement, scale: number = 1): void {
    const renderer = new GraphicsRenderer(canvas, scale);
    this.canvases.set(name, { canvas, renderer });
  }

  /**
   * Get canvas renderer
   */
  getRenderer(name: string): GraphicsRenderer | undefined {
    return this.canvases.get(name)?.renderer;
  }

  /**
   * Render commands to canvas
   */
  render(name: string, commands: GraphicsCommand[]): void {
    const renderer = this.getRenderer(name);
    if (renderer) {
      renderer.render(commands);
    }
  }

  /**
   * Clear canvas
   */
  clear(name: string, backgroundColor?: string): void {
    const renderer = this.getRenderer(name);
    if (renderer) {
      renderer.clear(backgroundColor);
    }
  }

  /**
   * Get all canvases
   */
  getAll(): string[] {
    return Array.from(this.canvases.keys());
  }
}
