/**
 * Gesture Service for Mobile Collaboration
 * Handles touch gestures for selection, cursor control, and editing
 */

import { Dimensions, PanResponder, Animated } from 'react-native'
import { GestureHandlerRootView, TapGestureHandler, LongPressGestureHandler, PanGestureHandler, PinchGestureHandler } from 'react-native-gesture-handler'

export class MobileGestureService {
  constructor(store) {
    this.store = store
    this.lastTapTime = 0
    this.lastTapPosition = { x: 0, y: 0 }
    this.selectionStart = null
    this.selectionMode = false
  }

  /**
   * Handle single tap for cursor placement
   */
  handleSingleTap = (event) => {
    const { nativeEvent } = event
    const cursorPosition = this.screenXToCursorPosition(nativeEvent.x)

    this.store.updateCursorPosition(cursorPosition)
    this.store.setCursorPosition(cursorPosition)
  }

  /**
   * Handle double tap for word selection
   */
  handleDoubleTap = (event) => {
    const { nativeEvent } = event
    const { document } = this.store
    const tapPosition = this.screenXToCursorPosition(nativeEvent.x)

    // Find word boundaries
    let start = tapPosition
    let end = tapPosition

    while (start > 0 && /\w/.test(document[start - 1])) {
      start--
    }

    while (end < document.length && /\w/.test(document[end])) {
      end++
    }

    this.store.setSelection(start, end)
  }

  /**
   * Handle triple tap for line selection
   */
  handleTripleTap = (event) => {
    const { nativeEvent } = event
    const { document } = this.store
    const tapPosition = this.screenXToCursorPosition(nativeEvent.x)

    // Find line boundaries
    let start = tapPosition
    let end = tapPosition

    // Find start of line
    while (start > 0 && document[start - 1] !== '\n') {
      start--
    }

    // Find end of line
    while (end < document.length && document[end] !== '\n') {
      end++
    }

    this.store.setSelection(start, end)
  }

  /**
   * Handle pan gesture for selection
   */
  handlePan = (event) => {
    const { nativeEvent } = event

    if (!this.selectionMode) {
      // Start selection
      this.selectionStart = this.screenXToCursorPosition(nativeEvent.x)
      this.selectionMode = true
    }

    // Update selection end
    const selectionEnd = this.screenXToCursorPosition(nativeEvent.x)
    const start = Math.min(this.selectionStart, selectionEnd)
    const end = Math.max(this.selectionStart, selectionEnd)

    this.store.setSelection(start, end)
  }

  /**
   * Handle pan release
   */
  handlePanEnd = () => {
    this.selectionMode = false
  }

  /**
   * Handle pinch gesture for zoom
   */
  handlePinch = (event) => {
    const { nativeEvent } = event
    const { scale } = nativeEvent

    // Adjust font size or zoom level
    const newZoom = Math.max(0.8, Math.min(2, scale))
    this.store.setZoom(newZoom)
  }

  /**
   * Handle long press for context menu
   */
  handleLongPress = (event) => {
    const { nativeEvent } = event
    const cursorPosition = this.screenXToCursorPosition(nativeEvent.x)

    // Show context menu
    this.store.showContextMenu(cursorPosition)
  }

  /**
   * Convert screen X coordinate to cursor position
   */
  screenXToCursorPosition = (screenX) => {
    const { document, charWidth = 8 } = this.store
    // Simple approximation: divide x by character width
    return Math.min(Math.floor(screenX / charWidth), document.length)
  }

  /**
   * Create pan responder for drag selection
   */
  createPanResponder = () => {
    return PanResponder.create({
      onStartShouldSetPanResponder: () => true,
      onMoveShouldSetPanResponder: () => true,
      onPanResponderGrant: (evt) => {
        this.selectionStart = this.screenXToCursorPosition(evt.nativeEvent.touches[0].pageX)
        this.selectionMode = true
      },
      onPanResponderMove: (evt) => {
        if (this.selectionMode) {
          const x = evt.nativeEvent.touches[0].pageX
          const selectionEnd = this.screenXToCursorPosition(x)
          const start = Math.min(this.selectionStart, selectionEnd)
          const end = Math.max(this.selectionStart, selectionEnd)
          this.store.setSelection(start, end)
        }
      },
      onPanResponderRelease: () => {
        this.selectionMode = false
      },
    })
  }

  /**
   * Handle keyboard shortcuts
   */
  handleKeyPress = (key, isShiftPressed = false) => {
    const { document, cursorPosition, selectionStart, selectionEnd } = this.store

    switch (key) {
      case 'Backspace':
        if (selectionStart !== selectionEnd) {
          // Delete selection
          this.store.applyLocalOperation({
            type: 'delete',
            position: selectionStart,
            length: selectionEnd - selectionStart,
          })
          this.store.setCursorPosition(selectionStart)
          this.store.setSelection(selectionStart, selectionStart)
        } else if (cursorPosition > 0) {
          // Delete character before cursor
          this.store.applyLocalOperation({
            type: 'delete',
            position: cursorPosition - 1,
            length: 1,
          })
          this.store.setCursorPosition(cursorPosition - 1)
        }
        break

      case 'Delete':
        if (selectionStart !== selectionEnd) {
          // Delete selection
          this.store.applyLocalOperation({
            type: 'delete',
            position: selectionStart,
            length: selectionEnd - selectionStart,
          })
          this.store.setCursorPosition(selectionStart)
          this.store.setSelection(selectionStart, selectionStart)
        } else if (cursorPosition < document.length) {
          // Delete character at cursor
          this.store.applyLocalOperation({
            type: 'delete',
            position: cursorPosition,
            length: 1,
          })
        }
        break

      case 'ArrowLeft':
        if (isShiftPressed) {
          // Extend selection left
          if (selectionEnd > 0) {
            this.store.setSelection(selectionStart, selectionEnd - 1)
          }
        } else {
          // Move cursor left
          this.store.setCursorPosition(Math.max(0, cursorPosition - 1))
          this.store.setSelection(cursorPosition - 1, cursorPosition - 1)
        }
        break

      case 'ArrowRight':
        if (isShiftPressed) {
          // Extend selection right
          if (selectionEnd < document.length) {
            this.store.setSelection(selectionStart, selectionEnd + 1)
          }
        } else {
          // Move cursor right
          this.store.setCursorPosition(Math.min(document.length, cursorPosition + 1))
          this.store.setSelection(cursorPosition + 1, cursorPosition + 1)
        }
        break

      case 'ArrowUp':
        // Move to previous line
        const lineStart = document.lastIndexOf('\n', cursorPosition - 1) + 1
        const prevLineStart = document.lastIndexOf('\n', lineStart - 2) + 1
        const cursorOffsetInLine = cursorPosition - lineStart
        const newPosition = prevLineStart + Math.min(cursorOffsetInLine, lineStart - prevLineStart - 1)
        this.store.setCursorPosition(newPosition)
        this.store.setSelection(newPosition, newPosition)
        break

      case 'ArrowDown':
        // Move to next line
        const currentLineStart = document.lastIndexOf('\n', cursorPosition - 1) + 1
        const currentLineEnd = document.indexOf('\n', cursorPosition)
        const nextLineStart = currentLineEnd + 1
        const nextLineEnd = document.indexOf('\n', nextLineStart)
        const offsetInLine = cursorPosition - currentLineStart
        const newPos = nextLineStart + Math.min(offsetInLine, nextLineEnd - nextLineStart)
        this.store.setCursorPosition(newPos)
        this.store.setSelection(newPos, newPos)
        break

      case 'Home':
        // Move to start of line
        const startOfLine = document.lastIndexOf('\n', cursorPosition - 1) + 1
        this.store.setCursorPosition(startOfLine)
        this.store.setSelection(startOfLine, startOfLine)
        break

      case 'End':
        // Move to end of line
        const endOfLine = document.indexOf('\n', cursorPosition)
        const finalPos = endOfLine === -1 ? document.length : endOfLine
        this.store.setCursorPosition(finalPos)
        this.store.setSelection(finalPos, finalPos)
        break

      default:
        break
    }
  }

  /**
   * Handle paste operation
   */
  handlePaste = async (content) => {
    const { selectionStart, selectionEnd, cursorPosition } = this.store

    // Delete selection if any
    if (selectionStart !== selectionEnd) {
      this.store.applyLocalOperation({
        type: 'delete',
        position: selectionStart,
        length: selectionEnd - selectionStart,
      })
    }

    // Insert pasted content
    const insertPosition = selectionStart !== selectionEnd ? selectionStart : cursorPosition
    this.store.applyLocalOperation({
      type: 'insert',
      position: insertPosition,
      content,
      length: content.length,
    })

    // Move cursor to end of inserted text
    const newCursorPosition = insertPosition + content.length
    this.store.setCursorPosition(newCursorPosition)
    this.store.setSelection(newCursorPosition, newCursorPosition)
  }

  /**
   * Handle copy operation
   */
  handleCopy = async () => {
    const { document, selectionStart, selectionEnd } = this.store

    if (selectionStart !== selectionEnd) {
      const selectedText = document.substring(selectionStart, selectionEnd)
      // Copy to clipboard
      // This would use react-native-clipboard
      return selectedText
    }
  }

  /**
   * Handle cut operation
   */
  handleCut = async () => {
    const { document, selectionStart, selectionEnd } = this.store

    if (selectionStart !== selectionEnd) {
      const selectedText = document.substring(selectionStart, selectionEnd)

      // Copy to clipboard
      // This would use react-native-clipboard

      // Delete selection
      this.store.applyLocalOperation({
        type: 'delete',
        position: selectionStart,
        length: selectionEnd - selectionStart,
      })

      this.store.setCursorPosition(selectionStart)
      this.store.setSelection(selectionStart, selectionStart)

      return selectedText
    }
  }
}

export default MobileGestureService
