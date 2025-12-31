/**
 * Advanced Collaboration Features - Merge Conflicts & Real-time Diffs
 * 
 * Provides:
 * - Real-time diff generation
 * - Three-way merge with conflict resolution
 * - Collaborative editing with automatic merging
 * - Change history and reversions
 * - Conflict visualization
 */

import { EventEmitter } from 'events';

// ===== DIFF TYPES =====

export interface DiffLine {
  type: 'add' | 'remove' | 'context' | 'conflict';
  content: string;
  lineNumber: number;
  originalLineNumber?: number;
}

export interface FileDiff {
  fileName: string;
  oldVersion: string;
  newVersion: string;
  additions: number;
  deletions: number;
  lines: DiffLine[];
  similarity: number; // 0-1
}

export interface ConflictBlock {
  id: string;
  startLine: number;
  endLine: number;
  currentVersion: string[];
  theirVersion: string[];
  baseVersion?: string[];
  markerLines: {
    start: number;
    separator: number;
    end: number;
  };
}

export interface MergeResult {
  success: boolean;
  conflicts: ConflictBlock[];
  merged: string;
  stats: {
    linesProcessed: number;
    conflictLines: number;
    resolutionsApplied: number;
  };
}

// ===== DIFF ENGINE =====

export class DiffEngine {
  /**
   * Generate unified diff between two strings
   */
  static generateDiff(oldContent: string, newContent: string): FileDiff {
    const oldLines = oldContent.split('\n');
    const newLines = newContent.split('\n');
    
    const lcs = this.computeLCS(oldLines, newLines);
    const lines: DiffLine[] = [];
    
    let oldIdx = 0;
    let newIdx = 0;
    let lineNum = 1;
    
    for (const match of lcs) {
      // Context before match
      while (oldIdx < match.oldIndex || newIdx < match.newIndex) {
        if (oldIdx < match.oldIndex && newIdx < match.newIndex) {
          // Matching line
          lines.push({
            type: 'context',
            content: oldLines[oldIdx],
            lineNumber: lineNum,
            originalLineNumber: oldIdx + 1,
          });
          oldIdx++;
          newIdx++;
        } else if (oldIdx < match.oldIndex) {
          // Line removed
          lines.push({
            type: 'remove',
            content: oldLines[oldIdx],
            lineNumber: lineNum,
            originalLineNumber: oldIdx + 1,
          });
          oldIdx++;
        } else {
          // Line added
          lines.push({
            type: 'add',
            content: newLines[newIdx],
            lineNumber: lineNum,
          });
          newIdx++;
        }
        lineNum++;
      }
      
      // Process matching line
      lines.push({
        type: 'context',
        content: oldLines[oldIdx],
        lineNumber: lineNum,
        originalLineNumber: oldIdx + 1,
      });
      oldIdx++;
      newIdx++;
      lineNum++;
    }
    
    // Remaining lines
    while (oldIdx < oldLines.length) {
      lines.push({
        type: 'remove',
        content: oldLines[oldIdx],
        lineNumber: lineNum,
        originalLineNumber: oldIdx + 1,
      });
      oldIdx++;
      lineNum++;
    }
    
    while (newIdx < newLines.length) {
      lines.push({
        type: 'add',
        content: newLines[newIdx],
        lineNumber: lineNum,
      });
      newIdx++;
      lineNum++;
    }
    
    return {
      fileName: 'code',
      oldVersion: oldContent,
      newVersion: newContent,
      additions: lines.filter(l => l.type === 'add').length,
      deletions: lines.filter(l => l.type === 'remove').length,
      lines,
      similarity: this.calculateSimilarity(oldLines, newLines),
    };
  }
  
  /**
   * Compute longest common subsequence
   */
  private static computeLCS(
    oldLines: string[],
    newLines: string[]
  ): Array<{ oldIndex: number; newIndex: number }> {
    const m = oldLines.length;
    const n = newLines.length;
    const dp: number[][] = Array(m + 1).fill(0).map(() => Array(n + 1).fill(0));
    
    for (let i = 1; i <= m; i++) {
      for (let j = 1; j <= n; j++) {
        if (oldLines[i - 1] === newLines[j - 1]) {
          dp[i][j] = dp[i - 1][j - 1] + 1;
        } else {
          dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
        }
      }
    }
    
    // Backtrack to find actual LCS
    const result: Array<{ oldIndex: number; newIndex: number }> = [];
    let i = m, j = n;
    
    while (i > 0 && j > 0) {
      if (oldLines[i - 1] === newLines[j - 1]) {
        result.unshift({ oldIndex: i - 1, newIndex: j - 1 });
        i--;
        j--;
      } else if (dp[i - 1][j] > dp[i][j - 1]) {
        i--;
      } else {
        j--;
      }
    }
    
    return result;
  }
  
  /**
   * Calculate similarity between two code blocks (0-1)
   */
  private static calculateSimilarity(oldLines: string[], newLines: string[]): number {
    const commonLines = oldLines.filter((line, idx) => newLines[idx] === line).length;
    const total = Math.max(oldLines.length, newLines.length);
    return total === 0 ? 1 : commonLines / total;
  }
}

// ===== MERGE ENGINE =====

export class MergeEngine {
  /**
   * Three-way merge with conflict detection
   */
  static merge(
    base: string,
    current: string,
    incoming: string
  ): MergeResult {
    const baseLines = base.split('\n');
    const currentLines = current.split('\n');
    const incomingLines = incoming.split('\n');
    
    const conflicts: ConflictBlock[] = [];
    const merged: string[] = [];
    let stats = {
      linesProcessed: 0,
      conflictLines: 0,
      resolutionsApplied: 0,
    };
    
    // Compute diffs
    const baseToCurrent = DiffEngine.generateDiff(base, current).lines;
    const baseToIncoming = DiffEngine.generateDiff(base, incoming).lines;
    
    // Three-way merge algorithm
    let i = 0, j = 0;
    
    while (i < baseToCurrent.length || j < baseToIncoming.length) {
      const currentLine = baseToCurrent[i];
      const incomingLine = baseToIncoming[j];
      
      // If both made same change, merge automatically
      if (currentLine && incomingLine &&
          currentLine.type === incomingLine.type &&
          currentLine.content === incomingLine.content) {
        merged.push(currentLine.content);
        i++;
        j++;
        stats.resolutionsApplied++;
      }
      // If only one side changed, apply the change
      else if (!incomingLine ||
               (currentLine && currentLine.type !== 'context')) {
        merged.push(currentLine!.content);
        i++;
      }
      else if (!currentLine ||
               (incomingLine && incomingLine.type !== 'context')) {
        merged.push(incomingLine!.content);
        j++;
      }
      // Both sides changed differently - conflict
      else if (currentLine && incomingLine &&
               currentLine.content !== incomingLine.content) {
        const conflict: ConflictBlock = {
          id: `conflict-${conflicts.length}`,
          startLine: merged.length,
          endLine: merged.length + 1,
          currentVersion: [currentLine.content],
          theirVersion: [incomingLine.content],
          baseVersion: [baseLines[i]],
          markerLines: {
            start: merged.length,
            separator: merged.length + 1,
            end: merged.length + 2,
          },
        };
        
        conflicts.push(conflict);
        merged.push(`<<<<<<< HEAD`);
        merged.push(currentLine.content);
        merged.push(`=======`);
        merged.push(incomingLine.content);
        merged.push(`>>>>>>> incoming`);
        
        stats.conflictLines += 3;
        i++;
        j++;
      }
      else {
        i++;
        j++;
      }
      
      stats.linesProcessed++;
    }
    
    return {
      success: conflicts.length === 0,
      conflicts,
      merged: merged.join('\n'),
      stats,
    };
  }
  
  /**
   * Resolve a conflict manually
   */
  static resolveConflict(
    mergedCode: string,
    conflictId: string,
    resolution: 'current' | 'incoming' | 'combined' | 'custom',
    customContent?: string
  ): string {
    const lines = mergedCode.split('\n');
    let inConflict = false;
    let conflictStart = -1;
    let conflictCount = 0;
    const result: string[] = [];
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      if (line.startsWith('<<<<<<<')) {
        if (conflictCount === parseInt(conflictId.split('-')[1])) {
          inConflict = true;
          conflictStart = result.length;
        }
        conflictCount++;
      } else if (line.startsWith('=======') && inConflict) {
        // Handle based on resolution type
        if (resolution === 'current') {
          // Keep lines before separator
        } else if (resolution === 'incoming') {
          // Remove lines before separator
          result.splice(conflictStart);
        }
      } else if (line.startsWith('>>>>>>>') && inConflict) {
        inConflict = false;
        
        // Add custom content if provided
        if (resolution === 'custom' && customContent) {
          result.push(customContent);
        }
      } else if (!inConflict) {
        result.push(line);
      } else if (resolution === 'incoming' && line.startsWith('=======')) {
        // Start including incoming version
        result.push(line);
      } else if (resolution === 'incoming' && !line.startsWith('<<<<<<<')) {
        result.push(line);
      }
    }
    
    return result.join('\n');
  }
}

// ===== COLLABORATION MANAGER =====

export interface Change {
  id: string;
  userId: string;
  timestamp: Date;
  type: 'insert' | 'delete' | 'modify';
  content: string;
  startLine: number;
  endLine: number;
  undoable: boolean;
}

export class CollaborationManager extends EventEmitter {
  private documentHistory: Map<string, string[]> = new Map();
  private changes: Change[] = [];
  private pendingMerges: Map<string, MergeResult> = new Map();
  
  /**
   * Record a change
   */
  recordChange(change: Change): void {
    this.changes.push(change);
    this.emit('change', change);
  }
  
  /**
   * Get change history for a line range
   */
  getLineHistory(startLine: number, endLine: number): Change[] {
    return this.changes.filter(c =>
      (c.startLine >= startLine && c.startLine <= endLine) ||
      (c.endLine >= startLine && c.endLine <= endLine)
    );
  }
  
  /**
   * Get blame information (who changed each line)
   */
  getBlame(code: string): Map<number, { user: string; timestamp: Date }> {
    const lines = code.split('\n');
    const blame = new Map<number, { user: string; timestamp: Date }>();
    
    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const lineChanges = this.changes.filter(c =>
        c.startLine <= lineNum && c.endLine >= lineNum
      );
      
      if (lineChanges.length > 0) {
        const lastChange = lineChanges[lineChanges.length - 1];
        blame.set(lineNum, {
          user: lastChange.userId,
          timestamp: lastChange.timestamp,
        });
      }
    }
    
    return blame;
  }
  
  /**
   * Merge incoming changes
   */
  attemptMerge(
    base: string,
    current: string,
    incoming: string
  ): MergeResult {
    const result = MergeEngine.merge(base, current, incoming);
    
    if (!result.success) {
      // Store for manual resolution
      const mergeId = `merge-${Date.now()}`;
      this.pendingMerges.set(mergeId, result);
      this.emit('merge-conflict', { mergeId, conflicts: result.conflicts });
    }
    
    return result;
  }
  
  /**
   * Get pending merges requiring manual resolution
   */
  getPendingMerges(): Map<string, MergeResult> {
    return this.pendingMerges;
  }
  
  /**
   * Resolve pending merge
   */
  resolveMerge(mergeId: string, resolutions: Map<string, 'current' | 'incoming' | 'custom'>): string {
    const merge = this.pendingMerges.get(mergeId);
    if (!merge) {
      throw new Error(`Merge ${mergeId} not found`);
    }
    
    let resolved = merge.merged;
    
    for (const [conflictId, resolution] of resolutions) {
      if (resolution === 'current' || resolution === 'incoming') {
        resolved = MergeEngine.resolveConflict(
          resolved,
          conflictId,
          resolution
        );
      }
    }
    
    this.pendingMerges.delete(mergeId);
    this.emit('merge-resolved', { mergeId, resolved });
    
    return resolved;
  }
  
  /**
   * Get diff for a time range
   */
  getDiffForTimeRange(startTime: Date, endTime: Date): Change[] {
    return this.changes.filter(c =>
      c.timestamp >= startTime && c.timestamp <= endTime
    );
  }
  
  /**
   * Undo a specific change
   */
  undoChange(changeId: string): boolean {
    const changeIndex = this.changes.findIndex(c => c.id === changeId);
    if (changeIndex === -1) {
      return false;
    }
    
    const change = this.changes[changeIndex];
    if (!change.undoable) {
      return false;
    }
    
    this.changes.splice(changeIndex, 1);
    this.emit('change-undone', change);
    
    return true;
  }
  
  /**
   * Get activity summary for team
   */
  getActivitySummary(): {
    totalChanges: number;
    byUser: Map<string, number>;
    byType: Map<string, number>;
    lastActivity: Date | null;
  } {
    const byUser = new Map<string, number>();
    const byType = new Map<string, number>();
    
    for (const change of this.changes) {
      byUser.set(change.userId, (byUser.get(change.userId) || 0) + 1);
      byType.set(change.type, (byType.get(change.type) || 0) + 1);
    }
    
    return {
      totalChanges: this.changes.length,
      byUser,
      byType,
      lastActivity: this.changes.length > 0 ?
        this.changes[this.changes.length - 1].timestamp :
        null,
    };
  }
}

// ===== EXPORT =====

export { DiffEngine, MergeEngine, CollaborationManager };
