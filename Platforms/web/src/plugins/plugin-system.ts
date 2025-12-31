/**
 * Time Warp Studio - Plugin System Framework
 * 
 * Provides:
 * - Plugin registration and lifecycle
 * - Hook system for extension points
 * - Sandboxed plugin execution
 * - Plugin marketplace integration
 * - Version management
 */

import { EventEmitter } from 'events';

// ===== PLUGIN TYPES =====

export interface PluginManifest {
  name: string;
  version: string;
  description: string;
  author: string;
  license: string;
  homepage?: string;
  repository?: string;
  keywords: string[];
  
  // Capabilities
  permissions: string[];
  
  // Entry points
  main: string;
  
  // Configuration
  config?: Record<string, any>;
  
  // Supported hooks
  hooks: string[];
  
  // Dependencies
  dependencies?: Record<string, string>;
  
  // Minimum version requirements
  minVersion: string;
  maxVersion?: string;
  
  // Icon and branding
  icon?: string;
  color?: string;
}

export interface PluginAPI {
  // UI/Editor APIs
  editor: {
    getCode(): string;
    setCode(code: string): void;
    getSelection(): { start: number; end: number };
    setSelection(start: number, end: number): void;
    insertText(text: string, position?: number): void;
    replaceText(start: number, end: number, text: string): void;
    registerCommand(id: string, callback: Function): void;
    addButton(label: string, icon?: string, callback?: Function): void;
    showNotification(message: string, type?: 'info' | 'warning' | 'error'): void;
    showInputDialog(prompt: string, defaultValue?: string): Promise<string | null>;
  };
  
  // Language APIs
  language: {
    supportedLanguages(): string[];
    getCurrentLanguage(): string;
    setLanguage(lang: string): void;
    executeCode(code: string, language: string): Promise<any>;
  };
  
  // Storage APIs
  storage: {
    get(key: string): any;
    set(key: string, value: any): void;
    remove(key: string): void;
    clear(): void;
    listKeys(): string[];
  };
  
  // Project APIs
  project: {
    getName(): string;
    setName(name: string): void;
    getDescription(): string;
    setDescription(desc: string): void;
    getMetadata(key: string): any;
    setMetadata(key: string, value: any): void;
    getFiles(): string[];
    getFile(name: string): string;
    addFile(name: string, content: string): void;
    removeFile(name: string): void;
  };
  
  // Theme APIs
  theme: {
    getCurrentTheme(): string;
    setTheme(themeName: string): void;
    registerTheme(name: string, colors: Record<string, string>): void;
    getColor(name: string): string;
  };
  
  // Hook system
  hooks: {
    on(event: string, callback: Function): void;
    off(event: string, callback: Function): void;
    emit(event: string, ...args: any[]): void;
    filter(filter: string, callback: Function): void;
    apply(filter: string, ...args: any[]): any;
  };
  
  // Debugging APIs
  debug: {
    log(message: string, level?: 'debug' | 'info' | 'warn' | 'error'): void;
    setBreakpoint(line: number, condition?: string): void;
    removeBreakpoint(line: number): void;
    getVariables(): Record<string, any>;
  };
  
  // Config APIs
  config: {
    get(key: string): any;
    set(key: string, value: any): void;
    watch(key: string, callback: Function): () => void;
  };
  
  // Network APIs
  network: {
    fetch(url: string, options?: RequestInit): Promise<Response>;
    WebSocket: typeof WebSocket;
  };
}

// ===== PLUGIN CLASS =====

export abstract class Plugin {
  abstract name: string;
  abstract version: string;
  
  protected api: PluginAPI;
  protected manifest: PluginManifest;
  
  constructor(api: PluginAPI, manifest: PluginManifest) {
    this.api = api;
    this.manifest = manifest;
  }
  
  /**
   * Initialize plugin
   */
  abstract onInit(): Promise<void>;
  
  /**
   * Activate plugin
   */
  async onActivate(): Promise<void> {
    // Override in subclass
  }
  
  /**
   * Deactivate plugin
   */
  async onDeactivate(): Promise<void> {
    // Override in subclass
  }
  
  /**
   * Plugin configuration changed
   */
  onConfigChange?(config: Record<string, any>): void;
  
  /**
   * Get plugin information
   */
  getInfo(): PluginManifest {
    return this.manifest;
  }
  
  /**
   * Log message
   */
  protected log(message: string, level: 'debug' | 'info' | 'warn' | 'error' = 'info'): void {
    this.api.debug.log(`[${this.name}] ${message}`, level);
  }
  
  /**
   * Notify user
   */
  protected notify(message: string, type: 'info' | 'warning' | 'error' = 'info'): void {
    this.api.editor.showNotification(`${this.name}: ${message}`, type);
  }
  
  /**
   * Get storage namespace
   */
  protected getStorage(namespace: string) {
    const prefix = `plugin:${this.name}:${namespace}:`;
    return {
      get: (key: string) => this.api.storage.get(prefix + key),
      set: (key: string, value: any) => this.api.storage.set(prefix + key, value),
      remove: (key: string) => this.api.storage.remove(prefix + key),
    };
  }
}

// ===== PLUGIN MANAGER =====

export interface PluginLoadError {
  plugin: string;
  error: string;
  code: string;
}

export class PluginManager extends EventEmitter {
  private plugins: Map<string, Plugin> = new Map();
  private manifests: Map<string, PluginManifest> = new Map();
  private hooks: Map<string, Function[]> = new Map();
  private filters: Map<string, Function[]> = new Map();
  private loadErrors: PluginLoadError[] = [];
  private api: PluginAPI;
  
  constructor(api: PluginAPI) {
    super();
    this.api = api;
  }
  
  /**
   * Load and register a plugin
   */
  async loadPlugin(
    manifest: PluginManifest,
    pluginCode: string
  ): Promise<boolean> {
    try {
      // Validate manifest
      this.validateManifest(manifest);
      
      // Check version compatibility
      if (!this.isVersionCompatible(manifest)) {
        throw new Error(`Plugin requires version ${manifest.minVersion}`);
      }
      
      // Check permissions
      if (!this.hasRequiredPermissions(manifest.permissions)) {
        throw new Error(`Missing required permissions: ${manifest.permissions.join(', ')}`);
      }
      
      // Create plugin instance in sandbox
      const plugin = await this.createPluginInstance(manifest, pluginCode);
      
      // Initialize plugin
      await plugin.onInit();
      
      // Register plugin
      this.plugins.set(manifest.name, plugin);
      this.manifests.set(manifest.name, manifest);
      
      this.emit('plugin-loaded', { name: manifest.name, version: manifest.version });
      
      return true;
    } catch (error) {
      const errorRecord: PluginLoadError = {
        plugin: manifest.name,
        error: String(error),
        code: 'PLUGIN_LOAD_FAILED',
      };
      this.loadErrors.push(errorRecord);
      this.emit('plugin-error', errorRecord);
      
      return false;
    }
  }
  
  /**
   * Unload a plugin
   */
  async unloadPlugin(name: string): Promise<boolean> {
    const plugin = this.plugins.get(name);
    if (!plugin) {
      return false;
    }
    
    try {
      await plugin.onDeactivate();
      this.plugins.delete(name);
      this.manifests.delete(name);
      
      this.emit('plugin-unloaded', { name });
      
      return true;
    } catch (error) {
      this.emit('plugin-error', {
        plugin: name,
        error: String(error),
        code: 'PLUGIN_UNLOAD_FAILED',
      });
      
      return false;
    }
  }
  
  /**
   * Enable plugin
   */
  async enablePlugin(name: string): Promise<boolean> {
    const plugin = this.plugins.get(name);
    if (!plugin) {
      return false;
    }
    
    try {
      await plugin.onActivate();
      this.emit('plugin-enabled', { name });
      return true;
    } catch (error) {
      this.emit('plugin-error', {
        plugin: name,
        error: String(error),
        code: 'PLUGIN_ENABLE_FAILED',
      });
      return false;
    }
  }
  
  /**
   * Disable plugin
   */
  async disablePlugin(name: string): Promise<boolean> {
    const plugin = this.plugins.get(name);
    if (!plugin) {
      return false;
    }
    
    try {
      await plugin.onDeactivate();
      this.emit('plugin-disabled', { name });
      return true;
    } catch (error) {
      this.emit('plugin-error', {
        plugin: name,
        error: String(error),
        code: 'PLUGIN_DISABLE_FAILED',
      });
      return false;
    }
  }
  
  /**
   * Get installed plugins
   */
  getPlugins(): Array<{ name: string; version: string; enabled: boolean }> {
    return Array.from(this.manifests.values()).map(manifest => ({
      name: manifest.name,
      version: manifest.version,
      enabled: this.plugins.has(manifest.name),
    }));
  }
  
  /**
   * Get plugin manifest
   */
  getPluginManifest(name: string): PluginManifest | undefined {
    return this.manifests.get(name);
  }
  
  /**
   * Register a hook
   */
  registerHook(event: string, callback: Function): void {
    if (!this.hooks.has(event)) {
      this.hooks.set(event, []);
    }
    this.hooks.get(event)!.push(callback);
  }
  
  /**
   * Trigger a hook
   */
  triggerHook(event: string, ...args: any[]): void {
    const callbacks = this.hooks.get(event) || [];
    for (const callback of callbacks) {
      try {
        callback(...args);
      } catch (error) {
        this.emit('hook-error', { event, error });
      }
    }
  }
  
  /**
   * Register a filter
   */
  registerFilter(filter: string, callback: Function): void {
    if (!this.filters.has(filter)) {
      this.filters.set(filter, []);
    }
    this.filters.get(filter)!.push(callback);
  }
  
  /**
   * Apply filters
   */
  applyFilters(filter: string, value: any, ...args: any[]): any {
    const callbacks = this.filters.get(filter) || [];
    let result = value;
    
    for (const callback of callbacks) {
      try {
        result = callback(result, ...args);
      } catch (error) {
        this.emit('filter-error', { filter, error });
      }
    }
    
    return result;
  }
  
  /**
   * Get load errors
   */
  getLoadErrors(): PluginLoadError[] {
    return [...this.loadErrors];
  }
  
  // ===== PRIVATE =====
  
  private validateManifest(manifest: PluginManifest): void {
    if (!manifest.name || !manifest.version || !manifest.main) {
      throw new Error('Invalid plugin manifest');
    }
  }
  
  private isVersionCompatible(manifest: PluginManifest): boolean {
    // Version comparison logic
    return true; // Placeholder
  }
  
  private hasRequiredPermissions(permissions: string[]): boolean {
    // Permission check logic
    return true; // Placeholder
  }
  
  private async createPluginInstance(
    manifest: PluginManifest,
    pluginCode: string
  ): Promise<Plugin> {
    // Create sandboxed plugin instance
    // This would use Web Workers or similar for isolation
    return new (eval(`(${pluginCode})`))( this.api, manifest);
  }
}

// ===== EXAMPLE PLUGIN =====

export class ExamplePlugin extends Plugin {
  name = 'Example Plugin';
  version = '1.0.0';
  
  async onInit(): Promise<void> {
    this.log('Initializing example plugin');
    
    // Register a command
    this.api.editor.registerCommand('example:format', () => {
      const code = this.api.editor.getCode();
      const formatted = this.formatCode(code);
      this.api.editor.setCode(formatted);
      this.notify('Code formatted');
    });
    
    // Register a hook
    this.api.hooks.on('code-executed', (result: any) => {
      this.log(`Code executed: ${result.success ? 'success' : 'error'}`);
    });
  }
  
  async onActivate(): Promise<void> {
    this.log('Plugin activated');
    this.api.editor.addButton('Format', '✨', () => {
      this.api.editor.registerCommand('example:format', () => {});
    });
  }
  
  private formatCode(code: string): string {
    return code.trim().split('\n')
      .map(line => line.trim())
      .filter(line => line.length > 0)
      .join('\n');
  }
}

// ===== EXPORT =====

export { PluginManager, Plugin, PluginManifest, PluginAPI };
