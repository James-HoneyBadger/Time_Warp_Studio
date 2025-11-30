/**
 * Time Warp Web IDE - Main Application
 * Entry point and application initialization
 */

class TimeWarpApp {
    constructor() {
        this.ui = null;
        this.initialized = false;
    }

    async initialize() {
        if (this.initialized) return;

        try {
            // Wait for DOM to be ready
            if (document.readyState === 'loading') {
                await new Promise(resolve => {
                    document.addEventListener('DOMContentLoaded', resolve);
                });
            }

            // Initialize UI
            this.ui = new TimeWarpUI();
            this.initialized = true;

            console.log('Time Warp Web IDE initialized successfully');

            // Show welcome message
            this.showWelcomeMessage();

            // Start periodic updates
            this.startPeriodicUpdates();

        } catch (error) {
            console.error('Failed to initialize Time Warp:', error);
            this.showInitializationError(error);
        }
    }

    showWelcomeMessage() {
        if (this.ui) {
            this.ui.addOutput('🌟 Time Warp 4.0.0 Web Edition Ready!', 'welcome');
            this.ui.addOutput('📚 Try the demo program or create your own!', 'info');
            this.ui.addOutput('💡 Tip: Use Ctrl+R to run, Ctrl+N for new file, Ctrl+S to save', 'info');
        }
    }

    showInitializationError(error) {
        const errorContainer = document.createElement('div');
        errorContainer.style.cssText = `
            position: fixed;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            background: #f8d7da;
            color: #721c24;
            padding: 20px;
            border-radius: 8px;
            border: 1px solid #f1aeb5;
            max-width: 400px;
            text-align: center;
            z-index: 9999;
        `;

        errorContainer.innerHTML = `
            <h3>Initialization Error</h3>
            <p>Failed to start Time Warp IDE:</p>
            <pre style="font-size: 12px; margin: 10px 0;">${error.message}</pre>
            <button onclick="location.reload()" style="
                background: #721c24;
                color: white;
                border: none;
                padding: 8px 16px;
                border-radius: 4px;
                cursor: pointer;
            ">Reload Page</button>
        `;

        document.body.appendChild(errorContainer);
    }

    startPeriodicUpdates() {
        // Update memory usage and other stats periodically
        setInterval(() => {
            if (this.ui && !this.ui.isRunning) {
                // Only update when not running to avoid interference
                this.updateApplicationStats();
            }
        }, 5000);
    }

    updateApplicationStats() {
        // Update timestamp in status bar
        const now = new Date();
        const timestamp = now.toLocaleTimeString();

        const timestampEl = document.getElementById('timestamp');
        if (timestampEl) {
            timestampEl.textContent = timestamp;
        }

        // Check for browser compatibility issues
        this.checkBrowserCompatibility();
    }

    checkBrowserCompatibility() {
        const issues = [];

        // Check for required features
        if (!window.Promise) issues.push('Promises');
        if (!window.fetch) issues.push('Fetch API');
        if (!document.querySelector) issues.push('Query Selector');
        if (!window.localStorage) issues.push('Local Storage');
        if (!document.createElement('canvas').getContext) issues.push('Canvas');

        if (issues.length > 0) {
            console.warn('Browser compatibility issues:', issues);
        }
    }

    // Static initialization method
    static async start() {
        const app = new TimeWarpApp();
        await app.initialize();
        return app;
    }
}

// Application configuration
const CONFIG = {
    version: '4.0.0',
    buildDate: '2024-01-01',
    features: {
        debugging: true,
        graphics: true,
        multiLanguage: true,
        performance: true,
        timeline: true,
        snippets: true,
        help: true
    },
    limits: {
        maxIterations: 10000,
        maxOutputLines: 1000,
        maxVariables: 1000,
        maxTimelineEntries: 500
    },
    graphics: {
        canvasWidth: 400,
        canvasHeight: 300,
        defaultTurtleSize: 10,
        defaultPenWidth: 2,
        colors: [
            'black', 'red', 'green', 'blue', 'yellow',
            'magenta', 'cyan', 'white', 'orange', 'purple',
            'pink', 'brown', 'gray', 'darkred', 'darkgreen'
        ]
    }
};

// Utility functions
const Utils = {
    // Escape HTML for safe display
    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    },

    // Format time duration
    formatDuration(milliseconds) {
        if (milliseconds < 1000) {
            return `${milliseconds.toFixed(0)}ms`;
        } else if (milliseconds < 60000) {
            return `${(milliseconds / 1000).toFixed(2)}s`;
        } else {
            const minutes = Math.floor(milliseconds / 60000);
            const seconds = ((milliseconds % 60000) / 1000).toFixed(0);
            return `${minutes}:${seconds.padStart(2, '0')}`;
        }
    },

    // Format number with commas
    formatNumber(num) {
        return num.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    },

    // Deep clone object
    deepClone(obj) {
        return JSON.parse(JSON.stringify(obj));
    },

    // Debounce function
    debounce(func, delay) {
        let timeoutId;
        return function (...args) {
            clearTimeout(timeoutId);
            timeoutId = setTimeout(() => func.apply(this, args), delay);
        };
    },

    // Throttle function
    throttle(func, delay) {
        let lastCall = 0;
        return function (...args) {
            const now = Date.now();
            if (now - lastCall >= delay) {
                lastCall = now;
                return func.apply(this, args);
            }
        };
    },

    // Generate unique ID
    generateId() {
        return '_' + Math.random().toString(36).substr(2, 9);
    },

    // Check if mobile device
    isMobile() {
        return /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
    },

    // Get browser info
    getBrowserInfo() {
        const ua = navigator.userAgent;
        let browser = 'Unknown';

        if (ua.includes('Chrome')) browser = 'Chrome';
        else if (ua.includes('Firefox')) browser = 'Firefox';
        else if (ua.includes('Safari')) browser = 'Safari';
        else if (ua.includes('Edge')) browser = 'Edge';

        return {
            browser,
            userAgent: ua,
            mobile: this.isMobile()
        };
    },

    // Local storage helpers
    storage: {
        get(key, defaultValue = null) {
            try {
                const value = localStorage.getItem(`timewarp_${key}`);
                return value ? JSON.parse(value) : defaultValue;
            } catch (error) {
                console.warn('Storage get error:', error);
                return defaultValue;
            }
        },

        set(key, value) {
            try {
                localStorage.setItem(`timewarp_${key}`, JSON.stringify(value));
                return true;
            } catch (error) {
                console.warn('Storage set error:', error);
                return false;
            }
        },

        remove(key) {
            try {
                localStorage.removeItem(`timewarp_${key}`);
                return true;
            } catch (error) {
                console.warn('Storage remove error:', error);
                return false;
            }
        },

        clear() {
            try {
                const keys = Object.keys(localStorage).filter(key => key.startsWith('timewarp_'));
                keys.forEach(key => localStorage.removeItem(key));
                return true;
            } catch (error) {
                console.warn('Storage clear error:', error);
                return false;
            }
        }
    }
};

// Error handling
window.addEventListener('error', (event) => {
    console.error('Global error:', event.error);

    // Show user-friendly error for critical failures
    if (event.error && event.error.message) {
        const message = event.error.message;
        if (message.includes('TimeWarp') || message.includes('interpreter') || message.includes('graphics')) {
            const errorDiv = document.createElement('div');
            errorDiv.style.cssText = `
                position: fixed;
                bottom: 20px;
                right: 20px;
                background: #f8d7da;
                color: #721c24;
                padding: 15px;
                border-radius: 8px;
                border: 1px solid #f1aeb5;
                max-width: 300px;
                z-index: 9999;
                box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            `;

            errorDiv.innerHTML = `
                <strong>Error:</strong> ${Utils.escapeHtml(message)}
                <button onclick="this.parentElement.remove()" style="
                    float: right;
                    background: none;
                    border: none;
                    font-size: 18px;
                    cursor: pointer;
                    color: #721c24;
                ">×</button>
            `;

            document.body.appendChild(errorDiv);

            // Auto-remove after 10 seconds
            setTimeout(() => {
                if (errorDiv.parentElement) {
                    errorDiv.remove();
                }
            }, 10000);
        }
    }
});

// Unhandled promise rejection handling
window.addEventListener('unhandledrejection', (event) => {
    console.error('Unhandled promise rejection:', event.reason);
    event.preventDefault(); // Prevent default browser behavior
});

// Performance monitoring
const Performance = {
    marks: {},

    start(name) {
        this.marks[name] = performance.now();
    },

    end(name) {
        if (this.marks[name]) {
            const duration = performance.now() - this.marks[name];
            delete this.marks[name];
            return duration;
        }
        return 0;
    },

    measure(name, fn) {
        this.start(name);
        const result = fn();
        const duration = this.end(name);
        console.debug(`Performance [${name}]: ${duration.toFixed(2)}ms`);
        return result;
    },

    async measureAsync(name, fn) {
        this.start(name);
        const result = await fn();
        const duration = this.end(name);
        console.debug(`Performance [${name}]: ${duration.toFixed(2)}ms`);
        return result;
    }
};

// Export for global access
window.TimeWarpApp = TimeWarpApp;
window.CONFIG = CONFIG;
window.Utils = Utils;
window.Performance = Performance;

// Auto-start when page loads
(async function () {
    try {
        Performance.start('app-initialization');

        // Wait for page load
        if (document.readyState !== 'complete') {
            await new Promise(resolve => {
                window.addEventListener('load', resolve);
            });
        }

        // Initialize application
        window.app = await TimeWarpApp.start();

        const initTime = Performance.end('app-initialization');
        console.log(`Time Warp initialized in ${initTime.toFixed(2)}ms`);

        // Save initialization info
        Utils.storage.set('last_init', {
            timestamp: Date.now(),
            duration: initTime,
            browser: Utils.getBrowserInfo(),
            config: CONFIG
        });

    } catch (error) {
        console.error('Failed to start Time Warp application:', error);
    }
})();