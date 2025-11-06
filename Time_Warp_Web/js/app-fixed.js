/**
 * Time Warp Web IDE - Fixed Application Initialization
 * Entry point with better error handling and dependency management
 */

// Global error tracking
const initErrors = [];

// Safe class instantiation helper
function safeInstantiate(className, constructor, ...args) {
    try {
        return new constructor(...args);
    } catch (error) {
        const errorMsg = `Failed to instantiate ${className}: ${error.message}`;
        console.error(errorMsg, error);
        initErrors.push(errorMsg);
        return null;
    }
}

// Check if all required classes are available
function checkDependencies() {
    const required = [
        { name: 'TimeWarpInterpreter', class: TimeWarpInterpreter },
        { name: 'TurtleGraphics', class: TurtleGraphics },
        { name: 'TimeWarpUI', class: TimeWarpUI }
    ];

    const missing = [];

    for (const dep of required) {
        if (typeof dep.class === 'undefined') {
            missing.push(dep.name);
        }
    }

    return missing;
}

class TimeWarpApp {
    constructor() {
        this.ui = null;
        this.initialized = false;
        this.retryCount = 0;
        this.maxRetries = 3;
    }

    async initialize() {
        if (this.initialized) return;

        console.log('Starting Time Warp initialization...');

        try {
            // Wait for DOM to be ready
            await this.waitForDOM();

            // Check dependencies with retries
            await this.waitForDependencies();

            // Initialize UI with error handling
            await this.initializeUI();

            this.initialized = true;
            console.log('Time Warp Web IDE initialized successfully');

            // Show success message
            this.showWelcomeMessage();

            // Start periodic updates
            this.startPeriodicUpdates();

        } catch (error) {
            console.error('Failed to initialize Time Warp:', error);
            this.showInitializationError(error);
        }
    }

    async waitForDOM() {
        if (document.readyState === 'loading') {
            await new Promise(resolve => {
                document.addEventListener('DOMContentLoaded', resolve);
            });
        }
    }

    async waitForDependencies() {
        const maxWait = 5000; // 5 seconds
        const checkInterval = 100; // 100ms
        let waited = 0;

        while (waited < maxWait) {
            const missing = checkDependencies();

            if (missing.length === 0) {
                console.log('All dependencies loaded successfully');
                return;
            }

            console.log(`Waiting for dependencies: ${missing.join(', ')}`);

            await new Promise(resolve => setTimeout(resolve, checkInterval));
            waited += checkInterval;
        }

        // Final check
        const missing = checkDependencies();
        if (missing.length > 0) {
            throw new Error(`Missing dependencies after ${maxWait}ms: ${missing.join(', ')}`);
        }
    }

    async initializeUI() {
        // Check required DOM elements
        const requiredElements = [
            'codeEditor',
            'output',
            'runBtn',
            'stopBtn',
            'debugBtn'
        ];

        const missingElements = requiredElements.filter(id => !document.getElementById(id));

        if (missingElements.length > 0) {
            throw new Error(`Missing required DOM elements: ${missingElements.join(', ')}`);
        }

        // Initialize UI with error handling
        this.ui = safeInstantiate('TimeWarpUI', TimeWarpUI);

        if (!this.ui) {
            throw new Error('Failed to create TimeWarpUI instance');
        }

        console.log('UI initialized successfully');
    }

    showWelcomeMessage() {
        if (this.ui && this.ui.addOutput) {
            try {
                this.ui.addOutput('🌟 Time Warp 2.1.0 Web Edition Ready!', 'welcome');
                this.ui.addOutput('📚 Try the demo program or create your own!', 'info');
                this.ui.addOutput('💡 Tip: Use Ctrl+R to run, Ctrl+N for new file, Ctrl+S to save', 'info');
            } catch (error) {
                console.warn('Could not show welcome message:', error);
            }
        }
    }

    showInitializationError(error) {
        console.error('Initialization error details:', {
            message: error.message,
            stack: error.stack,
            initErrors: initErrors
        });

        // Create error display
        const errorContainer = document.createElement('div');
        errorContainer.style.cssText = `
            position: fixed;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            background: #f8d7da;
            color: #721c24;
            padding: 30px;
            border-radius: 10px;
            border: 2px solid #f1aeb5;
            max-width: 500px;
            text-align: center;
            z-index: 9999;
            box-shadow: 0 4px 20px rgba(0,0,0,0.3);
        `;

        const allErrors = [error.message, ...initErrors].join('<br>');

        errorContainer.innerHTML = `
            <h2>⚠️ Time Warp Initialization Failed</h2>
            <p><strong>Error Details:</strong></p>
            <div style="background: #fff; color: #000; padding: 15px; border-radius: 5px; margin: 15px 0; font-size: 14px; text-align: left;">
                ${allErrors}
            </div>
            <div style="margin-top: 20px;">
                <button onclick="location.reload()" style="
                    background: #721c24;
                    color: white;
                    border: none;
                    padding: 12px 24px;
                    border-radius: 5px;
                    cursor: pointer;
                    font-size: 16px;
                    margin: 0 10px;
                ">🔄 Reload Page</button>
                <button onclick="this.parentElement.parentElement.remove()" style="
                    background: #6c757d;
                    color: white;
                    border: none;
                    padding: 12px 24px;
                    border-radius: 5px;
                    cursor: pointer;
                    font-size: 16px;
                    margin: 0 10px;
                ">❌ Dismiss</button>
            </div>
            <div style="margin-top: 15px; font-size: 12px; color: #6c757d;">
                Try using the minimal version: <a href="minimal.html">minimal.html</a>
            </div>
        `;

        // Remove any existing error containers
        const existingErrors = document.querySelectorAll('[data-timewarp-error]');
        existingErrors.forEach(el => el.remove());

        errorContainer.setAttribute('data-timewarp-error', 'true');
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
        try {
            // Update timestamp in status bar
            const now = new Date();
            const timestamp = now.toLocaleTimeString();

            const timestampEl = document.getElementById('timestamp');
            if (timestampEl) {
                timestampEl.textContent = timestamp;
            }

            // Check for browser compatibility issues
            this.checkBrowserCompatibility();
        } catch (error) {
            console.warn('Error updating application stats:', error);
        }
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

    // Static initialization method with retry logic
    static async start() {
        const app = new TimeWarpApp();

        try {
            await app.initialize();
            return app;
        } catch (error) {
            // Retry logic
            if (app.retryCount < app.maxRetries) {
                app.retryCount++;
                console.log(`Initialization failed, retrying... (${app.retryCount}/${app.maxRetries})`);

                // Wait a bit before retrying
                await new Promise(resolve => setTimeout(resolve, 1000));

                return TimeWarpApp.start();
            } else {
                console.error('Max retries exceeded, initialization failed permanently');
                throw error;
            }
        }
    }
}

// Enhanced error handling
window.addEventListener('error', (event) => {
    console.error('Global error:', event.error);
    initErrors.push(`Global error: ${event.error.message}`);

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
                <strong>Runtime Error:</strong> ${message.substring(0, 100)}${message.length > 100 ? '...' : ''}
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
    initErrors.push(`Promise rejection: ${event.reason}`);
    event.preventDefault(); // Prevent default browser behavior
});

// Configuration and utility exports
window.TimeWarpApp = TimeWarpApp;
window.CONFIG = {
    version: '2.1.0',
    buildDate: '2024-10-31',
    retryCount: 3,
    initTimeout: 10000
};

// Auto-start with improved error handling
(async function () {
    console.log('Time Warp Web IDE starting...');

    try {
        // Wait for page to fully load
        if (document.readyState !== 'complete') {
            await new Promise(resolve => {
                window.addEventListener('load', resolve);
            });
        }

        console.log('Page loaded, initializing application...');

        // Start application with timeout
        const initPromise = TimeWarpApp.start();
        const timeoutPromise = new Promise((_, reject) => {
            setTimeout(() => reject(new Error('Initialization timeout')), window.CONFIG.initTimeout);
        });

        window.app = await Promise.race([initPromise, timeoutPromise]);

        console.log('Time Warp initialized successfully');

    } catch (error) {
        console.error('Failed to start Time Warp application:', error);

        // Show fallback interface
        const fallbackHtml = `
            <div style="position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.8); z-index: 10000; display: flex; align-items: center; justify-content: center;">
                <div style="background: white; padding: 40px; border-radius: 10px; text-align: center; max-width: 500px;">
                    <h2>⚠️ Time Warp Failed to Load</h2>
                    <p>The full IDE couldn't initialize. Try these alternatives:</p>
                    <div style="margin: 20px 0;">
                        <a href="minimal.html" style="display: inline-block; background: #007bff; color: white; padding: 12px 24px; text-decoration: none; border-radius: 5px; margin: 5px;">Minimal Version</a>
                        <a href="diagnostic.html" style="display: inline-block; background: #17a2b8; color: white; padding: 12px 24px; text-decoration: none; border-radius: 5px; margin: 5px;">Diagnostic Test</a>
                    </div>
                    <button onclick="location.reload()" style="background: #28a745; color: white; border: none; padding: 12px 24px; border-radius: 5px; cursor: pointer; margin: 5px;">Reload Page</button>
                </div>
            </div>
        `;

        document.body.insertAdjacentHTML('beforeend', fallbackHtml);
    }
})();