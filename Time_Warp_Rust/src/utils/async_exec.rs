//! Async execution support for Time Warp IDE

use anyhow::Result;
use parking_lot::Mutex;
use std::sync::Arc;
use tokio::sync::mpsc;

// Note: These types are for future async execution features
// Currently unused but will be needed for non-blocking program execution
#[allow(dead_code)]
pub struct AsyncExecutor {
    runtime: tokio::runtime::Runtime,
}

impl Default for AsyncExecutor {
    fn default() -> Self {
        // Fall back to a minimal runtime; log error instead of panicking
        match Self::new() {
            Ok(exec) => exec,
            Err(e) => {
                eprintln!("❌ AsyncExecutor init failed: {e}; using dummy runtime");
                // Provide a dummy runtime that will error on use
                AsyncExecutor {
                    runtime: tokio::runtime::Builder::new_current_thread()
                        .build()
                        .expect("tokio build fallback"),
                }
            }
        }
    }
}

impl AsyncExecutor {
    #[allow(dead_code)]
    pub fn new() -> Result<Self> {
        // Use current_thread runtime - tasks run cooperatively on this thread
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        Ok(Self { runtime })
    }

    #[allow(dead_code)]
    pub fn execute_async<F>(&self, code: String, mut callback: F) -> Result<()>
    where
        F: FnMut(ExecutionEvent) + Send + 'static,
    {
        let (tx, mut rx) = mpsc::channel(100);

        // Spawn task using runtime handle
        let handle = self.runtime.handle().clone();
        handle.spawn(async move {
            let _ = tx.send(ExecutionEvent::Started).await;

            for (line_num, line) in code.lines().enumerate() {
                let _ = tx
                    .send(ExecutionEvent::LineExecuted {
                        line_number: line_num + 1,
                        line: line.to_string(),
                    })
                    .await;

                tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
            }

            let _ = tx.send(ExecutionEvent::Completed).await;
        });

        handle.spawn(async move {
            while let Some(event) = rx.recv().await {
                callback(event);
            }
        });

        Ok(())
    }

    #[allow(dead_code)]
    pub fn execute_with_timeout(&self, code: String, timeout_ms: u64) -> Result<ExecutionResult> {
        self.runtime.block_on(async {
            let result =
                tokio::time::timeout(tokio::time::Duration::from_millis(timeout_ms), async {
                    Self::execute_code_internal(code).await
                })
                .await;

            match result {
                Ok(r) => r,
                Err(_) => Err(anyhow::anyhow!("Execution timeout")),
            }
        })
    }

    #[allow(dead_code)]
    async fn execute_code_internal(code: String) -> Result<ExecutionResult> {
        Ok(ExecutionResult {
            output: vec![format!("Executed {} lines", code.lines().count())],
            variables: std::collections::HashMap::new(),
            execution_time_ms: 0,
        })
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ExecutionEvent {
    Started,
    LineExecuted { line_number: usize, line: String },
    Output(String),
    Error(String),
    Completed,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ExecutionResult {
    pub output: Vec<String>,
    pub variables: std::collections::HashMap<String, f64>,
    pub execution_time_ms: u64,
}

#[allow(dead_code)]
pub struct SharedExecutor {
    executor: Arc<Mutex<AsyncExecutor>>,
}

impl Default for SharedExecutor {
    fn default() -> Self {
        match Self::new() {
            Ok(shared) => shared,
            Err(e) => {
                eprintln!("❌ SharedExecutor init failed: {e}; constructing inert executor");
                // Inert executor: AsyncExecutor new() already returns Result, emulate empty
                let runtime = tokio::runtime::Builder::new_current_thread()
                    .build()
                    .expect("tokio build fallback");
                SharedExecutor {
                    executor: std::sync::Arc::new(parking_lot::Mutex::new(AsyncExecutor {
                        runtime,
                    })),
                }
            }
        }
    }
}

impl SharedExecutor {
    #[allow(dead_code)]
    pub fn new() -> Result<Self> {
        Ok(Self {
            executor: Arc::new(Mutex::new(AsyncExecutor::new()?)),
        })
    }

    #[allow(dead_code)]
    pub fn execute<F>(&self, code: String, callback: F) -> Result<()>
    where
        F: FnMut(ExecutionEvent) + Send + 'static,
    {
        self.executor.lock().execute_async(code, callback)
    }
}
