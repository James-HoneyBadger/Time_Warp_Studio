use thiserror::Error;

// Custom error types for Time Warp IDE
// Currently using anyhow::Result in most places, but these are available for typed errors
#[allow(dead_code)]
#[derive(Error, Debug)]
#[allow(clippy::enum_variant_names)]
pub enum TimeWarpError {
    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Runtime error: {0}")]
    RuntimeError(String),

    #[error("File error: {0}")]
    FileError(#[from] std::io::Error),

    #[error("Expression error: {0}")]
    ExpressionError(String),
}
