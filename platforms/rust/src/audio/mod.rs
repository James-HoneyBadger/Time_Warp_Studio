//! Audio system for Time Warp IDE
//! Supports sound playback and BASIC-style music strings

use anyhow::Result;
use std::collections::HashMap;
use std::path::PathBuf;

#[cfg(feature = "audio")]
use rodio::{Decoder, OutputStream, Sink};

pub struct AudioMixer {
    #[cfg(feature = "audio")]
    _stream: Option<OutputStream>,
    #[cfg(feature = "audio")]
    sink: Option<Sink>,
    sounds: HashMap<String, PathBuf>,
}

impl AudioMixer {
    pub fn new() -> Self {
        #[cfg(feature = "audio")]
        {
            if let Ok((_stream, stream_handle)) = OutputStream::try_default() {
                if let Ok(sink) = Sink::try_new(&stream_handle) {
                    return Self {
                        _stream: Some(_stream),
                        sink: Some(sink),
                        sounds: HashMap::new(),
                    };
                }
            }
        }

        Self {
            #[cfg(feature = "audio")]
            _stream: None,
            #[cfg(feature = "audio")]
            sink: None,
            sounds: HashMap::new(),
        }
    }

    pub fn register_sound(&mut self, name: String, path: PathBuf) {
        self.sounds.insert(name, path);
    }

    pub fn play_sound(&self, name: &str) -> Result<()> {
        #[cfg(feature = "audio")]
        {
            if let Some(path) = self.sounds.get(name) {
                if let Some(sink) = &self.sink {
                    let file = std::fs::File::open(path)?;
                    let source = Decoder::new(std::io::BufReader::new(file))?;
                    sink.append(source);
                }
            }
        }

        #[cfg(not(feature = "audio"))]
        {
            let _ = (name,);
            print!("\x07");
        }

        Ok(())
    }

    pub fn beep(&self) {
        print!("\x07");
    }

    pub fn play_music_string(&self, music: &str) -> Result<()> {
        // Use tokio async sleep instead of blocking thread::sleep
        // For now, avoid blocking; in future use tokio runtime or async version
        for _note in music.split_whitespace() {
            self.beep();
            // TODO: Replace with tokio::time::sleep in async context
            // Non-blocking approach: skip sleep in batch mode, or schedule async
        }
        Ok(())
    }
}

impl Default for AudioMixer {
    fn default() -> Self {
        Self::new()
    }
}
