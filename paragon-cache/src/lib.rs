use std::{
    path::{PathBuf, Path},
    io::Error,
    fs::read_to_string,
};

/// A unique identifier of the file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileId(usize);

/// A struct which cache files.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileCache {
    caches: Vec<(PathBuf, String)>,
}

impl FileCache {
    /// Create a new, empty file cache.
    pub fn new() -> Self {
        Self { caches: Vec::new() }
    }

    /// Cache file and return unique identifier of the file.
    /// If the file is already cached, return the same id.
    pub fn cache<P: AsRef<Path>>(&mut self, path: P) -> Result<FileId, Error> {
        // Find the id associated with given path.
        for (p, id) in self.caches.iter().map(|v| v.0.as_path()).zip(0..) {
            if p == path.as_ref() {
                return Ok(FileId(id))
            }
        }

        // If the file is not cached, cache it.
        let id = FileId(self.caches.len());
        let content = read_to_string(&path)?;
        self.caches.push((path.as_ref().to_path_buf(), content));
        Ok(id)
    }

    /// Fetch file path associated with the id.
    pub fn fetch_path(&self, id: FileId) -> Option<&Path> {
        let FileId(id) = id;
        self.caches.get(id).map(|(path, _)| path.as_path())
    }

    /// Fetch file content associated with the id.
    pub fn fetch_content(&self, id: FileId) -> Option<&str> {
        let FileId(id) = id;
        self.caches.get(id).map(|(_, content)| content.as_str())
    }
}
