#[derive(Debug)]
pub struct LineNumbers {
    pub line_starts: Vec<u32>,
    pub length: u32,
}

impl LineNumbers {
    pub fn new(src: &str) -> Self {
        Self {
            length: src.len() as u32,
            line_starts: std::iter::once(0)
                .chain(src.match_indices('\n').map(|(i, _)| i as u32 + 1))
                .collect(),
        }
    }

    /// Get the line number for a byte index
    pub fn line_number(&self, byte_index: u32) -> u32 {
        self.line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1) as u32
            + 1
    }

    pub fn line_and_column_number(&self, byte_index: u32) -> LineColumn {
        let line = self.line_number(byte_index);
        let column = byte_index
            - self
                .line_starts
                .get(line as usize - 1)
                .copied()
                .unwrap_or_default()
            + 1;
        LineColumn { line, column }
    }

    pub fn byte_index(&self, line: u32, character: u32) -> u32 {
        match self.line_starts.get((line) as usize) {
            Some(line_index) => *line_index + character,
            None => self.length,
        }
    }
}

pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}