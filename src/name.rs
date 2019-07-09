use std::hash::{Hash, Hasher};

#[derive(Debug, Default, Clone, PartialEq, PartialOrd, Eq)]
pub struct Name (pub Vec<String>);


impl Name {
    pub fn push(&mut self, v: String) {
        self.0.push(v);
    }

    pub fn pop(&mut self) -> Option<String> {
        self.0.pop()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_absolute(&self) -> bool {
        if let Some(s) = self.0.get(0) {
            return s.is_empty()
        } else {
            false
        }
    }
}

impl Hash for Name {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join("::"))
    }
}

impl From<&str> for Name {
    fn from(s: &str) -> Self {
        Name(s.split("::").map(|s|s.to_string()).collect())
    }
}

impl From<&String> for Name {
    fn from(s: &String) -> Self {
        Name(s.split("::").map(|s|s.to_string()).collect())
    }
}

