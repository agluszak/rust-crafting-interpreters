#[derive(Debug)]
pub enum Value {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Value {
    pub fn require_boolean(&self) -> Option<bool> {
        if let Value::Boolean(b) = self {
            return Some(*b);
        } else {
            return None;
        }
    }

    pub fn require_number(&self) -> Option<f64> {
        if let Value::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub fn require_string(&self) -> Option<String> {
        if let Value::String(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }

    pub fn require_nil(&self) -> Option<()> {
        if let Value::Nil = self {
            Some(())
        } else {
            None
        }
    }
}
