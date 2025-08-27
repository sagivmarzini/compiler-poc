use std::collections::HashMap;

pub enum Context {
    Program { data: ScopeData },
    Function { name: String, data: ScopeData },
}

impl Context {
    pub fn new_program() -> Self {
        Context::Program {
            data: ScopeData::new(),
        }
    }

    pub fn new_function(name: String) -> Self {
        Context::Function {
            name,
            data: ScopeData::new(),
        }
    }

    pub fn scope_data(&self) -> &ScopeData {
        match self {
            Context::Program { data } => data,
            Context::Function { data, .. } => data,
        }
    }

    pub fn scope_data_mut(&mut self) -> &mut ScopeData {
        match self {
            Context::Program { data } => data,
            Context::Function { data, .. } => data,
        }
    }
}

pub struct ScopeData {
    /// <VariableName, StackOffset>
    pub locals: HashMap<String, i64>,
    /// Grows upwards in jumps of a variable size (e.g. 8)
    pub stack_offset: i64,
}

impl ScopeData {
    fn new() -> Self {
        ScopeData {
            locals: HashMap::new(),
            stack_offset: 0,
        }
    }
}
