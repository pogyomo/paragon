use std::collections::HashMap;

use paragon_object::Object;

pub struct SymbolTable {
    global_syms: HashMap<String, GlobalSymbolInfo>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { global_syms: HashMap::new() }
    }

    pub fn add_global(&mut self, name: String, value: Object, bank: u16) {
        self.global_syms.insert(name, GlobalSymbolInfo::new(SymbolInfo::new(value, bank)));
    }

    pub fn add_local(&mut self, global: &str, name: String, value: Object, bank: u16) {
        self.global_syms.get_mut(global).map(|v| v.childs.insert(name, SymbolInfo::new(value, bank)));
    }

    pub fn fetch_global_value(&self, name: &str) -> Option<&Object> {
        self.global_syms.get(name).map(|v| &v.info.value)
    }

    pub fn fetch_global_bank(&self, name: &str) -> Option<u16> {
        self.global_syms.get(name).map(|v| v.info.defined_bank)
    }

    pub fn fetch_local_value(&self, global: &str, name: &str) -> Option<&Object> {
        self.global_syms.get(global)?.childs.get(name).map(|v| &v.value)
    }

    pub fn fetch_local_bank(&self, global: &str, name: &str) -> Option<u16> {
        self.global_syms.get(global)?.childs.get(name).map(|v| v.defined_bank)
    }
}

struct GlobalSymbolInfo {
    info: SymbolInfo,
    childs: HashMap<String, SymbolInfo>,
}

impl GlobalSymbolInfo {
    pub fn new(info: SymbolInfo) -> Self {
        Self { info, childs: HashMap::new() }
    }
}

struct SymbolInfo {
    value: Object,
    defined_bank: u16,
}

impl SymbolInfo {
    pub fn new(value: Object, defined_bank: u16) -> Self {
        Self { value, defined_bank }
    }
}
