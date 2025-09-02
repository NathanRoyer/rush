use std::ops::Index;
use super::*;

pub struct Names {
    store: Vec<String>,
    pub greater_equal: NameIndex,
    pub less_equal: NameIndex,
    pub different: NameIndex,
    pub multiply: NameIndex,
    pub subtract: NameIndex,
    pub greater: NameIndex,
    pub divide: NameIndex,
    pub negate: NameIndex,
    pub equal: NameIndex,
    pub dump: NameIndex,
    pub less: NameIndex,
    pub main: NameIndex,
    pub next: NameIndex,
    pub from: NameIndex,
    pub add: NameIndex,
    pub get: NameIndex,
    pub set: NameIndex,
}

impl Default for Names {
    fn default() -> Self {
        let mut store = Vec::with_capacity(64);

        let mut push = |name: &str| {
            let index = store.len();
            store.push(name.to_string());
            index as NameIndex
        };

        let greater_equal = push("greater_equal");
        let less_equal = push("less_equal");
        let different = push("different");
        let multiply = push("multiply");
        let subtract = push("subtract");
        let greater = push("greater");
        let divide = push("divide");
        let negate = push("negate");
        let equal = push("equal");
        let dump = push("dump");
        let less = push("less");
        let main = push("main");
        let next = push("next");
        let from = push("from");
        let add = push("add");
        let get = push("get");
        let set = push("set");

        Self {
            store,
            greater_equal,
            less_equal,
            different,
            multiply,
            subtract,
            greater,
            divide,
            negate,
            equal,
            dump,
            less,
            main,
            next,
            from,
            add,
            get,
            set,
        }
    }
}

impl Names {
    pub fn empty() -> Self {
        Self {
            store: Vec::new(),
            greater_equal: NameIndex::MAX,
            less_equal: NameIndex::MAX,
            different: NameIndex::MAX,
            multiply: NameIndex::MAX,
            subtract: NameIndex::MAX,
            greater: NameIndex::MAX,
            divide: NameIndex::MAX,
            negate: NameIndex::MAX,
            equal: NameIndex::MAX,
            dump: NameIndex::MAX,
            less: NameIndex::MAX,
            main: NameIndex::MAX,
            next: NameIndex::MAX,
            from: NameIndex::MAX,
            add: NameIndex::MAX,
            get: NameIndex::MAX,
            set: NameIndex::MAX,
        }
    }

    pub fn get(&mut self, name: &str) -> NameIndex {
        let mut index = self.store.len();

        match self.store.iter().position(|n| n == name) {
            Some(i) => index = i,
            None => self.store.push(name.to_string()),
        };

        index
    }
}

impl Index<NameIndex> for Names {
    type Output = str;

    fn index(&self, i: NameIndex) -> &str {
        &self.store[i]
    }
}
