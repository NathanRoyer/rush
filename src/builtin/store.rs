use super::*;

pub type Store<T> = Vec<Option<T>>;

/// Collection of reference-counted items
#[derive(Default)]
pub struct Stores {
    str_store: Store<RushStr>,
    vec_store: Store<ValueVec>,
    map_store: Store<ValueMap>,
}

impl Stores {
    pub fn get_str(&self, i: StrIndex) -> Option<&str> {
        let slot = self.str_store.get(i)?;
        Some(slot.as_ref()?.inner.as_str())
    }

    pub fn get_vec(&self, i: VecIndex) -> Option<&[Value]> {
        let slot = self.vec_store.get(i)?;
        Some(slot.as_ref()?.inner.as_slice())
    }

    pub fn get_map(&self, i: MapIndex) -> Option<&ValueMap> {
        self.map_store.get(i)?.as_ref()
    }

    pub fn get_vec_mut(&mut self, i: VecIndex) -> Option<&mut Vec<Value>> {
        let slot = self.vec_store.get_mut(i)?;
        Some(&mut slot.as_mut()?.inner)
    }

    pub fn get_map_mut(&mut self, i: MapIndex) -> Option<&mut ValueMap> {
        self.map_store.get_mut(i)?.as_mut()
    }

    pub fn new_string(&mut self, inner: String) -> Value {
        let ref_count = RefCount::new(());
        let mut iter = self.str_store.iter();

        let rush_str = RushStr {
            inner,
            ref_count: ref_count.clone(),
        };

        if let Some(i) = iter.position(Option::is_none) {
            self.str_store[i] = Some(rush_str);
            Value::from(BuiltIn::Str(i, ref_count))
        } else {
            let i = self.str_store.len();
            self.str_store.push(Some(rush_str));
            Value::from(BuiltIn::Str(i, ref_count))
        }
    }

    pub fn new_vec(&mut self, inner: Vec<Value>) -> Value {
        let ref_count = RefCount::new(());
        let mut iter = self.vec_store.iter();

        let vec = ValueVec {
            inner,
            ref_count: ref_count.clone(),
        };

        if let Some(i) = iter.position(Option::is_none) {
            self.vec_store[i] = Some(vec);
            Value::from(BuiltIn::Vec(i, ref_count))
        } else {
            let i = self.vec_store.len();
            self.vec_store.push(Some(vec));
            Value::from(BuiltIn::Vec(i, ref_count))
        }
    }

    pub fn new_map(&mut self, type_index: Option<TypeIndex>, inner: Vec<Entry>, sorted: bool) -> Value {
        let ref_count = RefCount::new(());
        let mut iter = self.map_store.iter();

        let map = ValueMap {
            inner,
            sorted,
            ref_count: ref_count.clone(),
        };

        let mut value = if let Some(i) = iter.position(Option::is_none) {
            self.map_store[i] = Some(map);
            Value::from(BuiltIn::Map(i, ref_count))
        } else {
            let i = self.map_store.len();
            self.map_store.push(Some(map));
            Value::from(BuiltIn::Map(i, ref_count))
        };

        if let Some(type_index) = type_index {
            value.type_index = type_index;
        }

        value
    }

    pub fn garbage_collect(&mut self) {
        for maybe_item in self.str_store.iter_mut() {
            let Some(item) = maybe_item.as_ref() else {
                continue;
            };

            if Arc::strong_count(&item.ref_count) == 1 {
                *maybe_item = None;
            }
        }

        for maybe_item in self.vec_store.iter_mut() {
            let Some(item) = maybe_item.as_ref() else {
                continue;
            };

            if Arc::strong_count(&item.ref_count) == 1 {
                *maybe_item = None;
            }
        }

        for maybe_item in self.map_store.iter_mut() {
            let Some(item) = maybe_item.as_ref() else {
                continue;
            };

            if Arc::strong_count(&item.ref_count) == 1 {
                *maybe_item = None;
            }
        }
    }
}
