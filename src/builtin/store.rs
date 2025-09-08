use std::marker::PhantomData;
use std::fmt;
use super::*;

pub type RefCount = Arc<()>;
pub struct RefIndex<T>(usize, PhantomData<T>);

/// Index to a string in a [`Stores`]
pub type StrIndex = RefIndex<RushStr>;
/// Index to a vector in a [`Stores`]
pub type VecIndex = RefIndex<ValueVec>;
/// Index to a map in a [`Stores`]
pub type MapIndex = RefIndex<ValueMap>;

pub struct RushStr {
    inner: String,
    ref_count: RefCount,
}

pub struct ValueVec {
    inner: Vec<Value>,
    ref_count: RefCount,
}

/// Key-Value pair from a [`ValueMap`]
pub struct Entry {
    pub key: Value,
    pub value: Value,
}

/// Internal map object
pub struct ValueMap {
    pub inner: Vec<Entry>,
    pub sorted: bool,
    ref_count: RefCount,
}

trait GetRefCount {
    fn ref_count(&self) -> &Arc<()>;
}

impl GetRefCount for RushStr {
    fn ref_count(&self) -> &Arc<()> {
        &self.ref_count
    }
}

impl GetRefCount for ValueVec {
    fn ref_count(&self) -> &Arc<()> {
        &self.ref_count
    }
}

impl GetRefCount for ValueMap {
    fn ref_count(&self) -> &Arc<()> {
        &self.ref_count
    }
}

struct Store<T: GetRefCount> {
    slots: Vec<Option<T>>,
}

impl<T: GetRefCount> Default for Store<T> {
    fn default() -> Self {
        Self { slots: Vec::new() }
    }
}

impl<T: GetRefCount> Store<T> {
    fn get(&self, index: RefIndex<T>) -> Option<&T> {
        self.slots.get(index.0)?.as_ref()
    }

    fn get_mut(&mut self, index: RefIndex<T>) -> Option<&mut T> {
        self.slots.get_mut(index.0)?.as_mut()
    }

    fn place(&mut self, value: T) -> RefIndex<T> {
        let mut iter = self.slots.iter();
        let mut i = self.slots.len();

        match iter.position(Option::is_none) {
            Some(pos) => i = pos,
            None => self.slots.push(None),
        }

        self.slots[i] = Some(value);
        RefIndex::<T>(i, PhantomData)
    }

    fn garbage_collect(&mut self) {
        for maybe_item in self.slots.iter_mut() {
            let Some(item) = maybe_item.as_ref() else {
                continue;
            };

            if Arc::strong_count(item.ref_count()) == 1 {
                *maybe_item = None;
            }
        }
    }
}

/// Collection of reference-counted items
#[derive(Default)]
pub struct Stores {
    str_store: Store<RushStr>,
    vec_store: Store<ValueVec>,
    map_store: Store<ValueMap>,
}

impl Stores {
    pub fn get_str(&self, i: StrIndex) -> Option<&str> {
        let string = self.str_store.get(i)?;
        Some(string.inner.as_str())
    }

    pub fn get_vec(&self, i: VecIndex) -> Option<&[Value]> {
        let vec = self.vec_store.get(i)?;
        Some(vec.inner.as_slice())
    }

    pub fn get_map(&self, i: MapIndex) -> Option<&ValueMap> {
        self.map_store.get(i)
    }

    pub fn get_str_mut(&mut self, i: StrIndex) -> Option<&mut String> {
        let string = self.str_store.get_mut(i)?;
        Some(&mut string.inner)
    }

    pub fn get_vec_mut(&mut self, i: VecIndex) -> Option<&mut Vec<Value>> {
        let vec = self.vec_store.get_mut(i)?;
        Some(&mut vec.inner)
    }

    pub fn get_map_mut(&mut self, i: MapIndex) -> Option<&mut ValueMap> {
        self.map_store.get_mut(i)
    }

    pub fn new_string(&mut self, inner: String) -> Value {
        let ref_count = RefCount::new(());

        let string = RushStr {
            inner,
            ref_count: ref_count.clone(),
        };

        let i = self.str_store.place(string);
        Value::from(BuiltIn::Str(i, ref_count))
    }

    pub fn new_vec(&mut self, inner: Vec<Value>) -> Value {
        let ref_count = RefCount::new(());

        let vec = ValueVec {
            inner,
            ref_count: ref_count.clone(),
        };

        let i = self.vec_store.place(vec);
        Value::from(BuiltIn::Vec(i, ref_count))
    }

    pub fn new_map(&mut self, inner: Vec<Entry>, sorted: bool) -> Value {
        let ref_count = RefCount::new(());

        let map = ValueMap {
            inner,
            sorted,
            ref_count: ref_count.clone(),
        };

        let i = self.map_store.place(map);
        Value::from(BuiltIn::Map(i, ref_count))
    }

    pub fn garbage_collect(&mut self) {
        self.str_store.garbage_collect();
        self.vec_store.garbage_collect();
        self.map_store.garbage_collect();
    }
}

impl<T: GetRefCount> fmt::Debug for RefIndex<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl<T: GetRefCount> Clone for RefIndex<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<T: GetRefCount> Copy for RefIndex<T> { }
