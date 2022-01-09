use std::{
    borrow::Borrow,
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    hash::{Hash, Hasher},
    hint::unreachable_unchecked,
    mem::discriminant,
};

use derive_more::IsVariant;
use inner::inner;
use unchecked_unwrap::UncheckedUnwrap;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum ValueError {
    NotIndexable,
    InvalidKey,
    NoValueForKey,
    IndexOutOfBounds,
    IndexNotAssignable,
    InvalidOperationForType,
    PopOnEmptyArray,
    InvalidComparison,
}

#[derive(Debug, IsVariant, PartialEq)]
pub enum Val {
    Undefined,
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Box<Array>),
}

impl Default for Val {
    fn default() -> Self {
        Val::Undefined
    }
}

impl<S: Into<String>> From<S> for Val {
    fn from(s: S) -> Self {
        Val::String(s.into())
    }
}

impl From<Array> for Val {
    fn from(a: Array) -> Self {
        Val::Array(Box::new(a))
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
enum DictKey {
    Undefined,
    Null,
    Boolean(bool),
    String(String),
}

impl DictKey {
    fn as_ref(&self) -> DictKeyRef<'_> {
        match self {
            DictKey::Undefined => DictKeyRef::Undefined,
            DictKey::Null => DictKeyRef::Null,
            DictKey::Boolean(b) => DictKeyRef::Boolean(*b),
            DictKey::String(s) => DictKeyRef::String(s),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
enum DictKeyRef<'a> {
    Undefined,
    Null,
    Boolean(bool),
    String(&'a str),
}

trait Key {
    fn to_key(&self) -> DictKeyRef<'_>;
}

impl Hash for dyn Key + '_ {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_key().hash(state)
    }
}
impl PartialEq for dyn Key + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.to_key() == other.to_key()
    }
}
impl Eq for dyn Key + '_ {}

impl Key for DictKey {
    fn to_key(&self) -> DictKeyRef<'_> {
        self.as_ref()
    }
}
impl Key for DictKeyRef<'_> {
    fn to_key(&self) -> DictKeyRef<'_> {
        *self
    }
}

impl<'a> Borrow<dyn Key + 'a> for DictKey {
    fn borrow(&self) -> &(dyn Key + 'a) {
        self
    }
}
impl<'a> Borrow<dyn Key + 'a> for DictKeyRef<'a> {
    fn borrow(&self) -> &(dyn Key + 'a) {
        self
    }
}

#[derive(Debug, PartialEq)]
pub struct Array {
    arr: VecDeque<Val>,
    dict: HashMap<DictKey, Val>,
}

impl Array {
    pub fn new() -> Self {
        Self {
            arr: VecDeque::new(),
            dict: HashMap::new(),
        }
    }

    fn len(&self) -> usize {
        self.arr.len() + self.dict.len()
    }

    fn index(&self, val: &Val) -> Result<&Val, ValueError> {
        match val {
            Val::Number(n) => self.index_arr(*n as usize),
            Val::Undefined => self.index_dict(&DictKeyRef::Undefined),
            Val::Null => self.index_dict(&DictKeyRef::Null),
            Val::Boolean(b) => self.index_dict(&DictKeyRef::Boolean(*b)),
            Val::String(s) => self.index_dict(&DictKeyRef::String(s)),
            Val::Array(_) => Err(ValueError::InvalidKey),
        }
    }
    fn index_arr(&self, i: usize) -> Result<&Val, ValueError> {
        self.arr.get(i).ok_or(ValueError::IndexOutOfBounds)
    }
    fn index_dict(&self, k: &dyn Key) -> Result<&Val, ValueError> {
        self.dict.get(k).ok_or(ValueError::NoValueForKey)
    }

    fn index_or_insert(&mut self, val: &Val) -> Result<&mut Val, ValueError> {
        match val {
            Val::Number(n) => Ok(self.index_arr_or_insert(*n as usize)),
            Val::Undefined => Ok(self.index_dict_or_insert(DictKey::Undefined)),
            Val::Null => Ok(self.index_dict_or_insert(DictKey::Null)),
            Val::Boolean(b) => Ok(self.index_dict_or_insert(DictKey::Boolean(*b))),
            Val::String(s) => Ok(self.index_dict_or_insert(DictKey::String(s.clone()))),
            Val::Array(_) => Err(ValueError::InvalidKey),
        }
    }
    fn index_arr_or_insert(&mut self, i: usize) -> &mut Val {
        if i >= self.arr.len() {
            self.arr.resize_with(i + 1, Default::default);
        }
        unsafe { self.arr.get_mut(i).unchecked_unwrap() }
    }
    fn index_dict_or_insert(&mut self, k: DictKey) -> &mut Val {
        self.dict.entry(k).or_default()
    }

    fn push(&mut self, val: Val) {
        self.arr.push_back(val)
    }
    fn pop(&mut self) -> Result<Val, ValueError> {
        self.arr.pop_front().ok_or(ValueError::PopOnEmptyArray)
    }
}

#[derive(Debug, PartialEq)]
pub enum ValOrRef<'a> {
    Val(Val),
    Ref(&'a Val),
}

impl<'a> ValOrRef<'a> {
    pub fn as_ref(&self) -> &Val {
        match self {
            ValOrRef::Val(v) => &v,
            ValOrRef::Ref(r) => r,
        }
    }
}

impl<V: Into<Val>> From<V> for ValOrRef<'_> {
    fn from(v: V) -> Self {
        ValOrRef::Val(v.into())
    }
}

impl<'a> From<&'a Val> for ValOrRef<'a> {
    fn from(v: &'a Val) -> Self {
        ValOrRef::Ref(v)
    }
}

fn index_string(s: &str, i: usize) -> Result<Val, ValueError> {
    s.chars()
        .nth(i)
        .ok_or(ValueError::IndexOutOfBounds)
        .map(Into::into)
}

fn index_string_with(s: &str, val: &Val) -> Result<Val, ValueError> {
    match val {
        Val::Number(n) => index_string(s, *n as usize),
        _ => Err(ValueError::InvalidKey),
    }
}

impl Val {
    pub fn index(&self, val: &Val) -> Result<ValOrRef, ValueError> {
        match self {
            Val::String(s) => index_string_with(s, val).map(ValOrRef::Val),
            Val::Array(a) => a.index(val).map(ValOrRef::Ref),
            _ => Err(ValueError::NotIndexable),
        }
    }

    pub fn index_or_insert(&mut self, val: &Val) -> Result<&mut Val, ValueError> {
        if self.is_undefined() {
            *self = Array::new().into();
        }
        match self {
            Val::Array(a) => a.index_or_insert(val),
            Val::String(_) => Err(ValueError::IndexNotAssignable),
            _ => Err(ValueError::NotIndexable),
        }
    }

    pub fn push(&mut self, val: Val) -> Result<(), ValueError> {
        if self.is_undefined() {
            *self = Array::new().into();
        }
        match self {
            Val::Array(a) => Ok(a.push(val)),
            _ => Err(ValueError::InvalidOperationForType),
        }
    }
    pub fn pop(&mut self) -> Result<Val, ValueError> {
        match self {
            Val::Array(a) => a.pop(),
            _ => Err(ValueError::InvalidOperationForType),
        }
    }
}

impl Val {
    pub fn decay(&self) -> ValOrRef {
        match self {
            Val::Array(a) => Val::Number(a.len() as f64).into(),
            v => ValOrRef::Ref(v),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Val::Undefined => false,
            Val::Null => false,
            Val::Boolean(b) => *b,
            Val::Number(n) => *n != 0.0,
            Val::String(_) => true,
            Val::Array(_) => true,
        }
    }

    fn cmp_coerced<'a>(&'a self, other: &'a Val) -> Option<(ValOrRef<'a>, ValOrRef<'a>)> {
        if discriminant(self) == discriminant(other) {
            Some((self.into(), other.into()))
        } else {
            match self {
                Val::Undefined => Some((self.into(), other.into())),
                Val::Array(_) => Some((self.into(), other.into())),

                Val::Null => other.cmp_coerced(self).map(|x| (x.1, x.0)),

                Val::Boolean(_) => match other {
                    Val::Null => Some((self.into(), Val::Boolean(false).into())),
                    _ => other.cmp_coerced(self).map(|x| (x.1, x.0)),
                },

                Val::Number(_) => match other {
                    Val::Boolean(_) => Some((Val::Boolean(self.is_truthy()).into(), other.into())),
                    Val::Null => Some((self.into(), Val::Number(0.0).into())),
                    _ => other.cmp_coerced(self).map(|x| (x.1, x.0)),
                },

                Val::String(s) => match other {
                    Val::Number(_) => s
                        .parse::<f64>()
                        .ok()
                        .map(|n| (Val::Number(n).into(), other.into())),
                    Val::Boolean(_) => Some((Val::Boolean(!s.is_empty()).into(), other.into())),
                    _ => Some((self.into(), other.into())),
                },
            }
        }
    }

    pub fn equals(&self, other: &Val) -> bool {
        self.cmp_coerced(other)
            .map(|(a, b)| a.as_ref() == b.as_ref())
            .unwrap_or(false)
    }

    pub fn compare(&self, other: &Val) -> Result<Option<Ordering>, ValueError> {
        self.cmp_coerced(other)
            .and_then(|(a, b)| {
                if discriminant(a.as_ref()) != discriminant(b.as_ref()) {
                    Some(Err(ValueError::InvalidComparison))
                } else {
                    match a.as_ref() {
                        Val::Undefined => Some(Ok(Ordering::Equal)),
                        Val::Null => Some(Ok(Ordering::Equal)),

                        Val::Number(n) => n.partial_cmp(inner!(b.as_ref(), if Val::Number)).map(Ok),
                        Val::String(s) => Some(Ok(s.cmp(inner!(b.as_ref(), if Val::String)))),

                        Val::Boolean(_) => Some(Err(ValueError::InvalidComparison)),
                        Val::Array(_) => Some(Err(ValueError::InvalidComparison)),
                    }
                }
            })
            .transpose()
    }

    pub fn inc(&mut self, x: isize) -> Result<(), ValueError> {
        if self.is_null() {
            *self = Val::Number(0.0);
        }
        match self {
            Val::Null => unsafe { unreachable_unchecked() },

            Val::Boolean(b) => {
                *b ^= x % 2 != 0;
                Ok(())
            }
            Val::Number(n) => {
                *n += x as f64;
                Ok(())
            }

            _ => Err(ValueError::InvalidOperationForType),
        }
    }

    fn plus_coerced<'a>(&'a self, other: &'a Val) -> (ValOrRef<'a>, ValOrRef<'a>) {
        match (self, other) {
            (Val::String(_), Val::Undefined) => (self.into(), Val::from("mysterious").into()),
            (Val::String(_), Val::Null) => (self.into(), Val::from("null").into()),
            (Val::String(_), Val::Boolean(b)) => (
                self.into(),
                Val::from(if *b { "true" } else { "false" }).into(),
            ),
            (Val::String(_), Val::Number(n)) => (self.into(), Val::from(n.to_string()).into()),

            (Val::String(_), Val::String(_)) => (self.into(), other.into()),

            (_, Val::String(_)) => {
                let (o, s) = other.plus_coerced(self);
                (s, o)
            }

            _ => (self.into(), other.into()),
        }
    }

    pub fn plus(&self, other: &Val) -> Result<Val, ValueError> {
        let (a, b) = self.plus_coerced(other);
        match (a.as_ref(), b.as_ref()) {
            (Val::String(a), Val::String(b)) => {
                Ok(Val::from(a.chars().chain(b.chars()).collect::<String>()))
            }
            (Val::Number(a), Val::Number(b)) => Ok(Val::Number(a + b)),

            _ => Err(ValueError::InvalidOperationForType),
        }
    }
}
