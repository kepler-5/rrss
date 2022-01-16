use std::{
    borrow::{Borrow, Cow},
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    hash::{Hash, Hasher},
    hint::unreachable_unchecked,
    mem::discriminant,
    rc::Rc,
};

use derive_more::IsVariant;
use inner::inner;
use itertools::repeat_n;
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

#[derive(Clone, Debug, IsVariant, PartialEq)]
pub enum Val {
    Undefined,
    Null,
    Boolean(bool),
    Number(f64),
    String(Rc<String>),
    Array(Rc<Array>),
}

impl Default for Val {
    fn default() -> Self {
        Val::Undefined
    }
}

impl<S: Into<String>> From<S> for Val {
    fn from(s: S) -> Self {
        Val::String(Rc::new(s.into()))
    }
}

impl From<Array> for Val {
    fn from(a: Array) -> Self {
        Val::Array(Rc::new(a))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq)]
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
            Val::String(s) => Ok(self.index_dict_or_insert(DictKey::String((**s).clone()))),
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

    fn push(&mut self, vals: impl Iterator<Item = Val>) {
        self.arr.extend(vals)
    }
    fn pop(&mut self) -> Result<Val, ValueError> {
        self.arr.pop_front().ok_or(ValueError::PopOnEmptyArray)
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
    pub fn index<'a>(&'a self, val: &Val) -> Result<Cow<'a, Val>, ValueError> {
        match self {
            Val::String(s) => index_string_with(s, val).map(Cow::Owned),
            Val::Array(a) => a.index(val).map(Cow::Borrowed),
            _ => Err(ValueError::NotIndexable),
        }
    }

    pub fn index_or_insert(&mut self, val: &Val) -> Result<&mut Val, ValueError> {
        if self.is_undefined() {
            *self = Array::new().into();
        }
        match self {
            Val::Array(a) => Rc::make_mut(a).index_or_insert(val),
            Val::String(_) => Err(ValueError::IndexNotAssignable),
            _ => Err(ValueError::NotIndexable),
        }
    }

    pub fn push(&mut self, vals: impl Iterator<Item = Val>) -> Result<(), ValueError> {
        if self.is_undefined() {
            *self = Array::new().into();
        }
        match self {
            Val::Array(a) => Ok(Rc::make_mut(a).push(vals)),
            _ => Err(ValueError::InvalidOperationForType),
        }
    }
    pub fn pop(&mut self) -> Result<Val, ValueError> {
        match self {
            Val::Array(a) => Rc::make_mut(a).pop(),
            _ => Err(ValueError::InvalidOperationForType),
        }
    }
}

impl Val {
    pub fn decay(&self) -> Cow<'_, Val> {
        match self {
            Val::Array(a) => Cow::Owned(Val::Number(a.len() as f64)),
            v => Cow::Borrowed(v),
        }
    }

    pub fn to_string_for_output(&self) -> Cow<'_, str> {
        let decayed = self.decay();
        match (&decayed, decayed.as_ref()) {
            (Cow::Borrowed(Val::String(s)), _) => Cow::Borrowed(&**s),

            (_, Val::Undefined) => Cow::Owned("mysterious".into()),
            (_, Val::Null) => Cow::Owned("null".into()),
            (_, Val::Boolean(b)) => Cow::Owned(if *b { "true" } else { "false" }.into()),
            (_, Val::Number(n)) => Cow::Owned(n.to_string()),
            (_, Val::String(s)) => Cow::Owned((**s).clone()),

            (_, Val::Array(_)) => unreachable!(),
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

    fn cmp_coerced<'a>(&'a self, other: &'a Val) -> Option<(Cow<'a, Val>, Cow<'a, Val>)> {
        if discriminant(self) == discriminant(other) {
            Some((Cow::Borrowed(self), Cow::Borrowed(other)))
        } else {
            match self {
                Val::Undefined => Some((Cow::Borrowed(self), Cow::Borrowed(other))),
                Val::Array(_) => Some((Cow::Borrowed(self), Cow::Borrowed(other))),

                Val::Null => other.cmp_coerced(self).map(|x| (x.1, x.0)),

                Val::Boolean(_) => match other {
                    Val::Null => Some((Cow::Borrowed(self), Cow::Owned(Val::Boolean(false)))),
                    _ => other.cmp_coerced(self).map(|x| (x.1, x.0)),
                },

                Val::Number(_) => match other {
                    Val::Boolean(_) => Some((
                        Cow::Owned(Val::Boolean(self.is_truthy())),
                        Cow::Borrowed(other),
                    )),
                    Val::Null => Some((Cow::Borrowed(self), Cow::Owned(Val::Number(0.0)))),
                    _ => other.cmp_coerced(self).map(|x| (x.1, x.0)),
                },

                Val::String(s) => match other {
                    Val::Number(_) => s
                        .parse::<f64>()
                        .ok()
                        .map(|n| (Cow::Owned(Val::Number(n)), Cow::Borrowed(other))),
                    Val::Boolean(_) => Some((
                        Cow::Owned(Val::Boolean(!s.is_empty())),
                        Cow::Borrowed(other),
                    )),
                    _ => Some((Cow::Borrowed(self), Cow::Borrowed(other))),
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

    fn plus_coerced<'a>(&'a self, other: &'a Val) -> (Cow<'a, Val>, Cow<'a, Val>) {
        match (self, other) {
            (Val::String(_), Val::Undefined) => {
                (Cow::Borrowed(self), Cow::Owned("mysterious".into()))
            }
            (Val::String(_), Val::Null) => (Cow::Borrowed(self), Cow::Owned("null".into())),
            (Val::String(_), Val::Boolean(b)) => (
                Cow::Borrowed(self),
                Cow::Owned(if *b { "true" } else { "false" }.into()),
            ),
            (Val::String(_), Val::Number(n)) => {
                (Cow::Borrowed(self), Cow::Owned(n.to_string().into()))
            }

            (Val::String(_), Val::String(_)) => (Cow::Borrowed(self), Cow::Borrowed(other)),

            (_, Val::String(_)) => {
                let (o, s) = other.plus_coerced(self);
                (s, o)
            }

            _ => (Cow::Borrowed(self), Cow::Borrowed(other)),
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

    pub fn multiply(&self, other: &Val) -> Result<Val, ValueError> {
        match (self, other) {
            (Val::Number(a), Val::Number(b)) => Ok(Val::Number(a * b)),
            (Val::String(a), Val::Number(b)) if *b >= 0.0 => Ok(Val::from(
                repeat_n(a.chars(), *b as usize)
                    .flatten()
                    .collect::<String>(),
            )),

            _ => Err(ValueError::InvalidOperationForType),
        }
    }

    pub fn subtract(&self, other: &Val) -> Result<Val, ValueError> {
        match (self, other) {
            (Val::Number(a), Val::Number(b)) => Ok(Val::Number(a - b)),
            _ => Err(ValueError::InvalidOperationForType),
        }
    }
    pub fn divide(&self, other: &Val) -> Result<Val, ValueError> {
        match (self, other) {
            (Val::Number(a), Val::Number(b)) => Ok(Val::Number(a / b)),
            _ => Err(ValueError::InvalidOperationForType),
        }
    }

    pub fn negate(&self) -> Result<Val, ValueError> {
        match self {
            Val::Number(n) => Ok(Val::Number(-n)),
            _ => Err(ValueError::InvalidOperationForType),
        }
    }

    pub fn round_up(&mut self) -> Result<(), ValueError> {
        match self {
            Val::Number(f) => {
                *f = f.ceil();
                Ok(())
            }
            _ => Err(ValueError::InvalidOperationForType),
        }
    }
    pub fn round_down(&mut self) -> Result<(), ValueError> {
        match self {
            Val::Number(f) => {
                *f = f.floor();
                Ok(())
            }
            _ => Err(ValueError::InvalidOperationForType),
        }
    }
    pub fn round_nearest(&mut self) -> Result<(), ValueError> {
        match self {
            Val::Number(f) => {
                *f = f.round();
                Ok(())
            }
            _ => Err(ValueError::InvalidOperationForType),
        }
    }
}
