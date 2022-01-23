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
use itertools::{repeat_n, Itertools};
use std::iter;
use unchecked_unwrap::UncheckedUnwrap;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum ValError {
    NotIndexable,
    InvalidKey,
    NoValueForKey,
    IndexOutOfBounds,
    IndexNotAssignable,
    InvalidOperationForType,
    PopOnEmptyArray,
    InvalidComparison,
    InvalidSplitDelimiter,
    InvalidJoinDelimiter,
    InvalidArrayElementForJoin(Val),
    ParsingStringAsNumberFailed(String),
    InvalidStringToIntegerRadix(Val),
    ConvertingNumberToCharacterFailed(f64),
    UnexpectedParameterToNumberToCharacterCast(Val),
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
        Self::with_arr_and_dict(VecDeque::new(), HashMap::new())
    }

    pub fn with_arr(arr: VecDeque<Val>) -> Self {
        Self::with_arr_and_dict(arr, HashMap::new())
    }

    fn with_arr_and_dict(arr: VecDeque<Val>, dict: HashMap<DictKey, Val>) -> Self {
        Self { arr, dict }
    }

    fn len(&self) -> usize {
        self.arr.len() + self.dict.len()
    }

    fn index(&self, val: &Val) -> Result<&Val, ValError> {
        match val {
            Val::Number(n) => self.index_arr(*n as usize),
            Val::Undefined => self.index_dict(&DictKeyRef::Undefined),
            Val::Null => self.index_dict(&DictKeyRef::Null),
            Val::Boolean(b) => self.index_dict(&DictKeyRef::Boolean(*b)),
            Val::String(s) => self.index_dict(&DictKeyRef::String(s)),
            Val::Array(_) => Err(ValError::InvalidKey),
        }
    }
    fn index_arr(&self, i: usize) -> Result<&Val, ValError> {
        self.arr.get(i).ok_or(ValError::IndexOutOfBounds)
    }
    fn index_dict(&self, k: &dyn Key) -> Result<&Val, ValError> {
        self.dict.get(k).ok_or(ValError::NoValueForKey)
    }

    fn index_or_insert(&mut self, val: &Val) -> Result<&mut Val, ValError> {
        match val {
            Val::Number(n) => Ok(self.index_arr_or_insert(*n as usize)),
            Val::Undefined => Ok(self.index_dict_or_insert(DictKey::Undefined)),
            Val::Null => Ok(self.index_dict_or_insert(DictKey::Null)),
            Val::Boolean(b) => Ok(self.index_dict_or_insert(DictKey::Boolean(*b))),
            Val::String(s) => Ok(self.index_dict_or_insert(DictKey::String((**s).clone()))),
            Val::Array(_) => Err(ValError::InvalidKey),
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
    fn pop(&mut self) -> Result<Val, ValError> {
        self.arr.pop_front().ok_or(ValError::PopOnEmptyArray)
    }

    fn val_iter(&self) -> impl Iterator<Item = &Val> {
        self.arr.iter().chain(self.dict.values())
    }

    fn is_empty(&self) -> bool {
        self.arr.is_empty() && self.dict.is_empty()
    }
}

fn index_string(s: &str, i: usize) -> Result<Val, ValError> {
    s.chars()
        .nth(i)
        .ok_or(ValError::IndexOutOfBounds)
        .map(Into::into)
}

fn index_string_with(s: &str, val: &Val) -> Result<Val, ValError> {
    match val {
        Val::Number(n) => index_string(s, *n as usize),
        _ => Err(ValError::InvalidKey),
    }
}

impl Val {
    pub fn index<'a>(&'a self, val: &Val) -> Result<Cow<'a, Val>, ValError> {
        match self {
            Val::String(s) => index_string_with(s, val).map(Cow::Owned),
            Val::Array(a) => a.index(val).map(Cow::Borrowed),
            _ => Err(ValError::NotIndexable),
        }
    }

    pub fn index_or_insert(&mut self, val: &Val) -> Result<&mut Val, ValError> {
        if self.is_undefined() {
            *self = Array::new().into();
        }
        match self {
            Val::Array(a) => Rc::make_mut(a).index_or_insert(val),
            Val::String(_) => Err(ValError::IndexNotAssignable),
            _ => Err(ValError::NotIndexable),
        }
    }

    pub fn array_coerce(&mut self) {
        if self.is_array() {
            return;
        }
        let old = std::mem::replace(self, Array::new().into());
        if !old.is_undefined() {
            match self {
                Val::Array(a) => Rc::make_mut(a).push(iter::once(old)),
                _ => unsafe { unreachable_unchecked() },
            };
        }
    }

    pub fn push(&mut self, vals: impl Iterator<Item = Val>) -> Result<(), ValError> {
        self.array_coerce();
        match self {
            Val::Array(a) => Ok(Rc::make_mut(a).push(vals)),
            _ => Err(ValError::InvalidOperationForType),
        }
    }
    pub fn pop(&mut self) -> Result<Val, ValError> {
        match self {
            Val::Array(a) => Rc::make_mut(a).pop(),
            _ => Err(ValError::InvalidOperationForType),
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

    pub fn compare(&self, other: &Val) -> Result<Option<Ordering>, ValError> {
        self.cmp_coerced(other)
            .and_then(|(a, b)| {
                if discriminant(a.as_ref()) != discriminant(b.as_ref()) {
                    Some(Err(ValError::InvalidComparison))
                } else {
                    match a.as_ref() {
                        Val::Undefined => Some(Ok(Ordering::Equal)),
                        Val::Null => Some(Ok(Ordering::Equal)),

                        Val::Number(n) => n.partial_cmp(inner!(b.as_ref(), if Val::Number)).map(Ok),
                        Val::String(s) => Some(Ok(s.cmp(inner!(b.as_ref(), if Val::String)))),

                        Val::Boolean(_) => Some(Err(ValError::InvalidComparison)),
                        Val::Array(_) => Some(Err(ValError::InvalidComparison)),
                    }
                }
            })
            .transpose()
    }

    pub fn inc(&mut self, x: isize) -> Result<(), ValError> {
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

            _ => Err(ValError::InvalidOperationForType),
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

    pub fn plus(&self, other: &Val) -> Result<Val, ValError> {
        let (a, b) = self.plus_coerced(other);
        match (a.as_ref(), b.as_ref()) {
            (Val::String(a), Val::String(b)) => {
                Ok(Val::from(a.chars().chain(b.chars()).collect::<String>()))
            }
            (Val::Number(a), Val::Number(b)) => Ok(Val::Number(a + b)),

            _ => Err(ValError::InvalidOperationForType),
        }
    }

    pub fn multiply(&self, other: &Val) -> Result<Val, ValError> {
        match (self, other) {
            (Val::Number(a), Val::Number(b)) => Ok(Val::Number(a * b)),
            (Val::String(a), Val::Number(b)) if *b >= 0.0 => Ok(Val::from(
                repeat_n(a.chars(), *b as usize)
                    .flatten()
                    .collect::<String>(),
            )),

            _ => Err(ValError::InvalidOperationForType),
        }
    }

    pub fn subtract(&self, other: &Val) -> Result<Val, ValError> {
        match (self, other) {
            (Val::Number(a), Val::Number(b)) => Ok(Val::Number(a - b)),
            _ => Err(ValError::InvalidOperationForType),
        }
    }
    pub fn divide(&self, other: &Val) -> Result<Val, ValError> {
        match (self, other) {
            (Val::Number(a), Val::Number(b)) => Ok(Val::Number(a / b)),
            _ => Err(ValError::InvalidOperationForType),
        }
    }

    pub fn negate(&self) -> Result<Val, ValError> {
        match self {
            Val::Number(n) => Ok(Val::Number(-n)),
            _ => Err(ValError::InvalidOperationForType),
        }
    }

    pub fn round_up(&mut self) -> Result<(), ValError> {
        match self {
            Val::Number(f) => {
                *f = f.ceil();
                Ok(())
            }
            _ => Err(ValError::InvalidOperationForType),
        }
    }
    pub fn round_down(&mut self) -> Result<(), ValError> {
        match self {
            Val::Number(f) => {
                *f = f.floor();
                Ok(())
            }
            _ => Err(ValError::InvalidOperationForType),
        }
    }
    pub fn round_nearest(&mut self) -> Result<(), ValError> {
        match self {
            Val::Number(f) => {
                *f = f.round();
                Ok(())
            }
            _ => Err(ValError::InvalidOperationForType),
        }
    }

    pub fn split(&mut self, delim: Option<Val>) -> Result<(), ValError> {
        match self {
            Val::String(s) if !s.is_empty() => {
                let delim = match &delim {
                    Some(d) => match &d {
                        Val::String(d) => d,
                        _ => return Err(ValError::InvalidSplitDelimiter),
                    },
                    None => "",
                };
                let splitted = if delim.is_empty() {
                    s.chars().map(Val::from).collect()
                } else {
                    s.split(delim).map(Val::from).collect()
                };
                *self = Array::with_arr(splitted).into();
                Ok(())
            }
            Val::String(s) if s.is_empty() => {
                if let Some(d) = &delim {
                    if !d.is_string() {
                        return Err(ValError::InvalidSplitDelimiter);
                    }
                }
                *self = Array::new().into();
                Ok(())
            }
            _ => Err(ValError::InvalidOperationForType),
        }
    }

    pub fn join(&mut self, delim: Option<Val>) -> Result<(), ValError> {
        match self {
            Val::Array(a) if !a.is_empty() => {
                let delim = match &delim {
                    Some(d) => match &d {
                        Val::String(s) => s,
                        _ => return Err(ValError::InvalidJoinDelimiter),
                    },
                    None => "",
                };
                for val in a.val_iter() {
                    match val {
                        Val::String(_) => {}
                        _ => return Err(ValError::InvalidArrayElementForJoin(val.clone())),
                    }
                }
                let string = a
                    .val_iter()
                    .map(|val| match val {
                        Val::String(s) => s,
                        _ => unsafe { unreachable_unchecked() },
                    })
                    .join(delim);
                *self = string.into();
                Ok(())
            }
            Val::Array(a) if a.is_empty() => {
                if let Some(d) = &delim {
                    if !d.is_string() {
                        return Err(ValError::InvalidJoinDelimiter);
                    }
                }
                *self = String::new().into();
                Ok(())
            }
            _ => Err(ValError::InvalidOperationForType),
        }
    }

    fn try_to_integer(f: f64, fail: impl FnOnce() -> ValError) -> Result<i64, ValError> {
        let i = f.trunc();
        (i == f).then(|| i as i64).ok_or_else(fail)
    }

    pub fn cast(&mut self, param: Option<Val>) -> Result<(), ValError> {
        match self {
            Val::Number(n) => {
                if let Some(param) = param {
                    Err(ValError::UnexpectedParameterToNumberToCharacterCast(
                        param.clone(),
                    ))
                } else {
                    let err = || ValError::ConvertingNumberToCharacterFailed(*n);
                    let i = Self::try_to_integer(*n, err)?;
                    let c = char::from_u32(i.try_into().map_err(|_| err())?).ok_or_else(err)?;
                    *self = Val::from(c);
                    Ok(())
                }
            }
            Val::String(s) => {
                if let Some(param) = param {
                    match param {
                        Val::Number(p) => {
                            let err = || ValError::InvalidStringToIntegerRadix(param.clone());
                            let radix = Self::try_to_integer(p, err)?
                                .try_into()
                                .map_err(|_| err())?;
                            let n = i64::from_str_radix(s, radix).map_err(|_| err())?;
                            *self = Val::Number(n as f64);
                            Ok(())
                        }
                        _ => Err(ValError::InvalidStringToIntegerRadix(param.clone())),
                    }
                } else {
                    *self = s
                        .parse()
                        .map(Val::Number)
                        .map_err(|_| ValError::ParsingStringAsNumberFailed((**s).clone()))?;
                    Ok(())
                }
            }

            _ => Err(ValError::InvalidOperationForType),
        }
    }
}
