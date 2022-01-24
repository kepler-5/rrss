use super::*;

use std::iter::once;

#[test]
fn array_index() {
    let mut arr = Val::from(Array::new());

    *arr.index_or_insert(&Val::Null).unwrap() = Val::Boolean(true);
    assert_eq!(arr.index(&Val::Null).unwrap().as_ref(), &Val::Boolean(true));
    *arr.index_or_insert(&Val::Null).unwrap() = Val::Number(0.0);
    assert_eq!(arr.index(&Val::Null).unwrap().as_ref(), &Val::Number(0.0));
    *arr.index_or_insert(&Val::Number(0.0)).unwrap() = Val::Number(1.0);
    assert_eq!(arr.index(&Val::Null).unwrap().as_ref(), &Val::Number(0.0));
    assert_eq!(
        arr.index(&Val::Number(0.0)).unwrap().as_ref(),
        &Val::Number(1.0)
    );

    assert_eq!(
        arr.index(&Val::Number(100.0)).unwrap().as_ref(),
        &Val::Undefined
    );
    assert_eq!(
        arr.index(&"100.0".into()).unwrap().as_ref(),
        &Val::Undefined
    );
}

#[test]
fn not_indexable() {
    assert_eq!(
        Val::Null.index(&Val::Number(100.0)),
        Err(ValError::NotIndexable(Val::Null))
    );
    assert_eq!(
        Val::Boolean(false).index(&Val::Number(100.0)),
        Err(ValError::NotIndexable(Val::Boolean(false)))
    );
    assert_eq!(
        Val::Number(100.0).index(&Val::Number(100.0)),
        Err(ValError::NotIndexable(Val::Number(100.0)))
    );
    assert_eq!(
        Val::Number(100.0).index(&"100.0".into()),
        Err(ValError::NotIndexable(Val::Number(100.0)))
    );

    // undefined's become arrays when you write to an index
    let mut und = Val::Undefined;
    assert!(und.is_undefined());
    assert_eq!(
        und.index(&Val::Null),
        Err(ValError::NotIndexable(und.clone()))
    );
    *und.index_or_insert(&Val::Null).unwrap() = Val::Boolean(true);
    assert!(und.is_array());
    assert_eq!(und.index(&Val::Null).unwrap().as_ref(), &Val::Boolean(true));
}

#[test]
fn string_index() {
    let mut s = Val::from("bar");

    assert_eq!(s.index(&Val::Number(0.0)), Ok(Cow::Owned("b".into())));
    assert_eq!(s.index(&Val::Number(1.0)), Ok(Cow::Owned("a".into())));
    assert_eq!(s.index(&Val::Number(2.0)), Ok(Cow::Owned("r".into())));
    assert_eq!(s.index(&Val::Number(3.0)), Ok(Cow::Owned(Val::Undefined)));
    assert_eq!(s.index(&Val::Null), Err(ValError::InvalidKey(Val::Null)));

    let cloned = s.clone();
    assert_eq!(
        s.index_or_insert(&Val::Null),
        Err(ValError::IndexNotAssignable(Val::Null, cloned))
    );
}

#[test]
fn array_push_pop() {
    let mut arr = Val::Undefined;

    let ok = arr.push(once(Val::Null));
    assert!(ok.is_ok());
    assert!(arr.is_array());

    assert_eq!(arr.index(&Val::Number(0.0)).unwrap().as_ref(), &Val::Null);
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(1.0)));

    let ok = arr.push(once(Val::Boolean(true)));
    assert!(ok.is_ok());
    assert_eq!(arr.index(&Val::Number(0.0)).unwrap().as_ref(), &Val::Null);
    assert_eq!(
        arr.index(&Val::Number(1.0)).unwrap().as_ref(),
        &Val::Boolean(true)
    );
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(2.0)));

    let ok = arr.pop();
    assert!(ok.is_ok());
    assert_eq!(
        arr.index(&Val::Number(0.0)).unwrap().as_ref(),
        &Val::Boolean(true)
    );
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(1.0)));

    let ok = arr.pop();
    assert!(ok.is_ok());
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(0.0)));

    let err = arr.pop();
    assert_eq!(err, Ok(Val::Undefined));

    let ok = arr.push([Val::Null, Val::Boolean(true)].into_iter());
    assert!(ok.is_ok());
    assert_eq!(arr.index(&Val::Number(0.0)).unwrap().as_ref(), &Val::Null);
    assert_eq!(
        arr.index(&Val::Number(1.0)).unwrap().as_ref(),
        &Val::Boolean(true)
    );
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(2.0)));

    let mut coerce = Val::Null;
    assert!(coerce.push(once(Val::Undefined)).is_ok());
    assert!(coerce.is_array());
    assert_eq!(
        coerce,
        Array::with_arr([Val::Null, Val::Undefined].into_iter().collect()).into()
    );
    assert_eq!(
        Val::Null.pop(),
        Err(ValError::InvalidOperationForType("pop", Val::Null))
    );
}

#[test]
fn decay() {
    assert_eq!(Val::Undefined.decay(), Cow::Borrowed(&Val::Undefined));
    assert_eq!(Val::Null.decay(), Cow::Borrowed(&Val::Null));
    assert_eq!(
        Val::Boolean(false).decay(),
        Cow::Borrowed(&Val::Boolean(false))
    );
    assert_eq!(Val::Number(0.0).decay(), Cow::Borrowed(&Val::Number(0.0)));
    assert_eq!(Val::from("foo").decay(), Cow::Borrowed(&Val::from("foo")));
    assert_eq!(
        Val::from(Array::new()).decay(),
        Cow::Owned(Val::Number(0.0))
    );

    let mut arr = Val::from(Array::new());

    *arr.index_or_insert(&Val::Null).unwrap() = Val::Boolean(true);
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(0.0)));
    *arr.index_or_insert(&Val::Boolean(false)).unwrap() = Val::Number(0.0);
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(0.0)));
    *arr.index_or_insert(&Val::Number(0.0)).unwrap() = Val::Number(1.0);
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(1.0)));
}

#[test]
fn to_string_for_output() {
    assert_eq!(
        Val::Undefined.to_string_for_output().into_owned(),
        "mysterious"
    );
    assert_eq!(Val::Null.to_string_for_output().into_owned(), "null");
    assert_eq!(
        Val::Boolean(false).to_string_for_output().into_owned(),
        "false"
    );
    assert_eq!(
        Val::Boolean(true).to_string_for_output().into_owned(),
        "true"
    );
    assert_eq!(Val::Number(0.0).to_string_for_output().into_owned(), "0");
    assert_eq!(
        Val::from("foo").to_string_for_output(),
        Cow::Borrowed("foo")
    );
    assert_eq!(
        Val::from(Array::new()).to_string_for_output().into_owned(),
        "0"
    );

    let mut arr = Val::from(Array::new());

    *arr.index_or_insert(&Val::Null).unwrap() = Val::Boolean(true);
    assert_eq!(arr.to_string_for_output().into_owned(), "0");
    *arr.index_or_insert(&Val::Boolean(false)).unwrap() = Val::Number(0.0);
    assert_eq!(arr.to_string_for_output().into_owned(), "0");
    *arr.index_or_insert(&Val::Number(0.0)).unwrap() = Val::Number(1.0);
    assert_eq!(arr.to_string_for_output().into_owned(), "1");
}

#[test]
fn is_truthy() {
    assert!(!Val::Undefined.is_truthy());
    assert!(!Val::Null.is_truthy());
    assert!(!Val::Boolean(false).is_truthy());
    assert!(Val::Boolean(true).is_truthy());
    assert!(!Val::Number(0.0).is_truthy());
    assert!(Val::Number(42.0).is_truthy());
    assert!(Val::from("").is_truthy());
    assert!(Val::from("foo").is_truthy());
    assert!(Val::from(Array::new()).is_truthy());
}

#[test]
fn equals() {
    macro_rules! commutative_equals {
        ($a:expr, $b:expr) => {
            assert!($a.equals(&$b));
            assert!($b.equals(&$a));
        };
    }
    macro_rules! commutative_notequals {
        ($a:expr, $b:expr) => {
            assert!(!$a.equals(&$b));
            assert!(!$b.equals(&$a));
        };
    }
    commutative_equals!(Val::Undefined, Val::Undefined);
    commutative_equals!(Val::Undefined, Val::Null);
    commutative_notequals!(Val::Undefined, Val::Boolean(false));
    commutative_notequals!(Val::Undefined, Val::Number(0.0));
    commutative_notequals!(Val::Undefined, Val::from(""));
    commutative_notequals!(Val::Undefined, Val::from(Array::new()));

    commutative_equals!(Val::Null, Val::Null);
    commutative_equals!(Val::Boolean(false), Val::Boolean(false));
    commutative_notequals!(Val::Boolean(true), Val::Boolean(false));
    commutative_equals!(Val::Number(1.0), Val::Number(1.0));
    commutative_notequals!(Val::Number(1.0), Val::Number(2.0));
    commutative_equals!(Val::from("foo"), Val::from("foo"));
    commutative_notequals!(Val::from("foo"), Val::from("bar"));
    commutative_equals!(Val::from(Array::new()), Val::from(Array::new()));
    {
        let mut a0 = Val::from(Array::new());
        assert!(a0.push(once(Val::Null)).is_ok());
        let mut a1 = Val::from(Array::new());
        commutative_notequals!(a0, a1);
        assert!(a1.push(once(Val::Null)).is_ok());
        commutative_equals!(a0, a1);
    }

    // string mixed equality
    commutative_equals!(Val::from("0"), Val::Number(0.0));
    commutative_notequals!(Val::from("ten"), Val::Number(0.0));
    commutative_equals!(Val::from(""), Val::Boolean(false));
    commutative_equals!(Val::from("x"), Val::Boolean(true));
    commutative_equals!(Val::from(""), Val::Null);
    commutative_notequals!(Val::from("x"), Val::Null);
    commutative_notequals!(Val::from("x"), Val::from(Array::new()));

    // number mixed equality
    commutative_equals!(Val::Number(0.0), Val::Boolean(false));
    commutative_equals!(Val::Number(1.0), Val::Boolean(true));
    commutative_equals!(Val::Number(0.0), Val::Null);
    commutative_notequals!(Val::Number(1.0), Val::Null);
    commutative_equals!(Val::Number(0.0), Val::from(Array::new()));
    commutative_equals!(
        Val::Number(1.0),
        Val::from(Array::with_arr([Val::Null].into_iter().collect()))
    );
    commutative_notequals!(Val::Number(1.0), Val::from(Array::new()));

    // boolean mixed equality
    commutative_equals!(Val::Boolean(false), Val::Null);
    commutative_notequals!(Val::Boolean(true), Val::Null);
    commutative_notequals!(Val::Boolean(false), Val::from(Array::new()));

    // null mixed equality
    commutative_equals!(Val::Null, Val::from(Array::new()));
    commutative_notequals!(
        Val::Null,
        Val::from(Array::with_arr([Val::Null].into_iter().collect()))
    );
}

#[test]
fn compare() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!(
                $a.compare(&$b),
                Err(ValError::InvalidComparison($a.clone(), $b.clone()))
            );
            assert_eq!(
                $b.compare(&$a),
                Err(ValError::InvalidComparison($b.clone(), $a.clone()))
            );
        };
    }
    macro_rules! commutative_less {
        ($a:expr, $b:expr) => {
            assert_eq!($a.compare(&$b), Ok(Some(Ordering::Less)));
            assert_ne!($b.compare(&$a), Ok(Some(Ordering::Less)));
        };
    }
    macro_rules! commutative_equal {
        ($a:expr, $b:expr) => {
            assert_eq!($a.compare(&$b), Ok(Some(Ordering::Equal)));
            assert_eq!($b.compare(&$a), Ok(Some(Ordering::Equal)));
        };
    }
    macro_rules! commutative_none {
        ($a:expr, $b:expr) => {
            assert_eq!($a.compare(&$b), Ok(None));
            assert_eq!($b.compare(&$a), Ok(None));
        };
    }

    commutative_equal!(Val::Undefined, Val::Null);
    commutative_invalid!(Val::Undefined, Val::Boolean(false));
    commutative_invalid!(Val::Undefined, Val::Number(0.0));
    commutative_invalid!(Val::Undefined, Val::from(""));
    commutative_invalid!(Val::Undefined, Val::from(Array::new()));

    commutative_invalid!(Val::Boolean(false), Val::Boolean(true));
    commutative_less!(Val::Number(1.0), Val::Number(10.0));
    commutative_less!(Val::from("aardvark"), Val::from("bar"));
    commutative_less!(Val::from("02"), Val::from("10"));
    commutative_invalid!(Val::from(Array::new()), Val::from(Array::new()));

    // string mixed cmp
    commutative_less!(Val::from("0"), Val::Number(1.0));
    commutative_none!(Val::from("ten"), Val::Number(0.0));
    commutative_invalid!(Val::from(""), Val::Boolean(false));
    commutative_invalid!(Val::from("x"), Val::Boolean(true));
    commutative_less!(Val::Null, Val::from("x"));
    commutative_invalid!(Val::from("x"), Val::from(Array::new()));

    // number mixed cmp
    commutative_invalid!(Val::Number(10.0), Val::Boolean(false));
    commutative_less!(Val::Number(-1.0), Val::Null);
    commutative_less!(Val::Number(-1.0), Val::from(Array::new()));
    commutative_less!(
        Val::Number(0.0),
        Val::from(Array::with_arr([Val::Null].into_iter().collect()))
    );

    // boolean mixed cmp
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));

    // null mixed cmp
    commutative_less!(
        Val::Null,
        Val::from(Array::with_arr([Val::Null].into_iter().collect()))
    );
}

#[test]
fn inc_dec() {
    let inc = |mut val: Val, amount| {
        val.inc(amount)?;
        Ok(val)
    };
    assert_eq!(
        inc(Val::Undefined, 1),
        Err(ValError::InvalidOperationForType(
            "increment",
            Val::Undefined
        ))
    );
    assert_eq!(
        inc(Val::from("foo"), 1),
        Err(ValError::InvalidOperationForType(
            "increment",
            Val::from("foo")
        ))
    );
    assert_eq!(
        inc(Val::Undefined, -1),
        Err(ValError::InvalidOperationForType(
            "decrement",
            Val::Undefined
        ))
    );
    assert_eq!(
        inc(Val::from("foo"), -1),
        Err(ValError::InvalidOperationForType(
            "decrement",
            Val::from("foo")
        ))
    );

    assert_eq!(inc(Val::Null, 1), Ok(Val::Number(1.0)));
    assert_eq!(inc(Val::Null, 5), Ok(Val::Number(5.0)));
    assert_eq!(inc(Val::Null, -5), Ok(Val::Number(-5.0)));
    assert_eq!(inc(Val::Number(0.0), 1), Ok(Val::Number(1.0)));
    assert_eq!(inc(Val::Number(0.0), 5), Ok(Val::Number(5.0)));
    assert_eq!(inc(Val::Number(0.0), -5), Ok(Val::Number(-5.0)));
    assert_eq!(inc(Val::Boolean(false), 1), Ok(Val::Boolean(true)));
    assert_eq!(inc(Val::Boolean(false), 2), Ok(Val::Boolean(false)));
    assert_eq!(inc(Val::Boolean(false), 5), Ok(Val::Boolean(true)));
    assert_eq!(inc(Val::Boolean(false), -5), Ok(Val::Boolean(true)));
}

#[test]
fn plus() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!($a.plus(&$b), Val::Undefined);
            assert_eq!($b.plus(&$a), Val::Undefined);
        };
    }
    commutative_invalid!(Val::Undefined, Val::Null);
    commutative_invalid!(Val::Undefined, Val::Boolean(false));
    commutative_invalid!(Val::Undefined, Val::Number(0.0));
    commutative_invalid!(Val::Undefined, Val::from(Array::new()));

    commutative_invalid!(Val::Boolean(false), Val::Boolean(true));
    assert_eq!(Val::Number(1.0).plus(&Val::Number(10.0)), Val::Number(11.0));
    assert_eq!(
        Val::from("aardvark").plus(&Val::from("bar")),
        Val::from("aardvarkbar")
    );
    assert_eq!(
        Val::from("bar").plus(&Val::from("aardvark")),
        Val::from("baraardvark")
    );
    commutative_invalid!(Val::from(Array::new()), Val::from(Array::new()));

    // string mixed plus
    assert_eq!(Val::from("0").plus(&Val::Number(1.0)), Val::from("01"));
    assert_eq!(Val::Number(1.0).plus(&Val::from("0")), Val::from("10"));
    assert_eq!(
        Val::from("x").plus(&Val::Boolean(false)),
        Val::from("xfalse")
    );
    assert_eq!(Val::from("x").plus(&Val::Boolean(true)), Val::from("xtrue"));
    assert_eq!(
        Val::Boolean(false).plus(&Val::from("x")),
        Val::from("falsex")
    );
    assert_eq!(Val::Boolean(true).plus(&Val::from("x")), Val::from("truex"));
    assert_eq!(Val::from("x").plus(&Val::Null), Val::from("xnull"));
    assert_eq!(Val::Null.plus(&Val::from("x")), Val::from("nullx"));
    assert_eq!(
        Val::from("x").plus(&Val::Undefined),
        Val::from("xmysterious")
    );
    assert_eq!(
        Val::Undefined.plus(&Val::from("x")),
        Val::from("mysteriousx")
    );
    commutative_invalid!(Val::from("x"), Val::from(Array::new()));

    // number mixed plus
    commutative_invalid!(Val::Number(10.0), Val::Boolean(false));
    assert_eq!(Val::Number(-1.0).plus(&Val::Null), Val::Number(-1.0));
    assert_eq!(Val::Null.plus(&Val::Number(-1.0)), Val::Number(-1.0));
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed plus
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn multiply() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!($a.multiply(&$b), Val::Undefined);
            assert_eq!($b.multiply(&$a), Val::Undefined);
        };
    }
    commutative_invalid!(Val::Undefined, Val::Null);
    commutative_invalid!(Val::Undefined, Val::Boolean(false));
    commutative_invalid!(Val::Undefined, Val::Number(0.0));
    commutative_invalid!(Val::Undefined, Val::from(""));
    commutative_invalid!(Val::Undefined, Val::from(Array::new()));

    commutative_invalid!(Val::Boolean(false), Val::Boolean(true));
    assert_eq!(
        Val::Number(2.0).multiply(&Val::Number(10.0)),
        Val::Number(20.0)
    );
    commutative_invalid!(Val::from("aardvark"), Val::from("bar"));
    commutative_invalid!(Val::from(Array::new()), Val::from(Array::new()));

    // string mixed multiply
    assert_eq!(Val::from("0").multiply(&Val::Number(1.0)), Val::from("0"));
    assert_eq!(Val::from("0").multiply(&Val::Number(3.0)), Val::from("000"));
    assert_eq!(Val::from("0").multiply(&Val::Number(0.0)), Val::from(""));
    commutative_invalid!(Val::from("0"), &Val::Number(-1.0));
    commutative_invalid!(Val::from("x"), Val::Boolean(false));
    commutative_invalid!(Val::from("x"), Val::Null);
    commutative_invalid!(Val::from("x"), Val::from(Array::new()));

    // number mixed multiply
    commutative_invalid!(Val::Number(10.0), Val::Boolean(false));
    assert_eq!(Val::Number(-1.0).multiply(&Val::Null), Val::Number(0.0));
    assert_eq!(Val::Null.multiply(&Val::Number(-1.0)), Val::Number(0.0));
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed multiply
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn subtract() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!($a.subtract(&$b), Val::Undefined);
            assert_eq!($b.subtract(&$a), Val::Undefined);
        };
    }
    commutative_invalid!(Val::Undefined, Val::Null);
    commutative_invalid!(Val::Undefined, Val::Boolean(false));
    commutative_invalid!(Val::Undefined, Val::Number(0.0));
    commutative_invalid!(Val::Undefined, Val::from(""));
    commutative_invalid!(Val::Undefined, Val::from(Array::new()));

    commutative_invalid!(Val::Boolean(false), Val::Boolean(true));
    assert_eq!(
        Val::Number(2.0).subtract(&Val::Number(10.0)),
        Val::Number(-8.0)
    );
    commutative_invalid!(Val::from("aardvark"), Val::from("bar"));
    commutative_invalid!(Val::from(Array::new()), Val::from(Array::new()));

    // string mixed subtract
    commutative_invalid!(Val::from("0"), Val::Number(1.0));
    commutative_invalid!(Val::from("0"), &Val::Number(-1.0));
    commutative_invalid!(Val::from("x"), Val::Boolean(false));
    commutative_invalid!(Val::from("x"), Val::Null);
    commutative_invalid!(Val::from("x"), Val::from(Array::new()));

    // number mixed subtract
    commutative_invalid!(Val::Number(10.0), Val::Boolean(false));
    assert_eq!(Val::Number(-1.0).subtract(&Val::Null), Val::Number(-1.0));
    assert_eq!(Val::Null.subtract(&Val::Number(-1.0)), Val::Number(1.0));
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed subtract
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn divide() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!($a.divide(&$b), Val::Undefined);
            assert_eq!($b.divide(&$a), Val::Undefined);
        };
    }
    commutative_invalid!(Val::Undefined, Val::Null);
    commutative_invalid!(Val::Undefined, Val::Boolean(false));
    commutative_invalid!(Val::Undefined, Val::Number(0.0));
    commutative_invalid!(Val::Undefined, Val::from(""));
    commutative_invalid!(Val::Undefined, Val::from(Array::new()));

    commutative_invalid!(Val::Boolean(false), Val::Boolean(true));
    assert_eq!(
        Val::Number(2.0).divide(&Val::Number(10.0)),
        Val::Number(0.2)
    );
    commutative_invalid!(Val::from("aardvark"), Val::from("bar"));
    commutative_invalid!(Val::from(Array::new()), Val::from(Array::new()));

    // string mixed divide
    commutative_invalid!(Val::from("0"), Val::Number(1.0));
    commutative_invalid!(Val::from("0"), &Val::Number(-1.0));
    commutative_invalid!(Val::from("x"), Val::Boolean(false));
    commutative_invalid!(Val::from("x"), Val::Null);
    commutative_invalid!(Val::from("x"), Val::from(Array::new()));

    // number mixed divide
    commutative_invalid!(Val::Number(10.0), Val::Boolean(false));
    assert_eq!(
        Val::Number(-1.0).divide(&Val::Null),
        Val::Number(f64::NEG_INFINITY)
    );
    assert_eq!(Val::Null.divide(&Val::Number(-1.0)), Val::Number(0.0));
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed divide
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn negate() {
    assert_eq!(
        Val::Undefined.negate(),
        Err(ValError::InvalidOperationForType("negate", Val::Undefined))
    );
    assert_eq!(
        Val::Null.negate(),
        Err(ValError::InvalidOperationForType("negate", Val::Null))
    );
    assert_eq!(
        Val::Boolean(false).negate(),
        Err(ValError::InvalidOperationForType(
            "negate",
            Val::Boolean(false)
        ))
    );
    assert_eq!(
        Val::from("").negate(),
        Err(ValError::InvalidOperationForType("negate", Val::from("")))
    );
    assert_eq!(
        Val::from(Array::new()).negate(),
        Err(ValError::InvalidOperationForType(
            "negate",
            Val::from(Array::new())
        ))
    );
    assert_eq!(Val::Number(1.5).negate(), Ok(Val::Number(-1.5)));
    assert_eq!(Val::Number(-1.5).negate(), Ok(Val::Number(1.5)));
    assert_eq!(
        Val::Number(1.5).negate().and_then(|v| v.negate()),
        Ok(Val::Number(1.5))
    );
}

#[test]
fn round() {
    let round_up = |mut val: Val| {
        val.round_up()?;
        Ok(val)
    };
    assert_eq!(
        round_up(Val::Undefined),
        Err(ValError::InvalidOperationForType(
            "round up",
            Val::Undefined
        ))
    );
    assert_eq!(
        round_up(Val::Null),
        Err(ValError::InvalidOperationForType("round up", Val::Null))
    );
    assert_eq!(
        round_up(Val::Boolean(false)),
        Err(ValError::InvalidOperationForType(
            "round up",
            Val::Boolean(false)
        ))
    );
    assert_eq!(
        round_up(Val::from("")),
        Err(ValError::InvalidOperationForType("round up", Val::from("")))
    );
    assert_eq!(
        round_up(Val::from(Array::new())),
        Err(ValError::InvalidOperationForType(
            "round up",
            Val::from(Array::new())
        ))
    );
    assert_eq!(round_up(Val::Number(1.5)), Ok(Val::Number(2.0)));
    assert_eq!(round_up(Val::Number(2.0)), Ok(Val::Number(2.0)));
    assert_eq!(round_up(Val::Number(-1.5)), Ok(Val::Number(-1.0)));

    let round_down = |mut val: Val| {
        val.round_down()?;
        Ok(val)
    };
    assert_eq!(
        round_down(Val::Undefined),
        Err(ValError::InvalidOperationForType(
            "round down",
            Val::Undefined
        ))
    );
    assert_eq!(
        round_down(Val::Null),
        Err(ValError::InvalidOperationForType("round down", Val::Null))
    );
    assert_eq!(
        round_down(Val::Boolean(false)),
        Err(ValError::InvalidOperationForType(
            "round down",
            Val::Boolean(false)
        ))
    );
    assert_eq!(
        round_down(Val::from("")),
        Err(ValError::InvalidOperationForType(
            "round down",
            Val::from("")
        ))
    );
    assert_eq!(
        round_down(Val::from(Array::new())),
        Err(ValError::InvalidOperationForType(
            "round down",
            Val::from(Array::new())
        ))
    );
    assert_eq!(round_down(Val::Number(1.5)), Ok(Val::Number(1.0)));
    assert_eq!(round_down(Val::Number(2.0)), Ok(Val::Number(2.0)));
    assert_eq!(round_down(Val::Number(-1.5)), Ok(Val::Number(-2.0)));

    let round_nearest = |mut val: Val| {
        val.round_nearest()?;
        Ok(val)
    };
    assert_eq!(
        round_nearest(Val::Undefined),
        Err(ValError::InvalidOperationForType(
            "round nearest",
            Val::Undefined
        ))
    );
    assert_eq!(
        round_nearest(Val::Null),
        Err(ValError::InvalidOperationForType(
            "round nearest",
            Val::Null
        ))
    );
    assert_eq!(
        round_nearest(Val::Boolean(false)),
        Err(ValError::InvalidOperationForType(
            "round nearest",
            Val::Boolean(false)
        ))
    );
    assert_eq!(
        round_nearest(Val::from("")),
        Err(ValError::InvalidOperationForType(
            "round nearest",
            Val::from("")
        ))
    );
    assert_eq!(
        round_nearest(Val::from(Array::new())),
        Err(ValError::InvalidOperationForType(
            "round nearest",
            Val::from(Array::new())
        ))
    );
    assert_eq!(round_nearest(Val::Number(1.2)), Ok(Val::Number(1.0)));
    assert_eq!(round_nearest(Val::Number(1.5)), Ok(Val::Number(2.0)));
    assert_eq!(round_nearest(Val::Number(1.7)), Ok(Val::Number(2.0)));
    assert_eq!(round_nearest(Val::Number(2.0)), Ok(Val::Number(2.0)));
    assert_eq!(round_nearest(Val::Number(-1.2)), Ok(Val::Number(-1.0)));
    assert_eq!(round_nearest(Val::Number(-1.5)), Ok(Val::Number(-2.0)));
    assert_eq!(round_nearest(Val::Number(-1.7)), Ok(Val::Number(-2.0)));
}

#[test]
fn split() {
    let split = |val: Val, p| {
        let mut copy = val.clone();
        copy.split(p).map(|_| copy)
    };

    assert_eq!(split(Val::from(""), None), Ok(Array::new().into()));
    assert_eq!(
        split(Val::from(""), Some(Val::from(','))),
        Ok(Array::new().into())
    );
    assert_eq!(
        split(Val::from("a,b,c"), Some(Val::from(','))),
        Ok(Array::with_arr(['a', 'b', 'c'].into_iter().map(Val::from).collect()).into())
    );
    assert_eq!(
        split(Val::from("a,b,c"), Some(Val::from(""))),
        Ok(Array::with_arr(
            ['a', ',', 'b', ',', 'c']
                .into_iter()
                .map(Val::from)
                .collect()
        )
        .into())
    );
    assert_eq!(
        split(Val::from("a,b,c"), None),
        Ok(Array::with_arr(
            ['a', ',', 'b', ',', 'c']
                .into_iter()
                .map(Val::from)
                .collect()
        )
        .into())
    );

    assert_eq!(
        split(Val::from(""), Some(Val::Undefined)),
        Err(ValError::InvalidSplitDelimiter(Val::Undefined))
    );
    assert_eq!(
        split(Val::from("x"), Some(Val::Undefined)),
        Err(ValError::InvalidSplitDelimiter(Val::Undefined))
    );
    assert_eq!(
        split(Val::from(""), Some(Val::Null)),
        Err(ValError::InvalidSplitDelimiter(Val::Null))
    );
    assert_eq!(
        split(Val::from("x"), Some(Val::Null)),
        Err(ValError::InvalidSplitDelimiter(Val::Null))
    );
    assert_eq!(
        split(Val::from(""), Some(Val::Boolean(false))),
        Err(ValError::InvalidSplitDelimiter(Val::Boolean(false)))
    );
    assert_eq!(
        split(Val::from("x"), Some(Val::Boolean(false))),
        Err(ValError::InvalidSplitDelimiter(Val::Boolean(false)))
    );
    assert_eq!(
        split(Val::from(""), Some(Val::Number(0.0))),
        Err(ValError::InvalidSplitDelimiter(Val::Number(0.0)))
    );
    assert_eq!(
        split(Val::from("x"), Some(Val::Number(0.0))),
        Err(ValError::InvalidSplitDelimiter(Val::Number(0.0)))
    );
    assert_eq!(
        split(Val::from(""), Some(Array::new().into())),
        Err(ValError::InvalidSplitDelimiter(Array::new().into()))
    );
    assert_eq!(
        split(Val::from("x"), Some(Array::new().into())),
        Err(ValError::InvalidSplitDelimiter(Array::new().into()))
    );

    assert_eq!(
        split(Val::Undefined, Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType("split", Val::Undefined))
    );
    assert_eq!(
        split(Val::Null, Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType("split", Val::Null))
    );
    assert_eq!(
        split(Val::Boolean(false), Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType(
            "split",
            Val::Boolean(false)
        ))
    );
    assert_eq!(
        split(Val::Number(0.0), Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType("split", Val::Number(0.0)))
    );
    assert_eq!(
        split(Array::new().into(), Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType(
            "split",
            Array::new().into()
        ))
    );
}

#[test]
fn join() {
    let join = |val: Val, p| {
        let mut copy = val.clone();
        copy.join(p).map(|_| copy)
    };

    assert_eq!(join(Array::new().into(), None), Ok(Val::from("")));
    assert_eq!(
        join(
            Array::with_arr(['1', '2', '3'].into_iter().map(Val::from).collect()).into(),
            Some(Val::from(", "))
        ),
        Ok(Val::from("1, 2, 3"))
    );
    assert_eq!(
        join(
            Array::with_arr(['1', '2', '3'].into_iter().map(Val::from).collect()).into(),
            None
        ),
        Ok(Val::from("123"))
    );
    assert_eq!(
        join(
            Array::with_arr(
                [Val::from('1'), Val::Number(2.0), Val::from('3')]
                    .into_iter()
                    .collect()
            )
            .into(),
            None
        ),
        Err(ValError::InvalidArrayElementForJoin(Val::Number(2.0)))
    );

    assert_eq!(
        join(
            Array::with_arr_and_dict(
                ['1', '2', '3'].into_iter().map(Val::from).collect(),
                [(DictKey::Null, Val::from('4'))].into_iter().collect()
            )
            .into(),
            Some(Val::from(", "))
        ),
        Ok(Val::from("1, 2, 3, 4"))
    );
    assert_eq!(
        join(
            Array::with_arr_and_dict(
                ['1', '2', '3'].into_iter().map(Val::from).collect(),
                [(DictKey::Null, Val::Number(4.0))].into_iter().collect()
            )
            .into(),
            None
        ),
        Err(ValError::InvalidArrayElementForJoin(Val::Number(4.0)))
    );

    assert_eq!(
        join(Array::new().into(), Some(Val::Undefined)),
        Err(ValError::InvalidJoinDelimiter(Val::Undefined))
    );
    assert_eq!(
        join(Array::new().into(), Some(Val::Null)),
        Err(ValError::InvalidJoinDelimiter(Val::Null))
    );
    assert_eq!(
        join(Array::new().into(), Some(Val::Boolean(false))),
        Err(ValError::InvalidJoinDelimiter(Val::Boolean(false)))
    );
    assert_eq!(
        join(Array::new().into(), Some(Val::Number(0.0))),
        Err(ValError::InvalidJoinDelimiter(Val::Number(0.0)))
    );
    assert_eq!(
        join(Array::new().into(), Some(Array::new().into())),
        Err(ValError::InvalidJoinDelimiter(Array::new().into()))
    );

    assert_eq!(
        join(Val::Undefined, Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType("join", Val::Undefined))
    );
    assert_eq!(
        join(Val::Null, Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType("join", Val::Null))
    );
    assert_eq!(
        join(Val::Boolean(false), Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType(
            "join",
            Val::Boolean(false)
        ))
    );
    assert_eq!(
        join(Val::Number(0.0), Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType("join", Val::Number(0.0)))
    );
    assert_eq!(
        join(Val::from(""), Some(Val::Undefined)),
        Err(ValError::InvalidOperationForType("join", Val::from("")))
    );
}

#[test]
fn cast() {
    let cast = |val: Val, p| {
        let mut copy = val.clone();
        copy.cast(p).map(|_| copy)
    };

    assert_eq!(cast(Val::Number(65.0), None), Ok(Val::from("A")));
    assert_eq!(cast(Val::Number(1046.0), None), Ok(Val::from("Ж")));
    assert_eq!(cast(Val::Number(128175.0), None), Ok(Val::from('💯')));
    assert_eq!(
        cast(Val::Number(1046.5), None),
        Err(ValError::ConvertingNumberToCharacterFailed(1046.5))
    );

    assert_eq!(cast(Val::from("123.45"), None), Ok(Val::Number(123.45)));
    assert_eq!(
        cast(Val::from("ff"), Some(Val::Number(16.0))),
        Ok(Val::Number(255.0))
    );
    assert_eq!(cast(Val::from("12345"), None), Ok(Val::Number(12345.0)));
    assert_eq!(
        cast(Val::from("AA"), Some(Val::Number(16.0))),
        Ok(Val::Number(170.0))
    );
    assert_eq!(
        cast(Val::from("AA"), Some(Val::Number(16.6))),
        Err(ValError::InvalidStringToIntegerRadix(Val::Number(16.6)))
    );

    assert_eq!(
        cast(Val::from("AA"), Some(Val::Undefined)),
        Err(ValError::InvalidStringToIntegerRadix(Val::Undefined))
    );
    assert_eq!(
        cast(Val::from("AA"), Some(Val::Null)),
        Err(ValError::InvalidStringToIntegerRadix(Val::Null))
    );
    assert_eq!(
        cast(Val::from("AA"), Some(Val::Boolean(false))),
        Err(ValError::InvalidStringToIntegerRadix(Val::Boolean(false)))
    );
    assert_eq!(
        cast(Val::from("AA"), Some(Val::from(""))),
        Err(ValError::InvalidStringToIntegerRadix(Val::from("")))
    );
    assert_eq!(
        cast(Val::from("AA"), Some(Array::new().into())),
        Err(ValError::InvalidStringToIntegerRadix(Array::new().into()))
    );

    assert_eq!(
        cast(Val::Undefined, None),
        Err(ValError::InvalidOperationForType("cast", Val::Undefined))
    );
    assert_eq!(
        cast(Val::Null, None),
        Err(ValError::InvalidOperationForType("cast", Val::Null))
    );
    assert_eq!(
        cast(Val::Boolean(false), None),
        Err(ValError::InvalidOperationForType(
            "cast",
            Val::Boolean(false)
        ))
    );
    assert_eq!(
        cast(Array::new().into(), None),
        Err(ValError::InvalidOperationForType(
            "cast",
            Array::new().into()
        ))
    );
}

#[test]
fn display() {
    assert_eq!(format!("{}", Val::Undefined), "mysterious");
    assert_eq!(format!("{}", Val::Null), "null");
    assert_eq!(format!("{}", Val::Boolean(false)), "false");
    assert_eq!(format!("{}", Val::Number(5.0)), "5");
    assert_eq!(format!("{}", Val::from("string")), "\"string\"");

    assert_eq!(format!("{}", Array::new()), "[]");
    assert_eq!(
        format!(
            "{}",
            Array::with_arr(
                [Val::Null, Val::Boolean(true), Val::from("hello")]
                    .into_iter()
                    .collect()
            )
        ),
        "[null, true, \"hello\"]"
    );
    assert_eq!(
        format!(
            "{}",
            Array::with_arr_and_dict(
                [Val::Null, Val::Boolean(true), Val::from("hello")]
                    .into_iter()
                    .collect(),
                [
                    (DictKey::Null, Val::Undefined),
                    (DictKey::String("text".into()), Array::new().into())
                ]
                .into_iter()
                .collect()
            )
        ),
        "[null, true, \"hello\", \"text\": [], null: mysterious]"
    );
    assert_eq!(
        format!(
            "{}",
            Array::with_arr_and_dict(
                [Val::Null, Val::Boolean(true), Array::with_arr_and_dict([Val::Null].into_iter().collect(), [(DictKey::Undefined, Val::Number(0.0))].into_iter().collect()).into()]
                    .into_iter()
                    .collect(),
                [
                    (DictKey::Null, Val::Undefined),
                    (DictKey::String("text".into()), Array::with_arr_and_dict([Val::Undefined].into_iter().collect(), [(DictKey::Boolean(true), Val::from("str"))].into_iter().collect()).into())
                ]
                .into_iter()
                .collect()
            )
        ),
        "[null, true, [null, mysterious: 0], \"text\": [mysterious, true: \"str\"], null: mysterious]"
    );
}
