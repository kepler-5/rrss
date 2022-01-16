use super::*;

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
        arr.index(&Val::Number(100.0)),
        Err(ValueError::IndexOutOfBounds)
    );
    assert_eq!(arr.index(&"100.0".into()), Err(ValueError::NoValueForKey));
}

#[test]
fn not_indexable() {
    assert_eq!(
        Val::Null.index(&Val::Number(100.0)),
        Err(ValueError::NotIndexable)
    );
    assert_eq!(
        Val::Boolean(false).index(&Val::Number(100.0)),
        Err(ValueError::NotIndexable)
    );
    assert_eq!(
        Val::Number(100.0).index(&Val::Number(100.0)),
        Err(ValueError::NotIndexable)
    );
    assert_eq!(
        Val::Number(100.0).index(&"100.0".into()),
        Err(ValueError::NotIndexable)
    );

    // undefined's become arrays when you write to an index
    let mut und = Val::Undefined;
    assert!(und.is_undefined());
    assert_eq!(und.index(&Val::Null), Err(ValueError::NotIndexable));
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
    assert_eq!(
        s.index(&Val::Number(3.0)),
        Err(ValueError::IndexOutOfBounds)
    );
    assert_eq!(s.index(&Val::Null), Err(ValueError::InvalidKey));

    assert_eq!(
        s.index_or_insert(&Val::Null),
        Err(ValueError::IndexNotAssignable)
    );
}

#[test]
fn array_push_pop() {
    let mut arr = Val::Undefined;

    let ok = arr.push(Val::Null);
    assert!(ok.is_ok());
    assert!(arr.is_array());

    assert_eq!(arr.index(&Val::Number(0.0)).unwrap().as_ref(), &Val::Null);
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(1.0)));

    let ok = arr.push(Val::Boolean(true));
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
    assert_eq!(err, Err(ValueError::PopOnEmptyArray));

    assert_eq!(
        Val::Null.push(Val::Null),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(Val::Null.pop(), Err(ValueError::InvalidOperationForType));
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
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(1.0)));
    *arr.index_or_insert(&Val::Boolean(false)).unwrap() = Val::Number(0.0);
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(2.0)));
    *arr.index_or_insert(&Val::Number(0.0)).unwrap() = Val::Number(1.0);
    assert_eq!(arr.decay(), Cow::Owned(Val::Number(3.0)));
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
    assert_eq!(arr.to_string_for_output().into_owned(), "1");
    *arr.index_or_insert(&Val::Boolean(false)).unwrap() = Val::Number(0.0);
    assert_eq!(arr.to_string_for_output().into_owned(), "2");
    *arr.index_or_insert(&Val::Number(0.0)).unwrap() = Val::Number(1.0);
    assert_eq!(arr.to_string_for_output().into_owned(), "3");
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
    commutative_notequals!(Val::Undefined, Val::Null);
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
        assert!(a0.push(Val::Null).is_ok());
        let mut a1 = Val::from(Array::new());
        commutative_notequals!(a0, a1);
        assert!(a1.push(Val::Null).is_ok());
        commutative_equals!(a0, a1);
    }

    // string mixed equality
    commutative_equals!(Val::from("0"), Val::Number(0.0));
    commutative_notequals!(Val::from("ten"), Val::Number(0.0));
    commutative_equals!(Val::from(""), Val::Boolean(false));
    commutative_equals!(Val::from("x"), Val::Boolean(true));
    commutative_notequals!(Val::from("x"), Val::Null);
    commutative_notequals!(Val::from("x"), Val::from(Array::new()));

    // number mixed equality
    commutative_equals!(Val::Number(0.0), Val::Boolean(false));
    commutative_equals!(Val::Number(1.0), Val::Boolean(true));
    commutative_equals!(Val::Number(0.0), Val::Null);
    commutative_notequals!(Val::Number(1.0), Val::Null);
    commutative_notequals!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed equality
    commutative_equals!(Val::Boolean(false), Val::Null);
    commutative_notequals!(Val::Boolean(true), Val::Null);
    commutative_notequals!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn compare() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!($a.compare(&$b), Err(ValueError::InvalidComparison));
            assert_eq!($b.compare(&$a), Err(ValueError::InvalidComparison));
        };
    }
    macro_rules! commutative_less {
        ($a:expr, $b:expr) => {
            assert_eq!($a.compare(&$b), Ok(Some(Ordering::Less)));
            assert_ne!($b.compare(&$a), Ok(Some(Ordering::Less)));
        };
    }
    macro_rules! commutative_none {
        ($a:expr, $b:expr) => {
            assert_eq!($a.compare(&$b), Ok(None));
            assert_eq!($b.compare(&$a), Ok(None));
        };
    }

    commutative_invalid!(Val::Undefined, Val::Null);
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
    commutative_invalid!(Val::from("x"), Val::Null);
    commutative_invalid!(Val::from("x"), Val::from(Array::new()));

    // number mixed cmp
    commutative_invalid!(Val::Number(10.0), Val::Boolean(false));
    commutative_less!(Val::Number(-1.0), Val::Null);
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed equality
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn inc_dec() {
    let inc = |mut val: Val, amount| {
        val.inc(amount)?;
        Ok(val)
    };
    assert_eq!(
        inc(Val::Undefined, 1),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        inc(Val::from("foo"), 1),
        Err(ValueError::InvalidOperationForType)
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
            assert_eq!($a.plus(&$b), Err(ValueError::InvalidOperationForType));
            assert_eq!($b.plus(&$a), Err(ValueError::InvalidOperationForType));
        };
    }
    commutative_invalid!(Val::Undefined, Val::Null);
    commutative_invalid!(Val::Undefined, Val::Boolean(false));
    commutative_invalid!(Val::Undefined, Val::Number(0.0));
    commutative_invalid!(Val::Undefined, Val::from(Array::new()));

    commutative_invalid!(Val::Boolean(false), Val::Boolean(true));
    assert_eq!(
        Val::Number(1.0).plus(&Val::Number(10.0)),
        Ok(Val::Number(11.0))
    );
    assert_eq!(
        Val::from("aardvark").plus(&Val::from("bar")),
        Ok(Val::from("aardvarkbar"))
    );
    assert_eq!(
        Val::from("bar").plus(&Val::from("aardvark")),
        Ok(Val::from("baraardvark"))
    );
    commutative_invalid!(Val::from(Array::new()), Val::from(Array::new()));

    // string mixed plus
    assert_eq!(Val::from("0").plus(&Val::Number(1.0)), Ok(Val::from("01")));
    assert_eq!(Val::Number(1.0).plus(&Val::from("0")), Ok(Val::from("10")));
    assert_eq!(
        Val::from("x").plus(&Val::Boolean(false)),
        Ok(Val::from("xfalse"))
    );
    assert_eq!(
        Val::from("x").plus(&Val::Boolean(true)),
        Ok(Val::from("xtrue"))
    );
    assert_eq!(
        Val::Boolean(false).plus(&Val::from("x")),
        Ok(Val::from("falsex"))
    );
    assert_eq!(
        Val::Boolean(true).plus(&Val::from("x")),
        Ok(Val::from("truex"))
    );
    assert_eq!(Val::from("x").plus(&Val::Null), Ok(Val::from("xnull")));
    assert_eq!(Val::Null.plus(&Val::from("x")), Ok(Val::from("nullx")));
    assert_eq!(
        Val::from("x").plus(&Val::Undefined),
        Ok(Val::from("xmysterious"))
    );
    assert_eq!(
        Val::Undefined.plus(&Val::from("x")),
        Ok(Val::from("mysteriousx"))
    );
    commutative_invalid!(Val::from("x"), Val::from(Array::new()));

    // number mixed plus
    commutative_invalid!(Val::Number(10.0), Val::Boolean(false));
    commutative_invalid!(Val::Number(-1.0), Val::Null);
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed plus
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn multiply() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!($a.multiply(&$b), Err(ValueError::InvalidOperationForType));
            assert_eq!($b.multiply(&$a), Err(ValueError::InvalidOperationForType));
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
        Ok(Val::Number(20.0))
    );
    commutative_invalid!(Val::from("aardvark"), Val::from("bar"));
    commutative_invalid!(Val::from(Array::new()), Val::from(Array::new()));

    // string mixed multiply
    assert_eq!(
        Val::from("0").multiply(&Val::Number(1.0)),
        Ok(Val::from("0"))
    );
    assert_eq!(
        Val::from("0").multiply(&Val::Number(3.0)),
        Ok(Val::from("000"))
    );
    assert_eq!(
        Val::from("0").multiply(&Val::Number(0.0)),
        Ok(Val::from(""))
    );
    commutative_invalid!(Val::from("0"), &Val::Number(-1.0));
    commutative_invalid!(Val::from("x"), Val::Boolean(false));
    commutative_invalid!(Val::from("x"), Val::Null);
    commutative_invalid!(Val::from("x"), Val::from(Array::new()));

    // number mixed multiply
    commutative_invalid!(Val::Number(10.0), Val::Boolean(false));
    commutative_invalid!(Val::Number(-1.0), Val::Null);
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed multiply
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn subtract() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!($a.subtract(&$b), Err(ValueError::InvalidOperationForType));
            assert_eq!($b.subtract(&$a), Err(ValueError::InvalidOperationForType));
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
        Ok(Val::Number(-8.0))
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
    commutative_invalid!(Val::Number(-1.0), Val::Null);
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed subtract
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn divide() {
    macro_rules! commutative_invalid {
        ($a:expr, $b:expr) => {
            assert_eq!($a.divide(&$b), Err(ValueError::InvalidOperationForType));
            assert_eq!($b.divide(&$a), Err(ValueError::InvalidOperationForType));
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
        Ok(Val::Number(0.2))
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
    commutative_invalid!(Val::Number(-1.0), Val::Null);
    commutative_invalid!(Val::Number(0.0), Val::from(Array::new()));

    // boolean mixed divide
    commutative_invalid!(Val::Boolean(false), Val::Null);
    commutative_invalid!(Val::Boolean(false), Val::from(Array::new()));
}

#[test]
fn negate() {
    assert_eq!(
        Val::Undefined.negate(),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(Val::Null.negate(), Err(ValueError::InvalidOperationForType));
    assert_eq!(
        Val::Boolean(false).negate(),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        Val::from("").negate(),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        Val::from(Array::new()).negate(),
        Err(ValueError::InvalidOperationForType)
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
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_up(Val::Null),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_up(Val::Boolean(false)),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_up(Val::from("")),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_up(Val::from(Array::new())),
        Err(ValueError::InvalidOperationForType)
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
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_down(Val::Null),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_down(Val::Boolean(false)),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_down(Val::from("")),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_down(Val::from(Array::new())),
        Err(ValueError::InvalidOperationForType)
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
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_nearest(Val::Null),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_nearest(Val::Boolean(false)),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_nearest(Val::from("")),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(
        round_nearest(Val::from(Array::new())),
        Err(ValueError::InvalidOperationForType)
    );
    assert_eq!(round_nearest(Val::Number(1.2)), Ok(Val::Number(1.0)));
    assert_eq!(round_nearest(Val::Number(1.5)), Ok(Val::Number(2.0)));
    assert_eq!(round_nearest(Val::Number(1.7)), Ok(Val::Number(2.0)));
    assert_eq!(round_nearest(Val::Number(2.0)), Ok(Val::Number(2.0)));
    assert_eq!(round_nearest(Val::Number(-1.2)), Ok(Val::Number(-1.0)));
    assert_eq!(round_nearest(Val::Number(-1.5)), Ok(Val::Number(-2.0)));
    assert_eq!(round_nearest(Val::Number(-1.7)), Ok(Val::Number(-2.0)));
}
