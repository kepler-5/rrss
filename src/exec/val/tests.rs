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

    assert_eq!(s.index(&Val::Number(0.0)), Ok("b".into()));
    assert_eq!(s.index(&Val::Number(1.0)), Ok("a".into()));
    assert_eq!(s.index(&Val::Number(2.0)), Ok("r".into()));
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
    assert_eq!(arr.decay(), Val::Number(1.0).into());

    let ok = arr.push(Val::Boolean(true));
    assert!(ok.is_ok());
    assert_eq!(arr.index(&Val::Number(0.0)).unwrap().as_ref(), &Val::Null);
    assert_eq!(
        arr.index(&Val::Number(1.0)).unwrap().as_ref(),
        &Val::Boolean(true)
    );
    assert_eq!(arr.decay(), Val::Number(2.0).into());

    let ok = arr.pop();
    assert!(ok.is_ok());
    assert_eq!(
        arr.index(&Val::Number(0.0)).unwrap().as_ref(),
        &Val::Boolean(true)
    );
    assert_eq!(arr.decay(), Val::Number(1.0).into());

    let ok = arr.pop();
    assert!(ok.is_ok());
    assert_eq!(arr.decay(), Val::Number(0.0).into());

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
    assert_eq!(Val::Undefined.decay(), (&Val::Undefined).into());
    assert_eq!(Val::Null.decay(), (&Val::Null).into());
    assert_eq!(Val::Boolean(false).decay(), (&Val::Boolean(false)).into());
    assert_eq!(Val::Number(0.0).decay(), (&Val::Number(0.0)).into());
    assert_eq!(Val::from("foo").decay(), (&Val::from("foo")).into());
    assert_eq!(Val::from(Array::new()).decay(), Val::Number(0.0).into());

    let mut arr = Val::from(Array::new());

    *arr.index_or_insert(&Val::Null).unwrap() = Val::Boolean(true);
    assert_eq!(arr.decay(), Val::Number(1.0).into());
    *arr.index_or_insert(&Val::Boolean(false)).unwrap() = Val::Number(0.0);
    assert_eq!(arr.decay(), Val::Number(2.0).into());
    *arr.index_or_insert(&Val::Number(0.0)).unwrap() = Val::Number(1.0);
    assert_eq!(arr.decay(), Val::Number(3.0).into());
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
