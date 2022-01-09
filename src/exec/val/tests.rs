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
