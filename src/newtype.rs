use std::num::One;
use std::hash::Hash;

/// Wrapper for more type-safe programming.
/// 
/// The first generic parameter denotes the type of the wrapped value while
/// the second one can be used as a "tag" (possibly a phantom type) to
/// differentiate between multiple types of the `NewType` family.
pub struct NewType<T, U> {
    /// the wrapped value
    pub wrapped: T
}

impl<T, U> NewType<T, U> {
    #[inline]
    pub fn wrap(v: T) -> NewType<T,U> { NewType::<T,U> { wrapped: v } }
    #[inline]
    pub fn unwrap(self) -> T { self.wrapped }
}

impl<S, T: Hash<S>, U> Hash<S> for NewType<T, U> {
    #[inline]
    fn hash(&self, state: &mut S) { self.wrapped.hash(state); }
}

impl<T: Clone, U> Clone for NewType<T, U> {
    #[inline]
    fn clone(&self) -> NewType<T, U> { NewType::wrap(self.wrapped.clone()) }
}

impl<T: PartialEq, U> PartialEq for NewType<T, U> {
    #[inline]
    fn eq(&self, other: &NewType<T, U>) -> bool { self.wrapped == other.wrapped }
    #[inline] 
    fn ne(&self, other: &NewType<T, U>) -> bool { self.wrapped != other.wrapped }
}

impl<T: Eq, U> Eq for NewType<T, U> {}

impl<T: PartialOrd, U> PartialOrd for NewType<T,U> {
    #[inline]
    fn partial_cmp(&self, other: &NewType<T,U>) -> Option<Ordering> {
        self.wrapped.partial_cmp(&other.wrapped)
    }
}

impl<T: Ord, U> Ord for NewType<T,U> {
    #[inline]
    fn cmp(&self, other: &NewType<T,U>) -> Ordering {
        self.wrapped.cmp(&other.wrapped)
    }
}

macro_rules! impl_binop(
    ($n1:ident, $n2:ident) => (
        impl<T: $n1<T,T>, U> $n1<NewType<T,U>,NewType<T,U>> for NewType<T,U> {
            #[inline]
            fn $n2(&self, other: &NewType<T,U>) -> NewType<T,U> {
                NewType::wrap(self.wrapped.$n2(&other.wrapped))
            }
        }
    )
)

impl_binop!(Add,add)
impl_binop!(Sub,sub)
impl_binop!(BitAnd,bitand)
impl_binop!(BitOr,bitor)
impl_binop!(BitXor,bitxor)

impl<Res, T: Neg<Res>, U> Neg<NewType<Res, U>> for NewType<T, U> {
    #[inline]
    fn neg(&self) -> NewType<Res, U> { NewType::wrap(-self.wrapped) }
}

impl<Res, T: Not<Res>, U> Not<NewType<Res, U>> for NewType<T, U> {
    #[inline]
    fn not(&self) -> NewType<Res, U> { NewType::wrap(!self.wrapped) }
}

/// Represents an iterator over a range of `NewType<T,U>` values
pub struct NewRange<T, U> {
    beg: T,
    end: T
}

impl<T: FromPrimitive, U> NewRange<T,U> {
    /// create `NewRange` of given half-open interval
    pub fn new(b: NewType<T,U>, e: NewType<T,U>) -> NewRange<T,U> {
        NewRange::<T,U> { beg: b.unwrap(), end: e.unwrap() }
    }
    /// create `NewRange` of with 0 being the start (inclusive)
    pub fn new0(e: NewType<T,U>) -> NewRange<T,U> {
        NewRange::<T,U> { beg: FromPrimitive::from_int(0).unwrap(), end: e.unwrap() }
    }
}

impl<U, T: Int + Clone + Add<T,T> + One + PartialEq>
Iterator<NewType<T, U>> for NewRange<T, U> {
    fn next(&mut self) -> Option<NewType<T, U>> {
        if self.beg == self.end {
            None
        } else {
            let tmp = self.beg.clone();
            self.beg = self.beg + One::one();
            Some(NewType::wrap(tmp))
        }
    }
}

impl<U, T: Int + Clone + Add<T,T> + Sub<T,T> + One + PartialEq>
DoubleEndedIterator<NewType<T, U>> for NewRange<T, U> {
    fn next_back(&mut self) -> Option<NewType<T, U>> {
        if self.beg == self.end {
            None
        } else {
            self.end = self.end - One::one();
            Some(NewType::wrap(self.end.clone()))
        }
    }
}

