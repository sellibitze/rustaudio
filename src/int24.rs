// The MIT License (MIT)
// 
// Copyright (c) 2014 Sebastian Gesemann
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

use std::ptr;
use std::default::Default;

/// Type for a packed 24-bit signed int in two's complement.
/// Conversion from and to i32 is provided.
#[packed]
#[allow(non_camel_case_types)]
pub struct i24 {
    repr: [u8,..3] // machine-dependend (little or big endian)
}

#[inline]
fn clipped(s: i32) -> i32 {
    if s < -0x800000 { return -0x800000; }
    if s > 0x7FFFFF { return 0x7FFFFF; }
    s
}

#[cfg(target_endian="little")]
#[inline]
fn raw24_ptr(r: &i32) -> *const u8 {
    // no need for an offset on a little endian machine
    r as *const i32 as *const u8
}

#[cfg(target_endian="big")]
#[inline]
fn raw24_ptr(r: &i32) -> *const u8 {
    // let's skip the most significant byte
    unsafe { (r as *const i32 as *const u8).offset(1) }
}

#[cfg(target_endian="little")]
#[inline]
fn raw24_mut_ptr(r: &mut i32) -> *mut u8 {
    // no need for an offset on a little endian machine
    r as *mut i32 as *mut u8
}

#[cfg(target_endian="big")]
#[inline]
fn raw24_mut_ptr(r: &mut i32) -> *mut u8 {
    // let's skip the most significant byte
    unsafe { (r as *mut i32 as *mut u8).offset(1) }
}

impl i24 {
    /// converts an i32 to an i24. Values that are not representable are clipped
    pub fn from_i32_clipped(s: i32) -> i24 {
        let src = clipped(s);
        let mut dst = i24 { repr: [0,..3] };
        unsafe {
            ptr::copy_nonoverlapping_memory::<u8>(
                &mut dst.repr[0], // target
                raw24_ptr(&src),  // source
                3);
        }
        dst
    }
    /// converts an i24 to an i32.
    pub fn to_i32(&self) -> i32 {
        let mut dst: i32 = 0;
        unsafe {
            ptr::copy_nonoverlapping_memory::<u8>(
                raw24_mut_ptr(&mut dst), // target
                &self.repr[0],           // source
                3);
        }
        // return with sign extension
        dst - ((dst & 0x800000) << 1)
    }
}

impl Default for i24 {
    fn default() -> i24 {
        i24 { repr: [0,..3] }
    }
}

#[cfg(test)]
mod i24_test {

    use super::i24;

    #[cfg(target_endian="little")]
    fn manually_assemble(x: i24) -> i32 {
        let tmp =
            (x.repr[0] as i32) |
            (x.repr[1] as i32) << 8 |
            (x.repr[2] as i32) << 16;
        if tmp >= 0x800000 {
            tmp - 0x1000000
        } else {
            tmp
        }
    }

    #[cfg(target_endian="big")]
    fn manually_assemble(x: i24) -> i32 {
        let tmp =
            (x.repr[0] as i32) << 16 |
            (x.repr[1] as i32) << 8 |
            (x.repr[2] as i32);
        if tmp >= 0x800000 {
            tmp - 0x1000000
        } else {
            tmp
        }
    }

    #[test]
    fn doit() {
        let numbers = [ -0x800001,-0x800000,-0x7FFFFF,
                        -2,-1,0i32,1,2,
                         0x7FFFFE,0x7FFFFF,0x800000 ];
        let expectd = [ -0x800000,-0x800000,-0x7FFFFF,
                        -2,-1,0i32,1,2,
                         0x7FFFFE,0x7FFFFF,0x7FFFFF ];
        for (&n1,&ex) in numbers.iter().zip(expectd.iter()) {
            let n2 = i24::from_i32_clipped(n1);
            let n3 = n2.to_i32();
            assert!(manually_assemble(n2) == n3);
            assert!(n3 == ex);
        }
    }

} // mod test

