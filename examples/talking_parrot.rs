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

//! talking_parrot simply records what you say at 32 kHz and plays it back to
//! you at 48 kHz

#![feature(unsafe_destructor)]

extern crate rustaudio;
extern crate num;

use rustaudio::{ RustAudio, RaResult, StreamFlags };
use rustaudio::{ RaStreamAny, RaOutputStreamExt, RaInputStreamExt, Sample };
use rustaudio::{ RaOutputStream, RaInputStream };

use std::iter::AdditiveIterator;

fn sum_of_squares(on: &[i16]) -> i64 {
    on.iter().map(|&x| x as i64).map(|x| x * x).sum()
}

fn root_mean_square(on: &[i16]) -> f64 {
    ((sum_of_squares(on) as f64) / (on.len() as f64)).sqrt()
}

/// Removes `count` elements from the front of the vector.
/// The function fails if the `v.len()` is less than `count`.
fn remove_at_front<T>(v: &mut Vec<T>, count: uint) {
    assert!(v.len() >= count);
    // Unfortunately, Vec does not offer such a function.
    // The sane way of doing this is actually to get our hands dirty.
    // (maybe a ringbuffer is the better approach but I like
    // the Vec interface otherwise w.r.t. slices and such).
    unsafe {
        let newlen = v.len() - count;
        {
            let sli = v.as_mut_slice();
            let ptr = sli.as_mut_ptr();
            // properly drop the first `count` elements
            for i in range(0, count as int) {
                drop(std::ptr::read(ptr.offset(i) as *const T));
            }
            // move remaining elements to the front
            std::ptr::copy_memory(ptr, // <-- destination
                                  ptr.offset(count as int) as *const T,
                                  newlen);
        }
        // restore all invariants by updating the length
        v.set_len(newlen);
    } // end of unsafe block
}

/// This function starts recording and checks for a certain "loudness" level.
/// if it's high enough for three consecutive blocks, the 5 buffered ones
/// are saved and new audio data is appended to the vector. `record` continues
/// to minotor loudness and if it's too quiet for too long, recording will stop
/// and the function will return the audio data.
fn record(inp: &RaInputStream<i16>, mut buff: Vec<i16>) -> RaResult<Vec<i16>> {
    let block = 2400;
    let keep = 4;
    let trigger_threshold = 70.0;
    println!("Checking for sounds above an RMS level of {}", trigger_threshold);
    buff.clear();
    buff.grow(block * keep, &0i16);
    try!(inp.start());
    let mut trig_run: uint = 0;
    for _ in range(0, 20 * 10u) {
        remove_at_front(&mut buff, block);
        buff.grow(block, &0i16);
        let (from,to) = ((keep-1) * block, keep * block);
        try!(inp.read(buff.mut_slice(from,to)));
        let rms = root_mean_square(buff.slice(from,to));
        println!("current RMS: {}", rms);
        match rms > trigger_threshold {
            false => trig_run = 0,
            true  => trig_run += 1
        };
        if trig_run == 2 {
            trig_run = 0;
            println!("NOW RECORDING");
            for _ in range(0, 10 * 20u) {
                let from = buff.len();
                buff.grow(block, &0i16);
                let to = buff.len();
                try!(inp.read(buff.mut_slice(from,to)));
                let rms = root_mean_square(buff.slice(from,to));
                match rms <= trigger_threshold {
                    false => trig_run = 0,
                    true  => trig_run += 1
                };
                if trig_run == 10 {
                    // long silence -> remove most of it
                    // and break
                    let newlen = buff.len() - 5 * block;
                    buff.truncate(newlen);
                    break;
                }
            }
            println!("done recording");
            try!(inp.stop());
            return Ok(buff);
        }
    }
    try!(inp.stop());
    buff.clear();
    println!("If you don't want to say anything, I'll just terminate myself!");
    Ok(buff)
}

/// simply plays back the data
fn playback<S: Sample>(out: &RaOutputStream<S>, wave: &[S]) -> RaResult<()> {
    println!("Playback...");
    try!(out.start());
    try!(out.write(wave));
    try!(out.stop());
    Ok(())
}

fn do_parrot() -> RaResult<()> {
    // get a handle for an initialized PortAudio library...
    let ra = try!(RustAudio::new());
    // Pick default output device...
    let indev = match try!(ra.default_input_device()) {
        None => { println!("There's no default input device"); return Ok(()); }
        Some(d) => d
    };
    // Pick default output device...
    let outdev = match try!(ra.default_output_device()) {
        None => { println!("There's no default input device"); return Ok(()); }
        Some(d) => d
    };
    // setup output stream..
    let inp: RaInputStream<i16> =
        try!(ra.open_input(indev, 1, // one channel
                           0.1, // suggested latency in seconds
                           32000.0, StreamFlags::empty()));
    let out: RaOutputStream<i16> =
        try!(ra.open_output(outdev, 1, // one channel
                            0.1, // suggested latency in seconds
                            48000.0, StreamFlags::empty()));
    let mut buff = Vec::new();
    loop {
        buff = try!(record(&inp, buff));
        if buff.len() == 0 { break; }
        try!(playback(&out, buff.as_slice()));
    }
    Ok(())
}

fn main() {
    match do_parrot() {
        Err(x) => println!("Error: {}", x),
        _ => ()
    }
}

