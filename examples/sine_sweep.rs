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

//! This is a simple example program that uses RustAudio to open an output
//! stream at 48 kHz to playback a generated sine sweep.

extern crate rustaudio;
extern crate num;

use rustaudio::{ RustAudio, RaResult, StreamFlags };
use rustaudio::{ RaStreamAny, RaOutputStreamExt };
use rustaudio::{ RaOutputStream };

use num::complex::Complex;

fn do_sine_sweep() -> RaResult<()> {
    // get a handle for an initialized PortAudio library...
    let ra = try!(RustAudio::new());
    // Pick default output device...
    let device = match try!(ra.default_output_device()) {
        None => { println!("There's no default output device"); return Ok(()); }
        Some(d) => d
    };
    // Get device info...
    let devinfo = try!(ra.device_info(device));
    println!("Using \"{}\" as output device ...", devinfo.name());
    // setup output stream..
    let fs = 48000.0;
    let out: RaOutputStream<i16> =
        try!(ra.open_output(device, 1, // one channel
                            0.1, // suggested latency in seconds
                            fs, StreamFlags::empty()));
    // check stream info...
    let sinfo = out.get_info();
    println!("Output stream info: output latency of {} seconds at {} Hz",
             sinfo.output_latency(), sinfo.sample_rate());
    // the sine sweep loop ...
    println!("Generating sine sweep from 0 Hz to 24 kHz at -20 dBFS ...");
    let mut block = Vec::from_elem(48*50, 0); // 50ms
    let mut phase = Complex::from_polar(&1.0f64, &0.0f64);
    let mut freq = Complex::from_polar(&1.0f64, &0.0f64);
    let sweep_speed = Complex::from_polar(&1.0f64, &0.00001);
    try!(out.start());
    loop {
        for smp in block.mut_iter() {
            *smp = (phase.im * 3276.0).round() as i16;
            phase = phase * freq;
            freq = freq * sweep_speed;
        }
        if freq.im < 0.0 { break; }
        let res = out.write(block.as_slice());
        match res {
            Err(rustaudio::RaOutputUnderflowed) => {
                println!("Oops! The output buffer underflowed. Latency too low?");
            }
            Err(x) => return Err(x),
            _ => ()
        }
    }
    try!(out.stop());
    Ok(())
}

fn main() {
    match do_sine_sweep() {
        Err(x) => println!("RustAudio error: {}", x),
        _ => ()
    }
}

