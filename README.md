## RustAudio

This is a binding from the Rust language to the PortAudio library. The intention is to provide a memory- and type-safe interface in the spirit of the Rust language. This library is very young and only tested on one platform right now.

### What works?

You can query what kind of sound APIs a host has to offer, what devices are available, you can open an output stream to make sound and an input stream to record sound. Opening duplex streams has been implemented but not yet tested.

### What does not (yet) work?

The PortAudio library supports a couple of things which have not been implemented. The most important of these is probably "callback streams" where PortAudio calls a custom function to process the data within a high priority thread. This is what you probably want if you're interested in low-latency realtime audio processing. PortAudio also supports non-interleaved multi-channel data. But I don't think supporting non-interleaved data is important enough. Getting it type-safe seems to require some work.

### Examples!

Check out the `examples` folder to see how this library can be used. Start with the sine sweep demo. The talking parrot demo is a bit messier. Cargo should coompile all the examples when you type `cargo test` and place them under `./target/test/`. Be sure to use a recent version of Cargo.

### But there is already such a binding called `rust-portaudio`!

True. But that other binding has some undesirable properties. The design is not very type-safe. It allows you to open a stream in one format and read/write in another sample format that is not matching the format PortAudio is expecting. It also does not try to prevent you from some errors like opening a stream without having PortAudio initialized or forgetting to call `Pa_Terminate`. I thought about contributing to that library project but I found too many things I didn't like about it (Sorry, Jeremy Letang). Also, I just wanted the experience of doing it myself.

### What now?

If you're interested, check it out and let `rustc` or `cargo` create the documentation. The documentation is probably the best way to get a feel for how this binding should be used. If you want to help improve the library, you can do that, too. Before it gets too messy, it should probably be modularized into smaller files. Right now, `rustaudio.rs` is pretty huge and covers almost all of the library.

