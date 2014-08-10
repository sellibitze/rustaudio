#![feature(default_type_params)]
#![feature(macro_rules)]

//! `RustAudio` is a safe PortAudio binding for Rust and thus provides the
//! ability to make use of a host's sound API for playback and recording
//! digital audio.  Compared to PortAudio's C API, this Rust binding includes
//! the following improvements:
//!
//!  * memory safety: Most PortAudio function require PortAudio to be
//!    initialized. This is enforced at compile-time in that these function
//!    calls are methods of `RustAudio` where the existance of a `RustAudio`
//!    handle implies an initialized PortAudio library. Also, some functions
//!    return references to info structs and strings that have been allocated
//!    by PortAudio but are only guaranteed to be alive for the duration
//!    PortAudio stays initialized or for the duration a stream is open.
//!    That's why their lifetime parameter is
//!    the same as the lifetime of the handle used to invoke the function.
//!
//!  * freedom of leaks: PortAudio is treated as a resource and is managed
//!    and shared among all `RustAudio` handles.  When the last handle ceases
//!    to exist, PortAudio will automatically be deinitialized. Another such
//!    handle is `RaStream` which encapsulates PortAudio resources.
//!
//!  * improved type safety 1: The PortAudio library uses indices in its
//!    interface to refer to host APIs and devices in different ways. In this
//!    Rust binding these indices are typed and not just integers. There is
//!    one specific index type to refer to host APIs, another type for an
//!    API-specific device index and yet another type for a globally unique
//!    device index. This prevents passing an API-specific device index to
//!    the `device_info` function that actually expects globally a unique
//!    device index. For each of these index types, there is a corresponding
//!    range type that allows easy forward and reverse iteration. Also,
//!    instead of using an integer type, bitflags! was used to create a
//!    specific type for the format flags.
//!
//!  * improved type safety 2: The PortAudio C API makes use of error codes
//!    as return values that are
//!    mixed with other values indicating a successful operation. Sometimes
//!    there are even three kinds of results if some value is optional.
//!    For example, asking PortAudio for the default output device
//!    can have three results: a valid device index, no device index, and an
//!    error code. RustAudio maps these results to values of algebraic types
//!    `RaResult<T>` or `RaResult<Option<T>>` where `RaResult<T>` is short
//!    for `Result<T, RaError>`.
//!
//! The bits about increased type safety may feel like a bit of a hassle when
//! you are using it but this is just the Rust way of being less error-prone.
//! There may be changes in the future in order to increase consistency w.r.t.
//! `RaResult` and `Option`.
//!
//! Some names have been shortened. Whereas the C API of PortAudio prefixes
//! many names with "Pa_", we can make use of modules in Rust. Only `RaError`,
//! `RaResult`, `RaStream` and the enum constructors use a `Ra` prefix for
//! RustAudio.
//!
//! What's next? See the `RustAudio` and `RaStream` structs and their methods.

extern crate sync;
extern crate libc;

use sync::mutex::{StaticMutex, MUTEX_INIT};
use std::fmt;
use std::mem;
use std::fmt::Show;
use std::str::raw::c_str_to_static_slice;

pub use newtype::NewType;
pub use newtype::NewRange;

/// Phantom type for tagging a `NewType`
pub enum HostApiTag {}
/// Phantom type for tagging a `NewType`
pub enum HostApiDeviceTag {}
/// Phantom type for tagging a `NewType`
pub enum GlobalDeviceTag {}

/// type of an API index
pub type HostApiIndex = NewType<uint,HostApiTag>;
/// type of an API's device index
pub type HostApiDeviceIndex = NewType<uint,HostApiDeviceTag>;
/// type of a globally unique device index
pub type GlobalDeviceIndex = NewType<uint,GlobalDeviceTag>;

/// range type for host API indices (iterator)
pub type HostApiIndexRange = NewRange<uint,HostApiTag>;
/// range type for API-specific device indices (iterator)
pub type HostApiDeviceIndexRange = NewRange<uint,HostApiDeviceTag>;
/// range type for globally unique device indices (iterator)
pub type GlobalDeviceIndexRange = NewRange<uint,GlobalDeviceTag>;

/// Floatingpoint type used for time (in seconds)
pub type Time = ffi::PaTime;

/// Unit type for "always true"
pub struct True;

mod newtype;

mod ffi {
    pub use libc::{ c_int, c_char, c_double, c_void, c_ulong, c_long };

    pub type PaError = c_int;

    pub type PaHostApiIndex = c_int;

    pub type PaDeviceIndex = c_int;
    pub static PaNoDevice: c_int = -1;
    pub static PaUseHostApiSpecificDeviceSpecification: c_int = -2;

    pub type PaTime = c_double;
    pub type PaStream = c_void;
    pub type PaSampleFormat = c_ulong;

    #[link(name = "portaudio")]
    extern "C" {
        // functions that should always work
        pub fn Pa_GetVersion() -> c_int;
        pub fn Pa_GetVersionText() -> *const c_char;
        pub fn Pa_GetErrorText(errorCode: PaError) -> *const c_char;

        // (de-)initialization
        pub fn Pa_Initialize() -> PaError;
        pub fn Pa_Terminate() -> PaError;

        // functions that should only be called when Pa has been initialized
        pub fn Pa_GetHostApiCount() -> PaHostApiIndex;
        pub fn Pa_GetDefaultHostApi() -> PaHostApiIndex;
        pub fn Pa_GetHostApiInfo(host_api: PaHostApiIndex)
            -> *const super::HostApiInfo;
        pub fn Pa_HostApiTypeIdToHostApiIndex(hat_id: super::HostApiTypeId)
            -> PaHostApiIndex;
        pub fn Pa_HostApiDeviceIndexToDeviceIndex(
            host_api: PaHostApiIndex, ha_dev_idx: PaDeviceIndex)
            -> PaDeviceIndex;
        pub fn Pa_GetDeviceCount() -> PaDeviceIndex;
        pub fn Pa_GetDefaultInputDevice() -> PaDeviceIndex;
        pub fn Pa_GetDefaultOutputDevice() -> PaDeviceIndex;
        pub fn Pa_GetDeviceInfo(di: PaDeviceIndex) -> *const super::DeviceInfo;
        pub fn Pa_IsFormatSupported(
            input_parameters:  *const super::StreamParams,
            output_parameters: *const super::StreamParams,
            sample_rate:       c_double) -> PaError;

        // stream-related functions...
        // ..open..stuff missing __
        pub fn Pa_CloseStream(stream: *mut PaStream) -> PaError;
        pub fn Pa_StartStream(stream: *mut PaStream) -> PaError;
        pub fn Pa_StopStream(stream: *mut PaStream) -> PaError;
        pub fn Pa_AbortStream(stream: *mut PaStream) -> PaError;
        pub fn Pa_IsStreamStopped(stream: *mut PaStream) -> PaError;
        pub fn Pa_IsStreamActive(stream: *mut PaStream) -> PaError;
        pub fn Pa_GetStreamInfo(stream: *mut PaStream) -> *const super::StreamInfo;
        pub fn Pa_GetStreamTime(stream: *mut PaStream) -> PaTime;

/* not yet used C functions that I intent to use in the future ...

        pub fn Pa_ReadStream(stream: *mut PaStream,
                             buffer: *mut c_void,
                             frames: c_ulong) -> PaError;
        pub fn Pa_WriteStream(stream: *mut PaStream,
                              buffer: *const c_void,
                              frames: c_ulong) -> PaError;
        pub fn Pa_GetStreamReadAvailable(stream: *mut PaStream) -> c_long;
        pub fn Pa_GetStreamWriteAvailable(stream: *mut PaStream) -> c_long;
        pub fn Pa_GetStreamHostApiType(stream: *mut PaStream) -> super::HostApiTypeId;

        // misc
        pub fn Pa_GetSampleSize(format: PaSampleFormat) -> PaError;
        pub fn Pa_Sleep(milliseconds: c_long);

*/

    } // extern "C"
} // mod ffi

fn raw_ptr<T>(x: Option<&T>) -> *const T {
    let raw: *const T = 
        match x {
            None => RawPtr::null(),
            Some(x) => x as *const T
        };
    raw
}

static mut PA_MTX: StaticMutex = MUTEX_INIT;
static mut PA_CTR: uint = 0;

/// Possible errors from PortAudio translated to a Rust enum
#[deriving(FromPrimitive)]
pub enum RaError {
    /// You should never see this error because this Rust binding prevents the
    /// use of any PortAudio function that requires PortAudio to be initialized first.
    RaNotInitialized = -10000,

    RaUnanticipatedHostError,
    RaInvalidChannelCount,
    RaInvalidSampleRate,
    RaInvalidDevice,
    RaInvalidFlag,
    RaSampleFormatNotSupported,
    RaBadIODeviceCombination,
    RaInsufficientMemory,
    RaBufferTooBig,
    RaBufferTooSmall,
    RaNullCallback,
    RaBadStreamPtr,
    RaTimedOut,
    RaInternalError,
    RaDeviceUnavailable,
    RaIncompatibleHostApiSpecificStreamInfo,
    RaStreamIsStopped,
    RaStreamIsNotStopped,
    RaInputOverflowed,
    RaOutputUnderflowed,
    RaHostApiNotFound,
    RaInvalidHostApi,
    RaCanNotReadFromACallbackStream,
    RaCanNotWriteToACallbackStream,
    RaCanNotReadFromAnOutputOnlyStream,
    RaCanNotWriteToAnInputOnlyStream,
    RaIncompatibleStreamHostApi,
    RaBadBufferPtr,

    /// This is not a PortAudio error. It was added for the Rust binding
    /// for error codes that are unknown to the binding but could possibly
    /// be returned by newer incompatible versions of PortAudio
    RaUnknownError
}

impl Show for RaError {
    /// gets you a human-readable, textual representation for a RaError
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        let s = match *self {
            RaUnknownError => "unknown error",
            x => unsafe { c_str_to_static_slice(
                          ffi::Pa_GetErrorText(x as ffi::c_int)) }
        };
        s.fmt(f)
    }
}

/// Result type for RustAudio functions that might not succeed
pub type RaResult<T> = Result<T,RaError>;

// evaluates as uint or returns early from the surrounding function
// with the appropriate error code.  Useful inside functions returning
// a RaResult<T> for some T.
// Note: Sometimes the C API gives you negative values that are NOT errors!
// These have to be handled before! Example: PaNoDevice
macro_rules! tryRa(
    ($e:expr) => (
        {
            let result = $e as int;
            if result < 0 { // PortAudio error codes are negative
                let ore: Option<RaError> = FromPrimitive::from_int(result);
                match ore {
                    Some(re) => return Err(re),
                    None     => return Err(RaUnknownError)
                }
            }

            result as uint // no error
        }
    )
)

/// Type ID for a host's sound API
#[repr(C)]
pub enum HostApiTypeId {
    RaInDevelopment=0, /* use while developing support for a new host API */
    RaDirectSound=1,
    RaMME=2,
    RaASIO=3,
    RaSoundManager=4,
    RaCoreAudio=5,
    RaOSS=7,
    RaALSA=8,
    RaAL=9,
    RaBeOS=10,
    RaWDMKS=11,
    RaJACK=12,
    RaWASAPI=13,
    RaAudioScienceHPI=14
}

/// Some infos on a specific host API
#[repr(C)]
pub struct HostApiInfo {
    // TODO this is struct version 1 and should probably checked some time
    _struct_version: ffi::c_int,

    // The well known unique identifier of this host API
    _type_id:  HostApiTypeId,

    // A textual description of the host API for display on user interfaces.
    _name: *const ffi::c_char,

    // The number of devices belonging to this host API. This field may be
    // used in conjunction with Pa_HostApiDeviceIndexToDeviceIndex() to enumerate
    // all devices for this host API.
    _device_count: ffi::c_int,

    // The default input device for this host API. The value will be a
    // device index ranging from 0 to (Pa_GetDeviceCount()-1), or paNoDevice
    // if no default input device is available.
    _default_input_device: ffi::PaDeviceIndex,

    // The default output device for this host API. The value will be a
    // device index ranging from 0 to (Pa_GetDeviceCount()-1), or paNoDevice
    // if no default output device is available.
    _default_output_device: ffi::PaDeviceIndex
}

impl HostApiInfo {
    /// returns the type ID of this API
    #[inline]
    pub fn type_id(&self) -> HostApiTypeId { self._type_id }
    /// returns a textual representation for the API type
    pub fn name(&self) -> &str {
        // The function intentionally returns &str, not &'static str
        // because the slice might not be valid anymore once ffi::Pa_Terminate
        // was called.
        unsafe { c_str_to_static_slice(self._name) }
    }
    /// returns the number of devices (local to this API)
    pub fn device_count(&self) -> HostApiDeviceIndex {
        NewType::wrap(self._device_count as uint)
    }
    /// returns an iterator over the API-local devices.
    pub fn device_iter(&self) -> HostApiDeviceIndexRange {
        NewRange::new0(self.device_count())
    }
    /// returns this API's default input device (if defined/available)
    pub fn default_input_device(&self) -> Option<GlobalDeviceIndex> {
        match self._default_input_device {
            ffi::PaNoDevice => None,
            // TODO PaUseHostApiSpecificDeviceSpecification
            ffi::PaUseHostApiSpecificDeviceSpecification => None, 
            x if x >= 0 => Some(NewType::wrap(x as uint)),
            _ => None // TODO figure out if this is ok
        }
    }
    /// returns this API's default output device (if defined/available)
    pub fn default_output_device(&self) -> Option<GlobalDeviceIndex> {
        match self._default_output_device {
            ffi::PaNoDevice => None,
            // TODO PaUseHostApiSpecificDeviceSpecification
            ffi::PaUseHostApiSpecificDeviceSpecification => None,
            x if x >= 0 => Some(NewType::wrap(x as uint)),
            _ => None // TODO figure out if this is ok
        }
    }
}

/// Some infos (name, channels, etc) on a specific audio device
#[repr(C)]
pub struct DeviceInfo {
    // TODO this is struct version 2 and should probably checked some time
    _struct_version: ffi::c_int,
    // portaudio.h is quiet about the lifetime of this one
    _name: *const ffi::c_char,
    // not actually a host API index, nor a type ID. I kid you not. portaudio.h FTW!
    _hostApi: ffi::PaHostApiIndex,

    _max_inp_channels: ffi::c_int,
    _max_out_channels: ffi::c_int,

    _default_low_inp_latency: ffi::PaTime,
    _default_low_out_latency: ffi::PaTime,

    // Default latency values for robust non-interactive applications
    // (eg. playing sound files).
    _default_high_inp_latency: ffi::PaTime,
    _default_high_out_latency: ffi::PaTime,

    _default_sample_rate: ffi::c_double
}

impl DeviceInfo {
    /// returns the name of the device
    pub fn name(&self) -> &str {
        // The function intentionally returns &str, not &'static str
        // because the slice might not be valid anymore once ffi::Pa_Terminate
        // was called.
        unsafe { c_str_to_static_slice(self._name) }
    }
    #[inline]
    pub fn max_input_channels(&self) -> int { self._max_inp_channels as int }
    #[inline]
    pub fn max_output_channels(&self) -> int { self._max_out_channels as int }
    /// Default latency values for interactive performance. (probably in seconds)
    #[inline]
    pub fn default_low_input_latency(&self) -> Time { self._default_low_inp_latency }
    /// Default latency values for interactive performance. (probably in seconds)
    #[inline]
    pub fn default_low_output_latency(&self) -> Time { self._default_low_out_latency }
    /// Default latency values for robust non-interactive applications
    /// (eg. playing sound files). (probably in seconds)
    #[inline]
    pub fn default_high_input_latency(&self) -> Time { self._default_high_inp_latency }
    /// Default latency values for robust non-interactive applications
    /// (eg. playing sound files). (probably in seconds)
    #[inline]
    pub fn default_high_output_latency(&self) -> Time { self._default_high_out_latency }
    /// the device's default sampling rate (probably in samples per second)
    #[inline]
    pub fn default_sampling_rate(&self) -> f64 { self._default_sample_rate as f64 }
}

bitflags!(
    flags SampleFormatFlags: u32 {
        static RAFF_FLOAT32         = 0x00000001,
        static RAFF_INT32           = 0x00000002,
        static RAFF_INT24           = 0x00000004,
        static RAFF_INT16           = 0x00000008,
        static RAFF_INT8            = 0x00000010,
        static RAFF_UINT8           = 0x00000020,
        static RAFF_CUSTOM_FORMAT   = 0x00010000,
        static RAFF_NON_INTERLEAVED = 0x80000000
    }
)

/// Parameters for one direction (input or output) of a stream.
#[repr(C)]
pub struct StreamParams {

    /* A valid device index in the range 0 to (Pa_GetDeviceCount()-1)
     specifying the device to be used or the special constant
     paUseHostApiSpecificDeviceSpecification which indicates that the actual
     device(s) to use are specified in hostApiSpecificStreamInfo.
     This field must not be set to paNoDevice.
    */
    _device: ffi::PaDeviceIndex,
    
    /* The number of channels of sound to be delivered to the
     stream callback or accessed by Pa_ReadStream() or Pa_WriteStream().
     It can range from 1 to the value of maxInputChannels in the
     PaDeviceInfo record for the device specified by the device parameter.
    */
    _channel_count: ffi::c_int,

    /* The sample format of the buffer provided to the stream callback,
     a_ReadStream() or Pa_WriteStream(). It may be any of the formats described
     by the PaSampleFormat enumeration.
    */
    _sample_format: ffi::PaSampleFormat,

    /* The desired latency in seconds. Where practical, implementations should
     configure their latency based on these parameters, otherwise they may
     choose the closest viable latency instead. Unless the suggested latency
     is greater than the absolute upper limit for the device implementations
     should round the suggestedLatency up to the next practical value - ie to
     provide an equal or higher latency than suggestedLatency wherever possible.
     Actual latency values for an open stream may be retrieved using the
     inputLatency and outputLatency fields of the PaStreamInfo structure
     returned by Pa_GetStreamInfo().
     @see default*Latency in PaDeviceInfo, *Latency in PaStreamInfo
    */
    _suggested_latency: ffi::PaTime,

    /* An optional pointer to a host api specific data structure
     containing additional information for device setup and/or stream processing.
     hostApiSpecificStreamInfo is never required for correct operation,
     if not used it should be set to NULL.
    */
    _host_api_specific_stream_info: *mut ffi::c_void
}

impl StreamParams {
    pub fn new(device: GlobalDeviceIndex,
               channel_count: uint,
               sample_format: SampleFormatFlags,
               suggested_latency: Time) -> StreamParams {
        StreamParams {
            _device: device.wrapped as ffi::PaDeviceIndex,
            _channel_count: channel_count as ffi::c_int,
            _sample_format: sample_format.bits() as ffi::PaSampleFormat,
            _suggested_latency: suggested_latency,
            _host_api_specific_stream_info: RawPtr::null()
        }
    }
    pub fn device(&self) -> GlobalDeviceIndex {
        NewType::wrap(self._device as uint)
    }
    pub fn set_device(&mut self, di: GlobalDeviceIndex) {
        self._device = di.wrapped as ffi::PaDeviceIndex;
    }
    pub fn channel_count(&self) -> uint {
        self._channel_count as uint
    }
    pub fn set_channel_count(&mut self, cc: uint) {
        self._channel_count = cc as ffi::c_int
    }
    pub fn sample_format(&self) -> SampleFormatFlags {
        SampleFormatFlags::from_bits(
            (self._sample_format as u32) & SampleFormatFlags::all().bits()
        ).unwrap()
    }
    pub fn set_sample_format(&mut self, sf: SampleFormatFlags) {
        self._sample_format = sf.bits() as ffi::PaSampleFormat;
    }
    pub fn suggested_latency(&self) -> Time {
        self._suggested_latency
    }
    pub fn set_suggested_latency(&mut self, t: Time) {
        self._suggested_latency = t;
    }
}

bitflags!(
    flags StreamFlags: u32 {
        static RASF_EMPTY                                     = 0x00000000,
        static RASF_CLIP_OFF                                  = 0x00000001,
        static RASF_DITHER_OFF                                = 0x00000002,
        static RASF_NEVER_DROP_INPUT                          = 0x00000004,
        static RASF_PRIME_OUTPUT_BUFFER_USING_STREAM_CALLBACK = 0x00000008,
        static RASF_PLATFORM_SPECIFIC                         = 0xFFFF0000
    }
)

/// A structure containing unchanging information about an open stream.
#[repr(C)]
pub struct StreamInfo {
    // TODO this is struct version 1 and should probably checked some time
    _struct_version: ffi::c_int,
    _input_latency: ffi::PaTime,
    _output_latency: ffi::PaTime,
    _sample_rate: ffi::c_double
}

impl StreamInfo {
    /// The input latency of the stream in seconds. This value provides the most
    /// accurate estimate of input latency available to the implementation. It may
    /// differ significantly from the suggested latency value passed to `open`.
    /// The value will be zero (0.) for output-only streams.
    #[inline]
    pub fn input_latency(&self) -> Time {
        self._input_latency
    }
    /// The output latency of the stream in seconds. This value provides the most
    /// accurate estimate of output latency available to the implementation. It may
    /// differ significantly from the suggested latency value passed to `open`.
    /// The value will be zero (0.) for input-only streams.
    #[inline]
    pub fn output_latency(&self) -> Time {
        self._output_latency
    }
    /// The sample rate of the stream in Hertz (samples per second). In cases
    /// where the hardware sample rate is inaccurate and PortAudio is aware of it,
    /// the value of this field may be different from the sampleRate parameter
    /// passed to `open`. If information about the actual hardware sample
    /// rate is not available, this field will have the same value as the sampleRate
    /// parameter passed to `open`.
    #[inline]
    pub fn sample_rate(&self) -> f64 {
        self._sample_rate as f64
    }
}

/// This is a handle to the underlying PortAudio audio library.
///
/// PortAudio needs to be initialized before we can start using it
/// and deinitialized after we're done using it.
/// `RustAudio` acts as a reference-counted, clonable handle, which
/// keeps the PortAudio library initialized until every `RustAudio`
/// handle vanished.
pub struct RustAudio {
    _priv: ()
}

pub enum StopState {
    /// The stream has not been started or has been stopped.
    /// For an output stream, this means that it still might play
    /// some remaining data in the output buffers and soon be inactive.
    Stopped,
    /// The stream has not been stopped. Note that `NotStopped` implies
    /// `Active` but not the other way around.
    NotStopped
}

pub enum Activity {
    /// Sound is recorded or played back. For an output stream this
    /// can be even the case when it has stopped already because
    /// there is still some remaining data in the output buffers.
    Active,
    /// No sound is recorded or played back. Note that `InActive`
    /// implies `Stopped` but not the other way around.
    InActive
}

pub struct RaStream {
    /// a pointer to PortAudio allocated things it deems necessary
    /// to manage a stream (whatever it is)
    raw_pa_stream: *mut ffi::PaStream,
    /// keeps PortAudio initialized
    /// (a Stream is one owner of an initialized PortAudio library).
    _lib_handle: RustAudio,

    // I will probably add something here to support callbacks in a way that
    // still allows streams to be safely movable (indirection à la Box).
}

impl RaStream {
    /// Commences audio processing.
    pub fn start(&mut self) -> RaResult<()> {
        unsafe {
            tryRa!(ffi::Pa_StartStream(self.raw_pa_stream));
            Ok(())
        }
    }
    /// Terminates audio processing. It waits until all pending
    /// audio buffers have been played before it returns (unless
    /// a certain callback has been installed which this binding
    /// does not support right now).
    pub fn stop(&mut self) -> RaResult<()> {
        unsafe {
            tryRa!(ffi::Pa_StopStream(self.raw_pa_stream));
            Ok(())
        }
    }
    /// Terminates audio processing immediately without waiting for pending
    /// buffers to complete.
    pub fn abort(&mut self) -> RaResult<()> {
        unsafe {
            tryRa!(ffi::Pa_AbortStream(self.raw_pa_stream));
            Ok(())
        }
    }

    /// checks whether the stream has been stopped (or not yet started).
    /// See `StopState` for more details.
    pub fn is_stopped(&self) -> RaResult<StopState> {
        unsafe {
            match tryRa!(ffi::Pa_IsStreamStopped(self.raw_pa_stream)) {
                1 => Ok(Stopped),
                _ => Ok(NotStopped)
            }
        }
    }
    /// checks whether the stream is active. See `Activity` for more details.
    pub fn is_active(&self) -> RaResult<Activity> {
        unsafe {
            match tryRa!(ffi::Pa_IsStreamActive(self.raw_pa_stream)) {
                1 => Ok(Active),
                _ => Ok(InActive)
            }
        }
    }

    /// returns a reference to the corresponding `StreamInfo` object.
    pub fn get_info(&self) -> &StreamInfo {
        // According to PortAudio's C API this pointer is valid
        // as long as the stream is open. Also, in error cases
        // it may be a null pointer, but I could not figure out
        // what reasons that could be besides an API misuse error.
        // Let's not bother the user use with the Option<> wrapper
        // if None can only be the result of an API misuse which
        // is what I'm trying to prevent here.
        unsafe {
            let raw = ffi::Pa_GetStreamInfo(self.raw_pa_stream);
            // We still check for null but optimistically
            // return a reference without wrapping it into an Option
            assert!(raw.is_not_null())
            &*raw
        }
    }

    /// Returns the current time in seconds for a stream, or 0.0 if an error
    /// occurred. The time values are monotonically increasing and have
    /// unspecified origin.
    /// 
    /// `get_stream_time` returns valid time values for the entire life of the
    /// stream, from when the stream is opened until it is closed. Starting and
    /// stopping the stream does not affect the passage of time returned by
    /// Pa_GetStreamTime.
    ///
    /// This time may be used for synchronizing other events to the audio
    /// stream, for example synchronizing audio to MIDI.
    pub fn get_stream_time(&self) -> Time {
        unsafe { ffi::Pa_GetStreamTime(self.raw_pa_stream) }
    }

    /// Function that closes but also invalidates this stream used in
    /// `close` as well as in `drop`. Since it invalidates the stream
    /// this function is private so that the user cannot mess with an
    /// invalid state.
    fn close_and_invalidate(&mut self) -> RaResult<()> {
        unsafe {
            assert!(self.raw_pa_stream.is_not_null());
            let rawptr = mem::replace(&mut self.raw_pa_stream, RawPtr::null() );
            tryRa!( ffi::Pa_CloseStream(rawptr) );
            Ok(())
        }
    }

    /// Closes an audio stream. If the audio stream is active it
    /// discards any pending buffers as if `abort` had been called.
    /// Usually you don't need this function because
    /// `RaStream` also closes the stream when it is dropped. But since
    /// the C API declares the corresponding function to return an error
    /// code you can use `close` explicitly to get access to the result
    /// because `drop` simply ignores it. It's probalby not worth the
    /// hassle, though.
    pub fn close(mut self) -> RaResult<()> {
        self.close_and_invalidate()
    }
}

impl Drop for RaStream {
    /// Closes an audio stream. If the audio stream is active it
    /// discards any pending buffers as if `abort` had been called.
    /// In case the corresponding C API function returns an error
    /// code (unlikely) you have no way of getting that.
    fn drop(&mut self) {
        // the internal raw pointer is always valid unless the user
        // explicitly called close on it in which case he lost access to it.
        // In case the user did not close the stream explicitly, it will be
        // still non_null and we need to clean up...
        if self.raw_pa_stream.is_not_null() {
            drop(self.close_and_invalidate());
        }
    }
}

impl RustAudio {
    /// create a new PortAudio handle.
    ///
    /// If it is the only existing handle,
    /// it will initialize the PortAudio library. When the last handle is
    /// dropped, PortAudio will automatically be deinitialized.
    pub fn new() -> RaResult<RustAudio> {
        unsafe {
            let _guard = PA_MTX.lock();
            if PA_CTR == 0 {
                tryRa!(ffi::Pa_Initialize());
            }
            PA_CTR += 1;
            Ok(RustAudio { _priv: () })
        }
    }
    /// returns the number of sound APIs this host supports on success.
    /// Consider using `try!`.
    pub fn host_api_count(&self) -> RaResult<HostApiIndex> {
        unsafe { Ok(NewType::wrap(tryRa!(ffi::Pa_GetHostApiCount()))) }
    }
    /// returns an iterator over all the API indices on success.
    /// Consider using `try!`.
    pub fn host_api_iter(&self) -> RaResult<HostApiIndexRange> {
        self.host_api_count().map(|c| { NewRange::new0(c) })
    }
    /// returns the index of the default host API on success.
    /// Consider using `try!`.
    pub fn default_host_api(&self) -> RaResult<HostApiIndex> {
        unsafe { Ok(NewType::wrap(tryRa!(ffi::Pa_GetDefaultHostApi()))) }
    }
    /// maps an API's type ID to its index
    pub fn host_api_type_to_index(&self, hat_id:  HostApiTypeId)
        -> RaResult<HostApiIndex> {
        unsafe {
            Ok(NewType::wrap(tryRa!(ffi::Pa_HostApiTypeIdToHostApiIndex(hat_id))))
        }
    }
    /// Retrieves a reference to an info struct that describes an API.
    /// If the index is invalid the function returns an `RaInvalidHostApi` error.
    pub fn host_api_info(&self, idx:  HostApiIndex) -> RaResult<&HostApiInfo> {
        // The PortAudio API only promises the resulting pointer to be
        // valid until the library is deinitialized. But this won't happen
        // before *self is dropped. So, all is good. :)
        unsafe {
            let raw = ffi::Pa_GetHostApiInfo(idx.wrapped as ffi::PaHostApiIndex);
            if raw.is_null() {
                Err(RaInvalidHostApi)
            } else {
                Ok(&*raw)
            }
        }
    }
    /// maps an API-specific device index to a global device index
    /// or returns an error
    pub fn global_device_index(&self,
                               ha_idx:  HostApiIndex,
                               hadi:  HostApiDeviceIndex)
        -> RaResult<GlobalDeviceIndex> {
        unsafe {
            Ok(NewType::wrap(tryRa!(
                ffi::Pa_HostApiDeviceIndexToDeviceIndex(
                    ha_idx.wrapped as ffi::PaHostApiIndex,
                    hadi.wrapped as ffi::PaDeviceIndex)
            )))
        }
    }
    /// returns the number of devices or an error
    pub fn device_count(&self) -> RaResult<GlobalDeviceIndex> {
        unsafe {
            Ok(NewType::wrap(tryRa!(ffi::Pa_GetDeviceCount())))
        }
    }
    /// returns an iterator over the global device indices on success.
    /// Consider using `try!`.
    pub fn device_iter(&self) -> RaResult<GlobalDeviceIndexRange> {
        self.device_count().map(|c| { NewRange::new0(c) })
    }
    /// returns an Option containing an index for the default input device
    /// or an error
    pub fn default_input_device(&self) -> RaResult<Option<GlobalDeviceIndex>> {
        let tmp = unsafe { ffi::Pa_GetDefaultInputDevice() };
        if tmp == ffi::PaNoDevice { return Ok(None); }
        Ok(Some(NewType::wrap(tryRa!(tmp))))
    }
    /// returns an Option containing an index for the default output device
    /// or an error
    pub fn default_output_device(&self) -> RaResult<Option<GlobalDeviceIndex>> {
        let tmp = unsafe { ffi::Pa_GetDefaultOutputDevice() };
        if tmp == ffi::PaNoDevice { return Ok(None); }
        Ok(Some(NewType::wrap(tryRa!(tmp))))
    }
    /// Retrieve a reference to a DeviceInfo structure containing information about
    /// the specified device.  If the index is invalid an RaInvalidDevice error
    /// is returned
    pub fn device_info(&self, di:  GlobalDeviceIndex) -> RaResult<&DeviceInfo> {
        unsafe {
            let raw = ffi::Pa_GetDeviceInfo(di.wrapped as ffi::PaDeviceIndex);
            if raw.is_null() { Err(RaInvalidDevice) }
            else { Ok(&*raw) }
        }
    }
    /// returns Ok(True) if the format is supported, Err(x) with x being a
    /// RaError if not. RaError includes variants that can explain why the
    /// format it not supported. So Err(_) is not necessarily an error of this
    /// operation. You should probably be careful with the use of `try!` on
    /// this result.
    ///
    /// If you want an output-only stream you must pass `None` as `inp`.
    /// If you want an input-only stream you must pass `None` as `out`.
    /// The given sampling rate applies to both, input and output, if given.
    pub fn is_format_supported(inp: Option<&StreamParams>,
                               out: Option<&StreamParams>,
                               sample_rate: f64) -> RaResult<True> {
        tryRa!(unsafe {
            ffi::Pa_IsFormatSupported(raw_ptr(inp), raw_ptr(out),
                                      sample_rate as ffi::c_double)
        });
        Ok(True)
    }
}

impl Drop for RustAudio {
    fn drop(&mut self) {
        unsafe {
            let _guard = PA_MTX.lock();
            debug_assert!(PA_CTR > 0);
            PA_CTR -= 1;
            if PA_CTR == 0 {
                // TODO figure out whether ignoring the result of Pa_Terminate
                // is bad and if so what to do about it
                ffi::Pa_Terminate();
            }
        }
    }
}

impl Clone for RustAudio {
    fn clone(&self) -> RustAudio {
        RustAudio::new().unwrap()
        // This can only fail if we're changing the reference count
        // from zero to one. But since we already havea PortAudio
        // value (*self), this cannot happen
    }
    #[inline]
    fn clone_from(&mut self, _: &RustAudio) {
        // nothing to do
    }
}

/// returns PortAudio's version as int
pub fn portaudio_version() -> int {
    unsafe { ffi::Pa_GetVersion() as int }
}

/// returns PortAudio's version as text
pub fn portaudio_version_text() -> &'static str {
    unsafe { c_str_to_static_slice(ffi::Pa_GetVersionText()) }
}

#[cfg(test)]
mod test {

    use super::RustAudio;
    use super::RaResult;
    use super::portaudio_version;
    use super::portaudio_version_text;

    fn check_apis_and_devices(ra: &RustAudio) -> RaResult<()> {
        println!("Iterating host APIs...");
        for ha_idx in try!(ra.host_api_iter()) {
            let info = try!(ra.host_api_info(ha_idx));
            println!("#{}: {}", ha_idx.wrapped, info.name());
            for devidx in info.device_iter() {
                let gdi = try!(ra.global_device_index(ha_idx, devidx));
                let devi = try!(ra.device_info(gdi));
                println!("    device {}: {}",
                    devidx.wrapped,
                    devi.name());
            }
            print!("    default input device: ");
            match info.default_input_device() {
                None => println!("none"),
                Some(gdi) => println!("\"{}\"", try!(ra.device_info(gdi)).name())
            }
            print!("    default output device: ");
            match info.default_output_device() {
                None => println!("none"),
                Some(gdi) => println!("\"{}\"", try!(ra.device_info(gdi)).name())
            }
        }
        println!("Default host API is #{}", try!(ra.default_host_api()).wrapped);
        print!("Checking for JACK: ");
        match ra.host_api_type_to_index(super::RaJACK) {
            Ok(idx) => println!("found (#{})", idx.wrapped),
            Err(er) => println!("not found ({})", er)
        }
        match try!(ra.default_input_device()) {
            None => println!("no global default input device"),
            Some(gdi) => {
                let inf = try!(ra.device_info(gdi));
                println!("global default input device: \"{}\"", inf.name());
            }
        }
        match try!(ra.default_output_device()) {
            None => println!("no global default output device"),
            Some(gdi) => {
                let inf = try!(ra.device_info(gdi));
                println!("global default output device: \"{}\"", inf.name());
            }
        }
        Ok(())
    }

    #[test]
    fn all() {
        println!("PortAudio version: {}, {}",
                 portaudio_version(),
                 portaudio_version_text() );
        match RustAudio::new() {
            Err(e) => { println!("RustAudio error: {}", e); fail!("{}",e); }
            Ok(ra) => {
                println!("RustAudio successfully initialized");
                match check_apis_and_devices(&ra) {
                    Err(e) => { println!("RustAudio error: {}", e); fail!("{}",e); }
                    _ => {}
                }
            }
        }
    }
    
} // mod test

