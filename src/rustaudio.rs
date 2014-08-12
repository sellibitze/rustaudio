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

#![feature(default_type_params)]
#![feature(macro_rules)]

//! # Overview
//!
//! RustAudio is a safe PortAudio binding for Rust and thus provides the
//! ability to make use of a host's sound API for playback and recording
//! digital audio. It is safe because it prevents some errors that are
//! possible if you access the PortAudio library directly from C.
//! Specifically, it is memory-safe and will not leak PortAudio resources
//! unless the user tries really hard to mess with it (`unsafe`).
//!
//! # Improved safety with a type-rich interface
//!
//! The PortAudio C library deals with raw integers for referring to different
//! sound APIs, devices, sample format flags or stream flags. This Rust
//! binding provides special types for these so that you cannot use an
//! API-specific device index as a globally unique device index, for example.
//!
//! # Improved safety with RAII and Rust's lifetime system
//!
//! Most PortAudio functions require the PortAudio library to be initialized.
//! Stream-specific functions like `start` and `stop` require a valid opened
//! stream. These preconditions are enforced at compile-time by design. An
//! initialized PortAudio library is treated as a resource that is shared
//! among all `RustAudio` handles. Functions that require the library to be
//! initialized are simply methods of this handle where the existance of such
//! a handle guarantees the PortAudio library to be initialized. Stream-
//! specific functions are methods of `RaStream` which represents an opened
//! stream. Internally, a `RaStream` objects also holds a `RustAudio` handle
//! to keep the library initialized. Some functions and methods return
//! references to info data structures or strings. These references have
//! lifetime parameters that are guaranteed by PortAudio's API documentation.
//! PortAudio's version text and the textual representations of error codes
//! always exist ('static). Other things are only guaranteed to exist for the
//! duration the PortAudio library is initialized or for the duration of an
//! opened stream.
//!
//! # Improved safety with algebraic types
//!
//! Correct error handling in a C program that uses the PortAudio library
//! directly is made difficult in that many PortAudio functions return an
//! integer that often serves two purposes and sometimes even three. Depending
//! on the value, it might be an error code, or just signal that there is no
//! answer. In this Rust binding, this is handled with the algebraic types
//! `RaResult<T>` or even `RaResult<Option<T>>` where `RaResult<T>` is short
//! for `Result<T, RaError>` with `RaError` being a PortAudio-compatible enum.
//! This way a user is required to explicitly unwrap the value she/he is
//! interested with (via `unwrap` or `try!`) or match against the different
//! cases.
//!
//! # What's next?
//!
//! See the `RustAudio` and `RaStream` structs and their methods.

extern crate sync;
extern crate libc;

#[cfg(test)]
extern crate num; // for complex numbers

use sync::mutex::{StaticMutex, MUTEX_INIT};
use std::fmt;
use std::mem;
use std::fmt::Show;
use std::str::raw::c_str_to_static_slice;
use std::default::Default;

pub use newtype::NewType;
pub use newtype::NewRange;
pub use int24::i24;

/// Phantom type for tagging a `NewType`, used for `HostApiIndex` type.
pub enum HostApiTag {}
/// Phantom type for tagging a `NewType`, used for `HostApiDeviceIndex` type.
pub enum HostApiDeviceTag {}
/// Phantom type for tagging a `NewType`, used for `GlobalDeviceIndex` type.
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

mod newtype;
mod int24;

mod ffi {
    pub use libc::{ c_int, c_char, c_double, c_void, c_ulong, c_long };

    pub type PaError = c_int;

    pub type PaHostApiIndex = c_int;

    pub type PaDeviceIndex = c_int;
    pub static PaNoDevice: c_int = -1;
//  pub static PaUseHostApiSpecificDeviceSpecification: c_int = -2;

    pub type PaTime = c_double;
    pub type PaStream = c_void;
    pub type PaSampleFormat = c_ulong;
    pub type PaStreamFlags = c_ulong;

    #[repr(C)]
    pub struct PaStreamParameters {
        // A valid device index in the range 0 to (Pa_GetDeviceCount()-1)
        // specifying the device to be used or the special constant
        // paUseHostApiSpecificDeviceSpecification which indicates that the actual
        // device(s) to use are specified in hostApiSpecificStreamInfo.
        // This field must not be set to paNoDevice.
        pub device: PaDeviceIndex,
        
        // The number of channels of sound to be delivered to the
        // stream callback or accessed by Pa_ReadStream() or Pa_WriteStream().
        // It can range from 1 to the value of maxInputChannels in the
        // PaDeviceInfo record for the device specified by the device parameter.
        pub channel_count: c_int,

        // The sample format of the buffer provided to the stream callback,
        // a_ReadStream() or Pa_WriteStream(). It may be any of the formats described
        // by the PaSampleFormat enumeration.
        pub sample_format: PaSampleFormat,

        // The desired latency in seconds. Where practical, implementations should
        // configure their latency based on these parameters, otherwise they may
        // choose the closest viable latency instead. Unless the suggested latency
        // is greater than the absolute upper limit for the device implementations
        // should round the suggestedLatency up to the next practical value - ie to
        // provide an equal or higher latency than suggestedLatency wherever possible.
        // Actual latency values for an open stream may be retrieved using the
        // inputLatency and outputLatency fields of the PaStreamInfo structure
        // returned by Pa_GetStreamInfo().
        // @see default*Latency in PaDeviceInfo, *Latency in PaStreamInfo
        pub suggested_latency: PaTime,

        // An optional pointer to a host api specific data structure
        // containing additional information for device setup and/or stream processing.
        // hostApiSpecificStreamInfo is never required for correct operation,
        // if not used it should be set to NULL.
        pub host_api_specific_stream_info: *mut c_void
    }

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
            input_parameters: *const PaStreamParameters,
            output_parameters: *const PaStreamParameters,
            sample_rate: c_double) -> PaError;

        pub fn Pa_OpenStream(stream: *mut*mut PaStream,
            inp_params: *const PaStreamParameters,
            out_params: *const PaStreamParameters,
            smp_rate: c_double,
            frames_per_buffer: c_ulong,
            stream_flags: PaStreamFlags,
            callback: *mut(),
            userdata: *mut()) -> PaError;

        // stream-related functions that require an open stream
        pub fn Pa_CloseStream(stream: *mut PaStream) -> PaError;
        pub fn Pa_StartStream(stream: *mut PaStream) -> PaError;
        pub fn Pa_StopStream(stream: *mut PaStream) -> PaError;
        pub fn Pa_AbortStream(stream: *mut PaStream) -> PaError;
        pub fn Pa_IsStreamStopped(stream: *mut PaStream) -> PaError;
        pub fn Pa_IsStreamActive(stream: *mut PaStream) -> PaError;
        pub fn Pa_GetStreamInfo(stream: *mut PaStream) -> *const super::StreamInfo;
        pub fn Pa_GetStreamTime(stream: *mut PaStream) -> PaTime;
        pub fn Pa_ReadStream(stream: *mut PaStream,
                             buffer: *mut c_void,
                             frames: c_ulong) -> PaError;
        pub fn Pa_WriteStream(stream: *mut PaStream,
                              buffer: *const c_void,
                              frames: c_ulong) -> PaError;



/* not yet used C functions that I intent to use in the future ...

        pub fn Pa_GetStreamReadAvailable(stream: *mut PaStream) -> c_long;
        pub fn Pa_GetStreamWriteAvailable(stream: *mut PaStream) -> c_long;
        pub fn Pa_GetStreamHostApiType(stream: *mut PaStream) -> super::HostApiTypeId;

        // misc
        pub fn Pa_GetSampleSize(format: PaSampleFormat) -> PaError;
        pub fn Pa_Sleep(milliseconds: c_long);

*/

    } // extern "C"
} // mod ffi

/// returns PortAudio's version as int
pub fn portaudio_version() -> int {
    unsafe { ffi::Pa_GetVersion() as int }
}

/// returns PortAudio's version as text
pub fn portaudio_version_text() -> &'static str {
    unsafe { c_str_to_static_slice(ffi::Pa_GetVersionText()) }
}

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
    /// You should never see this error because this Rust binding does not
    /// support the use of host API-specific stream infos.
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

// takes an ffi::PaError and evaluates as uint or returns early from the
// surrounding function with the appropriate RaError error code.
// Useful inside functions returning a RaResult<T> for some T.
// Note: Sometimes the C API gives you negative values that are NOT errors!
// These have to be handled before! Example: ffi::PaNoDevice
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

static HOST_API_INFO_STRUCT_VERSION: ffi::c_int = 1;
/// Some infos on a specific host API
#[repr(C)]
pub struct HostApiInfo {
    // should match HOST_API_INFO_STRUCT_VERSION
    _struct_version: ffi::c_int,

    // The well known unique identifier of this host API
    _type_id: HostApiTypeId,

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

        // The PortAudio API does not mention that this pointer may be null.
        assert!(self._name.is_not_null());
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
            x  if x >=0  => Some(NewType::wrap(x as uint)),
            _            => None
        }
    }
    /// returns this API's default output device (if defined/available)
    pub fn default_output_device(&self) -> Option<GlobalDeviceIndex> {
        match self._default_output_device {
            x  if x >=0  => Some(NewType::wrap(x as uint)),
            _            => None
        }
    }
}

static DEVICE_INFO_STRUCT_VERSION: ffi::c_int = 2;
/// Some infos (name, channels, etc) on a specific audio device
#[repr(C)]
pub struct DeviceInfo {
    // This should match DEVICE_INFO_STRUCT_VERSION
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

        // The PortAudio API does not mention that this pointer may be null.
        assert!(self._name.is_not_null());
        unsafe { c_str_to_static_slice(self._name) }
    }
    #[inline]
    pub fn max_input_channels(&self) -> uint { self._max_inp_channels as uint }
    #[inline]
    pub fn max_output_channels(&self) -> uint { self._max_out_channels as uint }
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
    pub fn default_sample_rate(&self) -> f64 { self._default_sample_rate as f64 }
}

/// Describes parameters for sound input or output.
pub struct StreamParams {
    /// A valid device index specifying the device to be used.
    pub device: GlobalDeviceIndex,
    /// The number of channels of sound to be delivered from or to
    /// `read` or `write`.
    /// It can range from 1 to the value of `max_input_channels()` of the
    /// `DeviceInfo` struct for the device specified by the device parameter.
    pub channel_count: uint,
    /// The sample format of the buffer provided to `read` or `write`.
    pub sample_type: SampleType,
    /// The desired latency in seconds. Where practical, implementations should
    /// configure their latency based on these parameters, otherwise they may
    /// choose the closest viable latency instead. Unless the suggested latency
    /// is greater than the absolute upper limit for the device implementations
    /// should round the suggestedLatency up to the next practical value - ie to
    /// provide an equal or higher latency than suggestedLatency wherever possible.
    /// Actual latency values for an open stream may be retrieved using the
    /// `input_latency()` and `output_latency()` methods of the StreamInfo
    /// structure returned by `get_info()` on a stream.
    pub suggested_latency: Time
}

impl StreamParams {
    fn to_c_struct(&self) -> ffi::PaStreamParameters {
        ffi::PaStreamParameters {
            device: self.device.wrapped as ffi::PaDeviceIndex,
            channel_count: self.channel_count as ffi::c_int,
            sample_format: self.sample_type as ffi::c_ulong,
            suggested_latency: self.suggested_latency,
            host_api_specific_stream_info: RawPtr::null()
        }
    }
}

/// Mutex for thread-safe counting of the number of RustAudio handles
static mut PA_MTX: StaticMutex = MUTEX_INIT;
/// Counts the number of `RustAudio` handles.
/// If counter == 0, PortAudio is NOT initialized.
/// If counter >= 1, PortAudio IS initialized.
static mut PA_CTR: uint = 0;

/// This is a handle to the underlying PortAudio library. Keep such
/// a handle alive as long as you want to use PortAudio. IF the last handle
/// vanishes, PortAudio will be deinitialized.
pub struct RustAudio {
    /// private zero-sized member just to prevent the user from creating
    /// their own handles without actually making sure PortAudio is
    /// initialized.
    _priv: ()
}

/// turns an Option<&T> into a nullable *const T
fn opt_ref_to_ptr<T>(x: Option<&T>) -> *const T {
    let raw: *const T = 
        match x {
            None => RawPtr::null(),
            Some(x) => x as *const T
        };
    raw
}


impl RustAudio {
    /// Creating and keeping such a handle makes sure that PortAudio
    /// is/stays initialized.
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
    pub fn host_api_type_to_index(&self, hat_id: HostApiTypeId) -> RaResult<HostApiIndex> {
        unsafe {
            Ok(NewType::wrap(tryRa!(
                ffi::Pa_HostApiTypeIdToHostApiIndex(hat_id)
            )))
        }
    }
    /// Retrieves a reference to an info struct that describes an API.
    /// If the index is invalid the function returns an `RaInvalidHostApi` error.
    pub fn host_api_info(&self, idx: HostApiIndex) -> RaResult<&HostApiInfo> {
        // The PortAudio API only promises the resulting pointer to be
        // valid until the library is deinitialized. But this won't happen
        // before *self is dropped. So, all is good. :)
        unsafe {
            let raw = ffi::Pa_GetHostApiInfo(idx.wrapped as ffi::PaHostApiIndex);
            if raw.is_null() {
                Err(RaInvalidHostApi)
            } else {
                let tmp = mem::copy_lifetime(self,&*raw);
                assert!(tmp._struct_version == HOST_API_INFO_STRUCT_VERSION);
                Ok(tmp)
            }
        }
    }
    /// maps an API-specific device index to a global device index
    /// or returns an error
    pub fn global_device_index(&self,
                               hai: HostApiIndex,
                               hadi: HostApiDeviceIndex) -> RaResult<GlobalDeviceIndex> {
        unsafe {
            Ok(NewType::wrap(tryRa!(
                ffi::Pa_HostApiDeviceIndexToDeviceIndex(
                    hai.wrapped as ffi::PaHostApiIndex,
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
    pub fn device_info(&self, di: GlobalDeviceIndex) -> RaResult<&DeviceInfo> {
        unsafe {
            let raw = ffi::Pa_GetDeviceInfo(di.wrapped as ffi::PaDeviceIndex);
            if raw.is_null() {
                Err(RaInvalidDevice)
            } else {
                let tmp = mem::copy_lifetime(self,&*raw);
                assert!(tmp._struct_version == DEVICE_INFO_STRUCT_VERSION);
                Ok(tmp)
            }
        }
    }
    /// returns `Ok(())` if the format is supported, `Err(x)` with x being a
    /// RaError if not. RaError includes variants that can explain why the
    /// format it not supported. So `Err(_)` is not necessarily an error of this
    /// operation. You should probably be careful with the use of `try!` on
    /// this result.
    ///
    /// If you want an output-only stream you must pass `None` as `inp`.
    /// If you want an input-only stream you must pass `None` as `out`.
    /// The given sampling rate applies to both, input and output, if given.
    pub fn is_format_supported(inp: Option<&StreamParams>,
                               out: Option<&StreamParams>,
                               sample_rate: f64) -> RaResult<()> {
        let inp_c = inp.map(|x|{x.to_c_struct()});
        let out_c = out.map(|x|{x.to_c_struct()});
        tryRa!(unsafe {
            ffi::Pa_IsFormatSupported(opt_ref_to_ptr(inp_c.as_ref()),
                                      opt_ref_to_ptr(out_c.as_ref()),
                                      sample_rate as ffi::c_double)
        });
        Ok(())
    }
} // impl RustAudio

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

impl Show for RustAudio {
    /// gets you the PortAudio version text
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        portaudio_version_text().fmt(f)
    }
}

/// Describes the type of samples
#[deriving(Clone,PartialEq,Eq)]
pub enum SampleType {
    /// 32-bit floating point samples
    Float32 = 0x00000001,
    /// 32-bit integer samples
    Int32   = 0x00000002,
    /// 24-bit packed integer samples, see type `i24`
    Int24   = 0x00000004,
    /// 16-bit integer samples
    Int16   = 0x00000008,
    /// 8-bit signed integer samples
    Int8    = 0x00000010,
    /// 8-bit unsigned integer samples
    Uint8   = 0x00000020
}

/*
   PortAudio also supports non-interleaved sample data for multiple channels
   but this is not easy to get right in this Rust binding in a way that it's
   also type-safe. So, for now, I'm ignoring the "noninterleved" feature.

#[deriving(Clone,PartialEq,Eq)]
pub enum SampleMemoryOrdering {
    /// The samples of all channels are interleaved
    Interleaved    = 0x00000000,
    /// For each channel there is a separate buffer
    NonInterleaved = 0x80000000
}
*/

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

static STREAM_INFO_STRUCT_VERSION: ffi::c_int = 1;
/// A structure containing unchanging information about an open stream.
#[repr(C)]
pub struct StreamInfo {
    // This should match STREAM_INFO_STRUCT_VERSION
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
    /// can be even the case when it has been stopped already because
    /// there is still some remaining data in the output buffers.
    Active,
    /// No sound is recorded or played back. Note that `InActive`
    /// implies `Stopped` but not the other way around.
    InActive
}

/// The smallest simple wrapper for a PortAudio stream that does nothing
/// but close the stream eventually. This is used as member in other structs
/// that provide a richer interface. So, this saves a bit of duplication.
struct StreamHandle {
    raw_ptr: *mut ffi::PaStream,
    _ra: RustAudio
}

impl StreamHandle {
    fn new(ra: RustAudio, raw_ptr: *mut ffi::PaStream) -> StreamHandle {
        StreamHandle { raw_ptr: raw_ptr, _ra: ra }
    }
    fn is_valid(&self) -> bool { self.raw_ptr.is_not_null() }
    fn close_and_invalidate(&mut self) -> ffi::PaError {
        if self.is_valid() {
            unsafe { ffi::Pa_CloseStream(self.raw_ptr) }
        } else {
            RaBadStreamPtr as ffi::PaError
        }
    }
}

impl Drop for StreamHandle {
    fn drop(&mut self) {
        if self.is_valid() { self.close_and_invalidate(); }
    }
}


//--- hierarchie of stream traits ... ---

#[doc(hidden)]
trait RaStreamPrivate {
    fn get_portaudio_stream_pointer(&self) -> *mut ffi::PaStream;
    fn stored_sample_rate(&self) -> f64;
    fn close_and_invalidate(&mut self) -> ffi::PaError;
}

/// Any kind of RustAudio stream supports these functions
pub trait RaStreamAny: RaStreamPrivate {
    /// returns the sample rate this stream has been opened with
    fn sample_rate(&self) -> f64 { self.stored_sample_rate() }
    /// Commences audio processing.
    fn start(&mut self) -> RaResult<()> {
        let pa_stream = self.get_portaudio_stream_pointer();
        unsafe {
            tryRa!(ffi::Pa_StartStream(pa_stream));
            Ok(())
        }
    }
    /// Terminates audio processing. It waits until all pending
    /// audio buffers have been played before it returns (unless
    /// a certain callback has been installed which this binding
    /// does not support right now).
    fn stop(&mut self) -> RaResult<()> {
        let pa_stream = self.get_portaudio_stream_pointer();
        unsafe {
            tryRa!(ffi::Pa_StopStream(pa_stream));
            Ok(())
        }
    }
    /// Terminates audio processing immediately without waiting for pending
    /// buffers to complete.
    fn abort(&mut self) -> RaResult<()> {
        let pa_stream = self.get_portaudio_stream_pointer();
        unsafe {
            tryRa!(ffi::Pa_AbortStream(pa_stream));
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
    fn close(mut self) -> RaResult<()> {
        tryRa!(self.close_and_invalidate());
        Ok(())
    }
    /// checks whether the stream has been stopped (or not yet started).
    /// See `StopState` for more details.
    fn is_stopped(&self) -> RaResult<StopState> {
        let pa_stream = self.get_portaudio_stream_pointer();
        unsafe {
            match tryRa!(ffi::Pa_IsStreamStopped(pa_stream)) {
                1 => Ok(Stopped),
                _ => Ok(NotStopped)
            }
        }
    }
    /// checks whether the stream is active. See `Activity` for more details.
    fn is_active(&self) -> RaResult<Activity> {
        let pa_stream = self.get_portaudio_stream_pointer();
        unsafe {
            match tryRa!(ffi::Pa_IsStreamActive(pa_stream)) {
                1 => Ok(Active),
                _ => Ok(InActive)
            }
        }
    }
    /// returns a reference to the corresponding `StreamInfo` object.
    fn get_info(&self) -> &StreamInfo {
        let pa_stream = self.get_portaudio_stream_pointer();
        // According to PortAudio's C API this pointer is valid as long
        // as the stream is open. Also, in error cases it may be a null
        // pointer, but I could not figure out what reasons that could be
        // besides an API misuse error. Let's not bother the user use with
        // the Option<> wrapper this time. if None can only be the result
        // of an API misuse which is what I'm trying to prevent here, then
        // we should be fine...
        unsafe {
            let raw = ffi::Pa_GetStreamInfo(pa_stream);
            // We still check for null but optimistically
            // return a reference without wrapping it into an Option
            assert!(raw.is_not_null())
            let result = mem::copy_lifetime(self,&*raw);
            assert!(result._struct_version == STREAM_INFO_STRUCT_VERSION);
            result
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
    fn get_stream_time(&self) -> Time {
        let pa_stream = self.get_portaudio_stream_pointer();
        unsafe { ffi::Pa_GetStreamTime(pa_stream) }
    }
}

/// Represents an open RustAudio input stream
pub trait RaInputStreamAny: RaStreamAny {
    fn input_sample_type(&self) -> SampleType;
    fn input_channel_count(&self) -> uint;
}

/// Represents an open RustAudio output stream
pub trait RaOutputStreamAny: RaStreamAny {
    fn output_sample_type(&self) -> SampleType;
    fn output_channel_count(&self) -> uint;
}

/// Represents an open RustAudio duplex stream
pub trait RaDuplexStreamAny: RaInputStreamAny + RaOutputStreamAny {}
impl<T: RaInputStreamAny + RaOutputStreamAny> RaDuplexStreamAny for T {}

#[doc(hidden)]
trait SamplePrivate: Default {
    fn get_sample_type(&self) -> SampleType;
}

/// Trait for all supported sample types
pub trait Sample: SamplePrivate {}

macro_rules! impl_sample_for(
    ($t:ty as $x:expr) => (
        impl SamplePrivate for $t {
            fn get_sample_type(&self) -> SampleType { $x }
        }
        impl Sample for $t {}
    )
)

impl_sample_for!(f32 as Float32)
impl_sample_for!(i32 as Int32)
impl_sample_for!(i24 as Int24)
impl_sample_for!(i16 as Int16)
impl_sample_for!(i8 as Int8)
impl_sample_for!(u8 as Uint8)

pub fn get_sample_type_of<T: Sample>() -> SampleType {
    let dummy: T = Default::default();
    dummy.get_sample_type()
}

/// Represents an open RustAudio output stream with sample type `S`
pub trait RaInputStreamExt<S: Sample> : RaInputStreamAny {
    /// reads audio frames into the buffer `into`. The buffer length has to
    /// to be a multiple of channel count. Otherwise this method `fail!`s
    /// Depending on the stream flags used to open this stream, the function
    /// might return an `Err(RaInputOverflowed)`. In that case, PortAudio
    /// has discarded some audio data prior to this function call.
    fn read(&mut self, into: & mut [S]) -> RaResult<()> {
        assert!(into.len() % self.input_channel_count() == 0);
        let num_frames = into.len() / self.input_channel_count();
        tryRa!(unsafe {
            ffi::Pa_ReadStream(self.get_portaudio_stream_pointer(),
                               into.as_mut_ptr() as *mut ffi::c_void,
                               num_frames as ffi::c_ulong)
        });
        Ok(())
    }
}

/// Represents an open RustAudio output stream with sample type `T`
pub trait RaOutputStreamExt<T: Sample> : RaOutputStreamAny {
    /// writes audio frames from the buffer to the device. The buffer length
    /// has to be a multiple of the channel count. Otherwise this method
    /// `fail!`s. Depending on the stream flags used to open this stream, the
    /// function might return an `Err(RaOutputUnderflowed)`. In that case,
    /// PortAudio has inserted zeros to fill the output buffer between the
    /// previous call and this one.
    fn write(&mut self, from: &[T]) -> RaResult<()> {
        assert!(from.len() % self.output_channel_count() == 0);
        let num_frames = from.len() / self.output_channel_count();
        tryRa!(unsafe {
            ffi::Pa_WriteStream(self.get_portaudio_stream_pointer(),
                               from.as_ptr() as *const ffi::c_void,
                               num_frames as ffi::c_ulong)
        });
        Ok(())
    }
}

/// Represents an open RustAudio duplex stream with `S` as input sample
/// type and `T` as output sample type
pub trait RaDuplexStreamExt<S: Sample, T: Sample>
    : RaInputStreamExt<S> + RaOutputStreamExt<T> {}

impl<S: Sample, T: Sample, X: RaInputStreamExt<S> + RaOutputStreamExt<T>>
    RaDuplexStreamExt<S,T> for X {}


//--- structs that implement the above traits ... ---

/// Represents an open stream we can read from
pub struct RaInputStream<S> {
    base: StreamHandle,
    smp_rate: f64,
    smp_type: SampleType,
    n_channels: uint
}

impl<S: Sample> RaStreamPrivate for RaInputStream<S> {
    fn get_portaudio_stream_pointer(&self) -> *mut ffi::PaStream {
        self.base.raw_ptr
    }
    fn stored_sample_rate(&self) -> f64 {
        self.smp_rate
    }
    fn close_and_invalidate(&mut self) -> ffi::PaError {
        self.close_and_invalidate()
    }
}

impl<S: Sample> RaStreamAny for RaInputStream<S> {}

impl<S: Sample> RaInputStreamAny for RaInputStream<S> {
    fn input_sample_type(&self) -> SampleType { self.smp_type }
    fn input_channel_count(&self) -> uint { self.n_channels }
}

impl<S: Sample> RaInputStreamExt<S> for RaInputStream<S> {}


/// Represents an open stream we can write to
pub struct RaOutputStream<T> {
    base: StreamHandle,
    smp_rate: f64,
    smp_type: SampleType,
    n_channels: uint
}

impl<T: Sample> RaStreamPrivate for RaOutputStream<T> {
    fn get_portaudio_stream_pointer(&self) -> *mut ffi::PaStream {
        self.base.raw_ptr
    }
    fn stored_sample_rate(&self) -> f64 {
        self.smp_rate
    }
    fn close_and_invalidate(&mut self) -> ffi::PaError {
        self.close_and_invalidate()
    }
}

impl<T: Sample> RaStreamAny for RaOutputStream<T> {}

impl<T: Sample> RaOutputStreamAny for RaOutputStream<T> {
    fn output_sample_type(&self) -> SampleType { self.smp_type }
    fn output_channel_count(&self) -> uint { self.n_channels }
}

impl<T: Sample> RaOutputStreamExt<T> for RaOutputStream<T> {}


/// Represents an open stream we can read from and write to
pub struct RaDuplexStream<S, T> {
    base: StreamHandle,
    smp_rate: f64,
    inp_smp_type: SampleType,
    inp_n_channels: uint,
    out_smp_type: SampleType,
    out_n_channels: uint
}

impl<S: Sample, T: Sample> RaStreamPrivate for RaDuplexStream<S, T> {
    fn get_portaudio_stream_pointer(&self) -> *mut ffi::PaStream {
        self.base.raw_ptr
    }
    fn stored_sample_rate(&self) -> f64 {
        self.smp_rate
    }
    fn close_and_invalidate(&mut self) -> ffi::PaError {
        self.close_and_invalidate()
    }
}

impl<S: Sample, T: Sample> RaStreamAny for RaDuplexStream<S, T> {}

impl<S: Sample, T: Sample> RaInputStreamAny for RaDuplexStream<S, T> {
    fn input_sample_type(&self) -> SampleType { self.inp_smp_type }
    fn input_channel_count(&self) -> uint { self.inp_n_channels }
}

impl<S: Sample, T: Sample> RaOutputStreamAny for RaDuplexStream<S, T> {
    fn output_sample_type(&self) -> SampleType { self.out_smp_type }
    fn output_channel_count(&self) -> uint { self.out_n_channels }
}

impl<S: Sample, T: Sample> RaInputStreamExt<S> for RaDuplexStream<S, T> {}
impl<S: Sample, T: Sample> RaOutputStreamExt<T> for RaDuplexStream<S, T> {}

/// create ffi::PaStreamParams for given parameters
fn make_c_params<T: Sample>(device: GlobalDeviceIndex,
                            channels: uint,
                            suggested_latency: Time) -> ffi::PaStreamParameters {
    StreamParams {
        device: device,
        channel_count: channels,
        sample_type: get_sample_type_of::<T>(),
        suggested_latency: suggested_latency
    }.to_c_struct()
}

impl RustAudio {
    pub fn open_input<S: Sample>(&self,
                                 device: GlobalDeviceIndex,
                                 channels: uint,
                                 suggested_latency: Time,
                                 sample_rate: f64,
                                 flags: StreamFlags) -> RaResult<RaInputStream<S>> {
        let inp_params = make_c_params::<S>(device,channels,suggested_latency);
        let mut s: *mut ffi::PaStream = RawPtr::null();
        tryRa!(unsafe {
            ffi::Pa_OpenStream(&mut s, &inp_params, RawPtr::null(),
                               sample_rate, 0, flags.bits as ffi::PaStreamFlags,
                               RawPtr::null(), RawPtr::null())
        });
        assert!(s.is_not_null());
        Ok(RaInputStream::<S> {
            base: StreamHandle::new(self.clone(), s),
            smp_rate: sample_rate,
            smp_type: get_sample_type_of::<S>(),
            n_channels: channels
        })
    }
    pub fn open_output<T: Sample>(&self,
                                  device: GlobalDeviceIndex,
                                  channels: uint,
                                  suggested_latency: Time,
                                  sample_rate: f64,
                                  flags: StreamFlags) -> RaResult<RaOutputStream<T>> {
        let out_params = make_c_params::<T>(device,channels,suggested_latency);
        let mut s: *mut ffi::PaStream = RawPtr::null();
        tryRa!(unsafe {
            ffi::Pa_OpenStream(&mut s, RawPtr::null(), &out_params,
                               sample_rate, 0, flags.bits as ffi::PaStreamFlags,
                               RawPtr::null(), RawPtr::null())
        });
        assert!(s.is_not_null());
        Ok(RaOutputStream::<T> {
            base: StreamHandle::new(self.clone(), s),
            smp_rate: sample_rate,
            smp_type: get_sample_type_of::<T>(),
            n_channels: channels
        })
    }
    pub fn open_duplex<S: Sample, T: Sample>(&self,
                      inp_device: GlobalDeviceIndex,
                      out_device: GlobalDeviceIndex,
                      inp_channels: uint,
                      out_channels: uint,
                      suggested_latency: Time,
                      sample_rate: f64,
                      flags: StreamFlags) -> RaResult<RaDuplexStream<S, T>> {
        let inp_params = make_c_params::<S>(inp_device,inp_channels,suggested_latency);
        let out_params = make_c_params::<T>(out_device,out_channels,suggested_latency);
        let mut s: *mut ffi::PaStream = RawPtr::null();
        tryRa!(unsafe {
            ffi::Pa_OpenStream(&mut s, &inp_params, &out_params,
                               sample_rate, 0, flags.bits as ffi::PaStreamFlags,
                               RawPtr::null(), RawPtr::null())
        });
        assert!(s.is_not_null());
        Ok(RaDuplexStream::<S, T> {
            base: StreamHandle::new(self.clone(), s),
            smp_rate: sample_rate,
            inp_smp_type: get_sample_type_of::<S>(),
            out_smp_type: get_sample_type_of::<T>(),
            inp_n_channels: inp_channels,
            out_n_channels: out_channels
        })
    }
}

/*

/// Handle for an _opened_ but dynamically-typed PortAudio stream
pub struct RaStreamAny {
    /// a pointer to PortAudio allocated things it deems necessary
    /// to manage a stream (whatever it is)
    raw_pa_stream: *mut ffi::PaStream,
    /// keeps PortAudio initialized
    /// (a Stream is one owner of an initialized PortAudio library).
    _ra: RustAudio,

    // I will probably add something here to support callbacks in a way that
    // still allows streams to be safely movable (indirection à la Box maybe).
}

impl RaStreamPrivate for RaStreamAny {
    fn get_portaudio_stream_pointer(&self) -> *mut ffi::PaStream {
        self.raw_pa_stream
    }
    /// Function that closes but also invalidates this stream. It is used in
    /// `close` as well as in `drop`. Since it invalidates the stream this
    /// function is private so that the user cannot mess with it anymore.
    /// After all, we promised that a `RaStream` represents an _opened_ stream.
    fn close_and_invalidate(&mut self) -> RaResult<()> {
        unsafe {
            assert!(self.raw_pa_stream.is_not_null());
            let rawptr = mem::replace(&mut self.raw_pa_stream, RawPtr::null() );
            // Note: self.raw_pa_stream is now a null pointer.
            // The drop function has to check for this.
            tryRa!( ffi::Pa_CloseStream(rawptr) );
            Ok(())
        }
    }
}

impl RaStream for RaStreamAny {}

impl Drop for RaStreamAny {
    /// Closes an audio stream. If the audio stream is active it
    /// discards any pending buffers as if `abort` had been called.
    /// In case the corresponding C API function returns an error
    /// code (unlikely) you have no way of getting that.
    fn drop(&mut self) {
        // the internal raw pointer is always valid unless the user
        // explicitly called `close` on it in which case he lost access to it.
        // In this case, `raw_pa_stream` will ne a null pointer and there is
        // no special cleanup to do.
        if self.raw_pa_stream.is_not_null() {
            drop(self.close_and_invalidate());
        }
    }
}

*/

#[cfg(test)]
mod rustaudio {

    use super::RustAudio;
    use super::RaResult;
    use super::{RaStreamAny, RaOutputStreamExt};
    use super::RaOutputStream;
    use super::StreamFlags;
    use super::portaudio_version;
    use super::portaudio_version_text;
    use num::complex::Complex;

    fn make_some_noise(ra: &RustAudio) -> RaResult<()> {
        println!("Let's make some NOOOOOIIISE...");
        println!("opening default audio device...");
        let device = match try!(ra.default_output_device()) {
            None => { println!("no default output?"); return Ok(()); }
            Some(d) => d
        };
        //let devinfo = try!(ra.device_info(device));
        let fs = 48000.0;
        let mut out: RaOutputStream<i16> =
            try!(ra.open_output(device, 1, // one channel
                                0.2, // suggested latency in seconds
                                fs, StreamFlags::empty()));
        let mut block = Vec::from_elem(4800, 0);
        let mut phase = Complex::from_polar(&1.0f64, &0.0f64);
        let mut freq = Complex::from_polar(&1.0f64, &0.0f64);
        let sweep_speed = Complex::from_polar(&1.0f64, &0.00001);
        try!(out.start());
        loop {
            for smp in block.mut_iter() {
                *smp = (phase.im * 3000.0).round() as i16;
                phase = phase * freq;
                freq = freq * sweep_speed;
            }
            if freq.im < 0.0 { break; }
            let res = out.write(block.as_slice());
            match res {
                Err(super::RaOutputUnderflowed) => {
                    println!("oops, the output buffer underflowed");
                }
                Err(x) => return Err(x),
                _ => ()
            }
        }
        try!(out.stop());
        Ok(())
    }

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

    fn all() -> RaResult<()> {
        let ra = try!(RustAudio::new());
        println!("RustAudio successfully initialized");
        try!(check_apis_and_devices(&ra));
        try!(make_some_noise(&ra));
        Ok(())
    }

    #[test]
    fn test() {
        println!("PortAudio version: {}, {}",
                 portaudio_version(),
                 portaudio_version_text() );
        match all() {
            Err(e) => { println!("RustAudio error: {}", e); fail!("{}",e); }
            Ok(()) => { println!("all good. :)"); }
        }
    }
    
} // mod test

