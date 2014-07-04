# Julia Wrapper of the types/structs/enums in portaudio.h
# Modules:
#   PortAudioTypes: Mostly wrappers around Cint/Cdouble, Ptr{Void} etc.
#   PortAudioEnums: Container types for enums, mostly integers
#   PortAudioStructs: Composite type representation mirroring the structs in portaudio.h

module PortAudioTypes

export PaError, PaDeviceIndex, PaHostApiIndex, PaStream, PaTime
export PaSampleFormat, PaStreamFlags, PaStreamCallbackFlags

typealias PaError               Cint
typealias PaDeviceIndex         Cint
typealias PaHostApiIndex        Cint
typealias PaStream              Ptr{Void}
typealias PaTime                Cdouble
typealias PaStreamFlags         Culong
typealias PaStreamCallbackFlags Culong


export paFloat32, paInt32, paInt24, paInt16, paInt8, paUInt8
export paCustomFormat, paNonInterleaved

typealias PaSampleFormat        Culong
paFloat32   = convert(PaSampleFormat, 0x00000001)
paInt32     = convert(PaSampleFormat, 0x00000002)
paInt24     = convert(PaSampleFormat, 0x00000004)
paInt16     = convert(PaSampleFormat, 0x00000008)
paInt8      = convert(PaSampleFormat, 0x00000010)
paUInt8     = convert(PaSampleFormat, 0x00000020)
paCustomFormat   = convert(PaSampleFormat, 0x00010000)
paNonInterleaved = convert(PaSampleFormat, 0x80000000)


export PortAudioError

type PortAudioError <: Exception
    errorCode::PaError
end

module PortAudioEnums
# These are just here so the code that refers to them is cleaner,
# Ultimately, until Julia has native Enum support they are all
# just treated as subtypes of Cint.
export PaEnum, PaErrorCode, PaHostApiTypeId, PaStreamCallbackResult

typealias PaEnum                 Cint
typealias PaErrorCode            PaEnum
typealias PaHostApiTypeId        PaEnum
typealias PaStreamCallbackResult PaEnum
end

module PortAudioStructs
# I'm not entirely sure how to deal with structs,
# everything I've found seems to just treat things as pointers
# and do a bunch of unsafe_load(...) operations.
using PortAudioTypes
using PortAudioEnums

export PaHostApiInfo, PaHostErrorInfo, PaDeviceInfo, PaStreamParameters
export PaStreamCallbackTimeInfo, PaStreamInfo

# Contains info about a particular host API
type PaHostApiInfo
    structVersion::Cint
    type_id::PaHostApiTypeId
    name::Ptr{Uint8}
    deviceCount::Cint
    defaultInputDevice::PaDeviceIndex
    defaultOutputDevice::PaDeviceIndex
end

# Used to return information about a host error condition
type PaHostErrorInfo
    hostApiType::PaHostApiTypeId
    errorCode::Clong
    errorText::Ptr{Uint8}
end

# Info and capabilities of PortAudio devices:
# Devices may input, output, or both,
type PaDeviceInfo
    structVersion::Cint
    name::Ptr{Uint8}
    hostApi::PaHostApiIndex
    maxInputChannels::Cint
    maxOutputChannels::Cint

    # Default latency values for interactive performance
    defaultLowInputLatency::PaTime
    defaultLowOutputPatency::PaTime

    # Default latency for robust non-interactive applications
    defaultHighInputLatency::PaTime
    defaultHighOutputLatency::PaTime

    defaultSampleRate::Cdouble
end

type PaStreamParameters
    device::PaDeviceIndex
    channelCount::Cint
    sampleFormat::PaSampleFormat
    suggestedLatency::PaTime
    hostApiSpecificStreamInfo::Ptr{Void}
end

type PaStreamCallbackTimeInfo
    # The time when the first sample of the input buffer was captured at the ADC input
    inputBufferAdcTime::PaTime
    # The time when the stream callback was invoked
    currentTime::PaTime
    # The time when the first sample of the output buffer will output the DAC
    outputBufferDacTime::PaTime
end

type PaStreamInfo
    structVersion::Cint
    # Times are expressed in SECONDS
    inputLatency::PaTime
    outputLatency::PaTime
    sampleRate::Cdouble
end
end
