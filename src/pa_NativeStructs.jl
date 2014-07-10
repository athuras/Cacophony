# Mixin for PortAudioTypes.jl
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
