module PortAudio
#  Wrappers surrounding the most frequently used portaudio calls

using PortAudioTypes

global const PA_LIB = "libportaudio"

# Library Context  ############################################################

export PA_Context
type PA_Context
    #  Singleton for holding library state.
    function PA_Context()
        context = new()
        info("Initializing PortAudio...")
        Pa_Initialize()
        finalizer(context, finalize)
        return context
    end
end
finalize(c::PA_Context) = Pa_Terminate()

Pa_Initialize() = paHandleStatus( ccall((:Pa_Initialize, PA_LIB), PaError, ()) )
Pa_Terminate() =  paHandleStatus( ccall((:Pa_Terminate, PA_LIB), PaError, ()) )

#  Error Reporting  ###########################################################

export PortAudioError, paHandleStatus, Pa_GetErrorText

type PortAudioError <: Exception
    errorMessage::String
end

function paHandleStatus(err::PaError)
    if err != 0
        throw( PortAudioError(Pa_GetErrorText(err)) )
    end
end

function Pa_GetErrorText(err::PaError)
    msg = ccall((:Pa_GetErrorText, PA_LIB), Ptr{Cchar}, (PaError,), err)
    return bytestring(msg)
end

#  Audio Stream  ##############################################################
#  This is where the shenanigans begin...
export Pa_OpenDefaultStream, Pa_CloseStream, Pa_StartStream, Pa_StopStream
export Pa_AbortStream

function Pa_OpenDefaultStream(stream::Ptr{PaStream},
                              inputChannels::Int,
                              outputChannels::Int,
                              sampleFormat::PaSampleFormat,
                              sampleRate::FloatingPoint,
                              framesPerBuffer::Int,
                              streamCallback::Ptr{Void},
                              userData::Ptr{Void})
    err = ccall( (:Pa_OpenDefaultStream, PA_LIB), PaError,
                 (Ptr{PaStream}, Cint, Cint, Cfloat, Cdouble,
                     Cint, Ptr{Void}, Ptr{Void}),
                 stream, inputChannels, outputChannels, sampleFormat, sampleRate,
                     framesPerBuffer, streamCallback, userData )
    return paHandleStatus(err)
end

#  Closes a stream. If the audio stream is active it discards pending buffers
#  as if Pa_AbortStream(...) had been called.
function Pa_CloseStream(stream::Ptr{PaStream})
    err = ccall((:Pa_CloseStream, PA_LIB), PaError, (Ptr{PaStream},), stream)
    return paHandleStatus(err)
end

#  Commences audio processing. i.e. the streamCallback function will start
#  getting called.
function Pa_StartStream(stream::Ptr{PaStream})
    err = ccall((:Pa_StartStream, PA_LIB), PaError, (Ptr{PaStream},), stream)
    return paHandleStatus(err)
end

#  Terminates audio processing after waiting for pending buffers to complete
function Pa_StopStream(stream::Ptr{PaStream})
    err = ccall((:Pa_StopStream, PA_LIB), PaError, (Ptr{PaStream},), stream)
    return paHandleStatus(err)
end

#  Terminates audio processing immediately without waiting for pending buffers
#  to complete
function Pa_AbortStream(stream::Ptr{PaStream})
    err = ccall((:Pa_AbortStream, PA_LIB), PaError, (Ptr{PaStream},), stream)
    return paHandleStatus(err)
end

#  Callback Generators  #######################################################
export cb_generator, whiteNoiseCallback
function whiteNoiseCallback(inputBuffer::Ptr{Void}, outputBuffer::Ptr{Void},
                      framesPerBuffer::Culong,
                      timeInfo::Ptr{PaStreamCallbackTimeInfo},
                      statusFlags::PaStreamCallbackFlags,
                      userData::Ptr{Void})
    data = rand(Uint8, framesPerBuffer)
    unsafe_copy!(outputBuffer, pointer(data), framesPerBuffer)
    return convert(PaError, 0)::PaError
end

function cb_generator(fn::Function)
    const fun = cfunction(fn, PaError,
                (Ptr{Void}, Ptr{Void}, Culong, Ptr{PaStreamCallbackTimeInfo},
                 PaStreamCallbackFlags, Ptr{Void}))
    return fun
end

end
