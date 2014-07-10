#  Mixin for PortAudioTypes.jl
export PaError, PaDeviceIndex, PaHostApiIndex, PaStream, PaTime, PaStreamFlags
export PaStreamCallbackFlags, PaSampleFormat

typealias PaError               Cint
typealias PaDeviceIndex         Cint
typealias PaHostApiIndex        Cint
typealias PaStream              Ptr{Void}
typealias PaTime                Cdouble
typealias PaStreamFlags         Culong
typealias PaStreamCallbackFlags Culong

export paFloat32, paInt32, paInt24, paInt16, paInt8, paUInt8, paCustomFormat
export paNonInterleaved

typealias PaSampleFormat        Culong
const paFloat32        = convert(PaSampleFormat, 1<<0)
const paInt16          = convert(PaSampleFormat, 1<<1)
const paInt32          = convert(PaSampleFormat, 1<<2)
const paInt24          = convert(PaSampleFormat, 1<<3)
const paPackedInt24    = convert(PaSampleFormat, 1<<4)
const paInt8           = convert(PaSampleFormat, 1<<4)
const paUInt8          = convert(PaSampleFormat, 1<<5)
const paCustomFormat   = convert(PaSampleFormat, 1<<6)
const paNonInterleaved = convert(PaSampleFormat, 1<<16)
