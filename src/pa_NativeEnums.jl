#  Mixin for PortAudioTypes.jl
export PaEnum, PaErrorCode, PaHostApiTypeId, PaStreamCallbackResult

typealias PaEnum                 Cint
typealias PaErrorCode            PaEnum
typealias PaHostApiTypeId        PaEnum
typealias PaStreamCallbackResult PaEnum
