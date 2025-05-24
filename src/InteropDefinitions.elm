module InteropDefinitions exposing (Flags, FromElm(..), ToElm(..), interop)

import TsJson.Codec as Codec exposing (Codec)
import TsJson.Decode as TD exposing (Decoder)
import TsJson.Encode exposing (Encoder)


interop :
    { flags : Decoder Flags
    , toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    }
interop =
    { flags = flags
    , toElm = toElm |> Codec.decoder
    , fromElm = fromElm |> Codec.encoder
    }


type alias Flags =
    {}


flags : Decoder Flags
flags =
    TD.null {}


type ToElm
    = WakeLockAvailable
    | WakeLockAcquired
    | WakeLockReleased
    | WakeLockError String


toElm : Codec ToElm
toElm =
    Codec.custom (Just "tag")
        (\vAvailable vAcquired vReleased vError value ->
            case value of
                WakeLockAvailable ->
                    vAvailable

                WakeLockAcquired ->
                    vAcquired

                WakeLockReleased ->
                    vReleased

                WakeLockError err ->
                    vError err
        )
        |> Codec.variant0 "WakeLockAvailable" WakeLockAvailable
        |> Codec.variant0 "WakeLockAcquired" WakeLockAcquired
        |> Codec.variant0 "WakeLockReleased" WakeLockReleased
        |> Codec.namedVariant1 "WakeLockError" WakeLockError ( "error", Codec.string )
        |> Codec.buildCustom


type FromElm
    = WakeLockCheck
    | WakeLockAcquire
    | WakeLockRelease


fromElm : Codec FromElm
fromElm =
    Codec.custom (Just "tag")
        (\vCheck vAcquire vRelease value ->
            case value of
                WakeLockCheck ->
                    vCheck

                WakeLockAcquire ->
                    vAcquire

                WakeLockRelease ->
                    vRelease
        )
        |> Codec.variant0 "WakeLockCheck" WakeLockCheck
        |> Codec.variant0 "WakeLockAcquire" WakeLockAcquire
        |> Codec.variant0 "WakeLockRelease" WakeLockRelease
        |> Codec.buildCustom
