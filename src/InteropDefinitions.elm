module InteropDefinitions exposing (Flags, FromElm(..), ToElm(..), interop)

import TsJson.Codec as Codec exposing (Codec)
import TsJson.Decode as Decode exposing (Decoder)
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
    { basePath : String
    }


flags : Decoder Flags
flags =
    Decode.map Flags
        (Decode.field "basePath" Decode.string)


type ToElm
    = WakeLockAvailable
    | WakeLockAcquired
    | WakeLockReleased
    | WakeLockError String
    | LanguageDetected String


toElm : Codec ToElm
toElm =
    Codec.custom (Just "tag")
        (\vAvailable vAcquired vReleased vError vLanguageDetected value ->
            case value of
                WakeLockAvailable ->
                    vAvailable

                WakeLockAcquired ->
                    vAcquired

                WakeLockReleased ->
                    vReleased

                WakeLockError err ->
                    vError err

                LanguageDetected lang ->
                    vLanguageDetected lang
        )
        |> Codec.variant0 "WakeLockAvailable" WakeLockAvailable
        |> Codec.variant0 "WakeLockAcquired" WakeLockAcquired
        |> Codec.variant0 "WakeLockReleased" WakeLockReleased
        |> Codec.namedVariant1 "WakeLockError" WakeLockError ( "error", Codec.string )
        |> Codec.namedVariant1 "LanguageDetected" LanguageDetected ( "language", Codec.string )
        |> Codec.buildCustom


type FromElm
    = WakeLockCheck
    | WakeLockAcquire
    | WakeLockRelease
    | DetectLanguage
    | SaveLanguage String


fromElm : Codec FromElm
fromElm =
    Codec.custom (Just "tag")
        (\vCheck vAcquire vRelease vDetectLanguage vSaveLanguagePreference value ->
            case value of
                WakeLockCheck ->
                    vCheck

                WakeLockAcquire ->
                    vAcquire

                WakeLockRelease ->
                    vRelease

                DetectLanguage ->
                    vDetectLanguage

                SaveLanguage lang ->
                    vSaveLanguagePreference lang
        )
        |> Codec.variant0 "WakeLockCheck" WakeLockCheck
        |> Codec.variant0 "WakeLockAcquire" WakeLockAcquire
        |> Codec.variant0 "WakeLockRelease" WakeLockRelease
        |> Codec.variant0 "DetectLanguage" DetectLanguage
        |> Codec.namedVariant1 "SaveLanguage" SaveLanguage ( "lang", Codec.string )
        |> Codec.buildCustom
