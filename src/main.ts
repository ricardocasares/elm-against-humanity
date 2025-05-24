import { Elm } from "./Main.elm";
import { match } from "ts-pattern";

export const node = document.getElementById("app");
export const app = Elm.Main.init({ node, flags: null });
const send = app.ports.interopToElm.send;

app.ports.interopFromElm.subscribe((msg) =>
  match<Elm.FromElm>(msg)
    .with({ tag: "WakeLockCheck" }, wakeLockCheck)
    .with({ tag: "WakeLockAcquire" }, wakeLockAcquire)
    .with({ tag: "WakeLockRelease" }, wakeLockRelease)
    .with({ tag: "DetectLanguage" }, detectLanguage)
    .with({ tag: "SaveLanguagePreference" }, ({ language }) =>
      saveLanguagePreference(language)
    )
    .exhaustive()
);

const wakeLockCheck = () =>
  match<boolean>("wakeLock" in navigator)
    .with(true, () => send({ tag: "WakeLockAvailable" }))
    .with(false, () => send({ tag: "WakeLockError", error: "Not supported" }))
    .exhaustive();

const wakeLockAcquire = () =>
  navigator.wakeLock
    .request("screen")
    .then(() => send({ tag: "WakeLockAcquired" }))
    .catch((err) =>
      send({
        tag: "WakeLockError",
        error: String(err?.message || err),
      })
    );

const wakeLockRelease = () =>
  navigator.wakeLock
    .request("screen")
    .then((lock) =>
      lock.release().then(() => send({ tag: "WakeLockReleased" }))
    )
    .catch((err) =>
      send({
        tag: "WakeLockError",
        error: String(err?.message || err),
      })
    );

const detectLanguage = () => {
  // Check localStorage first for saved preference
  const savedLanguage = localStorage.getItem("humanity-language");
  if (savedLanguage) {
    send({ tag: "LanguageDetected", language: savedLanguage });
    return;
  }

  // Otherwise detect from navigator.language
  const browserLanguage =
    navigator.language || navigator.languages?.[0] || "en";
  send({ tag: "LanguageDetected", language: browserLanguage });
};

const saveLanguagePreference = (language: string) => {
  localStorage.setItem("humanity-language", language);
};
