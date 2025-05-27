import { Elm } from "./Main.elm";
import { match, P } from "ts-pattern";

const basePath = import.meta.env.BASE_URL!.replace(
  /^\/(?!$)(.*?)(\/?)$/,
  "/$1/"
);

export const node = document.getElementById("app");
export const app = Elm.Main.init({
  node,
  flags: {
    basePath,
  },
});
const send = app.ports.interopToElm.send;

app.ports.interopFromElm.subscribe((msg) =>
  match<Elm.FromElm>(msg)
    .with({ tag: "WakeLockCheck" }, wakeLockCheck)
    .with({ tag: "WakeLockAcquire" }, wakeLockAcquire)
    .with({ tag: "WakeLockRelease" }, wakeLockRelease)
    .with({ tag: "DetectLanguage" }, detectLanguage)
    .with({ tag: "SaveLanguage" }, ({ lang }) => saveLang(lang))
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
    .then((lock) => lock.release())
    .then(() => send({ tag: "WakeLockReleased" }))
    .catch((err) =>
      send({
        tag: "WakeLockError",
        error: String(err?.message || err),
      })
    );

const detectLanguage = () => {
  const language = localStorage.getItem("lang") || navigator.language || "en";
  send({ tag: "LanguageDetected", language });
};

const saveLang = (lang: string) => {
  localStorage.setItem("lang", lang);
};
