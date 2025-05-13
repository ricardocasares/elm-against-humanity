import "./main.css";
// @ts-expect-error
import { Elm } from "./Main.elm";

export const node = document.querySelector("#app");
export const app = Elm.Main.init({ node });
