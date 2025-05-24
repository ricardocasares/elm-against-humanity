import { defineConfig } from "vite";
import css from "@tailwindcss/vite";
import elm from "vite-plugin-elm";
import ssl from "vite-plugin-mkcert";

export default defineConfig({
  plugins: [css(), elm(), ssl()],
});
