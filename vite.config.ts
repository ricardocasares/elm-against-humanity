import { defineConfig } from "vite";
import css from "@tailwindcss/vite";
import elm from "vite-plugin-elm";

export default defineConfig({
  plugins: [css(), elm()],
});
