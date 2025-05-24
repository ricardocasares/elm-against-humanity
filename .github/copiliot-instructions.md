# Copilot Instructions

This codebase uses Elm, a pure functional language.

## General Guidelines

- Keep your responses short and concise
- Vite dependency is already installed and configured for bundling sources
- Use `bun dev` on the @terminal to get a working http server
- Run `bun compile` on the @terminal to check for Elm compiler errors
- Run `bun lint` on the @terminal for linting Elm files and fix errors
- Run `bun run test` on the @terminal for testing your changes
- Use Elm v0.19.1 when generating Elm code
- Minimize usage of external dependencies
- Check the #problems tab to resolve issues
- Create small reusable functions
- Keep the `update` function clean and readable
- Keep indentation low by using ELm's pipeline operator |>
- Always use TypeScript for type-safety interop with Elm

## Commit messages

- Keep commit messages short and concise
- Choose one of the following prefixes:
  - ai: for changes made to copilot instructions
  - ui: for cosmetic changes only
  - ci: for changes made to github workflows
  - doc: for README.md updates or code comments
  - dep: for changes in dependencies
  - fix: for code fixes
  - feat: for new features implemented in the code