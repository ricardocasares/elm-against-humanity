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

## Implementing New Features

When asked to implement a new feature, DO NOT WRITE ANY CODE!
Instead, write a new RFC to think about new features that will be added to the codebase.

- Be short and concise.
- Target a technical audience.
- Write RFC inside `rfc` folder.
- Name files based on the feature and add a `.rfc.md` suffix:
  - `rfc/001-some-new-feature.rfc.md`
  - `rfc/002-some-new-endpoint.rfc.md`
- Iterate over the RFC to learn more.

### Mandatory RFC sections

1. Overview: describes the purpose of the RFC.
2. Goals: Clearly defined objectives that the RFC intends to achieve.
3. Proposal: detailed explanation of the proposed changes, for example:
   - Add step-by-step instructions.
   - Data structures, or architectural changes.
   - Changes to the current application `Model`.
   - OpenAPI yaml specifications.
   - Pseudocode or code snippets.
   - Dependency or integration concerns.
   - Technical considerations.
   - Always use Elm and/or TypeScript in your code.
4. Test plan: how the feature will be tested, added unit tests.

### Research Instructions

When required, use the tools available to you in order to find more information:

- Find quality documentation
- Research about Web APIs when needed
- Understand the application current architectural state
