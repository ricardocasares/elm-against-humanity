---
applyTo: "**/*.rfc.md"
---

# RFC Instructions

We use RFC to think about new features that will be added to the codebase.

- Be short and concise.
- Target a technical audience.
- Write RFC inside `rfc` folder.
- Name files based on the feature and add a `.rfc.md` suffix:
  - `rfc/some-new-feature.rfc.md`
  - `rfc/some-new-endpoint.rfc.md`

## Mandatory sections

1. Overview: describes the purpose of the RFC.
2. Goals: Clearly defined objectives that the RFC intends to achieve.
3. Proposal: detailed explanation of the proposed changes, for example:
   - Data structures, or architectural changes.
   - Changes to the current application `Model`.
   - OpenAPI yaml specifications.
   - Pseudocode or code snippets.
   - Dependency or integration concerns.
   - Technical considerations.
   - Always use Elm and/or TypeScript in your code.
4. Test plan: how the feature will be tested.

## Research Instructions

When required, use the tools available to you in order to find more information:

- Find quality documentation
- Research about Web APIs when needed
- Understand the application current architectural state
