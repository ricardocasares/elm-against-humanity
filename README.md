# Elm Against Humanity

A Cards Against Humanity-style party game built with Elm, featuring multiple language support and a modern web interface.

## Features

- **Multi-language support**: English, Spanish, and Polish card decks
- **Responsive design**: Works on desktop and mobile devices
- **Score tracking**: Keep track of player scores across rounds
- **Touch-friendly**: Swipe gestures for mobile navigation
- **PWA ready**: Installable as a web app

## Tech Stack

- [Elm](https://elm-lang.org/) - Functional frontend language
- [Tailwind CSS](https://tailwindcss.com/) v4 - Utility-first CSS framework
- [DaisyUI](https://daisyui.com/) v5 - Component library for Tailwind
- [Vite](https://vitejs.dev/) - Fast build tool and dev server
- [Bun](https://bun.sh/) - JavaScript runtime and package manager

## Development

```sh
# Clone the repository
git clone https://github.com/ricardocasares/elm-against-humanity.git
cd elm-against-humanity

# Install dependencies
bun install

# Start development server
bun dev

# Build for production
bun run build

# Run tests
bun run test

# Lint code
bun run lint
```

## Project Structure

- `src/` - Elm source code
- `public/` - Static assets including card decks in multiple languages
- `tests/` - Elm test files
- `.github/instructions/` - Development guidelines for the codebase
