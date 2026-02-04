# Commit Messages

Use semantic commit format:
- feat: new features
- fix: bug fixes
- docs: documentation changes
- refactor: code changes without new features or fixes
- perf: performance improvements
- test: adding or updating tests
- chore: maintenance, dependencies, tooling

Format: `<type>: <subject>` or `<type>(<scope>): <subject>`

# PR Messages

Do not include test scenarios in PR descriptions. Focus on what the PR changes and why, and reference related issues or PRs where relevant.

# Python

Use `uv` instead of calling `python` or `pip` directly. For example:
- `uv run` instead of `python`
- `uv pip install` instead of `pip install`
- `uv venv` instead of `python -m venv`

# Emacs Lisp

Use the following tool preference order for building, testing, and running Emacs Lisp projects:

1. **`make`** — if a Makefile exists, use it first
2. **`eldev`** — preferred development tool; handles dependency setup, isolation, and testing
3. **Manual `emacs` invocations** — last resort only

Eldev ([docs](https://emacs-eldev.github.io/eldev/), [repo](https://github.com/emacs-eldev/eldev)) key commands:
- `eldev test` — run tests (supports ERT, Buttercup, Doctest)
- `eldev lint` — lint source code
- `eldev emacs` — launch Emacs with only the project loaded (isolated environment)
- `eldev exec EXPR` — evaluate an expression in the project's context
- `eldev compile` — byte-compile the project
- `eldev package` — build a package `.tar` or `.el`
- `eldev clean` — remove build artifacts

Eldev configuration lives in `Eldev` and `Eldev-local` files (Elisp). It isolates the project by default, so dependencies are managed separately from the user's Emacs config.

# Testing

Write tests before implementation (test-driven development). Create or update test files first, verify they fail, then write the implementation to make them pass.

# Documentation and Changelogs

When implementing a feature or making a change, also update any relevant documentation and changelog files in the project.
