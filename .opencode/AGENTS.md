# General Instructions for AI Agents

## Coding Style

- Organize code so it reads from top to bottom. Main/public logic should be at the top, helpers below.

### Comments
- Don't comment what code clearly explains
- Use descriptive function names instead of comments
- Comment only for complex logic, workarounds, or public APIs

### Testing
- Prefer TDD, unless ther is a strong reason not to.
- Force TDD, if starting a new project.
- Avoid brittle tests that are tightly coupled to internal implementation details.
- Avoid writing tests for trivial stuff that doesn't add value.
- Prefer co-located unit tests and separate integration tests.
- Create meaningful tests that focus on behavior

## Git Conventions
- Use conventional commits for commit messages
