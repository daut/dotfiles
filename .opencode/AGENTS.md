# General Instructions for AI Agents

## Writing Style

- When outling a plan be concise and use simple language to make it easier to understand and skim.
- Prefer using examples to illustrate concepts and outline plans and explanations.
- If it is possible to cut a word out, always cut it out.
- Never use a long word where a short one will do.
- Never use fillers, get straight to the point.
- Apply ASD-STE100 principles as much as possible

## Coding Style

- Organize code so it reads from top to bottom. Main/public logic should be at the top, helpers below.
- Prefer readable code over clever code. Extract complex conditionals into well-named private methods. Keep public methods short and high-level — they should read like a summary, with details in helpers below.

### Comments
- Don't comment what code clearly explains
- Use descriptive function names instead of comments
- Comment only for complex logic, workarounds, or public APIs

### Testing
- Prefer TDD, unless ther is a strong reason not to.
- Force TDD, if starting a new project.
- Avoid brittle tests that are tightly coupled to internal implementation details.
- Avoid flaky tests that fail intermittently without code changes.
- Avoid writing tests for trivial stuff that doesn't add value.
- Prefer co-located unit tests and separate integration tests.
- Create meaningful tests that focus on behavior
- Don't test library behavior (e.g., Jackson deserialization, Lombok getters). Trust your dependencies.

## Git Conventions
- Use conventional commits for commit messages

## Browser Automation

Use `agent-browser` for web automation. Run `agent-browser --help` for all commands.

Core workflow:
1. `agent-browser open <url>` - Navigate to page
2. `agent-browser snapshot -i` - Get interactive elements with refs (@e1, @e2)
3. `agent-browser click @e1` / `fill @e2 "text"` - Interact using refs
4. Re-snapshot after page changes
