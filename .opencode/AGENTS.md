# General Instructions for AI Agents

## Coding Style

- Organize code so it reads from top to bottom. Main/public logic should be at the top, helpers below.
- Prefer readable code over clever code. Extract complex conditionals into well-named private methods. Keep public methods short and high-level — they should read like a summary, with details in helpers below.
- Before writing new code, search for existing helpers to reuse. Remove dead code your change leaves behind.

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
- Prefer squash before merge

## Browser Automation

Use `agent-browser` for web automation. Run `agent-browser --help` for all commands.

Core workflow:
1. `agent-browser open <url>` - Navigate to page
2. `agent-browser snapshot -i` - Get interactive elements with refs (@e1, @e2)
3. `agent-browser click @e1` / `fill @e2 "text"` - Interact using refs
4. Re-snapshot after page changes

## Output contract

Work exhaustively. Report tersely.

- First line is the answer. Then at most 3 bullets. Then stop.
- Report deviations, blockers, and surprises. Skip the happy path.
- No preamble, no closing summary, no restating the question.
- No headers for answers under 10 lines. No lists with 1-2 items.
- Prefer a short example over a paragraph of explanation.
- Details only when asked.
- Apply ASD-STE100 principles: simple words, short sentences, one idea per sentence.

Bad:
> Great question! I looked into this and there are several things to consider. First, the cache layer... [40 lines] ... In summary, the cache is stale.

Good:
> The cache is stale. `invalidate()` never runs because `ttl` is read before the config loads (`cache.ts:42`).
