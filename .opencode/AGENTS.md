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
- TDD by default. Mandatory for new projects.
- Test behavior, not implementation. Don't test library behavior (e.g., Jackson deserialization, Lombok getters).
- Co-located unit tests. Integration tests separate.
- Skip tests for trivial code.

## Git Conventions
- Conventional commits, unless project rules say otherwise.
- Commit body: why and non-obvious decisions, not what.
- Squash before merge: `glab mr create --squash-before-merge --remove-source-branch`, `glab mr merge --squash`, `gh pr merge --squash`.
- No AI attribution or co-author tags in commits.

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
