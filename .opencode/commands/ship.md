---
description: Implement, review, commit, and open a PR/MR
agent: build
---

Implement this task. Use all context from this conversation.

**Task:** $ARGUMENTS

## Steps

1. **Context**
   - `git branch --show-current`
   - `git remote get-url origin`: github.com -> `gh` and PR; gitlab -> `glab` and MR; unclear -> ask.

2. **Plan**
   - List the files to change. Track steps in the todo list.

3. **Implement**
   - Write the code yourself. Prefer TDD. Run the tests.
   - Exception: 2+ units that share no files. Run one `coder` per unit in parallel (max 4).
     Give each: the full requirements, its unit, files it owns, files it may only read.
     Keep each `task_id` for fixes.

4. **Review**
   - Delegate to `code-reviewer` with the requirements and a summary of what changed.
   - Large or risky diff (>300 lines, >6 files, or touches auth, payments, or migrations):
     run 3 `code-reviewer` in parallel, each with a `Focus:`
     (1) correctness, requirements, tests
     (2) security, error handling, edge cases
     (3) conventions, reuse, dead code
     Merge by `file:line`. Raised by 2+ reviewers -> treat as confidence >= 75.
   - Verify each `FIX_NOW` finding against the code before fixing. Wrong -> do not fix; list it
     under Review Notes as "Disputed".
   - Fix `FIX_NOW` findings (resume the owning coder by `task_id` if one wrote the code).
     Re-review. Max 2 rounds. Leftover `FIX_NOW` -> Review Notes as "Not addressed".

5. **Ship**
   - Feature branch: conventional commit, `git push -u origin <branch>`, open the PR/MR with
     `--assignee @me` (`--reviewer @me` on GitLab only). Body sections:
     - **Summary**: what and why
     - **Changes**: files and what changed in each
     - **Review Notes**: `REPORT` findings verbatim, plus any Disputed or Not addressed. Omit if none.
   - main/master: show a summary, ask before committing. Do not push or open a PR/MR unless
     the user explicitly asks.
