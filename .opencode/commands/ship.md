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
   - **Complex** = diff >300 lines, >6 files, or touches auth, payments, or migrations.
   - Delegate to `code-reviewer` with the requirements and a summary of what changed.
   - Complex: run 3 `code-reviewer` in parallel, each with a `Focus:`
     (1) correctness, requirements, tests
     (2) security, error handling, edge cases
     (3) conventions, reuse, dead code
     Merge by `file:line`. Raised by 2+ reviewers -> treat as confidence >= 75.
     Keep every `Look hardest at` line for step 6.
   - Verify each `FIX_NOW` finding against the code before fixing. Wrong -> do not fix; list it
     under Review Notes as "Disputed".
   - Fix `FIX_NOW` findings (resume the owning coder by `task_id` if one wrote the code).
     Re-review. Max 2 rounds. Leftover `FIX_NOW` -> Review Notes as "Not addressed".

5. **Commit**
   - Simple: one conventional commit.
   - Complex: one conventional commit per unit, in reading order:
     (1) enabling refactors, behavior-preserving
     (2) core logic with its tests
     (3) wiring: routes, config, callers
     (4) mechanical: generated files, bulk renames, docs
     Split by file: `git add <files>` per commit. A file in two units goes in the earlier
     one; do not split hunks. Keep both paths of a rename in one commit. Coder units -> one
     commit each. Tests must pass at HEAD; per-commit green is not required.
   - main/master: show a summary, ask before committing. Do not push or open a PR/MR unless
     the user explicitly asks.

6. **Ship** (feature branch)
   - `git push -u origin <branch>`. Open the PR/MR with `--assignee @me`
     (`--reviewer @me` on GitLab only). Title: subject of the core commit. Body sections:
     - **Summary**: what and why
     - **Where to start**: Complex only. Commits from `git log --oneline`, in order. Per commit:
       short SHA, subject, `read` or `skim`, what to look for. For `read`: the test file to
       start with. Then two lines:
       - `Least sure:` 1-3 `file:line — why` where you guessed: unverified API behavior,
         inferred requirements, untested paths
       - `Look hardest at:` reviewer lines, deduped
     - **Changes**: files and what changed in each
     - **Review Notes**: `REPORT` findings verbatim, plus any Disputed or Not addressed. Omit if none.
   - Print the PR/MR URL. Complex: also print **Where to start**. If `read` commits total
     >400 lines, add one line naming which commits could be their own MR.

   Example **Where to start**:
   ```
   1. `a1b2c3d refactor: extract PaymentGateway` — skim. Behavior-preserving.
   2. `d4e5f6a feat: retry failed payments` — read. Start with `tests/payment_test.py`,
      then `service/payment.py:40-95`. Watch the retry branch.
   3. `789abcd feat: wire retry into POST /payments` — skim. Wiring only.
   Least sure: `service/payment.py:71` — idempotency key reuse on retry
   Look hardest at: `service/payment.py:88` — timeout error swallowed
   ```
