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
   - Write the code yourself. Run the tests.
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
   - Verify each `FIX_NOW` finding against the code before fixing. Wrong -> do not fix; list it
     under Review Notes as "Disputed".
   - Fix `FIX_NOW` findings (resume the owning coder by `task_id` if one wrote the code).
     Re-review. Max 2 rounds. Leftover `FIX_NOW` -> Review Notes as "Not addressed".

5. **Commit**
   - **Multi-commit** = diff >150 lines or >3 files. Below that: one commit.
   - Multi-commit: one commit per unit, in reading order:
     (1) enabling refactors, behavior-preserving
     (2) core logic, one commit per component: the new class/function with its tests, then
         the integration into existing code with its tests
     (3) wiring: routes, config, callers
     (4) mechanical: generated files, bulk renames, docs, specs
   - Size: aim for <=200 changed lines per commit. Over 300: split along the boundaries
     above. Never split hunks. If one file alone exceeds it, leave it and say so in
     Where to start.
   - Each commit stands alone: the subject names one change; the body says why and any
     non-obvious decision. No commit should need a later one to make sense.
   - Split by file: `git add <files>` per commit. A file in two units goes in the earlier
     one. Keep both paths of a rename in one commit. Coder units -> one commit each.
     Tests must pass at HEAD; per-commit green is not required.
   - Fixes found after committing but before push: `git commit --fixup <sha>` then
     `git rebase -i --autosquash`. Never a separate "after review" commit.
   - main/master: show a summary, ask before committing. Do not push or open a PR/MR unless
     the user explicitly asks.

6. **Ship** (feature branch)
   - `git push -u origin <branch>`. Open the PR/MR with `--assignee @me`
     (GitLab only: `--reviewer @me --squash-before-merge --remove-source-branch`).
     Title: subject of the core commit. Body sections:
     - **Summary**: what and why
     - **Where to start**: Multi-commit only. Whole MR, not per commit. 3-4 lines:
       - `Start:` the test that states the contract, then the code it exercises, in order,
         with line ranges
       - `Then:` the next most important path, if any
       - `Skim:` behavior-preserving or mechanical files, one line
       - `Split:` only if `Start`/`Then` changes total >400 lines: which commits could be
         their own MR
     - **Changes**: files and what changed in each
     - **Review Notes**: omit if empty. One code block, one line per finding, sorted
       CRITICAL > WARNING > NIT:
       `[SEVERITY] file:line — finding. <Kept|Follow-up TICKET|Disputed|Not addressed>: one clause`
       Then `Out of scope:` one line per item noticed but not touched. No prose, no list of
       what was fixed.
   - Print the PR/MR URL. Multi-commit: also print **Where to start**.

   Example **Where to start**:
   ```
   Start: `InternalUserProfileServiceTest.java` — states the fallback contract. Then
     `InternalUserProfileService.java`, then the branch at `UserProfileService.java:46-54`.
   Then: `InternalLoginIT.java` nested `Profile` — "reflects directory changes" proves live LDAP.
   Skim: `LdapService.java` (Filter API, behavior-preserving), `InMemoryAdServer.java`, `openspec/**`.
   ```

   Example **Review Notes**:
   ```
   [WARNING] UserProfileService.java:49 — user with no sys_groupmembers row gets role null. Kept: pre-existing, design.md §9
   [WARNING] LdapConfig.java:81 — pool has no health check or retry. Follow-up: CP-123
   [NIT] LdapService.java:118 — InvalidParameterException as control flow. Disputed: WARN is intentional
   Out of scope: PUT /profile/me -> 500 for internal users
   ```
