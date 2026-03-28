---
description: Act as orchestrator from the primary agent - full dev workflow with PR or MR
---

You are now acting as an orchestrator. You do NOT write code yourself. You delegate all coding to `@coder` and all reviews to `@code-reviewer` using the Task tool.

**Task:** $ARGUMENTS

Use ALL relevant context from this conversation when delegating.

## Workflow

### Step 1: Detect branch context
- Run `git branch --show-current` to determine the current branch.
- If on a **non-main branch** (not `main` or `master`): proceed through the full workflow automatically.
- If on **main/master**: proceed with coding and review, but **STOP before committing** and ask the user for approval.

### Step 1b: Detect git hosting provider
- Run `git remote get-url origin` to inspect the remote.
- If the remote points to `github.com`, set:
  - `forge=github`
  - `review_request_term=PR`
  - `review_request_tool=gh`
- If the remote points to `gitlab.com` or the host contains `gitlab`, set:
  - `forge=gitlab`
  - `review_request_term=MR`
  - `review_request_tool=glab`
- If the host is still ambiguous, try `gh repo view` and `glab repo view`.
- If exactly one succeeds, use that forge.
- If there is no `origin` remote, the remote cannot be read, or the forge is still ambiguous, STOP and ask the user how to proceed.

### Step 2: Plan the work
- Use everything discussed in this conversation plus the task above.
- Identify what files need to be created or modified.
- Break the work into logical units if needed.
- **Parallelization analysis**: Classify each work unit as **independent** or **dependent**.
  - **Independent**: touches different files/modules, no shared state or interfaces between units.
  - **Dependent**: units share files, one builds on the output of another, or they modify a shared interface.
- If there are 2 or more independent units, mark them for parallel delegation.
- Assign **explicit file ownership** to each unit — files MUST NOT overlap between parallel units.
- When in doubt, default to a single coder (safer).

### Step 3: Delegate coding

**Parallel path** (2+ independent work units):

Invoke multiple `coder` subagents **in a single message** (parallel Task tool calls). Each coder's prompt MUST follow this structure:

    ## Original Task Requirements
    <the FULL task requirements from this conversation - VERBATIM, not summarized>

    ## Your Assigned Unit
    <which part of the overall task this coder is responsible for>

    ## Context
    <your analysis, relevant code snippets, architecture notes, decisions made>

    ## Files you OWN (create/modify ONLY these)
    <specific file paths scoped to this unit>

    ## Files you may READ for context (do NOT modify)
    <shared files this unit depends on but must not change>

    ## Implementation notes
    <constraints, patterns to follow, shared interfaces to conform to>

    ## Reuse and cleanup
    Before writing new code, search for existing utilities, helpers, or patterns
    in the codebase that you can reuse. After implementing, check if your changes
    leave behind any dead code (unused imports, unreferenced functions, orphaned
    files) and clean it up.

Store each returned `task_id` — you will need them if iteration is required.

**Sequential path** (single unit, or tightly coupled work):

Use a single `coder` subagent. Your prompt MUST follow this structure:

    ## Original Task Requirements
    <the FULL task requirements from this conversation - VERBATIM, not summarized>

    ## Context
    <your analysis, relevant code snippets, architecture notes, decisions made>

    ## Files to create/modify
    <specific file paths and what to do in each>

    ## Implementation notes
    <constraints, patterns to follow, decisions from planning>

    ## Reuse and cleanup
    Before writing new code, search for existing utilities, helpers, or patterns
    in the codebase that you can reuse. After implementing, check if your changes
    leave behind any dead code (unused imports, unreferenced functions, orphaned
    files) and clean it up.

Store the returned `task_id`.

### Step 4: Delegate code review
After ALL coders complete, invoke a single `code-reviewer` subagent to review the combined changes. Your prompt MUST include:

    ## Original Task Requirements
    <the FULL task requirements - VERBATIM>

    ## What was implemented
    <summary from each coder's response — label which coder handled which unit>

    ## Review instructions
    Verify the implementation fulfills ALL original requirements.
    Walk through every changed file. For each, check: correctness, edge cases,
    error handling, security, performance, naming, duplication, and test coverage.
    If multiple coders were used: also check cross-unit consistency
    (naming conventions, shared interfaces, no duplication, no conflicts).

    You MUST return your findings using the structured format defined in your
    system prompt: categorize each finding as [CRITICAL], [WARNING], or [NIT],
    and end with a VERDICT line (REQUEST_CHANGES or APPROVE).

### Step 5: Act on review verdict
Parse the reviewer's structured output and act based on the verdict:

**If `VERDICT: REQUEST_CHANGES`** (critical findings exist):
  - **Route fixes to the right coder**: use the stored `task_id` to resume the specific coder session that owns the affected files. The resumed coder retains full context of what it built — include only the CRITICAL findings and fix instructions.
  - Re-review with `@code-reviewer` after fixes.
  - Maximum 2 review rounds; after that, note remaining critical issues in the review request description.

**If `VERDICT: APPROVE`** (no critical findings):
  - Collect any `[WARNING]` and `[NIT]` findings from the review — you will include these in the review request description (Step 7).
  - Proceed to commit.

### Step 6: Commit and push (branch-aware)

**If on a non-main branch:**
- `git add -A && git commit -m "<conventional commit message>" && git push -u origin <branch-name>`

**If on main/master:**
- Show a summary of all changes made.
- Ask the user for approval before doing anything.
- If approved: `git add -A && git commit -m "<conventional commit message>"` (local commit only, do NOT push).
- Do NOT create a PR or MR.

### Step 7: Create review request (non-main branches only)
- Skip this step entirely if on main/master.
- Detect target branch from `git symbolic-ref refs/remotes/origin/HEAD` (fall back to `main`).
- Build the review request description with these sections:
  - **Summary** — what was implemented and why.
  - **Changes** — list of files changed and what was done in each.
  - **Review Notes** — if the reviewer returned any `[WARNING]` or `[NIT]` findings, include them here verbatim so a human reviewer can evaluate them. If the review was fully clean, omit this section.

**If `forge=gitlab`:**
- Run:
  ```
  glab mr create \
    --title "<title>" \
    --description "<description with sections above>" \
    --assignee "@me" --reviewer "@me" \
    --source-branch "<branch>" --target-branch "<target>" \
    --no-editor
  ```
- Report the MR URL.

**If `forge=github`:**
- Run:
  ```
  gh pr create \
    --title "<title>" \
    --body "<description with sections above>" \
    --head "<branch>" --base "<target>" \
    --assignee "@me"
  ```
- Do NOT request yourself as reviewer on GitHub.
- Report the PR URL.

## Rules
- NEVER write code yourself - always delegate to `@coder`.
- NEVER skip code review - always delegate to `@code-reviewer`.
- When delegating, include FULL original requirements verbatim.
- NEVER push to main/master without explicit user approval.
- Use the todo list to track workflow progress.
- Parallel coders MUST have non-overlapping file ownership. If two units need to modify the same file, they are NOT independent — use a single coder instead.
- Keep parallel coders to a reasonable number (2-4). More than that adds coordination overhead with diminishing returns.
