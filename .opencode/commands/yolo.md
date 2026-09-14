---
description: Implement, review, commit, push to current branch. No branch, no PR.
agent: build
---

Implement this task. Use all context from this conversation.

**Task:** $ARGUMENTS

1. List the files to change. Track steps in the todo list.
2. Implement it yourself. Prefer TDD. Run the tests.
3. Delegate review to `code-reviewer`. Verify each `FIX_NOW` finding against the code, then fix it.
   Max 2 rounds. Put `REPORT`, Disputed, and Not addressed findings in your final summary.
4. Conventional commit, `git push` to the current branch.
   On main/master: show a summary, ask before committing and pushing.
