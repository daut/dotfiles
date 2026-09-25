---
description: From main, create a worktree + branch, then run the ship flow there
agent: build
---

**Task:** $ARGUMENTS

You are on main in the main worktree. Do not work here. Stop and suggest `/ship` if you are on
another branch, in a linked worktree, or the tree is dirty.

Fetch origin. Create a branch from `origin/<main>` in a new worktree at `.worktrees/<slug>`.
Name it `<TICKET-ID>-<slug>` if there is a ticket, else `<type>/<slug>`. If git does not ignore
`.worktrees/`, add it to `.git/info/exclude`. Run project setup there if needed (deps, `.env`).

From now on, work only in the worktree. Use absolute paths under it for file tools and set
`workdir` to it for bash. Say the same in every subagent prompt. Then read
`~/.config/opencode/commands/ship.md` and follow it. When done, print the worktree path, the
PR/MR URL, and `git worktree remove .worktrees/<slug>`.
