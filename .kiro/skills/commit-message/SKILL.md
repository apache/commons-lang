---
name: commit-message
description: ALWAYS use this skill when creating any commit. Never write a commit message without following these steps first.
---
ALWAYS follow these steps when writing a commit message:
1. Run `git diff main...HEAD` to see all changes on this branch
2. Write a description following this format:

## What
One sentence explaining what this commit does.

## Why
Brief context on why this change is needed

## Changes
- Bullet points of specific changes made
- Group related changes together
- Mention any files deleted or renamed
