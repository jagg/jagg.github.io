+++
title = 'Git Workflow: Feature Branches and Clean History'
date = 2023-03-15T11:00:00-07:00
draft = false
tags = ['git', 'workflow', 'version-control']
+++

A clean Git history is essential for maintainable projects. Here's my approach to feature branches and keeping commits organized.

<!--more-->

## The Feature Branch Workflow

Instead of working directly on `main`, create dedicated branches for each feature:

```bash
# Create and switch to new branch
git checkout -b feature/user-authentication

# Work on your feature...
git add .
git commit -m "Add user login form"
git commit -m "Implement password validation"

# Push to remote
git push -u origin feature/user-authentication
```

## Interactive Rebase for Clean History

Before merging, clean up your commits using interactive rebase:

```bash
# Rebase last 3 commits
git rebase -i HEAD~3

# In the editor, you can:
# - squash commits together
# - reword commit messages
# - reorder commits
```

## Merge vs Rebase

Two strategies for integrating changes:

```bash
# Merge (preserves branch history)
git checkout main
git merge feature/user-authentication

# Rebase (linear history)
git checkout feature/user-authentication
git rebase main
git checkout main
git merge feature/user-authentication --ff-only
```

## Commit Message Convention

I follow this format for consistency:

```
type(scope): description

- feat: new feature
- fix: bug fix
- docs: documentation
- style: formatting
- refactor: code restructuring
- test: adding tests
```

Example: `feat(auth): add password reset functionality`

## Useful Aliases

Add these to your `.gitconfig` for faster workflows:

```bash
[alias]
    co = checkout
    br = branch
    st = status
    cm = commit -m
    lg = log --oneline --graph --decorate --all
```

A clean Git history makes debugging easier and code reviews more effective. Take the time to organize your commits - future you will thank you.