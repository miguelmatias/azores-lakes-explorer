#!/bin/sh
# Publish this folder: GitHub (public) + shinyapps.io.
# Requires: gh auth login, and rsconnect::setAccountInfo(...) once.
set -e
cd "$(dirname "$0")"
export PATH="$HOME/.local/bin:$PATH"

if ! gh auth status >/dev/null 2>&1; then
  echo "Run: gh auth login --hostname github.com --git-protocol ssh --web"
  exit 1
fi

if ! git remote get-url origin >/dev/null 2>&1; then
  gh repo create azores-lakes-explorer --public --source=. --remote=origin --push
else
  git push -u origin HEAD
fi

Rscript deploy.R
