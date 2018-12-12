#!/bin/bash

git remote set-branches origin '*'
git fetch -vvv
git remote -a

if [ -n "$GITHUB_ACCESS_TOKEN" ]; then
  echo "Changing origin url."
  githut_path=xxxnell/flex.git
  git remote set-url origin https://${GITHUB_ACCESS_TOKEN}@github.com/${githut_path}
else
  echo "GITHUB_ACCESS_TOKEN doesn't exist."
fi
