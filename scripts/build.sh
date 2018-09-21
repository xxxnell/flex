#!/bin/bash

cmd=
scala_version=
branch=

# scala version

if [ -n "$TRAVIS_SCALA_VERSION" ]; then
  scala_version=$TRAVIS_SCALA_VERSION
else
  scala_version=$(scala -version 2>&1 | grep -oE "(\d+\.)+(\d)+")
fi

# branch

if [ -n "$TRAVIS_BRANCH" ]; then
  branch=$TRAVIS_BRANCH
else
  branch=$(git symbolic-ref HEAD | sed -e 's,.*/\(.*\),\1,')
fi

# cmd

no_release_cmd="sbt ++$scala_version && bash scripts/test.sh"
cmd="$no_release_cmd"

echo $cmd
eval $cmd
