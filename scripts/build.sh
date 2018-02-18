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

if [[ $branch == "master" && $(cat version.sbt) =~ "-SNAPSHOT" ]]; then
  release_cmd="sbt ++$scala_version 'release with-defaults'"
  cmd="$release_cmd"
else
  no_release_cmd="sbt ++$scala_version clean coverage test coverageReport"
  cmd="$no_release_cmd"
fi

echo $cmd
eval $cmd
