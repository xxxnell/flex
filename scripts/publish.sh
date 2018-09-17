#!/bin/bash

cmd=

if [[ $branch == "master" && $(cat version.sbt) =~ "-SNAPSHOT" ]]; then
  release_cmd="sbt ++$scala_version && sbt 'release with-defaults'"
  cmd="$release_cmd"
fi

microsite_release_cmd="sbt publishMicrosite"

if [ -n "$cmd" ]; then
  cmd="$cmd && $microsite_release_cmd"
else
  cmd="$microsite_release_cmd"
fi

echo $cmd
eval $cmd
