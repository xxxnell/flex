#!/bin/bash

cmd=
source_release_cmd="sbt ++$scala_version && sbt 'release with-defaults'"
microsite_release_cmd="sbt publishMicrosite"

if [[ $branch == "master" && $(cat version.sbt) =~ "-SNAPSHOT" ]]; then
  cmd="$source_release_cmd && $microsite_release_cmd"
fi

echo $cmd
eval $cmd
