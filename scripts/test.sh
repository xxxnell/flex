#!/bin/bash

cmd="sbt clean scalafmtCheck scalafmtSbtCheck coverage test coverageReport"

echo $cmd
eval $cmd
