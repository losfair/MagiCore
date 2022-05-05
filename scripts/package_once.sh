#!/bin/bash

set -e
mkdir "$1"
cp "./socs/$1."* "./$1/"
cd "$1"
tar c . | xz -9 > "../$1.tar.xz"
