#!/bin/sh

set -e

cd "$(readlink -f $(dirname "$0"))/.."

unset _JAVA_OPTIONS

clj -M -m sand.core ../../resources/2022/day14.txt
