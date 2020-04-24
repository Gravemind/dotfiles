#!/bin/bash

set -euo pipefail
set -x

github_name="Kitware/CMake"

tag="$(curl -s https://api.github.com/repos/$github_name/releases/latest |
	    sed -nE 's/.*"tag_name"\s*:\s*"(.*?)".*/\1/p')"

echo $tag

curl -o cmake-mode.el "https://raw.githubusercontent.com/$github_name/$tag/Auxiliary/cmake-mode.el"

echo OK
