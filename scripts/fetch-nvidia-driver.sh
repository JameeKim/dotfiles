#!/bin/sh
set -e

version=${1:?"NVIDIA driver version not provided as first argument"}
url="https://download.nvidia.com/XFree86/Linux-x86_64/${version}/NVIDIA-Linux-x86_64-${version}.run"

echo "Downloading NVIDIA driver from $url" 1>&2
nix store prefetch-file $url
