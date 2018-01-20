#!/bin/sh

set -e

gutenberg build
touch public/.nojekyll
git add public/.
git commit -m "update build"
git subtree push --prefix public origin master
