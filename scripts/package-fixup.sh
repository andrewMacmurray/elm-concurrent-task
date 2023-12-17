#! /bin/bash

# Adds package.json files to cjs/mjs subtrees

cat >lib/cjs/package.json <<!EOF
{
    "type": "commonjs"
}
!EOF

cat >lib/mjs/package.json <<!EOF
{
    "type": "module"
}
!EOF