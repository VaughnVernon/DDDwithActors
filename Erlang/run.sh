#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

erlc *.erl

erl -noshell -s account_runner run1 -s init stop
