#! /usr/bin/env sh

readonly limit="${1:-1M}"

sudo projmod -s -K "process.max-file-descriptor=(priv,$limit,deny)" default
