#!/bin/bash


DAY_NAME=$1
DAY_NO=${DAY_NAME#day}
DAY_NO=$(( DAY_NO ))

mkdir -p {cmd,inputs}/${DAY_NAME}/
export DAY_NO
envsubst '$DAY_NO' <./main.go.tpl >cmd/$DAY_NAME/main.go
