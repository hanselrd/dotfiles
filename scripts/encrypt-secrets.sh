#!/bin/sh
set -xe

MASTER_KEY_NAME=master

for file in ./secrets/*; do
	if echo "$file" | grep -v -q '^.*\.age$'; then
		age -a -R ./keys/${MASTER_KEY_NAME}.age.pub "$file" >"$file.age"
	fi
done
