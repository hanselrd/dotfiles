#!/bin/sh
set -xe

MASTER_KEY_NAME=master

age-keygen -o ./keys/${MASTER_KEY_NAME}.age
age-keygen -y -o ./keys/${MASTER_KEY_NAME}.age.pub ./keys/${MASTER_KEY_NAME}.age
mkdir -p ~/.keys
cp ./keys/* ~/.keys/.
