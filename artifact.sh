#!/bin/bash

BUCKET="multiset-artifacts/oscilloscope"
PLATFORM=$(uname | awk '{print tolower($0)}')
aws s3 sync rel/releases/ s3://$BUCKET/$PLATFORM/releases
aws s3 sync rel/upgrades/ s3://$BUCKET/$PLATFORM/upgrades
