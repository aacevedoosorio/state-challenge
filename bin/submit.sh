#!/bin/sh

SUBMISSION_ENDPOINT=https://api.techfu.io/v1

# Check whether we have assessment id
AT_GUID=$1
if [ "$AT_GUID" == "" ]; then
  echo "Usage: submit.sh [assessment id]"
  exit 1
fi

# Check whether required tools are available
if [ ! `command -v curl` ]; then
  echo "curl command is required for submitting your solution"
  exit 1
fi

echo "Preparing to submit solution to assessment ${AT_GUID}"
echo ""

../bin/package.sh

if [ "$?" -ne "0" ]; then
  echo "Packaging solution failed"
  exit 1
fi

SUBMISSION_SIZE=$(stat -f %z "submission.zip")
if [ "$SUBMISSION_SIZE" -gt "100000000" ]; then
  echo ""
  echo "Submission file size limit for submitting via this script is 10MB."
  echo "Please submit your solution via email."
  exit 1
fi

echo ""
echo "Submitting assessment $AT_GUID"

# Submit assessment
OUTPUT=`curl -q -H "Content-Type: application/octet-stream" --data-binary @submission.zip ${SUBMISSION_ENDPOINT}/assessment/${AT_GUID}/submit 2>&1`

if [ "$?" -ne "0" ]; then
  echo "Submitting solution failed: ${OUTPUT}"
  exit 1
fi

echo "Done."
