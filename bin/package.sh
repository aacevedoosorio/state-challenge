#!/bin/sh

echo "Preparing to package solution"

# Check that we have a run.sh script
if [ ! -e "run.sh" ]; then
  echo "run.sh driver script not found"
  exit 1
fi

# Remove any previous submission.zip file
rm -f submission.zip

# Run the test cases
./bin/test.sh

if [ "$?" -ne "0" ]; then
  echo "Testing solution failed"
  exit 1
fi

echo ""
echo "Packaging assessment solution"

# Create the submission zip file
zip -r submission.zip *

echo "Created submission.zip submission file"
