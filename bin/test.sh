#!/bin/bash

function runSingleTest {
  local CURRENT_TEST=$1

  #trap 'publishTechnicalError "Error in ${BASH_SOURCE} at line ${LINENO}"' ERR

  TEST_INITIAL_INPUT=${CURRENT_TEST}-*-input.*
  TEST_EVENTS_INPUT=${CURRENT_TEST}-*-events.*
  EXPECTED_OUTPUT=${CURRENT_TEST}-*-output.*
  TEST_RESULT=/tmp/${CURRENT_TEST}-out.txt

  # Enable shell exit on error
  set -e

  echo "Running test ${CURRENT_TEST}"
  $COMMAND $TEST_INITIAL_INPUT $TEST_EVENTS_INPUT > $TEST_RESULT

  DIFF_COMMAND="diff -B"

  # Disable shell exit on error since diff command outputs exit code 1 on difference
  trap - ERR
  set +e

  DIFF=$($DIFF_COMMAND $EXPECTED_OUTPUT $TEST_RESULT 2>&1)
  DIFF_EXIT=$?

  # Standard diff exits with code 2 if there is a technical error
  if [ "$DIFF_EXIT" == "2" ]; then
    echo "Error in ${BASH_SOURCE} at line ${LINENO}"
    exit 1
  fi

  # Re-enable shell exit on error
  set -e

  # Check the results of the diff command
  if [ "$DIFF" != "" ]; then
    echo "Test ${CURRENT_TEST} failed:"
    echo "${DIFF}"
  else
    echo "Test ${CURRENT_TEST} succeeded."
  fi
}

### Main

# Check that we have a run.sh script
if [ ! -e "run.sh" ]; then
  echo "run.sh driver script not found"
  exit 1
fi

echo "Testing assessment solution"
echo ""

COMMAND="sh run.sh"

for T in `ls -1 *-input.* | cut -c 1`; do
  runSingleTest ${T}
done
