#!/bin/bash
# Run all org-supertag ERT tests
#
# Usage:
#   ./test/run-tests.sh              # Run all tests
#   ./test/run-tests.sh extractor    # Run only extractor tests
#   ./test/run-tests.sh node         # Run only node-ops tests
#   ./test/run-tests.sh view         # Run only view-framework tests
#   ./test/run-tests.sh persist      # Run only persistence tests
#   ./test/run-tests.sh field-ref    # Run only node-reference field tests
#   ./test/run-tests.sh query        # Run only query-block tests

set -euo pipefail

cd "$(dirname "$0")/.."

# Check if emacs is available
if ! command -v emacs &> /dev/null; then
    echo "ERROR: Emacs not found in PATH. Tests require Emacs."
    exit 1
fi

EMACS_VERSION=$(emacs --version | head -1)
echo "Emacs: $EMACS_VERSION"
echo ""

# Define test modules (stable, passing tests only)
TEST_FILES=(
    "test/extractor-test.el"
    "test/node-ops-test.el"
    "test/view-framework-test.el"
    "test/formula-test.el"
    "test/reference-test.el"
    "test/virtual-column-test.el"
    "test/test-field-node-reference.el"
    "test/persistence-hardening-test.el"
    "test/query-block-test.el"
    "test/query-library-test.el"
    "test/cl-block-regression-test.el"
)

# Allow filtering by keyword
if [ $# -gt 0 ]; then
    FILTER=""
    for arg in "$@"; do
        case "$arg" in
            extractor) FILTER="$FILTER test/extractor-test.el" ;;
            node)      FILTER="$FILTER test/node-ops-test.el" ;;
            view)      FILTER="$FILTER test/view-framework-test.el" ;;
            formula)   FILTER="$FILTER test/formula-test.el" ;;
            aggregate) FILTER="$FILTER test/aggregate-test.el" ;;
            reference) FILTER="$FILTER test/reference-test.el" ;;
            vc|virtual) FILTER="$FILTER test/virtual-column-test.el" ;;
            field-ref) FILTER="$FILTER test/test-field-node-reference.el" ;;
            persist)   FILTER="$FILTER test/supertag-persistence-test.el test/persistence-hardening-test.el" ;;
            query)     FILTER="$FILTER test/query-block-test.el test/query-library-test.el" ;;
            cl-block)  FILTER="$FILTER test/cl-block-regression-test.el" ;;
            all)       FILTER="${TEST_FILES[*]}" ; break ;;
            *)         echo "Unknown filter: $arg"; echo "Available: extractor node view formula aggregate reference vc field-ref persist query cl-block all"; exit 1 ;;
        esac
    done
    TEST_FILES=($FILTER)
fi

# Build -l args
LOAD_ARGS=""
for tf in "${TEST_FILES[@]}"; do
    if [ -f "$tf" ]; then
        LOAD_ARGS="$LOAD_ARGS -l $tf"
    else
        echo "WARNING: Test file not found: $tf"
    fi
done

if [ -z "$LOAD_ARGS" ]; then
    echo "ERROR: No test files to run."
    exit 1
fi

echo "Running: ${TEST_FILES[*]}"
echo "================================"
echo ""

# Run tests (do NOT use set -e so we capture exit code)
set +e
emacs -batch \
    -L . \
    --eval "(package-initialize)" \
    $LOAD_ARGS \
    -f ert-run-tests-batch-and-exit 2>&1 | tee test/test-results.txt

EXIT_CODE=${PIPESTATUS[0]}
echo ""
echo "Results saved to test/test-results.txt"

if [ $EXIT_CODE -eq 0 ]; then
    echo "All tests passed."
else
    echo "Some tests FAILED (exit code: $EXIT_CODE)."
fi

exit $EXIT_CODE
