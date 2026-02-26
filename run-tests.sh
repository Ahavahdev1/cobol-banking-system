#!/bin/sh
#================================================================
# VAULT-CBS Test Runner
# Compiles and runs all test suites, reports results
# Exit code = total number of test failures across all suites
#================================================================

COBC="cobc"
COBFLAGS="-x -free"
COPY_DIR="copybooks"
SRC_DIR="src"
TEST_DIR="tests"
BIN_DIR="bin"

mkdir -p "$BIN_DIR"

TOTAL_SUITES=0
TOTAL_FAILED=0
COMPILE_ERRORS=0

compile_and_run() {
    TEST_NAME="$1"
    shift
    SOURCES="$@"

    TOTAL_SUITES=$((TOTAL_SUITES + 1))
    TEST_FILE="$TEST_DIR/${TEST_NAME}.cob"
    TEST_BIN="$BIN_DIR/${TEST_NAME}"

    echo ""
    echo "--- Compiling ${TEST_NAME} ---"

    # Build compile command with all source files
    COMPILE_CMD="$COBC $COBFLAGS -I $COPY_DIR -o $TEST_BIN $TEST_FILE"
    for src in $SOURCES; do
        COMPILE_CMD="$COMPILE_CMD $SRC_DIR/${src}.cob"
    done

    if eval "$COMPILE_CMD" 2>&1; then
        echo "Compiled OK"
    else
        echo "COMPILE ERROR: ${TEST_NAME}"
        COMPILE_ERRORS=$((COMPILE_ERRORS + 1))
        return 0
    fi

    echo "--- Running ${TEST_NAME} ---"

    # Run the test; capture exit code (= failure count)
    "$TEST_BIN"
    RC=$?

    if [ "$RC" -eq 0 ]; then
        echo "SUITE PASSED: ${TEST_NAME} (0 failures)"
    else
        echo "SUITE FAILED: ${TEST_NAME} (${RC} failures)"
        TOTAL_FAILED=$((TOTAL_FAILED + RC))
    fi
}

echo "========================================"
echo " VAULT-CBS Test Suite Runner"
echo "========================================"

# Phase 1: Foundation Tests
echo ""
echo "=== PHASE 1: FOUNDATION ==="

compile_and_run "TEST-DATEUTIL" "DATEUTIL"
compile_and_run "TEST-GLPOST" "GLPOST0"
compile_and_run "TEST-CIFMGMT" "CIFMGMT"
compile_and_run "TEST-ACCTMGMT" "ACCTMGMT" "CIFMGMT"
compile_and_run "TEST-TXNPOST" "TXNPOST0"

# Phase 2: Core Banking Tests
echo ""
echo "=== PHASE 2: CORE BANKING ==="

compile_and_run "TEST-INTCALC" "INTCALC0"
compile_and_run "TEST-FEECALC" "FEECALC0"
compile_and_run "TEST-HOLDCALC" "HOLDCALC0" "DATEUTIL"
compile_and_run "TEST-ODMGMT" "ODMGMT0"
compile_and_run "TEST-BSAAML" "BSACTRO"
compile_and_run "TEST-REGD" "REGDCHK0"

# Phase 3: Payments + Integration Tests
echo ""
echo "=== PHASE 3: PAYMENTS + INTEGRATION ==="

compile_and_run "TEST-ACHRECV" "ACHRECV0"
compile_and_run "TEST-INTEG-EOD" "TXNPOST0" "INTCALC0" "GLPOST0" \
    "HOLDCALC0" "DATEUTIL" "ODMGMT0" "BSACTRO" "FEECALC0"

# Phase 4: Production Hardening Tests
echo ""
echo "=== PHASE 4: PRODUCTION HARDENING ==="

mkdir -p data
rm -f data/*.dat

compile_and_run "TEST-OVERFLOW" "TXNPOST0" "GLPOST0" "INTCALC0" "FEECALC0"
compile_and_run "TEST-HOLDDATE" "HOLDCALC0" "DATEUTIL"
compile_and_run "TEST-SAR" "BSACTRO"
compile_and_run "TEST-AUDTLOG" "AUDTLOG0"
compile_and_run "TEST-FILEIO" "FILEIO0"
compile_and_run "TEST-BATCH-EOD" "EODPROC0" "TXNPOST0" "INTCALC0" "GLPOST0" \
    "HOLDCALC0" "DATEUTIL" "AUDTLOG0" "FEECALC0"

# Summary
echo ""
echo "========================================"
echo " TEST SUMMARY"
echo "========================================"
echo " Suites run:     $TOTAL_SUITES"
echo " Compile errors: $COMPILE_ERRORS"
echo " Total failures: $TOTAL_FAILED"
echo "========================================"

if [ "$TOTAL_FAILED" -eq 0 ] && [ "$COMPILE_ERRORS" -eq 0 ]; then
    echo "ALL TESTS PASSED"
else
    echo "TESTS FAILED: ${TOTAL_FAILED} test failures, ${COMPILE_ERRORS} compile errors"
fi

exit $((TOTAL_FAILED + COMPILE_ERRORS))
