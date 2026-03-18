#!/bin/bash

# SAMPLE Data Test Runner - Comprehensive with Inventory Validation
# Tests all SAMPLE datasets with appropriate test selection AND validates inventory correctness

set -e

echo "=== VoLCA SAMPLE Test Suite ==="
echo "Comprehensive testing of all SAMPLE datasets"
echo "Using --no-cache to ensure fresh parsing and matrix assembly"
echo "Includes inventory correctness validation with expected results"
echo

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Set library path
export LD_LIBRARY_PATH="/home/dadafkas/projets/VoLCA/petsc/arch-linux-c-debug/lib:/home/dadafkas/projets/VoLCA/slepc/arch-linux-c-debug/lib:$LD_LIBRARY_PATH"

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

run_test() {
    local dataset="$1"
    local command="$2"
    local description="$3"
    local expect_warnings="${4:-false}"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "${BLUE}[$TOTAL_TESTS]${NC} $description"
    echo "    ./run.sh --data $dataset --no-cache $command"

    if timeout 25s ./run.sh --data "$dataset" --no-cache $command >/dev/null 2>&1; then
        echo -e "    ${GREEN}✓ PASSED${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "    ${RED}✗ FAILED${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    echo
}

# Test strategic new datasets with full coverage
echo -e "${YELLOW}=== New Strategic Datasets ===${NC}"

echo -e "${BLUE}SAMPLE.units${NC} - Unit Conversion (Critical for 1M scaling bug prevention)"
run_test "SAMPLE.units" "activity steel-prod-uuid_steel-uuid" "Mass conversions (t↔kg↔g)"
run_test "SAMPLE.units" "activity elec-gen-uuid_electricity-uuid" "Energy conversions (MJ↔kWh↔GJ)"
run_test "SAMPLE.units" "activity nuclear-power-uuid_nuclear-electricity-uuid" "Activity units (kBq - Radon-222 critical test)"
run_test "SAMPLE.units" "activity agriculture-uuid_wheat-uuid" "Compound units (kg/ha, MJ/kg)"
run_test "SAMPLE.units" "activity water-treat-uuid_cooling-water-uuid" "Volume/area units (m³↔l, ha)"
run_test "SAMPLE.units" "activity unknown-units-uuid_standard-product-uuid" "Unknown unit handling"

echo -e "${BLUE}SAMPLE.multiproduct${NC} - Multi-Output Allocation"
run_test "SAMPLE.multiproduct" "activity 11111111-2222-3333-4444-555555555555_gasoline-uuid" "Joint production (refinery: 4 co-products)"
run_test "SAMPLE.multiproduct" "activity aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee_lumber-uuid" "Mixed units (m³ lumber + kg sawdust)"
run_test "SAMPLE.multiproduct" "activity ffffffff-aaaa-bbbb-cccc-dddddddddddd_main-chemical-uuid" "Cut-off strategy (zero co-products)"

echo -e "${BLUE}SAMPLE.edge${NC} - Robustness & Error Handling"
run_test "SAMPLE.edge" "activity no-ref-uuid_product-a-uuid" "Missing reference product"
run_test "SAMPLE.edge" "activity zero-neg-uuid_normal-product-uuid" "Zero/negative amounts"
run_test "SAMPLE.edge" "activity invalid-refs-uuid_valid-product-uuid" "Invalid UUID references"
run_test "SAMPLE.edge" "activity xml-edge-uuid_xml-test-product-uuid" "XML parsing edge cases"

echo -e "${BLUE}SAMPLE.switching${NC} - Process Switching & Reference Product Tests"
run_test "SAMPLE.switching" "activity 22222222-3333-4444-5555-666666666661_chemical-b-uuid" "Chemical plant (Chemical B mode)"
run_test "SAMPLE.switching" "activity 33333333-4444-5555-6666-777777777772_gas-power-uuid" "Power plant (gas mode)"

# Test original samples (basic functionality)
echo -e "${YELLOW}=== Original Sample Datasets ===${NC}"

if [ -d "SAMPLE.min3" ]; then
    echo -e "${BLUE}SAMPLE.min3${NC} - Original working test data"
    run_test "SAMPLE.min3" "activity aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_productX-uuid" "Basic activity query"
fi

if [ -d "SAMPLE.min" ]; then
    echo -e "${BLUE}SAMPLE.min${NC} - Complex sample with loops"
    run_test "SAMPLE.min" "activity aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_productX-uuid" "Self-loop handling"
fi

# Summary
echo -e "${BLUE}=== RESULTS ===${NC}"
echo "Tests run: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $PASSED_TESTS${NC}"
if [ $FAILED_TESTS -gt 0 ]; then
    echo -e "${RED}Failed: $FAILED_TESTS${NC}"
fi

if [ $TOTAL_TESTS -gt 0 ]; then
    success_rate=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo "Success rate: $success_rate%"
fi

echo

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}🎉 ALL TESTS PASSED!${NC}"
    echo "Now running inventory correctness validation..."
    echo

    # Run inventory validation
    if source expected_results.sh && run_inventory_validation; then
        echo
        echo -e "${GREEN}✅ COMPLETE SUCCESS!${NC}"
        echo "VoLCA is functioning correctly across all test scenarios with correct inventory calculations."
        exit 0
    else
        echo
        echo -e "${RED}❌ INVENTORY VALIDATION FAILED!${NC}"
        echo "Basic tests passed but inventory calculations are incorrect."
        exit 1
    fi
else
    echo -e "${RED}⚠ $FAILED_TESTS tests failed.${NC}"
    echo "Check individual test outputs for debugging."
    exit 1
fi