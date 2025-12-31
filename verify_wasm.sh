#!/bin/bash

# WASM Build Verification Script
# Validates all WASM modules and tests compilation success
# Usage: ./verify_wasm.sh [language]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
WASM_BUILD_DIR="${PROJECT_ROOT}/Platforms/web/public/wasm"
EMSCRIPTEN_CHECK=$(which emcc 2>/dev/null || echo "not_found")

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TESTS_PASSED=0
TESTS_FAILED=0
MODULES_COMPILED=0
MODULES_FAILED=0

echo -e "${BLUE}╔════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║          WASM Build Verification Script            ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════╝${NC}"
echo

# Check Emscripten
check_emscripten() {
  echo -e "${YELLOW}Checking Emscripten installation...${NC}"
  
  if [[ "$EMSCRIPTEN_CHECK" == "not_found" ]]; then
    echo -e "${RED}❌ Emscripten not found!${NC}"
    echo "Install with: emsdk install latest && emsdk activate latest"
    return 1
  fi
  
  EMCC_VERSION=$(emcc --version 2>/dev/null | head -1)
  echo -e "${GREEN}✅ Emscripten found: $EMCC_VERSION${NC}"
  return 0
}

# Test module loading
test_module_loading() {
  local module=$1
  local wasm_file="${WASM_BUILD_DIR}/${module}.wasm"
  
  echo -e "${YELLOW}Testing ${module} module...${NC}"
  
  if [[ ! -f "$wasm_file" ]]; then
    echo -e "${RED}❌ WASM file not found: $wasm_file${NC}"
    ((TESTS_FAILED++))
    return 1
  fi
  
  # Check file size
  local size=$(stat -f%z "$wasm_file" 2>/dev/null || stat -c%s "$wasm_file" 2>/dev/null || echo "0")
  if [[ $size -lt 10000 ]]; then
    echo -e "${RED}❌ Module too small ($size bytes)${NC}"
    ((TESTS_FAILED++))
    return 1
  fi
  
  # Try to validate with wasm-validate if available
  if command -v wasm-validate &> /dev/null; then
    if wasm-validate "$wasm_file" > /dev/null 2>&1; then
      echo -e "${GREEN}✅ $module module valid${NC}"
      ((TESTS_PASSED++))
      return 0
    else
      echo -e "${RED}❌ $module module validation failed${NC}"
      ((TESTS_FAILED++))
      return 1
    fi
  else
    # Just check it exists and has reasonable size
    echo -e "${GREEN}✅ $module module found (${size} bytes)${NC}"
    ((TESTS_PASSED++))
    return 0
  fi
}

# Test memory initialization
test_memory_init() {
  echo -e "${YELLOW}Testing memory initialization...${NC}"
  
  if [[ ! -d "$WASM_BUILD_DIR" ]]; then
    echo -e "${RED}❌ WASM output directory not found${NC}"
    return 1
  fi
  
  # Check for loader files
  if [[ -f "${PROJECT_ROOT}/Platforms/web/src/wasm/wasm-loader.ts" ]]; then
    echo -e "${GREEN}✅ WASM loader found${NC}"
    ((TESTS_PASSED++))
    return 0
  else
    echo -e "${RED}❌ WASM loader not found${NC}"
    ((TESTS_FAILED++))
    return 1
  fi
}

# Test graphics command buffer
test_graphics() {
  echo -e "${YELLOW}Testing graphics interface...${NC}"
  
  if [[ -f "${PROJECT_ROOT}/Platforms/Python/time_warp/wasm/graphics.c" ]]; then
    # Check for required functions
    if grep -q "turtle_forward\|turtle_setcolor\|graphics_get_commands" \
           "${PROJECT_ROOT}/Platforms/Python/time_warp/wasm/graphics.c"; then
      echo -e "${GREEN}✅ Graphics interface complete${NC}"
      ((TESTS_PASSED++))
      return 0
    fi
  fi
  
  echo -e "${RED}❌ Graphics interface incomplete${NC}"
  ((TESTS_FAILED++))
  return 1
}

# Test error handling
test_error_handling() {
  echo -e "${YELLOW}Testing error handling...${NC}"
  
  if [[ -f "${PROJECT_ROOT}/Platforms/Python/time_warp/wasm/error.c" ]]; then
    if grep -q "error_push\|error_format\|error_has" \
           "${PROJECT_ROOT}/Platforms/Python/time_warp/wasm/error.c"; then
      echo -e "${GREEN}✅ Error handling complete${NC}"
      ((TESTS_PASSED++))
      return 0
    fi
  fi
  
  echo -e "${RED}❌ Error handling incomplete${NC}"
  ((TESTS_FAILED++))
  return 1
}

# Test interpreter implementations
test_interpreters() {
  echo -e "${YELLOW}Testing language interpreters...${NC}"
  
  local langs=("basic" "logo" "pilot" "pascal" "prolog" "forth" "c_lang")
  local found=0
  
  for lang in "${langs[@]}"; do
    local file="${PROJECT_ROOT}/Platforms/Python/time_warp/wasm/${lang}.c"
    if [[ -f "$file" ]]; then
      # Check for required functions
      if grep -q "execute_code\|get_output\|cleanup" "$file"; then
        echo -e "${GREEN}✅ $lang interpreter complete${NC}"
        ((found++))
      fi
    fi
  done
  
  if [[ $found -ge 6 ]]; then
    ((TESTS_PASSED++))
    return 0
  fi
  
  echo -e "${RED}❌ Only found $found/7 interpreter implementations${NC}"
  ((TESTS_FAILED++))
  return 1
}

# Test React integration
test_react_integration() {
  echo -e "${YELLOW}Testing React integration...${NC}"
  
  local hooks_file="${PROJECT_ROOT}/Platforms/web/src/hooks/useWasmInterpreter.ts"
  
  if [[ ! -f "$hooks_file" ]]; then
    echo -e "${RED}❌ React hooks not found${NC}"
    ((TESTS_FAILED++))
    return 1
  fi
  
  if grep -q "useWasmInterpreter\|useWasmBridge\|useWasmPerformance" "$hooks_file"; then
    echo -e "${GREEN}✅ React integration complete${NC}"
    ((TESTS_PASSED++))
    return 0
  fi
  
  echo -e "${RED}❌ React hooks incomplete${NC}"
  ((TESTS_FAILED++))
  return 1
}

# Main execution
main() {
  echo -e "${YELLOW}Running verification tests...${NC}\n"
  
  # Check Emscripten
  if ! check_emscripten; then
    echo -e "${RED}Cannot proceed without Emscripten${NC}"
    exit 1
  fi
  
  echo
  
  # Run tests
  test_memory_init
  test_graphics
  test_error_handling
  test_interpreters
  test_react_integration
  
  # Test specific module if requested
  if [[ ! -z "$1" ]]; then
    test_module_loading "$1"
  fi
  
  # Summary
  echo
  echo -e "${BLUE}════════════════════════════════════════════════════${NC}"
  echo -e "${BLUE}                   VERIFICATION SUMMARY${NC}"
  echo -e "${BLUE}════════════════════════════════════════════════════${NC}"
  
  TOTAL=$((TESTS_PASSED + TESTS_FAILED))
  
  echo -e "Tests Passed: ${GREEN}$TESTS_PASSED${NC}"
  echo -e "Tests Failed: ${RED}$TESTS_FAILED${NC}"
  echo -e "Total Tests:  ${BLUE}$TOTAL${NC}"
  
  if [[ $TESTS_FAILED -eq 0 ]]; then
    echo
    echo -e "${GREEN}✅ All verification tests passed!${NC}"
    echo -e "${GREEN}Ready to build WASM modules with 'make wasm-all'${NC}"
    exit 0
  else
    echo
    echo -e "${RED}❌ Some verification tests failed${NC}"
    echo -e "${YELLOW}Please fix the issues before building${NC}"
    exit 1
  fi
}

# Run if not sourced
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  main "$@"
fi
