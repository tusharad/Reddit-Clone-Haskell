#!/bin/bash

# URL to test
URL="http://localhost:3000"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Counter for tests
passed=0
failed=0

# Function to check if string exists in response
check_element() {
    local description="$1"
    local pattern="$2"
    local response="$3"
    
    if echo "$response" | grep -q "$pattern"; then
        echo -e "${GREEN}PASS:${NC} $description"
        ((passed++))
    else
        echo -e "${RED}FAIL:${NC} $description"
        ((failed++))
    fi
}

# Get the response from the server
echo "Fetching response from $URL..."
response=$(curl -s "$URL")

if [ -z "$response" ]; then
    echo -e "${RED}ERROR:${NC} Could not connect to $URL or no response received"
    exit 1
fi

echo -e "\nRunning UI tests...\n"

# Test key UI elements
check_element "Header with HaskRead title present" "HaskRead" "$response"
check_element "Signup button present" "href='/register'" "$response"
check_element "Login button present" "href='/login'" "$response"
check_element "Search input present" "data-on-input='SearchTerm" "$response"
check_element "Sort menu with Top voted button" "Top voted" "$response"
check_element "Threads section title" "Threads" "$response"
check_element "Thread card for Haskell learning" "Best way to learn Haskell" "$response"
# check_element "Thread card for Fitness Goals" "Fitness Goals - Motivation and Advice" "$response"
check_element "Thread card for Functional Programming" "the point of functional programming?" "$response"
check_element "Communities section" "Communities" "$response"
# check_element "Haskell community link" "href='/?communityId=1'" "$response"
check_element "Footer with HaskRead" "<strong>HaskRead</strong>" "$response"
check_element "Tailwind CSS included" "https://cdn.tailwindcss.com" "$response"
check_element "Boxicons included" "boxicons" "$response"
check_element "Upvote button functionality" "UpdateUpVote" "$response"
check_element "Downvote button functionality" "UpdateDownVote" "$response"
check_element "Navigation buttons present" "HandleNext" "$response"

# Summary
total=$((passed + failed))
echo -e "\nTest Summary:"
echo -e "Total tests: $total"
echo -e "${GREEN}Passed: $passed${NC}"
echo -e "${RED}Failed: $failed${NC}"

if [ $failed -eq 0 ]; then
    echo -e "\n${GREEN}All tests passed successfully!${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed.${NC}"
    exit 1
fi
