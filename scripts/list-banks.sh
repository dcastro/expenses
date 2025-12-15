#!/bin/bash

# -e: exit on error
# -u: error on undefined var
# -x: print command before execution
# -o pipefail: exit on command pipe failure
set -e
set -u
# set -x
set -o pipefail

# https://developer.gocardless.com/bank-account-data/quick-start-guide/
# https://developer.gocardless.com/bank-account-data/bank-selection-ui/

LANG=C.UTF-8
TOKEN=$(
  curl --silent -X POST "https://bankaccountdata.gocardless.com/api/v2/token/new/" \
    -H 'accept: application/json' \
    -H 'Content-Type: application/json' \
    -d "{
          \"secret_id\": \"${EXPENSES_NORDIGEN_SECRET_ID}\",
          \"secret_key\": \"${EXPENSES_NORDIGEN_SECRET_KEY}\"
        }" | jq .access -r
)

echo "TOKEN=$TOKEN"
echo ""

curl -X GET "https://bankaccountdata.gocardless.com/api/v2/institutions/?country=pt" \
  -H  "accept: application/json" \
  -H  "Authorization: Bearer $TOKEN" \
  | jq .[].name \
  | fzf
