#!/bin/bash

# -e: exit on error
# -u: error on undefined var
# -x: print command before execution
# -o pipefail: exit on command pipe failure
set -e
set -u
# set -x
set -o pipefail

MOEY_ACCOUNT="229642fa-e480-4822-a482-00af893c2e6e"


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

curl --silent -H  "Content-Type: application/json" -H "Authorization: Bearer $TOKEN" \
  -X GET "https://bankaccountdata.gocardless.com/api/v2/accounts/$MOEY_ACCOUNT/balances/" | jq
