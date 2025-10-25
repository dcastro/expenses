#!/bin/bash

# -e: exit on error
# -u: error on undefined var
# -x: print command before execution
# -o pipefail: exit on command pipe failure
set -e
set -u
# set -x
set -o pipefail


# Docs:
#   * https://nordigen.com/en/account_information_documenation/integration/quickstart_guide/

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

REQS=$(curl --silent -H  "Content-Type: application/json" -H "Authorization: Bearer $TOKEN" -X GET "https://bankaccountdata.gocardless.com/api/v2/requisitions/" | jq '.results[].id' -r)
readarray -t REQS <<<"$REQS"

echo "Requisition IDs"
printf "%s\n\n" "${REQS[@]}"


echo "Deleting requisition IDs"
for req in "${REQS[@]}"
do
  echo "REQ: $req"
  curl --silent -H  "Content-Type: application/json" -H "Authorization: Bearer $TOKEN" -X DELETE "https://bankaccountdata.gocardless.com/api/v2/requisitions/$req/"
  echo ""
done
echo ""


echo "Creating requisition"
curl --silent -H  "Content-Type: application/json" -H "Authorization: Bearer $TOKEN" -X POST "https://bankaccountdata.gocardless.com/api/v2/requisitions/" -d \
  "{
    \"redirect\": \"http://www.google.com\",
    \"institution_id\": \"CETELEM_CETMPTP1XXX\",
    \"reference\": \"124152\",
    \"user_language\":\"EN\" }" | jq .link -r
curl --silent -H  "Content-Type: application/json" -H "Authorization: Bearer $TOKEN" -X POST "https://bankaccountdata.gocardless.com/api/v2/requisitions/" -d \
  "{
    \"redirect\": \"http://www.google.com\",
    \"institution_id\": \"MILLENIUMBCP_BCOMPTPL\",
    \"reference\": \"124151\",
    \"user_language\":\"EN\" }" | jq .link -r
curl --silent -H  "Content-Type: application/json" -H "Authorization: Bearer $TOKEN" -X POST "https://bankaccountdata.gocardless.com/api/v2/requisitions/" -d \
  "{
    \"redirect\": \"http://www.google.com\",
    \"institution_id\": \"BANCOACTIVOBANK_ACTVPTPL\",
    \"reference\": \"124153\",
    \"user_language\":\"EN\" }" | jq .link -r
curl --silent -H  "Content-Type: application/json" -H "Authorization: Bearer $TOKEN" -X POST "https://bankaccountdata.gocardless.com/api/v2/requisitions/" -d \
  "{
    \"redirect\": \"http://www.google.com\",
    \"institution_id\": \"CAIXA_CENTRAL_DE_CREDITO_AGRICOLA_CCCMPTPL\",
    \"reference\": \"124154\",
    \"user_language\":\"EN\" }" | jq .link -r
echo ""


read -p "Authorize the requisitions and press any key to resume ..."


curl --silent -H  "Content-Type: application/json" -H "Authorization: Bearer $TOKEN" -X GET "https://bankaccountdata.gocardless.com/api/v2/requisitions/" | jq .results
