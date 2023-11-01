#--header "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiY2hhbl9hcmNoaXZlciJ9.rGIKZokTDKTuQLIv8138bUby5PELfDipYYIDpJzH02c" \
time curl \
    -v \
    -H "Content-Type: application/json" \
    -d '{ "max_time": "2023-10-26", "max_row_read": 1001 }' \
    -X POST http://localhost:3000/rpc/fetch_catalog
