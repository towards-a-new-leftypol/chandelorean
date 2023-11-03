#--header "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiY2hhbl9hcmNoaXZlciJ9.rGIKZokTDKTuQLIv8138bUby5PELfDipYYIDpJzH02c" \
time curl \
    -v \
    -H "Content-Type: application/json" \
    -d '{ "search_text": "found this board lainchan" }' \
    -X POST http://localhost:3000/rpc/search_posts
