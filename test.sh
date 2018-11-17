# Simple test script to diff .sea file with .out file
# Run: ./test.sh

RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m'

mkdir temp

for t in ./tests/*.sea;
do
  out=`stack exec sea -- $t`

  filename=$(basename -- "$t")
  extension="${filename##*.}"
  filename="${filename%.*}"

  echo "$out" > "temp/$filename.tmp"

  diff=`diff ./temp/$filename.tmp ./tests/$filename.out`

  if [[ $diff ]]
  then
    echo "${RED}$filename.sea failed${NC}"
    echo "${CYAN}output:${NC}"
    echo "$out"
    echo ""
    echo "${CYAN}diff:${NC}"
    echo "$diff"
  else
    echo "${GREEN}$filename.sea passed${NC}"
  fi
done

rm -r ./temp