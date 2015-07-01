#/bin/bash

TEMP_SUBMIT=/tmp/submission_trim.csv

grep -v Id $1 > $TEMP_SUBMIT
head -1 sampleSubmission.csv > $1
cat $TEMP_SUBMIT >> $1
