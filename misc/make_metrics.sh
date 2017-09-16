#!/bin/bash
# switch to right directory
cd /Users/matt_hoover/git/NGS2

# check to ensure on the right branch; this can go away once merged
repo=$(git symbolic-ref --short -q HEAD)
if [ $repo != 'batch-eligibility-identification' ]
    then
        git stash
        echo "NOTICE! `git stash`'d changes; be aware."

        git co batch-eligibility-identification
fi

# identify the number of breadboard files
of=$(cat misc/nfiles)
e1=$(ls NGS2-Cycle1-Experiment1/data | wc -l)
e2=$(ls NGS2-Cycle1-Experiment2/data | wc -l)
nf=$(echo $e1 + $e2 | bc)

# run participation metrics script
if [ $of != $nf ]
    then
        python misc/participation_metrics.py

    # grab files and email them to others
    DATE=$(date '+\%Y-\%m-\%d')
    printf "The latest participation numbers from $DATE attached. \
            Questions? Email matt_hoover@gallup.com." |
    uuencode misc/current* |
    mail -s "NGS2 Metrics: Updated participation numbers" \
         matt_hoover@gallup.com

    # update the `nfile` number
    echo $nf > misc/nfiles
fi

