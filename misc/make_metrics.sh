#!/bin/bash

# switch to right directory
cd /Users/matt_hoover/git/NGS2

# identify the number of breadboard files
of=$(cat misc/nfiles)
e1=$(ls NGS2-Cycle1-Experiment1/data | wc -l)
e2=$(ls NGS2-Cycle1-Experiment2/data | wc -l)
nf=$(echo $e1 + $e2 | bc)

# check/run participation metrics script
if [ $of != $nf ]
    then
        # execute python program to calculate tallies
        python misc/participation_metrics.py

        # grab files and email them to others
        DATE=$(date '+%Y-%m-%d')
        BODY=$(echo "The latest participation numbers from $DATE attached. Email matt_hoover@gallup.com with questions.")
        (echo $BODY; uuencode misc/current_experiment_statistics_detail.csv current_experiment_statistics.csv) |
        mailx -s "NGS2 Metrics: Updated participation numbers" \
            matt_hoover@gallup.com \
            dan_foy@gallup.com \
            sofia_kluch@gallup.com \
            c_pablo_diego-rosell@gallup.com \
            anuradha_uduwage@gallup.com \
            mark.mcknight@yale.edu \
            zach_bikus@gallup.com

        # update the `nfile` number
        echo $nf > misc/nfiles
fi
