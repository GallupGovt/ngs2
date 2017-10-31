#!/bin/bash
# script to run and process input file for ngs2 flash mob. the input arrives
# via email two hours ahead of time and just needs to be saved to the right
# directory. the entire process will output a file to be uploaded to qualtrics
# for an email blast. another file of cleaned mobile numbers to send an sms
# reminder is also created.
# inputs:
#   $1: excel file from todd phillips that arrives two hours before experiment
#   $2: either `strict` or `loose` to indicate how to calculate eligibles
#   $3: name of the experiment, e.g., elegantElephant
#   $4: country being processed, currently either `PH` or `US`
#   $5: either `exp` or `noexp` to indicate if email list should be randomized
#   $6: name of directory for data storage
#   $7: complete flash mob data file name
#   $8: path name to project folder for unsubscribes on the server

# get abilty to access gallup servers
function logon {
    osascript -e 'mount volume "smb://noam.gallup.com/'$1'"'
}

# create some useful variables before starting the actual work
DATE=$(echo `date +%d%b%Y` | tr '[:upper:]' '[:lower:]')
IFS='.' read -ra FNAME <<< "$1"

# step 1: process excel signups
# this takes an excel worksheet of everyone that has signed up for experiments
# and determines who is/isn't eligible for experiments at the current time.
# the output is a csv.
python id_batch_eligibility.py \
    -x NGS2-Cycle1-Experiment1/data NGS2-Cycle1-Experiment2/data \
    -d data/$1 \
    -w $6/all_ids_24oct2017.csv \
    -c $2

# step 2: ensure `do not contacts` are purged from the current list
# this accesses the server and grabs the most up-to-date list of `do not
# contact` list to add in to processing.
if [ ! -d /Volumes/Clients ]; then
    logon Clients
fi
DND_DIR=$(ls "$8"/turn_off_respondent_email_sends_2* | tail -1)
cp "$DND_DIR" .
DND_FILE=`basename "$DND_DIR"`

# step 3: create the list of eligibles for the experiment
# this takes the csv output from step 1 and creates a csv that will be uploaded
# into qualtrics to be emailed out 30 minutes before experiment time.
python misc/prep_eligibles.py \
    -d $DND_FILE \
    -e data/${FNAME[0]}_updated_$2.csv \
    -m $6/$7 \
    -o $6/flash_mob_${DATE}_$3.csv
rm $DND_FILE

# step 4: determine if invites are to be randomized
# this uses command-line input to determine if the csv created in step 3 for
# the email blast through qualtrics should be randomized into two groups to run
# an incentive experiment.
if [ $5 == 'exp' ]
    then
        # step 4.1: randomize invites
        # this takes the csv from step 3 and randomly splits it into `control`
        # and `treatment` groups. these are then the two lists to go to
        # qualtrics for an email push.
        python misc/randomize_invites.py \
            -d $6/flash_mob_${DATE}_$3.csv \
            -o $6/flash_mob_${DATE}_$3.csv \
            -l

        # step 5: create the sms messages list
        # this takes the csv outputs from step 4.1 and creates csv's that will
        # be used to send sms messages via the twilio api. it drops records
        # that have opted-out of getting messages as well as cleans up or drops
        # phone number entries. it is used to push sms messages 15 minutes
        # before experiment time.
        cd $6
        python prep_flash_mob_sms_send.py \
            -i flash_mob_${DATE}_$3_control.csv \
            -o flash_mob_${DATE}_$3_control_clean.csv \
            -c $4
        python prep_flash_mob_sms_send.py \
            -i flash_mob_${DATE}_$3_treatment.csv \
            -o flash_mob_${DATE}_$3_treatment_clean.csv \
            -c $4
        mv \
            flash_mob_${DATE}_$3_control_clean.csv \
            flash_mob_${DATE}_$3_treatment_clean.csv \
            messages/distros/
    else
        # step 5: create the sms messages list
        # this takes the csv output from step 3 and creates a csv that will be
        # used to send sms messages via the twilio api. it drops records that
        # have opted-out of getting messages as well as cleans up or drops
        # phone number entries. it is used to push sms messages 15 minutes
        # before experiment time.
        cd $6
        python prep_flash_mob_sms_send.py \
            -i flash_mob_${DATE}_$3.csv \
            -o flash_mob_${DATE}_$3_clean.csv \
            -c $4
        mv flash_mob_${DATE}_$3_clean.csv messages/distros/
fi
