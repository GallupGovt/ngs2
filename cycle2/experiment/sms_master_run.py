#!/usr/local/Cellar/python/3.6.5/bin
# -*- coding: utf-8 -*-
import argparse
import os
import pandas as pd
import re

from datetime import datetime

from ngs2.cycle2.experiment.daily_sms_lists import prep_data
from NGS2apis import qualtrics
from NGS2apis.messaging.sms import send_messages


def execute_programs(args_dict):
    today = datetime.strftime(datetime.today(), '%d%b%Y')

    # step 1: go get survey data from qualtrics
    api = qualtrics(os.getenv('QUALTRICS_KEY'))
    api.pull_survey(args_dict['dir'], ftype='json', survid=args_dict['id'][0])

    # step 2: read json of survey into python for processing
    d = api.data_to_df('{}/{}'.format(args_dict['dir'], args_dict['id'][1]),
                          encoding='utf-8')
    d = d[(d.Finished=='1') & (d.Status=='0')]
    d.to_csv('{}/sms_data_{}.csv'.format(args_dict['dir'], today), index=False,
             encoding='utf-8')

    # step 3: prep data and combine with phone numbers for nightly lists
    prep_data(args_dict)

    # step 4: send data via twilio
    send_messages(args_dict)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Pull together all pieces to '
                                     'send daily SMS for NGS2 cycle 2 '
                                     'experiments.')
    parser.add_argument('-c', '--content', required=True, help='Path/file to '
                        'txt with message content information.')
    parser.add_argument('-d', '--dir', required=True, help='The directory to '
                        'which all data are written.')
    parser.add_argument('-e', '--empanel', required=True, help='Path/name to '
                        'empanelment data.')
    parser.add_argument('-i', '--id', required=True, nargs=2, help='The '
                        'Qualtrics survey ID to pull.')
    parser.add_argument('-l', '--logdir', required=True, help='The directory '
                        'for Twilio logging capabilities.')
    parser.add_argument('-n', '--nightly', required=True, help='Path/name to '
                        'SMS survey data.')
    parser.add_argument('-r', '--error_check', action='store_true',
                        help='Indicate if phone numbers should be checked '
                        'against a known bad/stop list.')
    parser.add_argument('-s', '--session', required=True, nargs=2,
                        help='sessions of interest.')
    parser.add_argument('-u', '--url_link', action='store_true',
                    help='Indicates if a URL should be sent with the SMS.')
    args_dict = vars(parser.parse_args())

    execute_programs(args_dict)
