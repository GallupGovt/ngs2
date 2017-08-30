#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import pandas as pd
import os


VARNAMES = [
    'ExternalDataReference',
    'signups',
    'experiments',
]


def create_experiment_counts(files):
    tmp = pd.DataFrame(
        {
            'bbid': [item for sublst in files for item in sublst],
        }
    )
    tmp = tmp.groupby('bbid').size().reset_index()
    tmp.rename(columns = {0: 'experiments'}, inplace=True)
    return tmp


def determine_eligibility(row):
    if (row.signups==2) & (row.experiments>0):
        return 'ineligible'
    else:
        return 'eligible'


def gather_data(directory):
    files = os.listdir(directory)
    res = []
    for file in files:
        if os.path.splitext(file)[-1]=='.csv':
            tmp = pd.read_csv('{}/{}'.format(directory, file), sep=None,
                              engine='python')
            bbid = (tmp['data value']
                    [(tmp.event=='clientLogIn') &
                     (tmp['data name']=='clientId')]
                    .unique()
                    .tolist()
            )
            res += bbid
    return res


def run(args_dict):
    # load raw data
    exp = [gather_data(directory) for directory in args_dict['experiment']]
    inv = pd.read_csv(args_dict['data'], sep=None, engine='python')
    xwalk = pd.read_csv(args_dict['crosswalk'], sep=None, engine='python')

    # process breadboard ids in crosswalk
    xwalk['bbid'] = xwalk.ROUTER_URL.apply(lambda x: x.split('/')[-1])

    # generate count of invites/experiments in which respondent participated
    exp_count = create_experiment_counts(exp)
    inv_count = inv.groupby('ExternalDataReference').size().reset_index()
    inv_count.rename(columns = {0: 'signups'}, inplace=True)

    # merge experiment data to crosswalk
    participation = xwalk[['EMPLOYEE_KEY_VALUE', 'bbid']].merge(exp_count,
                                                                on='bbid',
                                                                how='left')

    # merge invites and participation together
    status = inv_count.merge(participation, left_on='ExternalDataReference',
                             right_on='EMPLOYEE_KEY_VALUE', how='left')
    status = status[VARNAMES].fillna(0)

    # determine email eligibility
    status['eligibility'] = status.apply(lambda x: determine_eligibility(x),
                                         axis=1)

    # output file
    FILEOUT = os.path.splitext(args_dict['data'])
    status.to_csv('{}_updated{}'.format(FILEOUT[0], FILEOUT[1]), index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Identify eligibility for '
                                     'future experiment invites.')
    parser.add_argument('-d', '--data', required=True, help='Path/name of file '
                        'housing experiment invitation data.')
    parser.add_argument('-w', '--crosswalk', required=True, help='Path/name of '
                        'crosswalk file from Gallup/Breadboard IDs.')
    parser.add_argument('-x', '--experiment', required=True, nargs='*',
                        help='Name of directories housing experiment data.')
    args_dict = vars(parser.parse_args())

    run(args_dict)


# INSTRUCTIONS
# (signup, experiment)
# 1. Participant has completed no experiments and is not signed up for any
#       additional sessions = Eligible (0, 0)
# 2. Participant has completed no experiments and is signed up for one
#       additional session = Eligible (1, 0)
# 3. Participant has completed no experiments and is signed up for two
#       additional sessions (unlikely, but possible since we are doing manual
#       additions) = Ineligible, alert Dan (2, 0)
# 4. Participant has completed one experiment and is not signed up for any
#       additional sessions = Eligible (1, 1)
# 5. Participant has completed one experiment and is signed up for one
#       additional session = Ineligible (2, 1)
# 6. Participant has completed two experiments = Ineligible (2, 2)

# ELIGIBLE: [1] (0, 0), [2] (1, 0), [3] (2, 0), [4] (1, 1)
# INELIGIBLE: [5] (2, 1), [6] (2, 2)
