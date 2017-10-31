#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import pandas as pd
import os
import sys


VARNAMES = [
    'ExternalDataReference',
    'contact_ok',
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


def loose_eligibility(row):
    if row.experiments<=1:
        return 'eligible'
    else:
        return 'ineligible'


def strict_eligibility(row, ids):
    if row.ExternalDataReference in ids:
        if ((row.experiments==0) | ((row.signups==1) & (row.experiments==1))):
            return 'eligible'
        else:
            return 'ineligible'
    elif row.experiments<=1:
        return 'eligible'
    else:
        return 'ineligible'


def yesno(x):
    if 'N' in x.tolist():
        return 0
    else:
        return 1


def run(args_dict):
    # load raw data
    exp = [gather_data(directory) for directory in args_dict['experiment']]
    inv = pd.read_excel(args_dict['data'])
    xwalk = pd.read_csv(args_dict['crosswalk'], sep=None, engine='python')

    # rename key field in signup data
    inv.rename(columns={
        'EMPLOYEE_KEY_VALUE': 'ExternalDataReference',
    }, inplace=True)

    # id the current experiment
    curr_exp_ids = inv.ExternalDataReference[inv.Batch==max(inv.Batch)].tolist()


    # process breadboard ids in crosswalk
    xwalk['bbid'] = xwalk.ROUTER_URL.apply(lambda x: x.split('/')[-1])

    # generate count of invites/experiments in which respondent participated
    exp_count = create_experiment_counts(exp)
    inv_count = (inv
                 .groupby('ExternalDataReference')
                 .agg({'Batch': 'size', 'EMAIL_CONTACT_APPROVED': yesno})
                 .reset_index())
    inv_count.rename(columns={
        'Batch': 'signups',
        'EMAIL_CONTACT_APPROVED': 'contact_ok',
    }, inplace=True)

    # merge experiment data to crosswalk
    participation = xwalk[['EMPLOYEE_KEY_VALUE', 'bbid']].merge(exp_count,
                                                                on='bbid',
                                                                how='left')

    # merge invites and participation together
    status = inv_count.merge(participation, left_on='ExternalDataReference',
                             right_on='EMPLOYEE_KEY_VALUE', how='left')
    status = status[VARNAMES].fillna(0)

    # check for manual adjustments
    if args_dict['manuals']:
        man = pd.read_csv('misc/bb_ids_nonstarts.csv', sep=None, engine='python')
        man = man.merge(xwalk[['EMPLOYEE_KEY_VALUE', 'bbid']],
                        on='bbid', how='left')
        change_vals = pd.DataFrame({
            'ExternalDataReference': man.EMPLOYEE_KEY_VALUE,
            'reducer': 1,
        })
        status = status.merge(change_vals, on='ExternalDataReference', how='left')
        status.reducer.fillna(0, inplace=True)
        status.experiments = status.experiments - status.reducer
        status.drop('reducer', axis=1, inplace=True)

    # determine email eligibility
    if 'loose' in args_dict['criteria']:
        status['eligibility'] = status.apply(lambda x: loose_eligibility(x),
                                             axis=1)
    elif 'strict' in args_dict['criteria']:
        status['eligibility'] = status.apply(lambda x: strict_eligibility(x, curr_exp_ids),
                                             axis=1)
    else:
        sys.exit('PROBLEM! Not programmed for `{}`'.format(args_dict['criteria']))

    # output file
    FILEOUT = os.path.splitext(args_dict['data'])
    status.to_csv('{}_updated_{}.csv'.format(FILEOUT[0], args_dict['criteria']),
                  index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Identify eligibility for '
                                     'future experiment invites.')
    parser.add_argument('-c', '--criteria', required=True, choices=['loose',
                        'strict'], help='Identifies criteria to use in '
                        'selecting eligibility.')
    parser.add_argument('-d', '--data', required=True, help='Path/name of file '
                        'housing experiment invitation data.')
    parser.add_argument('-m', '--manuals', action='store_true', help='File '
                        'with IDs that will be adjusted for eligibility.')
    parser.add_argument('-w', '--crosswalk', required=True, help='Path/name of '
                        'crosswalk file from Gallup/Breadboard IDs.')
    parser.add_argument('-x', '--experiment', required=True, nargs='*',
                        help='Name of directories housing experiment data.')
    args_dict = vars(parser.parse_args())

    run(args_dict)
