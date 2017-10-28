#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import pandas as pd
import re
import sys


def read_input_files(x, category=['data', 'treatment']):
    tmp = pd.read_csv(x, sep=None, engine='python')
    if category=='data':
        ftype = x.split('/')[-1].split('_')[0]
        tmp['experiment'] = ftype
    else:
        if re.search('control', x):
            ftype = 'control'
        elif re.search('treatment', x):
            ftype = 'treatment'
        else:
            sys.exit('STOP! {} is not a valid treatment/control file.'.format(x))
        tmp['treat_status'] = ftype

    return tmp


def gather_ids(x):
    return pd.DataFrame({
        'id': (x['data value']
               [(x.event=='clientLogIn') &
                (x['data name']=='clientId')]
               .unique()
               .tolist()),
        'experiment': x.experiment[0],
    })


def gather_exp_status(x):
    return pd.DataFrame({
        'id': [row.split('/')[-1] for row in x.ROUTER_URL],
        'treat_status': x.treat_status[0],
    })


def run(args_dict):
    # read in data
    d = [read_input_files(x, 'data') for x in args_dict['data']]
    s = [read_input_files(x, 'treatment') for x in args_dict['treatment']]

    # process ids from experiment
    ids = pd.concat([gather_ids(x) for x in d], axis=0)

    # process randomized lists
    stat = pd.concat([gather_exp_status(x) for x in s], axis=0)

    # merge
    res = ids.merge(stat, on='id')

    # write to disk
    res.to_csv(args_dict['output'], index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Identify post-hoc '
                                     'participation by treatment status.')
    parser.add_argument('-d', '--data', required=True, nargs='*',
                        help='Path/name to experiment results.')
    parser.add_argument('-t', '--treatment', required=True, nargs=2,
                        help='Path/name to treatment status lists.')
    parser.add_argument('-o', '--output', required=True, help='Path/name '
                        'for location to write data.')
    args_dict = vars(parser.parse_args())

    run(args_dict)
