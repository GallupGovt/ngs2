#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import numpy as np
import os
import pandas as pd
import random


def generate_groups(x):
    n = len(x) / 2
    tmp = random.sample(x, n)
    res = [tmp, list(set(x) - set(tmp))]

    return res


def determine_strat_level(d):
    num_length = d.WLE_SCRN_Q40_SMS_CON_NUM.apply(lambda x: len(str(x)))
    num_length = [True if (x==10) | (x==11) else False for x in num_length]
    contact = [True if x==1 else False for x in d.WLE_SCRN_Q39_SMS_CON_APP]

    return [True if x & y else False for x, y in zip(num_length, contact)]


def run(args_dict):
    random.seed(args_dict['seed'])

    # load data
    d = pd.read_csv(args_dict['data'], sep=None, engine='python')

    # generate ids per group
    if args_dict['level']:
        sms = determine_strat_level(d)
        groups = (d
                  .groupby(sms)
                  .ExternalDataReference
                  .apply(lambda x: generate_groups(x))
                  .reset_index()
        )
        groups = [x + y for x, y in zip(groups.ExternalDataReference[0],
                                        groups.ExternalDataReference[1])]
    else:
        groups = generate_groups(d.ExternalDataReference)

    # write files to disk
    FOUT = os.path.splitext(args_dict['output'])
    for i in xrange(2):
        if i==0:
            iname = 'control'
        else:
            iname = 'treatment'
        (d[d.ExternalDataReference.isin(groups[i])]
         .to_csv('{}_{}{}'.format(FOUT[0], iname, FOUT[1]), index=False))

    print('For this iteration, the random seed was: {}'.format(args_dict['seed']))
    # For this iteration, the random seed was: 0.29513627845 (26 oct 2017)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Randomize invitations to '
                                     'NGS2 flash mobs.')
    parser.add_argument('-d', '--data', required=True, help='Invitation list '
                        'to be randomly allocated to experimental conditions.')
    parser.add_argument('-l', '--level', action='store_true', help='Stratify '
                        'by SMS for randomization.')
    parser.add_argument('-o', '--output', required=True, help='Base path/name '
                        'of file to write results; note, will be multiple '
                        'files based on number of experimental conditions.')
    parser.add_argument('-s', '--seed', default=random.random(), type=float,
                        help='Set and record seed for reproducibility.')
    args_dict = vars(parser.parse_args())

    run(args_dict)
