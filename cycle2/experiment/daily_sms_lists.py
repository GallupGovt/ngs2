#!/usr/local/Cellar/python/3.6.5/bin
# -*- coding: utf-8 -*-
import argparse
import os
import pandas as pd
import re


def prep_data(args_dict):
    # read in data
    d = pd.read_csv('{}/{}'.format(args_dict['dir'], args_dict['nightly']),
                    sep=None, engine='python')
    e = pd.read_csv(args_dict['empanel'], sep=None, engine='python')

    # filter to relevant day
    d = d[d.apply(lambda x: (x['Q1_{}'.format(args_dict['session'][0])]==1) or
                  (x['Q1_{}'.format(args_dict['session'][1])]==1), axis=1)]

    # merge in phone numbers and clean
    d.RecipientEmail = d.RecipientEmail.apply(lambda x: x.lower())
    e.RecipientEmail = e.RecipientEmail.apply(lambda x: x.lower())
    m = d[['RecipientFirstName', 'RecipientLastName', 'RecipientEmail']].merge(
        e[['RecipientEmail', 'Q40']], on='RecipientEmail'
    )
    m.Q40 = m.Q40.apply(lambda x: re.sub('-|\\(|\\)| |\\+1|^1|[a-zA-Z]|\\.|=', '', x) if
                        isinstance(x, str) else '')

    m.drop_duplicates(inplace=True)
    m = m[m.Q40!='']
    m.rename(columns={
        'Q40': 'SMS_PHONE_CLEAN',
        'RecipientEmail': 'ExternalDataReference'
    }, inplace=True)

    # write data to disk
    fpath = os.path.splitext(args_dict['nightly'])
    m.to_csv('{}/{}_ready{}'.format(args_dict['dir'], fpath[0], fpath[1]),
             index=False)
