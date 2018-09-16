#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import pandas as pd
import re


def run():
    # load data
    d = pd.read_csv(args_dict['file'], sep=None, engine='python')

    if args_dict['merge']:
        # load weblink data for merging
        m = pd.read_csv(args_dict['merge'], sep=None, engine='python')
        m.rename(columns={
            'External Data Reference': 'ExternalDataReference',
            'Link': 'link',
        }, inplace=True)

        # merge data
        d = d.merge(m[['ExternalDataReference', 'link']],
                    on='ExternalDataReference')

        # write data
        d.to_csv(args_dict['output'], index=False)
    else:
        # drop those with no phone number
        d = d[[not re.match('^999', str(x)) for x in d.WP12355]]

        # strip country code (country code is added later by default)
        d.WP12355 = [str(str(x)[2:]) for x in d.WP12355]

        # drop landlines (start with 02)
        d = d[[not re.match('^02', x) for x in d.WP12355]]

        # drop duplicate number
        d = d[~((d.WP5889==396) & (d.WP12355=='9556888667'))]

        # assert all numbers are of length 10 (valid for ph mobile numbers)
        assert all([len(x) for x in d.WP12355])

        # create unique identifier and email dummies
        d['Email'] = ['fake_{}@fakedomain.com'.format(i) for i in xrange(len(d.WP12355))]
        d['ExternalDataReference'] = ['PH_24oct2017_{}'.format(i) for i in
                                      xrange(len(d.WP12355))]
        d['FirstName'] = 'Unknown'
        d['LastName'] = 'Name'

        # rename phone number variable
        d.rename(columns={
            'WP12355': 'SMS_PHONE_CLEAN',
        }, inplace=True)

        # write data
        VARS = [
            'FirstName',
            'LastName',
            'Email',
            'ExternalDataReference',
            'SMS_PHONE_CLEAN',
        ]
        d[VARS].to_csv(args_dict['output'], index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Process recontacts from WP '
                                     'for use in SMS/email contact with NGS2.')
    parser.add_argument('-f', '--file', required=True, help='Path/name of file '
                        'to process.')
    parser.add_argument('-m', '--merge', required=False, help='Path/name of '
                        'file to merge weblinks with phone data.')
    parser.add_argument('-o', '--output', required=True, help='Path/name of '
                        'file to write cleaned data to.')

    args_dict = vars(parser.parse_args())

    run()
