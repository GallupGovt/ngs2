#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import pandas as pd


def run(args_dict):
    # load data
    e = pd.read_csv(args_dict['eligible'], sep=None, engine='python')
    m = pd.read_csv(args_dict['master'], sep=None, engine='python')
    dnd = pd.read_excel(args_dict['dnd'])

    dnd.rename(columns={
        'EMPLOYEE_KEY_VALUE': 'ExternalDataReference',
    }, inplace=True)

    # merge data and drop ineligibles
    m = m.merge(e, on='ExternalDataReference', how='left')
    m = m[(m.eligibility!='ineligible') & (m.contact_ok!=0)]

    # merge in unsubscribes and drop
    m = m.merge(dnd, on='ExternalDataReference', how='left')
    m = m[m.EMAIL_CONTACT_APPROVED!='N']

    # write data to disk
    m.to_csv(args_dict['output'], index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Use experiment eligibles '
                                     'to determine flash mob distro.')
    parser.add_argument('-d', '--dnd', required=True, help='Path/name to CSV '
                        'with unsubscribes.')
    parser.add_argument('-e', '--eligible', required=True, help='Path/name '
                        'to CSV file of eligibles.')
    parser.add_argument('-m', '--master', required=True, help='Path/name to '
                        'CSV file of master list of empanlements.')
    parser.add_argument('-o', '--output', required=True, help='Path/name to '
                        'CSV file for results.')
    args_dict = vars(parser.parse_args())

    run(args_dict)
