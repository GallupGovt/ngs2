#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import pandas as pd


def run(args_dict):
    # load data
    e = pd.read_csv(args_dict['empanelment'], sep=None, engine='python')
    r = pd.read_csv(args_dict['router'], sep=None, engine='python')

    e.rename(columns={
        'RecipientFirstName': 'FirstName',
        'RecipientLastName': 'LastName',
        'RecipientEmail': 'Email',
    }, inplace=True)
    r.rename(columns={
        'url': 'ROUTER_URL',
    }, inplace=True)

    # merge data
    m = e.merge(r, on='ExternalDataReference')

    # output data
    m.to_csv(args_dict['output'], index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Put together empanelment '
                                     'and router data.')
    parser.add_argument('-e', '--empanelment', required=True, help='Empanelment '
                        'data path/name.')
    parser.add_argument('-r', '--router', required=True, help='Router data '
                        'path/name.')
    parser.add_argument('-o', '--output', required=True, help='Output data '
                        'path/name.')
    args_dict = vars(parser.parse_args())

    run(args_dict)
