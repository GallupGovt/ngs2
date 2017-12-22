#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import glob
import pandas as pd


def merge_data(c, d, var=['ExternalDataReference', 'id']):
    tmp = c.merge(d, on=var, how='left')
    return tmp[tmp.start!=1]


def process_data(file):
    tmp = pd.read_csv(file, sep=None, engine='python')
    return pd.DataFrame({
        'id': (tmp['data value']
               [(tmp.event=='clientLogIn') &
                (tmp['data name']=='clientId')]
               .unique()
               .tolist()),
        'start': 1,
    })


def run(args_dict):
    # load data
    data_files = glob.glob('{}/*.csv'.format(args_dict['data']))
    d = pd.concat([process_data(file) for file in data_files], axis=0)
    ct = [pd.read_csv(file, sep=None, engine='python') for file in
          args_dict['contacts']]

    # links ids
    ct[0]['id'] = ct[0].ROUTER_URL.apply(lambda x: x.split('/')[-1])
    d = d.merge(ct[0][['id', 'ExternalDataReference']], on='id', how='left')

    # merge gameplay with contacts
    ct = [merge_data(file, d, var) for file, var in zip(ct, ['id', 'ExternalDataReference'])]

    # write data to disk
    [file.to_csv(loc, index=False) for file, loc in zip(ct, args_dict['output'])]


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Process and create experiment '
                                     'email/sms lists for international samples '
                                     'after initial gameplay.')
    parser.add_argument('-c', '--contacts', required=True, nargs=2, help='Data '
                        'used for initial contact; `email` then `sms`.')
    parser.add_argument('-d', '--data', required=True, help='Directory '
                        'containing gameplay data.')
    parser.add_argument('-n', '--nation', required=True, choices=['MA', 'PH'],
                        help='Country to process.')
    parser.add_argument('-o', '--output', required=True, nargs=2, help='Where '
                        'data is to be written.')
    args_dict = vars(parser.parse_args())

    run(args_dict)
