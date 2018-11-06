#!/usr/local/Cellar/python/3.6.5/bin
# -*- coding: utf-8 -*-
import argparse
import os
import pandas as pd


def run(args_dict):
    # read in data
    emp = pd.read_csv(args_dict['empanel'], sep=None, engine='python')
    ids = pd.read_csv(args_dict['ids'], sep=None, engine='python')

    # clean up data
    ids.drop_duplicates(inplace=True)

    # manipulate data
    emp = emp[[x not in ids.qualtrics_id.tolist() for x in emp.ResponseID]]

    # write data to disk
    fpath = os.path.splitext(args_dict['empanel'])
    emp.to_csv('{}_filtered{}'.format(fpath[0], fpath[1]), index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Filter out previous '
                                     'completes before processing newly '
                                     'empaneled participants.')
    parser.add_argument('-e', '--empanel', required=True, help='Path/name to '
                        'new empanelment data.')
    parser.add_argument('-i', '--ids', required=True, help='IDs of people who '
                        'have played before.')
    args_dict = vars(parser.parse_args())

    run(args_dict)
