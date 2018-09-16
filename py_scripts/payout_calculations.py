#!/usr/bin/python
# -*- coding: utf-8 -*-
import argparse
import ConfigParser
import datetime
import sys

import pandas as pd

from NGS2 import Ledger


def run(args_dict, config):
    # instantiate class
    pmt = Ledger(args_dict, config)

    # execute actions
    if 'create' in args_dict['action']:
        # load data
        emp, exp = pmt.data_etl()

        # crosswalk empanelment and breadboard data
        emp = pmt.add_xwalk(emp)

        # create payout ledger
        d = pmt.create(emp, exp)
        d = pmt.format_paypal_payout(d)
    elif 'update' in args_dict['action']:
        # update ledger for manual payment additions
        d = pmt.update()
    elif 'payout' in args_dict['action']:
        pass
    else:
        sys.exit('STOP! Something went seriously wrong. You shouldn\'t be seeing this.')

    # output ledger
    if 'Pilot' in pmt.cycle:
        d.to_csv(pmt.ledgers[pmt.cycle], index=False)
    else:
        d.to_csv(pmt.ledgers[pmt.cycle][pmt.country], index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Calculate individual payouts '
                                    'for World Lab experiments.')
    parser.add_argument('-a', '--action', required=True, choices=['create',
                        'payout', 'update'], help='Action to perform within '
                        'payout calculation.')
    parser.add_argument('-c', '--cycle', required=True, choices=['Pilot',
                        'Cycle1'], help='Determine cycle to process for '
                        'getting experiment results.')
    parser.add_argument('-d', '--data', required=True, help='The data input '
                        'needed, given the `action` argument.')
    parser.add_argument('-m', '--convert_currency', action='store_true',
                        help='Boolean; determine if currency converted from '
                        'USD or not.')
    args_dict = vars(parser.parse_args())

    cfg = ConfigParser.ConfigParser()
    cfg.read('config.cfg')
    config = {}
    for k, v in cfg.items('setup'):
        config[k] = eval(v)

    run(args_dict, config)
