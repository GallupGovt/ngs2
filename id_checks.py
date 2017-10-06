#!/usr/bin/python
# -*- coding: utf-8 -*-
import pandas as pd


def run():
    d = [pd.read_csv(f, sep=None, engine='python') for f in FILES]
    d[0].rename(columns={
        'RANDOMIZED_ID': 'bbid',
        'EXPERIMENT_ID': 'experiment',
    }, inplace=True)
    d[0].experiment = d[0].experiment.apply(lambda x: 3 if x==5 else x)
    d[0]['db'] = 1
    d[1]['exp'] = 1

    dm = reduce(lambda x, y: pd.merge(x, y, on=['bbid', 'experiment'],
                                      how='outer'), d)
    dm.fillna(0, inplace=True)

    dm.experiment = dm.experiment.apply(lambda x: 5 if x==3 else x)

    dm[dm.exp==0][['bbid', 'experiment']].to_csv('misc/bb_ids_nonstarts.csv',
                                                 index=False)


if __name__ == '__main__':
    FILES = [
        'data/router_db_dump.csv',
        'misc/bb_ids_start.csv',
    ]

    run()
