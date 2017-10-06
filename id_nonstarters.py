#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import pandas as pd


def gather_data(directory, experiment):
    files = os.listdir(directory)
    res = []
    for file in files:
        if os.path.splitext(file)[-1]=='.csv':
            tmp = pd.read_csv('{}/{}'.format(directory, file), sep=None,
                              engine='python')
            bbid = (tmp['data value']
                    [(tmp.event=='clientLogIn') &
                     (tmp['data name']=='clientId')]
                    .unique()
                    .tolist()
            )
            if experiment==1:
                bbstart = (tmp['data value']
                           [(tmp.event=='InitialScore') &
                           (tmp['data name']=='pid')]
                           .unique()
                           .tolist()
                )
            else:
                bbstart = (tmp['data value']
                           [(tmp.event=='ChooseGroup') &
                           (tmp['data name']=='pid')]
                           .unique()
                           .tolist()
                )
            res += list(set(bbid) - set(bbstart))
    return pd.DataFrame({
        'experiment': experiment,
        'id': res,
    })


def run():
    # gather ids
    ids = pd.concat(
        [gather_data(experiment[1], experiment[0]) for experiment in EXPERIMENTS],
        axis=0
    )

    # output ids
    ids.to_csv('misc/bb_ids_nonstart.csv', index=False)


if __name__ == '__main__':
    EXPERIMENTS = [
        (1, 'NGS2-Cycle1-Experiment1/data'),
        (2, 'NGS2-Cycle1-Experiment2/data'),
    ]

    run()
