#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import pandas as pd


def convert_nbr(x):
    try:
        return int(x)
    except ValueError:
        return 0


def exp1_processing(data):
    tmp_wide = (data[data.event=='cooperationEvent']
                .pivot(index='id', columns='data name',
                       values='data value'))
    tmp_wide['round'] = tmp_wide['round'].apply(lambda x: convert_nbr(x))
    tmp_wide = tmp_wide[tmp_wide['round']>0]
    bbstart = tmp_wide.pid.unique().tolist()

    bbend = data[data.event=='FinalScore']['data name'].unique().tolist()

    return list(set(bbstart).union(bbend))


def exp2_processing(data):
    bbstart = (data['data value']
               [(data.event=='ChooseGroup') &
               (data['data name']=='pid')]
               .unique()
               .tolist()
    )

    bbend = (data['data value']
             [(data.event=='FinalScore') &
             (data['data name']=='pid')]
             .unique()
             .tolist()
    )

    return list(set(bbstart).union(bbend))


def gather_data(directory, experiment):
    files = os.listdir(directory)
    res = []
    for file in files:
        if os.path.splitext(file)[-1]=='.csv':
            tmp = pd.read_csv('{}/{}'.format(directory, file), sep=None,
                              engine='python')
            if experiment==1:
                ids = exp1_processing(tmp)
            elif experiment==2:
                ids = exp2_processing(tmp)
            else:
                if float(file.split('-')[1]) < 2:
                    ids = exp1_processing(tmp)
                else:
                    ids = exp2_processing(tmp)
            res += ids
    return pd.DataFrame({
        'experiment': experiment,
        'bbid': res,
    })


def run():
    # gather ids
    ids = pd.concat(
        [gather_data(experiment[1], experiment[0]) for experiment in EXPERIMENTS],
        axis=0
    )

    # output ids
    ids.to_csv('misc/bb_ids_start.csv', index=False)


if __name__ == '__main__':
    EXPERIMENTS = [
        (1, 'NGS2-Cycle1-Experiment1/data'),
        (2, 'NGS2-Cycle1-Experiment2/data'),
        (3, 'NGS2-Cycle1-Experiment3/data'),
    ]

    run()
