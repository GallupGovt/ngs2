#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import numpy as np
import pandas as pd


VARORDER = [
    'experiment',
    'started',
    'finished',
    'total_experiments',
]
VARORDER_DETAIL = [
    'experiment',
    'condition',
    'started',
    'finished',
    'total_experiments',
    'experiment_nbrs',
]


def calculate_conditions(data, exp):
    if exp==1:
        cond = float(data['data value'][(data.event=='initParameters') &
                                        (data['data name']=='k')])
        if cond==0.0:
            return 'Static'
        elif cond==0.1:
            return 'Viscous'
        elif cond==0.3:
            return 'Fluid'
        elif cond==1.0:
            return 'Random'
        else:
            return 'Other'
    else:
        c1 = float(data['data value'][data['data name']=='M'])
        c2 = float(data['data value'][data['data name']=='sameGroupConnectivity'])
        c3 = float(data['data value'][data['data name']=='diffGroupConnectivity'])

        if c1==2.0 and c2>c3:
            return 'Biased-2'
        elif c1==4.0 and c2>c3:
            return 'Biased-4'
        elif c1==2.0 and c2==c3:
            return 'Unbiased-2'
        elif c1==4.0 and c2==c3:
            return 'Unbiased-4'
        else:
            return 'Other'


def create_output_tables(data, experiment, detail=False):
    def experiment_number(x):
        return len(set(x))

    def experiment_types(x):
        return ', '.join(list(set(x)))

    if detail:
        data['exp_num'] = data.experiment.apply(lambda x: x.split('_')[0])
        table = data.groupby('condition').agg({
            'id': 'count',
            'final_score': 'count',
            'experiment': experiment_number,
            'exp_num': experiment_types,
        }).reset_index()
        table.rename(columns={
            'id': 'started',
            'final_score': 'finished',
            'exp_num': 'experiment_nbrs',
            'experiment': 'total_experiments',
        }, inplace=True)
        table['experiment'] = experiment
        table.finished = table.apply(lambda x: np.nan if (x.experiment==3) &
                                     (x.finished==0) else x.finished, axis=1)
        return table
    else:
        table = pd.DataFrame({
            'started': [data.id.count()],
            'finished': [data.final_score.count()],
            'total_experiments': [len(set(data.experiment))],
            'experiment': [experiment],
        })
        table.finished = table.apply(lambda x: np.nan if (x.experiment==3) &
                                     (x.finished==0) else x.finished, axis=1)
        return table


def gather_data(directory, file, experiment):
    fileid = file.split('_')
    tmp = pd.read_csv('{}/{}'.format(directory, file), sep=None,
                      engine='python')
    starters = pd.DataFrame({
        'id': (tmp['data value']
               [(tmp.event=='clientLogIn') &
                (tmp['data name']=='clientId')]
               .unique()
               .tolist()),
    })
    if experiment in [1, 2]:
        finishers = tmp[tmp.event=='FinalScore']
        rounds = (tmp['data value']
                  [(tmp.event=='cooperationEvent') &
                   (tmp['data name']=='round')]
                  .unique()
                  .tolist()
        )
        rds = []
        for i in rounds:
            try:
                rds.append(int(i))
            except ValueError:
                pass
        if finishers.shape[0]>=8:
            if experiment==1:
                condition = calculate_conditions(tmp, experiment)
                if ('Other' in condition) | (max(rds)<2):
                    return pd.DataFrame()
                else:
                    finishers = finishers[['data name', 'data value']].rename(columns={
                        'data name': 'id',
                        'data value': 'final_score',
                    })
            else:
                condition = calculate_conditions(tmp, experiment)
                finishers = finishers.pivot(index='id', columns='data name',
                                            values='data value').reset_index()
                finishers = finishers[['pid', 'score']].rename(columns={
                    'pid': 'id',
                    'score': 'final_score',
                })
            res = starters.merge(finishers, on='id', how='left')
            res['condition'] = condition
            res['experiment'] = '{}_{}'.format(fileid[0], fileid[1])

            return res
        else:
            return pd.DataFrame()
    else:
        starters['final_score'] = np.nan
        starters['condition'] = 'Coloring'
        starters['experiment'] = '{}_{}'.format(fileid[0], fileid[1])

        return starters


def process_directory(directory, experiment):
    files = os.listdir(directory)
    return pd.concat(
        [gather_data(directory, file, experiment) for file in files if
         os.path.splitext(file)[-1]=='.csv']
    )


def run():
    # load data
    data = [process_directory(directory, experiment) for directory, experiment in
            zip(RAW_DATA, EXPERIMENTS)]

    # build output tables
    table = pd.concat([create_output_tables(d, exp, False) for d, exp in
                      zip(data, EXPERIMENTS)])
    table_detail = pd.concat([create_output_tables(d, exp, True) for d, exp in
                             zip(data, EXPERIMENTS)])

    # output table
    table[VARORDER].to_csv('misc/current_experiment_statistics.csv', index=False)
    table_detail[VARORDER_DETAIL].to_csv('misc/current_experiment_statistics_detail.csv',
                                         index=False)


if __name__ == '__main__':
    RAW_DATA = [
        'NGS2-Cycle1-Experiment1/data',
        'NGS2-Cycle1-Experiment2/data',
        'NGS2-Cycle1-Experiment3/data',
    ]
    EXPERIMENTS = [
        1,
        2,
        3,
    ]

    run()
