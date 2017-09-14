#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import pandas as pd


VARORDER = [
    'experiment_nbr',
    'started',
    'finished',
    'total_experiments',
]
VARORDER_DETAIL = [
    'experiment_nbr',
    'started',
    'finished',
    'total_experiments',
    'condition',
]


def create_output_tables(data, proc, experiment, detail=False):
    def experiment_number(x):
        return len(set(x))

    def modal_condition(x):
        return max(set(x))

    proc['experiment_nbr'] = proc.session.apply(lambda x: x.split('_')[0])
    condition = (proc
                 .groupby('experiment_nbr')
                 .agg({'condition': modal_condition})
                 .reset_index()
    )

    if detail:
        data['exp_num'] = data.experiment.apply(lambda x: x.split('_')[0])
        table = data.groupby('exp_num').agg({
            'id': 'count',
            'final_score': 'count',
            'experiment': experiment_number,
        }).reset_index()
        table.rename(columns={
            'id': 'started',
            'final_score': 'finished',
            'exp_num': 'experiment_nbr',
            'experiment': 'total_experiments',
        }, inplace=True)
        table = table.merge(condition, on='experiment_nbr')
        return table
    else:
        return pd.DataFrame({
            'started': [data.id.count()],
            'finished': [data.final_score.count()],
            'total_experiments': [len(set(data.experiment))],
            'experiment_nbr': [experiment],
        })


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
    finishers = tmp[tmp.event=='FinalScore']
    if experiment==1:
        finishers = finishers[['data name', 'data value']].rename(columns={
            'data name': 'id',
            'data value': 'final_score',
        })
    else:
        finishers = finishers.pivot(index='id', columns='data name',
                                    values='data value').reset_index()
        finishers = finishers[['pid', 'score']].rename(columns={
            'pid': 'id',
            'score': 'final_score',
        })
    res = starters.merge(finishers, on='id', how='left')
    res['experiment'] = '{}_{}'.format(fileid[0], fileid[1])

    return res


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
    proc = [pd.read_csv(file, sep=None, engine='python') for file in PROCESSED_DATA]


    # build output tables
    table = pd.concat([create_output_tables(d, p, exp, False) for
                      d, p, exp in zip(data, proc, EXPERIMENTS)])
    table_detail = pd.concat([create_output_tables(d, p, exp, True) for
                             d, p, exp in zip(data, proc, EXPERIMENTS)])

    # output table
    table[VARORDER].to_csv('misc/current_experiment_statistics.csv', index=False)
    table_detail[VARORDER_DETAIL].to_csv('misc/current_experiment_statistics_detail.csv',
                                         index=False)

if __name__ == '__main__':
    RAW_DATA = [
        'NGS2-Cycle1-Experiment1/data',
        'NGS2-Cycle1-Experiment2/data',
    ]
    PROCESSED_DATA = [
        'NGS2-Cycle1-Experiment1/cooperation_exp1.csv',
        'NGS2-Cycle1-Experiment2/cooperation_exp2.csv',
    ]
    EXPERIMENTS = [
        1,
        2,
    ]

    run()
