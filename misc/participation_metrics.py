#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
import pandas as pd


def create_output_tables(data, experiment):
    return pd.DataFrame({
        'started': [data.id.count()],
        'finished': [data.final_score.count()],
        'total_experiments': [len(set(data.experiment))],
        'experiment_nbr': [experiment]
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
            zip(FOLDERS, EXPERIMENTS)]

    # build output tables
    table = pd.concat([create_output_tables(d, exp) for d, exp in
                       zip(data, EXPERIMENTS)])

    # output table
    table.to_csv('misc/current_experiment_statistics.csv', index=False)


if __name__ == '__main__':
    FOLDERS = [
        'NGS2-Cycle1-Experiment1/data',
        'NGS2-Cycle1-Experiment2/data',
    ]
    EXPERIMENTS = [
        1,
        2,
    ]

    run()
