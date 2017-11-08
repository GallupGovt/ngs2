import datetime
import os
import random
import re
import string
import sys

import numpy as np
import pandas as pd

class Ledger(object):
    ledger_columns = {
        'Pilot': [
            'ExternalDataReference',
            'first_name',
            'receiver_email',
            'country',
            'empanelment',
            'partic_exp1',
            'score_exp1',
            'partic_exp2',
            'score_exp2',
            'value',
            'batch_id',
            'currency',
            'item_id',
            'processed_code',
        ],
        'Cycle1': [
            'ExternalDataReference',
            'first_name',
            'receiver_email',
            'country',
            'empanelment',
            'partic_exp1',
            'score_exp1',
            'partic_exp2',
            'score_exp2',
            'score_exp3',
            'value',
            'batch_id',
            'currency',
            'item_id',
            'processed_code',
        ],
    }
    ledget_columns_updated = {
        'Cycle1': [
            'ExternalDataReference',
            'first_name',
            'receiver_email',
            'country',
            'empanelment',
            'partic_exp1',
            'score_exp1',
            'partic_exp2',
            'score_exp2',
            'score_exp3',
            'partic_extra',
            'value',
            'batch_id',
            'currency',
            'item_id',
            'processed_code',
        ],
    }
    NUM_COLS = {
        'Pilot': [
            'score_exp1',
            'score_exp2',
            'partic_exp1',
            'partic_exp2',
            'empanelment',
        ],
        'Cycle1': [
            'score_exp1',
            'score_exp2',
            'score_exp3',
            'partic_exp1',
            'partic_exp2',
            'partic_exp3',
            'empanelment',
        ],
    }

    def __init__(self, args_dict, config):
        self.action = args_dict['action']
        self.currency = args_dict['convert_currency']
        self.cycle = args_dict['cycle']
        self.data = args_dict['data']

        for k, v in config.items():
            setattr(self, k, v)

        self.datapaths = [x for x in filter(os.path.isdir, os.listdir('.')) if
                         re.search(self.cycle, x)]

    def add_xwalk(self, data):
        # identify the right crosswalk
        if 'Pilot' in self.cycle:
            xwalk = self.crosswalks[self.cycle]
        else:
            xwalk = self.crosswalks[self.cycle][self.country]

        # load crosswalk data
        xwalk_df = pd.read_csv(xwalk, sep=None, engine='python')

        # filter for the right ids
        if self.cycle=='Pilot':
            xwalk_df.rename(columns={
                'USERID': 'bbid',
            }, inplace=True)
            data = data.merge(xwalk_df[['LastName', 'FirstName', 'bbid']],
                              left_on=['RecipientLastName', 'RecipientFirstName'],
                              right_on=['LastName', 'FirstName'],
                              how='left')
            data.bbid = data.bbid.apply(lambda x: re.sub('\\.0|nan', '', str(x)))
        else:
            xwalk_df['bbid'] = xwalk_df.ROUTER_URL.apply(lambda x: x.split('/')[-1])
            xwalk_df.rename(columns={
                'EMPLOYEE_KEY_VALUE': 'ExternalDataReference',
            }, inplace=True)
            data = data.merge(xwalk_df, on='ExternalDataReference', how='left')

        return data

    @staticmethod
    def calculate_exp3_score(x):
        game_start = datetime.datetime.strptime(
            x[x.event=='GameStart'].datetime.values[0], '%Y-%m-%d %H:%M:%S,%f'
        )

        finishers = x[x.event=='PlayerWaiting'][['datetime', 'data value']]
        finishers.datetime = finishers.datetime.apply(
            lambda x: datetime.datetime.strptime(x, '%Y-%m-%d %H:%M:%S,%f')
        )

        timings = [(x - game_start).seconds for x in finishers.datetime]
        finishers['score'] = [3000 - x*10 if x<=300 else 0 for x in timings]

        finishers.rename(columns={
            'data value': 'pid'
        }, inplace=True)

        return finishers[['pid', 'score']]

    @staticmethod
    def calculate_total(df):
        return df.filter(regex='^sco|par|emp').sum(axis=1)

    @staticmethod
    def convert_currency(row, ctry):
        if 'MA' in ctry:
            return row.filter(regex='^sco|par|emp') * 5
        elif 'PH' in ctry:
            return row.filter(regex='^sco|par|emp') * 25
        else:
            return row.filter(regex='^sco|par|emp')

    @staticmethod
    def convert_play(var, exp):
        if exp==1:
            return var*2.34 if ~np.isnan(var) else 0
        elif exp==2:
            return var*2.33 if ~np.isnan(var) else 0
        elif exp==3:
            return var*2.33 if ~np.isnan(var) else 0
        else:
            raise Exception('Your choice -- {} -- is not valid.'.format(exp))

    @staticmethod
    def convert_score(var, conv):
        return int(var)/float(conv) if ~np.isnan(var) else 0

    def create(self, emp, exp):
        COLS = [
            'ExternalDataReference',
            'bbid',
            'country',
            'RecipientFirstName',
            'RecipientEmail',
        ]

        # merge data
        res = emp[COLS].merge(exp, on='bbid', how='left')

        # calculate payment amounts
        res['empanelment'] = 2.33
        res.score_exp1 = res.score_exp1.apply(lambda x:
                                              self.convert_score(x, 1000))
        res.score_exp2 = res.score_exp2.apply(lambda x:
                                              self.convert_score(x, 1000))
        res.partic_exp1 = res.partic_exp1.apply(lambda x:
                                                self.convert_play(x, 1))
        res.partic_exp2 = res.partic_exp2.apply(lambda x:
                                                self.convert_play(x, 2))
        if 'Cycle1' in self.cycle:
            res.score_exp3 = res.score_exp3.apply(lambda x:
                                                  self.convert_score(x, 1000))
            res.partic_exp3 = res.partic_exp3.apply(lambda x:
                                                    self.convert_play(x, 3))

        # convert currencies
        if self.currency:
            res[self.NUM_COLS[self.cycle]] = (
                res.apply(lambda x: self.convert_currency(x, x.country), axis=1)
            )

        # calculate total payment
        res['total'] = self.calculate_total(res)

        return res

    @staticmethod
    def create_experiment_nbr(path):
        if re.search('.*Experiment1', path) or re.search('.*Overflow1', path):
            return 1
        elif re.search('.*Experiment2', path) or re.search('.*Overflow2', path):
            return 2
        elif re.search('.*Experiment3', path):
            return 3
        else:
            sys.exit('STOP! Unknown experiment type.')

    @staticmethod
    def currency_choice(var):
        return 'USD' if var in ['US', 'MA'] else 'PHP'

    def data_etl(self):
        # get main dataset, whether empanelment or ledger
        res = self.load_main_data()

        # get experiment results
        exp = self.load_experiment_data()

        return res, exp

    @staticmethod
    def determine_batch_parameters(df, size):
        nr = df.shape[0]
        n = size
        i = nr / size
        j = nr % size
        return nr, n, i, j

    def determine_country(self, x):
        if re.search('_MA_', x):
            self.country = 'MA'
            return 'MA'
        elif re.search('_PH_', x):
            self.country = 'PH'
            return 'PH'
        else:
            self.country = 'US'
            return 'US'

    def develop_transaction_ids(self, df):
        nr, n, i, j = self.determine_batch_parameters(df, 250)
        if j==0:
            batch_ids = self.gen_random_id(i)
        else:
            batch_ids = self.gen_random_id(i+1)
        all_batch_ids = []
        [all_batch_ids.append(np.repeat(x, s).tolist()) for s, x in
         zip(np.repeat(n, i).tolist() + [j], batch_ids)]
        all_batch_ids = [item for lst in all_batch_ids for item in lst]
        item_ids = ['{}_{}'.format(bi, x % nr % n) for bi, x in
                    zip(all_batch_ids, xrange(nr))]

        return all_batch_ids, item_ids

    def exp_etl(self, file):
        # load raw data
        tmp = pd.read_csv(file[1], sep=None, engine='python')

        # identify experiment number
        exp_nbr = file[1].split('/')[-1]
        try:
            exp_nbr = re.search('[1-2]\.[1-9]', exp_nbr).group(0)
        except AttributeError:
            exp_nbr = '3'

        # extract starters
        startids = tmp['data value'][
            (tmp.event=='clientLogIn') &
            (tmp['data name']=='clientId')].unique().tolist()

        # extract finishers
        if file[0]==1:
            if 'FinalScore' in tmp.event.unique().tolist():
                finalids = tmp[['data name', 'data value']][tmp.event=='FinalScore']
                finalids.rename(columns={
                    'data name': 'pid',
                    'data value': 'score'
                }, inplace=True)
        elif file[0]==2:
            if 'FinalScore' in tmp.event.unique().tolist():
                finalids_long = tmp[tmp.event=='FinalScore']
                finalids_wide = finalids_long.pivot(index='id', columns='data name', values='data value')
                finalids = finalids_wide[['pid', 'score']]
        else:
            finalids = self.calculate_exp3_score(tmp)

        # create data frame of starters/finishers
        if file[0]!=3:
            if 'finalids' in locals():
                res = pd.DataFrame({
                    'pid': startids,
                    'exp': file[0],
                }).merge(finalids, on='pid', how='left')
            else:
                res = pd.DataFrame({
                    'pid': startids,
                    'exp': file[0],
                    'score': 0,
                })
            res['partic'] = 1
        else:
            res = pd.DataFrame({
                'pid': startids,
                'exp': file[0],
            }).merge(finalids, on='pid', how='left')
            res['partic'] = 0

        return res

    def format_paypal_payout(self, df):
        batch_ids, item_ids = self.develop_transaction_ids(df)
        df['batch_id'] = batch_ids
        df['item_id'] = item_ids
        df['processed_code'] = ''
        df['currency'] = df.country.apply(lambda x: self.currency_choice(x))
        df.RecipientFirstName = df.RecipientFirstName.apply(lambda x: x.title())

        df.rename(columns={
            'RecipientFirstName': 'first_name',
            'RecipientEmail': 'receiver_email',
            'total': 'value',
        }, inplace=True)

        return df[self.ledger_columns[self.cycle]]

    @staticmethod
    def gen_random_id(n, size=6):
        chars = string.ascii_lowercase + string.digits
        return [''.join(random.choice(chars) for _ in xrange(size)) for _
                in xrange(n)]

    def load_experiment_data(self):
        # load experiments
        filedir = [(self.create_experiment_nbr(x),
                    os.listdir('{}/data'.format(x))) for x in self.datapaths]
        files = [(f[0], '{}/data/{}'.format(d, x)) for d, f in
                 zip(self.datapaths, filedir) for x in f[1]]

        # extract relevant start/finish information
        tmp = pd.concat([self.exp_etl(file) for file in files]).drop_duplicates()

        # reshape to wide
        tmp.score.fillna(0, inplace=True)
        tmp.score = tmp.score.apply(lambda x: int(x))
        tmp = tmp.pivot_table(index='pid', columns='exp', aggfunc='sum').reset_index()
        tmp.columns = ['{}_exp{}'.format(x[0], x[1]) for x in tmp.columns]
        tmp.rename(columns={
            'pid_exp': 'bbid',
        }, inplace=True)

        return tmp

    def load_main_data(self):
        # load data
        tmp = pd.read_csv(self.data, sep=None, engine='python')

        tmp = self.shape_data(tmp)
        tmp['country'] = (tmp
                          .ExternalDataReference
                          .apply(lambda x: self.determine_country(x)))
        return tmp

    def shape_data(self, data):
        # only valid completes
        data = data[
            (data.Finished==1) &
            (data.Status==0)
        ]
        try:
            edits = self.id_edits[self.cycle]
            data.ExternalDataReference = [
                edits[1] if x==edits[0] else y for x, y in
                zip(data.RecipientEmail, data.ExternalDataReference)
            ]
            data = data[data.ExternalDataReference.apply(lambda x: isinstance(x, str))]
        except KeyError:
            pass

        # filter based on cycle
        cut = self.cutoffs[self.cycle]
        data.EndDate = data.EndDate.apply(lambda x:
            datetime.datetime.strptime(x, '%Y-%m-%d %H:%M:%S'))
        data = data[(data.EndDate >= cut[0]) & (data.EndDate <= cut[1])]

        return data

    def update(self):
        # determine country
        self.country = self.data[-6:-4].upper()

        # load ledger data
        tmp = pd.read_csv(self.data, sep=None, engine='python')

        # load edits data
        edt = pd.read_csv(self.payment_edits[self.cycle], sep=None, engine='python')
        edt = edt.groupby('empanelment_id').count().reset_index()

        # update payment ledger
        tmp = tmp.merge(pd.DataFrame({
            'ExternalDataReference': edt.empanelment_id,
            'partic_extra': [x*2.33 for x in edt.experiment_nbr],
        }), on='ExternalDataReference', how='left')
        tmp.partic_extra.fillna(0, inplace=True)

        # update total value
        tmp.value = self.calculate_total(tmp)

        return tmp[self.ledget_columns_updated[self.cycle]]

