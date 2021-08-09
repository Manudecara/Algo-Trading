#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 21 16:17:08 2021

@author: manu
"""

import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
# matplotlib.use('Agg')
import datetime

from finrl.config import config
from finrl.marketdata.yahoodownloader import YahooDownloader
from finrl.preprocessing.preprocessors import FeatureEngineer
from finrl.preprocessing.data import data_split
from finrl.env.env_stocktrading import StockTradingEnv
from finrl.model.models import DRLAgent
from finrl.trade.backtest import backtest_stats, backtest_plot, get_daily_return, get_baseline

from pprint import pprint

import sys
sys.path.append("../FinRL-Library")

import itertools



import os
if not os.path.exists("./" + config.DATA_SAVE_DIR):
    os.makedirs("./" + config.DATA_SAVE_DIR)
if not os.path.exists("./" + config.TRAINED_MODEL_DIR):
    os.makedirs("./" + config.TRAINED_MODEL_DIR)
if not os.path.exists("./" + config.TENSORBOARD_LOG_DIR):
    os.makedirs("./" + config.TENSORBOARD_LOG_DIR)
if not os.path.exists("./" + config.RESULTS_DIR):
    os.makedirs("./" + config.RESULTS_DIR)
    
    
    
config.START_DATE

config.END_DATE

print(config.DOW_30_TICKER)


                           ############# DOWNLOAD DATA #############
                           
                           
df = YahooDownloader(start_date = '2008-01-01',
                     end_date = '2021-01-01',
                     ticker_list = config.DOW_30_TICKER).fetch_data()    

df.shape
df.sort_values(['date','tic'],ignore_index=True).head()
df.info()

                    ############# PERFORM FEATURE ENGINEERING #############
                    
                    
fe = FeatureEngineer(use_technical_indicator=True,
                     tech_indicator_list = config.TECHNICAL_INDICATORS_LIST,
                     use_turbulence=True,
                     user_defined_feature = False)

processed = fe.preprocess_data(df)


list_ticker = processed["tic"].unique().tolist()
list_date = list(pd.date_range(processed['date'].min(),processed['date'].max()).astype(str))
combination = list(itertools.product(list_date,list_ticker))

processed_full = pd.DataFrame(combination,columns=["date","tic"]).merge(processed,on=["date","tic"],how="left")
processed_full = processed_full[processed_full['date'].isin(processed['date'])]
processed_full = processed_full.sort_values(['date','tic'])

processed_full = processed_full.fillna(0)

processed_full.sort_values(['date','tic'],ignore_index=True).head(10)

'''Train and Trade Split'''
train = data_split(processed_full, '2009-01-01','2019-01-01')
trade = data_split(processed_full, '2019-01-01','2021-01-01')
print(len(train))
print(len(trade))



                         ############# BUILD TRAINING ENVIRONMENT #############

'''Get Space and Dimensions'''
stock_dimension = len(train.tic.unique())
state_space = 1 + 2*stock_dimension + len(config.TECHNICAL_INDICATORS_LIST)*stock_dimension
print(f"Stock Dimension: {stock_dimension}, State Space: {state_space}")


'''Set Env Parameters'''
env_kwargs = {
    "hmax": 100, 
    "initial_amount": 1000000, 
    "buy_cost_pct": 0.001,
    "sell_cost_pct": 0.001,
    "state_space": state_space, 
    "stock_dim": stock_dimension, 
    "tech_indicator_list": config.TECHNICAL_INDICATORS_LIST, 
    "action_space": stock_dimension, 
    "reward_scaling": 1e-4
    
}

'''Create Train Env'''
e_train_gym = StockTradingEnv(df = train, **env_kwargs)
env_train, _ = e_train_gym.get_sb_env()



                           ############# BUILD AGENT #############


agent = DRLAgent(env = env_train)

'''Set Agent Parameters'''
A2C_params = {
    "n_steps": 2048,
    "ent_coef": 0.01,
    "learning_rate": 0.00025,
}

model_a2c = agent.get_model("a2c", model_kwargs = A2C_params)

'''Train Agent'''
trained_a2c = agent.train_model(model=model_a2c, 
                                tb_log_name='a2c',
                                total_timesteps=1000)



                        ############# BUILD TRADING ENV #############


'''Set Turbulence Threshold'''
data_turbulence = processed_full[(processed_full.date<'2019-01-01') & (processed_full.date>='2009-01-01')]
insample_turbulence = data_turbulence.drop_duplicates(subset=['date'])

turbulence_threshold = np.quantile(insample_turbulence.turbulence.values,1)
turbulence_threshold

'''Create Trade Env'''
e_trade_gym = StockTradingEnv(df = trade, turbulence_threshold = 380, **env_kwargs)


                            ############# TRADE #############

df_account_value, df_actions = DRLAgent.DRL_prediction(model= trained_a2c, environment= e_trade_gym)
df_account_value.shape
df_account_value.tail()




                            ############# BACKTEST #############


print("==============Get Backtest Results===========")
now = datetime.datetime.now().strftime('%Y%m%d-%Hh%M')

perf_stats_all = backtest_stats(account_value=df_account_value)
perf_stats_all = pd.DataFrame(perf_stats_all)


#baseline stats
print("==============Get Baseline Stats===========")
baseline_df = get_baseline(
        ticker="^DJI", 
        start = '2019-01-01',
        end = '2021-01-01')

stats = backtest_stats(baseline_df, value_col_name = 'close')


print("==============Compare to DJIA===========")
# S&P 500: ^GSPC
# Dow Jones Index: ^DJI
# NASDAQ 100: ^NDX
backtest_plot(df_account_value, 
             baseline_ticker = '^DJI', 
             baseline_start = '2019-01-01',
             baseline_end = '2021-01-01')

df_account_value.date

