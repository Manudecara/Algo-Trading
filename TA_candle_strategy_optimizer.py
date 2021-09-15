#General
from datetime import datetime
from collections import Counter
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import lines
import seaborn as sns 
from seaborn import displot
from seaborn import lineplot
import statsmodels.api as sm  
import talib as ta
from statsmodels.stats.proportion import *
from math import sqrt
import math
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
from multiprocessing import Process
from scipy.spatial.distance import pdist, squareform
from itertools import compress
from plotnine import *
import talib as ta

date = pd.date_range(start = '2020-01-1 00:00:00', end = '2021-06-10 00:00:00', freq='min')


                            ###   DATA PREFERENCE   ###


def chose_timeframe(timeframe):
    if (timeframe == 'daily'):
        df = pd.read_csv('/Users/manu/Quant Finance/OakTree & Lion/Binance_BTCUSDT_daily.csv')
        df = df[0:1270]
        df.info()
        df.index = df.date
        df.index = pd.to_datetime(df.index)
        df['returns'] = df.close.pct_change()
        df.info()
        return df
    elif (timeframe == 'hourly'):
        df = pd.read_csv('/Users/manu/Quant Finance/OakTree & Lion/Binance_BTCUSDT_hourly.csv')
        df = df[0:4806]
        df.info()
        df.index = df.date
        df.index = pd.to_datetime(df.index)
        df['returns'] = df.close.pct_change()
        df.info()
        return df
    elif (timeframe == 'minute'):
        df = pd.read_csv('/Users/manu/Quant Finance/OakTree & Lion/Binance_BTCUSDT_minute.csv')
        df = df[0:752458]
        df.info()
        df.index = df.date
        df.index = pd.to_datetime(df.index)
        df['returns'] = df.close.pct_change()
        return df
    
    
    
'''chose between = 'daily', 'hourly', 'minute'''
    
df = chose_timeframe(timeframe = 'hourly') 


def chose_dependant_variable(df, variable, window):
    if (variable == 'price'):
        df[variable] = df['close']
        df = df[['open', 'high', 'low', 'close', 'Volume USDT', 'tradecount', 'returns', 'price']]
        df.info()
        return df
    elif (variable == "volatility"):
        df[variable]= df['returns'].rolling(window).std()*(365**0.5)
        df = df[['open', 'high', 'low', 'close', 'Volume USDT', 'tradecount', 'returns', 'volatility']]
        df.info()
        return df
    elif (variable == "garman_klass"):
        def get_estimator(price_data, window=window, trading_periods=365, clean=True):
            log_hl = (price_data['high'] / price_data['low']).apply(np.log)
            log_co = (price_data['close'] / price_data['open']).apply(np.log)
            rs = 0.5 * log_hl**2 - (2*math.log(2)-1) * log_co**2
            def f(v):
                return (trading_periods * v.mean())**0.5
            
            result = rs.rolling(window=window, center=False).apply(func=f)
            if clean:
                return result.dropna()
            else:
                return result
        df[variable] = get_estimator(price_data=df, window=window, trading_periods=365, clean=True)     
        df = df[['open', 'high', 'low', 'close', 'Volume USDT', 'tradecount', 'returns', 'garman_klass']]
        df.info()
        return df
    elif (variable == 'conrads_vol'):
        df['pt1'] = df['open']-df['low']+df['high']-df['close']
        df['pt2'] = df['high']-df['open']+df['close']-df['low']
        df['daymovement'] = np.sign(df['close'] - df['open']) * (df[min('pt1','pt2')] + (df['high']-df['low']))
        df['conrads_vol'] = np.log(1+(df['daymovement']/df['open']))
        df = df[['open', 'high', 'low', 'close', 'Volume USDT', 'tradecount', 'returns', 'conrads_vol']]
        df.info()
        return df

'''chose between = 'price', 'volatility', 'garman_klass, conrads_vol'''


df = chose_dependant_variable(df = df, 
                              variable = 'price', 
                              window = 7)

   

def chose_look_ahead_period(data, variable):
    data['1_behind'] = data[variable].shift(-1)
    data['2_behind'] = data[variable].shift(-2)
    data['3_behind'] = data[variable].shift(-3)
    data['4_behind'] = data[variable].shift(-4)
    data['5_behind'] = data[variable].shift(-5)
    
    data['now'] = data[variable]
    
    data['1_ahead'] = data[variable].shift(1)
    data['2_ahead'] = data[variable].shift(2)
    data['3_ahead'] = data[variable].shift(3)
    data['4_ahead'] = data[variable].shift(4)
    data['5_ahead'] = data[variable].shift(5)
    data['6_ahead'] = data[variable].shift(6)
    data['7_ahead'] = data[variable].shift(7)
    data['8_ahead'] = data[variable].shift(8)
    data['9_ahead'] = data[variable].shift(9)
    data['10_ahead'] = data[variable].shift(10)
    data['11_ahead'] = data[variable].shift(11)
    data['12_ahead'] = data[variable].shift(12)
    data['13_ahead'] = data[variable].shift(13)
    data['14_ahead'] = data[variable].shift(14)
    data['15_ahead'] =  data[variable].shift(15)
    data['16_ahead'] = data[variable].shift(16)
    data['17_ahead'] = data[variable].shift(17)
    data['18_ahead'] = data[variable].shift(18)
    data['19_ahead'] = data[variable].shift(19)
    data['20_ahead'] = data[variable].shift(20)
    return data


'''chose between = 'price', 'volatility', 'garman_klass, conrads_vol'''

chose_look_ahead_period(data = df, 
                        variable = 'price')
 


all_period_list = ['5_behind','4_behind','3_behind','2_behind','1_behind','now','1_ahead','2_ahead','3_ahead','4_ahead','5_ahead','6_ahead','7_ahead','8_ahead','9_ahead','10_ahead', '11_ahead','12_ahead','13_ahead','14_ahead','15_ahead', '16_ahead', '17_ahead', '18_ahead', '19_ahead', '20_ahead']
ahead_period_list = ['now','1_ahead','2_ahead','3_ahead','4_ahead','5_ahead','6_ahead','7_ahead','8_ahead','9_ahead','10_ahead','11_ahead','12_ahead','13_ahead','14_ahead','15_ahead','16_ahead','17_ahead','18_ahead','19_ahead','20_ahead']
    
       
    
                            ###   CANDLES ADDITION   ###


# extract OHLC 
op = df['open']
hi = df['high']
lo = df['low']
cl = df['close']

candle_names = ta.get_function_groups()['Pattern Recognition']

# create columns for each pattern
for candle in candle_names:
    # below is same as;
    # df["CDL3LINESTRIKE"] = talib.CDL3LINESTRIKE(op, hi, lo, cl)
    df[candle] = getattr(ta, candle)(op, hi, lo, cl)
    
 
# drops candles with no pattern found
cols = df.select_dtypes([np.number]).columns
diff = df[cols].diff().abs().sum()
df = df.drop(diff[diff== 0].index, axis=1)    
    

candle_names = list(df.columns[34:])


#sort by bear and bull signals
bulls = df.iloc[:,0:34]     
bears = df.iloc[:,0:34]     
   
def sort_bulls(candle_name): 
    if (df[candle_name].values > 0).any():
        bulls[candle_name] = (df[candle_name] > 0)*1
        return bulls
    
[sort_bulls(candle_name= candle) for candle in candle_names]  

def sort_bears(candle_name): 
    if (df[candle_name].values < 0).any():
        bears[candle_name] = (df[candle_name] < 0)*1
        return bears
    
[sort_bears(candle_name= candle) for candle in candle_names]  

bull_candle_names = list(bulls.columns[34:])
bear_candle_names = list(bears.columns[34:])


column_length = len(df.columns)
df['avg_win'] = (((df.iloc[:,:column_length].pct_change()*100 > 0)*1).sum(axis=1)*100)/column_length






def function_1(df, candle_name):
    pattern_count = ((df[candle_name] != 0)*1).sum()
    avg_candle_distance = ((df[candle_name] == 0)*1).sum()/pattern_count
    d = {'name': [candle_name], 'pattern_count': [pattern_count], 
         'avg_candle_distance':[avg_candle_distance]}
    general_candle_stats = pd.DataFrame(data = d)
    return general_candle_stats
     

#Bulls
general_candle_stats = [function_1(df= bulls, candle_name= candle) for candle in bull_candle_names] 
general_candle_stats = pd.concat(general_candle_stats)

#Bears
general_candle_stats = [function_1(df= bears, candle_name= candle) for candle in bear_candle_names] 
general_candle_stats = pd.concat(general_candle_stats)


def function_2(df, candle_name):
    avg_candle_stats = pd.DataFrame()
    avg_candle_stats = [(df.loc[df[candle_name] != 0, day]) for day in ahead_period_list]
    avg_candle_stats = pd.concat(avg_candle_stats, axis=1)  
    avg_candle_stats = avg_candle_stats.T
    avg_candle_stats.reindex(sorted(avg_candle_stats), axis=1)
    column_length = len(avg_candle_stats.columns)
    avg_candle_stats['total_wins'] = ((avg_candle_stats.iloc[:,:column_length].pct_change()*100 > 0)*1).sum(axis=1)
    avg_candle_stats['total_losses'] = ((avg_candle_stats.iloc[:,:column_length].pct_change()*100 < 0)*1).sum(axis=1)
    avg_candle_stats['avg_win'] = (((avg_candle_stats.iloc[:,:column_length].pct_change()*100 > 0)*1).sum(axis=1)*100)/column_length
    avg_candle_stats['avg_loss'] = (((avg_candle_stats.iloc[:,:column_length].pct_change()*100 < 0)*1).sum(axis=1)*100)/column_length
    avg_candle_stats['avg_returns'] = (avg_candle_stats.iloc[:,:column_length].pct_change()*100).sum(axis=1)/column_length
    avg_candle_stats['avg_drawdown'] = ((avg_candle_stats.iloc[:,:column_length].pct_change()*100).cumsum() - (avg_candle_stats.iloc[:,:column_length].pct_change()*100).cummax()).sum(axis=1)/column_length
    avg_candle_stats['avg_distribution'] = avg_candle_stats['avg_returns'].values.reshape(-1, 1)
    plt.figure()
    sns.lineplot(data=avg_candle_stats["avg_returns"], legend='full').set_title(candle_name)
    plt.show()
    plt.figure()
    sns.lineplot(data=avg_candle_stats["avg_drawdown"], legend='full').set_title(candle_name)
    plt.show()
    plt.figure()
    p = sns.displot(data = avg_candle_stats['avg_distribution'], kind='kde')
    p.set_axis_labels(candle_name)
    plt.show(p)
    return avg_candle_stats

avg_candle_stats = [function_2(df= bulls, candle_name=candle) for candle in bull_candle_names] 


#Bulls
avg_candle_stats = [function_2(df= bulls, candle_name= candle) for candle in bull_candle_names]

#Bears
avg_candle_stats = [function_2(df= bears, candle_name= candle) for candle in bear_candle_names]


def function_3(df, candle_name):
    single_candle_stats = pd.DataFrame()
    single_candle_stats = [(df.loc[df[candle_name] != 0, day]) for day in ahead_period_list]
    single_candle_stats = pd.concat(single_candle_stats, axis=1)  
    single_candle_stats = single_candle_stats.T
    single_candle_stats.reindex(sorted(single_candle_stats), axis=1)
    column_length = len(single_candle_stats.columns)
    returns = (single_candle_stats.iloc[:,:column_length].pct_change()*100)
    returns.iloc[:1,:] = 0
    drawdown_1 = (single_candle_stats.iloc[:,:column_length].pct_change()*100).cumsum()
    drawdown_1.iloc[:1,:] = 0
    drawdown_2 = (single_candle_stats.iloc[:,:column_length].pct_change()*100).cummax()
    drawdown_2.iloc[:1,:] = 0
    drawdown = drawdown_1 - drawdown_2
    
    #Plots
    p_single_candle_stats = pd.DataFrame()
    p_single_candle_stats = [(df.loc[df[candle_name] != 0, day]) for day in all_period_list]
    p_single_candle_stats = pd.concat(p_single_candle_stats, axis=1)  
    p_single_candle_stats = p_single_candle_stats.T
    p_single_candle_stats.reindex(sorted(p_single_candle_stats), axis=1)
    p_column_length = len(p_single_candle_stats.columns)
    p_returns = (p_single_candle_stats.iloc[:,:p_column_length].pct_change()*100)
    p_returns.iloc[:1,:] = 0
    p_drawdown_1 = (p_single_candle_stats.iloc[:,:p_column_length].pct_change()*100).cumsum()
    p_drawdown_1.iloc[:1,:] = 0
    p_drawdown_2 = (p_single_candle_stats.iloc[:,:p_column_length].pct_change()*100).cummax()
    p_drawdown_2.iloc[:1,:] = 0
    p_drawdown = p_drawdown_1 - p_drawdown_2
    for i in range(len(p_single_candle_stats.columns)):
            x1 = list(p_single_candle_stats.iloc[0:6,i].index)
            y1 = list(p_single_candle_stats.iloc[0:6,i].values)
            x2 = list(p_single_candle_stats.iloc[5:26,i].index)
            y2 = list(p_single_candle_stats.iloc[5:26,i].values)
            fig = plt.figure()
            ax = fig.add_subplot(111)
            ax.set_title('Price')
            plt.plot(x1, y1, "r", label = candle_name)
            plt.plot(x2, y2, "b", label = str(p_single_candle_stats.columns[i]))
            plt.legend(loc="upper right")
            plt.show()
        
            x1 = list(p_returns.iloc[0:6,i].index)
            y1 = list(p_returns.iloc[0:6,i].values)
            x2 = list(p_returns.iloc[5:26,i].index)
            y2 = list(p_returns.iloc[5:26,i].values)
            fig = plt.figure()
            ax = fig.add_subplot(111)
            ax.set_title('Returns')
            plt.plot(x1, y1, "r", label = candle_name)
            plt.plot(x2, y2, "b", label = str(p_single_candle_stats.columns[i]))
            plt.legend(loc="upper right")
            plt.show()
            
            x1 = list(p_drawdown.iloc[0:6,i].index)
            y1 = list(p_drawdown.iloc[0:6,i].values)
            x2 = list(p_drawdown.iloc[5:26,i].index)
            y2 = list(p_drawdown.iloc[5:26,i].values)
            fig = plt.figure()
            ax = fig.add_subplot(111)
            ax.set_title('Drawdown')
            plt.plot(x1, y1, "r", label = candle_name)
            plt.plot(x2, y2, "b", label = str(p_single_candle_stats.columns[i]))
            plt.legend(loc="upper right")
            plt.show()
            
    return (single_candle_stats, returns, drawdown)


#Bulls
single_candle_stats = function_3(df= bulls, candle_name='CDLHARAMI')

#Bears
single_candle_stats = function_3(df= bears, candle_name='CDLHARAMI')





#Need to create the optimizer with preffered 'variable' - ask Conrad





def candle_optimizer(df, candle_name):
    indivdual_candle_stats = pd.DataFrame()
    indivdual_candle_stats = [(df.loc[df[candle_name] != 0, day]) for day in ahead_period_list]
    indivdual_candle_stats = pd.concat(indivdual_candle_stats, axis=1)  
    indivdual_candle_stats = indivdual_candle_stats.T
    return indivdual_candle_stats
indivdual_candle_stats = [candle_optimizer(df= bulls, candle_name=candle) for candle in bull_candle_names] 

#Optimize by profits
#Plot of avg movement + distribution

for i, n in zip(indivdual_candle_stats, bull_candle_names):
    i.reindex(sorted(i), axis=1)
    i['most_returns'] = i.sum(axis=1)/len(i.columns)
    amount = str(i["most_returns"].max())
    what_day = i["most_returns"].idxmax()
    print('For candle ' + n + ', period ' + what_day + ' is the best day to get out giving an average of ' + amount + ' points')
    plt.figure()
    sns.lineplot(data=i["most_returns"], legend='full').set_title(n)
    sns.displot(data=i["most_returns"], kind='kde')
    plt.show() 

#Optimize by drawdown
#Plot of avg drawdown

for i, n in zip(indivdual_candle_stats, bull_candle_names):
    i.reindex(sorted(i), axis=1)
    i = i.pct_change()
    i = i.cumsum()
    i = i-i.cummax()
    i['least_drawdown'] = i.sum(axis=1)
    amount = str(i['least_drawdown'].iloc[2:15].max())
    what_day = i["least_drawdown"].iloc[2:15].idxmax()
    print('For candle ' + n + ', period ' + what_day + ' has the least average drawdown with ' + amount)
    plt.figure()
    sns.lineplot(data=i["least_drawdown"], legend='full').set_title(n)
    plt.show() 