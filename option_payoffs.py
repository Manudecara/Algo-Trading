#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 26 09:26:05 2021

@author: manu
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
plt.style.use('ggplot')

#### make a funcion that lets you specify a few parameters and calculates the payoff 

# S = underlying price
# K = strike price 
# Price = premium paid for option

def long_call(S, K, Price):
    # Long Call Payoff = max(underlying - Strike Price, 0) 
    # If we are long a call, we would only elect to call if the current price is greater than the strike price on our option     
    P = list(map(lambda x: max(x - K, 0) - Price, S))
    return P

def long_put(S, K, Price):
    # Long Put Payoff = max(Strike Price - underlying, 0)     
    # If we are long a call, we would only elect to call if the current price is less than the strike price on our option     
    P = list(map(lambda x: max(K - x, 0) - Price, S))
    return P
   
def short_call(S, K, Price):
    # Payoff a shortcall is just the inverse of the payoff of a long call     
    P = long_call(S, K, Price)
    return [-1.0*p for p in P]

def short_put(S,K, Price):
    # Payoff a short put is just the inverse of the payoff of a long put 
    P = long_put(S,K, Price)
    return [-1.0*p for p in P]

def bull_spread(S, E1, E2, Price1, Price2):
    
    P_1 = long_call(S, E1, Price1)
    P_2 = short_call(S, E2, Price2)
    return [x+y for x,y in zip(P_1, P_2)] 
     
def bear_spread(S, E1, E2, Price1, Price2):
    
    P = bull_spread(S,E1, E2, Price1, Price2)
    return [-1.0*p + 1.0 for p in P] 

def straddle(S, E, Price1, Price2):
    
    P_1 = long_call(S, E, Price1)
    P_2 = long_put(S, E, Price2)
    return [x+y for x,y in zip(P_1, P_2)]

def strangle(S, E1, E2, Price1, Price2):
    
    P_1 = long_call(S, E1, Price1)
    P_2 = long_put(S, E2, Price2)
    return [x + y for x, y in zip(P_1, P_2)]

    

S = [t/5 for t in range(0,1000)] # Define some series of prices 


fig, ax = plt.subplots(ncols= 2, nrows=2, sharex=True, sharey=True, figsize = (15,10))
fig.suptitle('Payoff Functions for Long/Short Put/Calls', fontsize=20, fontweight='bold')
fig.text(0.5, 0.04, 'Underlying Price ($)', ha='center', fontsize=14, fontweight='bold')
fig.text(0.08, 0.5, 'Option Payoff ($)', va='center', rotation='vertical', fontsize=14, fontweight='bold')

lc_P = long_call(S,100, 10)
plt.subplot(221)
plt.plot(S, lc_P, 'r')
plt.legend(["Long Call"])

lp_P = long_put(S,100, 10)
plt.subplot(222)
plt.plot(S, lp_P, 'b')
plt.legend(["Long Put"])

sc_P = short_call(S,100, 10)
plt.subplot(223)
plt.plot(S, sc_P, 'r')
plt.legend(["Short Call"])

sp_P = short_put(S,100, 10)
plt.subplot(224)
plt.plot(S, sp_P, 'b')
plt.legend(["Short Put"])

plt.show()



fig, ax = plt.subplots(nrows=3, sharex=True, sharey=True, figsize=(30, 20))
fig.suptitle('Payoff Functions for Long/Short Put/Calls', fontsize=20, fontweight='bold')
fig.text(0.5, 0.08, 'Underlying Price ($)', ha='center', fontsize=18, fontweight='bold')
fig.text(0.08, 0.5, 'Option Payoff ($)', va='center', rotation='vertical', fontsize=18, fontweight='bold')


plt.subplot(321)
P1 = bull_spread(S,50, 100, 15, 10)
long_c = long_call(S, 50, 15)
short_c = short_call(S, 100, 10)
plt.plot(S, P1, 'r')
plt.plot(S, long_c, 'r--')
plt.plot(S, short_c, 'b--')
plt.legend(["Bull Spread", "Long Call", "Short Call"])
plt.title("Bull Spread")

plt.subplot(323)
P = straddle(S,100, 10, 10)
P_longcall = long_call(S, 100, 10)
P_longput = long_put(S, 100, 10)
plt.plot(S, P) 
plt.plot(S, P_longcall, 'r--')
plt.plot(S, P_longput, 'b--')
plt.legend(["Straddle", "Long Call", "Long Put"])
plt.title("Straddle")

plt.subplot(325)
P = strangle(S,100, 75, 10, 10)
P_longcall = long_call(S, 100, 10)
P_longput = long_put(S, 75, 10)
plt.plot(S, P, 'g')
plt.plot(S, P_longcall, 'b--')
plt.plot(S, P_longput, 'r--')
plt.legend(["Strangle", "Long Short", "Long Put"])
plt.title("Strangle")

plt.show()