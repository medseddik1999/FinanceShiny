#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 16 18:45:04 2023

@author: midou
"""

import pandas as pd
from yahoo_fin import stock_info as si


# gather stock symbols from major US exchanges
df1 = pd.DataFrame( si.tickers_sp500() )
df2 = pd.DataFrame( si.tickers_nasdaq() )
df3 = pd.DataFrame( si.tickers_dow() )
df4 = pd.DataFrame( si.tickers_other() )

# convert DataFrame to list, then to sets
sym1 = set( symbol for symbol in df1[0].values.tolist() )
sym2 = set( symbol for symbol in df2[0].values.tolist() )
sym3 = set( symbol for symbol in df3[0].values.tolist() )
sym4 = set( symbol for symbol in df4[0].values.tolist() )

# join the 4 sets into one. Because it's a set, there will be no duplicate symbols
symbols = set.union( sym1, sym2, sym3, sym4 )

# Some stocks are 5 characters. Those stocks with the suffixes listed below are not of interest.
my_list = ['W', 'R', 'P', 'Q']
del_set = set()
sav_set = set()

for symbol in symbols:
    if (len( symbol ) > 4 and symbol[-1] in my_list) or len(symbol) == 0 :
        del_set.add( symbol )
    else:
        sav_set.add( symbol )

print( f'Removed {len( del_set )} unqualified stock symbols...' )
print( f'There are {len( sav_set )} qualified stock symbols...' )


p=pd.DataFrame(sav_set )  
p.to_csv('tickers_symbols.csv')  




import json
import pandas as pd
import requests


def get_exchange_data(key, exchange='NYSE'):
    """
    returns metadata for a specific exchange
    available: US, NASDAQ, OTCBB, PINK, BATS
    """
    endpoint = f"https://eodhistoricaldata.com/api/exchange-symbol-list/"
    endpoint += f"{exchange}?api_token={key}&fmt=json"
    print("Downloading data")
    call =requests.get(endpoint).text
    exchange_data = pd.DataFrame(json.loads(call))
    print("Completed")
    return exchange_data


df=get_exchange_data('6467481ee0f964.52328806')


def main():
    key = '6467481ee0f964.52328806'
    get_exchange_data(key)
    
    
    

if __name__ == '__main__':
     df=main()  
     df.to_csv('tickers_symbols.csv')








