# Make sure that you have all these libaries available to run the code successfully
from pandas_datareader import data
import matplotlib.pyplot as plt
import pandas as pd
import datetime as dt
import urllib.request, json
import os
import numpy as np
import tensorflow as tf # This code has been tested with TensorFlow 1.6
from sklearn.preprocessing import MinMaxScaler

# https://www.datacamp.com/community/tutorials/lstm-python-stock-market

def plot_litecoin(df:pandas.DataFrame) -> None:
    ax = plt.gca()
    df.plot(kind='line', x='DATE', y='CBLTCUSD')


df = pd.read_csv('../Tidsrekker/CBLTCUSD.csv',
                delimiter=',',
                usecols=['DATE','CBLTCUSD'],
                parse_dates=[0]
                )
# Sort out invalid price
df['CBLTCUSD'] = df['CBLTCUSD'][~(df['CBLTCUSD'] == '.' )].astype(float)
print("Sucessfully read and parsed data")

prices_matrix = df.loc[:,'CBLTCUSD'].as_matrix()
train_data = prices_matrix[:200]
test_data = prices_matrix[200:]
scaler = MinMaxScaler()
train_data = train_data.reshape(-1,1)
test_data = test_data.reshape(-1,1)

smoothing_window_size = 340 # Might have to change this


for di in range(0,10000,smoothing_window_size): # This probably wont run
    scaler.fit(train_data[di:di+smoothing_window_size,:])
    train_data[di:di+smoothing_window_size,:] = scaler.transform(train_data[di:di+smoothing_window_size,:])

# You normalize the last bit of remaining data
scaler.fit(train_data[di+smoothing_window_size:,:])
train_data[di+smoothing_window_size:,:] = scaler.transform(train_data[di+smoothing_window_size:,:])
