from os import read
import pandas as pd
import numpy as np
import tensorflow as tf # This code has been tested with TensorFlow 1.6
from sklearn.preprocessing import MinMaxScaler
import matplotlib.pyplot as plt
import seaborn as sns

def read_data():
    print("Reading data ...")
    df = pd.read_csv('../Tidsrekker/CBLTCUSD.csv',
                    delimiter=',',
                    usecols=['DATE','CBLTCUSD'],
                    parse_dates=[0]
                    )
    # Sort out invalid price
    df['CBLTCUSD'] = df['CBLTCUSD'][~(df['CBLTCUSD'] == '.' )].astype(float)
    print("Sucessfully read and parsed data")
    return df

def split_data(df):
    print("Splitting data ...")
    n = len(df)
    train_df = df[0:int(n*0.7)]
    val_df = df[int(n*0.7):int(n*0.9)]
    test_df = df[int(n*0.9):]
    num_features = df.shape[1]
    return train_df, val_df, test_df, num_features

def normalize(TR, V, TE):
    print("Normalizing ...")
    TR.loc[:,'CBLTCUSD']=(TR.loc[:,'CBLTCUSD']-TR.loc[:,'CBLTCUSD'].mean())/TR.loc[:,'CBLTCUSD'].std()
    V.loc[:,'CBLTCUSD']=(V.loc[:,'CBLTCUSD']-V.loc[:,'CBLTCUSD'].mean())/V.loc[:,'CBLTCUSD'].std()
    TE.loc[:,'CBLTCUSD']=(TE.loc[:,'CBLTCUSD']-TE.loc[:,'CBLTCUSD'].mean())/TE.loc[:,'CBLTCUSD'].std()

    return TR, V, TE

class WindowGenerator():
    def __init__(self, input_width, label_width, shift,
                train_df, val_df, test_df,
                label_columns=None):
        # Store the raw data.
        self.train_df = train_df
        self.val_df = val_df
        self.test_df = test_df

        # Work out the label column indices.
        self.label_columns = label_columns
        if label_columns is not None:
            self.label_columns_indices = {name: i for i, name in
                                        enumerate(label_columns)}
        self.column_indices = {name: i for i, name in
                                enumerate(train_df.columns)}

        # Work out the window parameters.
        self.input_width = input_width
        self.label_width = label_width
        self.shift = shift

        self.total_window_size = input_width + shift

        self.input_slice = slice(0, input_width)
        self.input_indices = np.arange(self.total_window_size)[self.input_slice]

        self.label_start = self.total_window_size - self.label_width
        self.labels_slice = slice(self.label_start, None)
        self.label_indices = np.arange(self.total_window_size)[self.labels_slice]

    def __repr__(self):
        return '\n'.join([
            f'Total window size: {self.total_window_size}',
            f'Input indices: {self.input_indices}',
            f'Label indices: {self.label_indices}',
            f'Label column name(s): {self.label_columns}'])
    
    def split_window(self, features):
        inputs = features[:, self.input_slice, :]
        labels = features[:, self.labels_slice, :]
        if self.label_columns is not None:
            labels = tf.stack(
                [labels[:, :, self.column_indices[name]] for name in self.label_columns],
                axis=-1)
                    
        # Slicing doesn't preserve static shape information, so set the shapes
        # manually. This way the `tf.data.Datasets` are easier to inspect.
        inputs.set_shape([None, self.input_width, None])
        labels.set_shape([None, self.label_width, None])

        return inputs, labels

def test_wingen():
    w1 = WindowGenerator(input_width=24, label_width=1, label_columns=['CBLTCUSD'])
    WindowGenerator.split_window = split_window


def main():
    df_full = read_data()
    train_df, val_df, test_df, num_features = split_data(df_full)
    train_df, val_df, test_df = normalize(train_df, val_df, test_df)

if __name__ == "__main__":
    main()

