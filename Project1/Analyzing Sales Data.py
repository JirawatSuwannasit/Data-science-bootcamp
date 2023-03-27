# import data
import pandas as pd

df = pd.read_csv("sample-store.csv")

# preview top 5 rows
df.head()

# shape of dataframe
df.shape

# see data frame information using .info()
df.info()

# example of pd.to_datetime() function
pd.to_datetime(df['Order Date'].head(), format='%m/%d/%Y')

# TODO - convert order date and ship date to datetime in the original dataframe
date_column = df[['Order Date', 'Ship Date']].apply(pd.to_datetime, format='%m/%d/%Y')
date_column

# TODO - count nan in postal code column
df['Postal Code'].isna().value_counts().reset_index()

# TODO - filter rows with missing values
df[df['Postal Code'].isna()]

# TODO - Explore this dataset on your owns, ask your own questions # Find top 10 sales order
df.sort_values('Sales', ascending=False).head(10)

# TODO 01 - how many columns, rows in this dataset
df.shape

# TODO 02 - is there any missing values?, if there is, which colunm? how many nan values?
df.isna().sum()
#Found missing values in Postal Code column, 11 nan values

# TODO 03 - your friend ask for `California` data, filter it and export csv for him
california_data = df[df['State'] == 'California']
california_data

california_data.to_csv('california_data.csv')

# TODO 04 - your friend ask for all order data in `California` and `Texas` in 2017 (look at Order Date), send him csv file


df_2017 = df[date_column['Order Date'].dt.strftime('%Y') == '2017']
df_2017_cal_tex = df_2017[(df_2017['State'] == 'California') | (df_2017['State'] == 'Texas')]
df_2017_cal_tex

df_2017_cal_tex.to_csv('df_2017_cal_tex.csv')

# TODO 05 - how much total sales, average sales, and standard deviation of sales your company make in 2017
df_2017_cal_tex['Sales'].describe()

# TODO 06 - which Segment has the highest profit in 2018
df_2018 = df[date_column['Order Date'].dt.strftime('%Y') == '2018']
df_2018.groupby(['Segment'])['Profit'].agg(['sum'])\
.sort_values('sum', ascending=False).head(1)

# TODO 07 - which top 5 States have the least total sales between 15 April 2019 - 31 December 2019

df_15apr_31dec_2019 = df[(date_column['Order Date'] >= '2019-4-15') & (date_column['Order Date'] <= '2019-12-31')]
total_sales = df_15apr_31dec_2019.groupby(['State'])['Sales'].agg(['sum']).sort_values('sum', ascending=False).tail(5)
total_sales

# TODO 08 - what is the proportion of total sales (%) in West + Central in 2019 e.g. 25% 
df_2019 = df[date_column['Order Date'].dt.strftime('%Y') == '2019']

df_west_central_2019 = df_2019[(df_2019['Region'] == 'West') | (df_2019['Region'] == 'Central')]

total_sales_west_central_2019 = (df_west_central_2019['Sales'].sum() / df_2019['Sales'].sum()) * 100
total_sales_west_central_2019

# TODO 09 - find top 10 popular products in terms of number of orders vs. total sales during 2019-2020
df_2019_2020 = df[(date_column['Order Date'].dt.strftime('%Y') == '2019')\
                  | (date_column['Order Date'].dt.strftime('%Y') == '2020')]
import numpy as np
result2 = pd.pivot_table(
   df_2019_2020,
   index=['Product Name'],
   aggfunc={'Sales': np.sum, 'Product Name': len}
).rename(columns={'Product Name': 'count'})

result2.sort_values(by='count', ascending=False).head(10)

# TODO 10 - plot at least 2 plots, any plot you think interesting :)
total_sales_plot = total_sales.rename(columns={'sum': 'total sales'})
total_sales_plot['total sales'].plot(kind='bar', color=['salmon', 'orange', 'gold' , 'gold', 'gold'])

df_2018_sec = df_2018.groupby(['Segment'])['Profit'].agg(['sum']).sort_values('sum', ascending=False)
df_2018_sec_plot = df_2018_sec.rename(columns={'sum': 'total profit'})
df_2018_sec_plot['total profit'].plot(kind='pie', autopct='%1.0f%%' )df_2018_sec = df_2018.groupby(['Segment'])['Profit'].agg(['sum']).sort_values('sum', ascending=False)
df_2018_sec_plot = df_2018_sec.rename(columns={'sum': 'total profit'})
df_2018_sec_plot['total profit'].plot(kind='pie', autopct='%1.0f%%' )

# TODO Bonus - use np.where() to create new column in dataframe to help you answer your own questions
result2['new_column'] = np.where(result2['Sales']>300, True, False)
result2.sort_values(by='count', ascending=False)
