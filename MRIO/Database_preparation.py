# -*- coding: utf-8 -*-
"""
Created on Fri Jan 22 18:03:34 2021

@author: nigolred
"""
'''
This code is taking advantage of MARIO's ability to read hybrid version of EXIOBASE 3.3 with a user selection of its extensions. 
It also delivers a txt version of the database that can be more easily and quickly handled.
'''

import MARIO
import time
import pandas as pd

#%% Importing the original database downloaded from https://www.exiobase.eu/index.php/data-download/exiobase3hyb (usually takes 10 minutes)

# Enter the path of your local copy of hybrid EXIOBASE 3.3
read_from = r'C:\Users\Gollinucci\Desktop\Nicolò\Lavoro\FEEM\Databases\EXIOBASE_3.3.18_hsut_2011'
Original = MARIO.Database(name='Original MR_HIOT_2011_v3_3_18')

start_xlsb = time.time() # Compute the time for reading the original database
Original.parse_exiobase(read_from, exio_version='MR_HIOT_2011_v3_3_18', table='IOT', 
                     hybrid_ext_read=['resource_act','resource_FD','Land_act','Land_FD','Emiss_act','Emiss_FD'])

Original.calc_all(['f','F','w'])
delta_xlsb = time.time()-start_xlsb
print('Original database imported in {} seconds.'.format(delta_xlsb))

#%% Adding the mid-point extensions

# Reading and storing the information required to move from EXIOBASE extensions to mid-point extensions 
Ext = pd.read_excel(r'Inputs\Support information for MRIO\Extensions.xlsx', header=[0,1,2], index_col=[0]).droplevel(1, axis=1)
ext_main = list(Ext.index)
ext_add = dict(Ext.columns)

add = pd.DataFrame(0, index=ext_add.keys(), columns=Original.E.columns)

for i in list(ext_add.keys()):
    father_extensions = Ext.loc[:,i][Ext.loc[:,i]!=0].dropna()
    add_ = Original.E.loc[father_extensions.index]*father_extensions.values
    add.loc[i] = add_.sum()

# Adding them to the original table and re-calculating
Original.add_extensions(add, 'E', **ext_add)

#%% Get Excel aggregation file
Original.get_aggregation_excel(levels=['Region','Consumption category'], path=r'Aggregations\Database_Region_Renaming.xlsx')

#%% Renaming the regions name and condensing final demand categories
Original.aggregate(drop=True, levels=['Region','Consumption category'], path=r'Aggregations\Database_Region_Renaming.xlsx')
#%% Writing the database in txt

# Enter the path of where you want to store your local copy of hybrid EXIOBASE 3.3 in txt
write_in = r'C:\Users\Gollinucci\Desktop\Nicolò\Lavoro\FEEM\Databases\EXIOBASE_3.3.18_Africa_Meat'

start_write = time.time() # Compute the time for writing the txt database
Original.database_to_txt(write_in)

print('Database written in {} seconds.'.format(time.time()-start_write))
#%% Reimporting the database to measure the increase in efficiency

# Enter the path of where you have to stored your local copy of hybrid EXIOBASE 3.3 in txt adding \flows
read_txt_from = r'C:\Users\Gollinucci\Desktop\Nicolò\Lavoro\FEEM\Databases\EXIOBASE_3.3.18_Africa_Meat\flows'
Txt = MARIO.Database(name='Txt version of MR_HIOT_2011_v3_3_18')

start_txt = time.time() # Compute the time for reading the txt database
Txt.parse_from_txt(read_txt_from, 'IOT', 'basic')

delta_txt = time.time()-start_txt
print('Txt database written in {} seconds.'.format(delta_txt))
print('Adopting txt written database saves {}% of time with respect to importing original database.'.format((delta_xlsb-delta_txt)/delta_xlsb*100))

