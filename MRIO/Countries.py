# -*- coding: utf-8 -*-
"""
Created on Fri Jan 22 11:38:19 2021

@author: nigolred
"""
'''
In this code the choice of regional aggregation made for determining meat consumption pathways is linked with EXIOBASE aggregation
'''
import pandas as pd

reg1 = pd.read_excel(r'Inputs\Support Information for MRIO\Countries.xlsx', sheet_name='UN to EXIOBASE', index_col=[0])
reg2 = pd.read_excel(r'Inputs\Support Information for MRIO\Countries.xlsx', sheet_name='UN to Reference', index_col=[1])

reg3 = pd.DataFrame(None, index=reg1.index, columns=['EXIOBASE'])

#%% Build mapping vector UN to EXIOBASE
for i in reg3.index:
    for j in reg1.columns:
        if reg1.loc[i,j] == 1:
            reg3.loc[i,'EXIOBASE'] = j

#Hybrid exio does not have explicit Taiwan
reg3.loc['Taiwan'] = 'RoW Asia and Pacific'

#%% Complementing reference regions with respective EXIOBASE region
reg2['EXIOBASE'] = None
for i in reg2.index:
    if i in reg3.index:
        reg2.loc[i,'EXIOBASE'] = reg3.loc[i,'EXIOBASE']

    elif i == 'Bosnia & Herzegovina':
        reg2.loc[i,'EXIOBASE'] = 'RoW Europe'
        
    elif i == 'Côte d’Ivoire':
        reg2.loc[i,'EXIOBASE'] = 'RoW Africa'
        
    elif i == 'Congo - Brazzaville':
        reg2.loc[i,'EXIOBASE'] = 'RoW Africa'
        
    elif i == 'Czechia':
        reg2.loc[i,'EXIOBASE'] = 'RoW Europe'
                
    elif i == 'Kyrgyzstan':
        reg2.loc[i,'EXIOBASE'] = 'RoW Asia and Pacific'
                
    elif i == 'Myanmar (Burma)':
        reg2.loc[i,'EXIOBASE'] = 'RoW Asia and Pacific'
                
    elif i == 'São Tomé & Príncipe':
        reg2.loc[i,'EXIOBASE'] = 'RoW Africa'
                
    elif i == 'Bosnia & Herzegovina':
        reg2.loc[i,'EXIOBASE'] = 'RoW Europe'
                
    elif i == 'Trinidad & Tobago':
        reg2.loc[i,'EXIOBASE'] = 'RoW America'
    
    else:
        print('This country is not mapped')
        reg2.loc[i,'EXIOBASE'] = None
        
