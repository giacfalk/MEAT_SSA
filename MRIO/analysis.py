# -*- coding: utf-8 -*-
"""
Created on Mon Feb  8 14:34:17 2021

@author: nigolred
"""

import MARIO
import pandas as pd
import copy
All = slice(None) # to lighten the writing of code

# Enter the path of where you have to stored your local copy of hybrid EXIOBASE 3.3 for Sub-Saharian Africa (SSA) Meat analysis in txt format adding \flows
read_from = r'C:\Users\Gollinucci\Desktop\Nicolò\Lavoro\FEEM\Databases\EXIOBASE_3.3.18_Africa_Meat\flows' # zenodo link for downloading the file
World = MARIO.Database(name='Txt version of MR_HIOT_2011_v3_3_18 updated for Africa Meat analysis')

World.parse_from_txt(read_from, 'IOT', 'basic', mode='flows')

World.aggregate(drop=True, levels=['Sector','Region'],path=r'Aggregations/Fossil_Fuels.xlsx')
#%%
World.calc_all(['w'])
Z = AM.
b = MARIO.calc_b(World.Z, World.X)
z = MARIO.calc_z(World.Z, World.X)
e = MARIO.calc_e(World.E, World.X).loc['Fossil Fuels']
f = MARIO.calc_f(World.e, World.w).loc['Fossil Fuels']
E = World.E.loc['Fossil Fuels']
F = MARIO.calc_F(World.f, World.Y.sum(axis=1)).loc['Fossil Fuels']
