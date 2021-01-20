# -*- coding: utf-8 -*-
"""
Created on Sat Jan 16 09:29:36 2021

@author: nigolred
"""
# Preparing the database

import MARIO
import pandas as pd
import pyioa

Projections_old = pd.read_csv('Inputs/all_projections_2050_SSAfrica_old.csv', index_col =[0,1,2,4])
Projections_new = pd.read_csv('Inputs/all_projections_2050_SSAfrica_new.csv', index_col =[0,1,2,4])
pyioa.plot_projections(Projections_old,'total_Kt','Projections_old')
pyioa.plot_projections(Projections_new,'total_Kt','Projections_new')

#%% Importing database
path = r'C:\Users\Gollinucci\Desktop\Nicolò\Lavoro\FEEM\Databases\EXIOBASE_3.3.18_hsut_2011'
World = MARIO.Model(name='Meat_SSA')
World.parse_exiobase(path, exio_version='MR_HIOT_2011_v3_3_18', table='IOT', 
                     hybrid_ext_read=['resource_act','resource_FD','Land_act','Land_FD','Emiss_act','Emiss_FD'])

World.database_to_excel(path=r'C:\Users\Gollinucci\Desktop\big.xlsx')
# Elaborate my additional interested resource
# Eutrophication
# Greenhouse gases
# ...

#%%
import copy

V = copy.deepcopy(World.V)
E = copy.deepcopy(World.E)
Z = copy.deepcopy(World.Z)
Y = copy.deepcopy(World.Y)

V_index = V.index.to_list()
V.index = [['Satellite Accounts']*len(V_index),['Economic Factors']*len(V_index),V_index]
E_index = E.index.to_list()
E.index = [['Satellite Accounts']*len(E_index),['Environmental Factors']*len(E_index),E_index]

index_0 = Z.index.get_level_values(0).to_list()+V.index.get_level_values(0).to_list()+E.index.get_level_values(0).to_list()
index_1 = Z.index.get_level_values(1).to_list()+V.index.get_level_values(1).to_list()+E.index.get_level_values(1).to_list()
index_2 = Z.index.get_level_values(2).to_list()+V.index.get_level_values(2).to_list()+E.index.get_level_values(2).to_list()

col_0   = Z.columns.get_level_values(0).to_list()+Y.columns.get_level_values(0).to_list()
col_1   = Z.columns.get_level_values(1).to_list()+Y.columns.get_level_values(1).to_list()
col_2   = Z.columns.get_level_values(2).to_list()+Y.columns.get_level_values(2).to_list()

data = pd.DataFrame(index = [index_0,index_1,index_2],columns = [col_0,col_1,col_2]).fillna(0)
 
#%%
import xlsxwriter

file = r'C:\Users\Gollinucci\Desktop\big.xlsx'
workbook = xlsxwriter.Workbook(file)

# Add a format for the header cells.
header_format = workbook.add_format({
    'border': 1,
    'bg_color': '#C6EFCE',
    'bold': True,
    'text_wrap': False,
    'valign': 'vcenter',
    'indent': 1,
})
    
# Filling the index indeces sheet
coefficients = workbook.add_worksheet('coefficients') 
coeff_format = workbook.add_format({'num_format': '0.000;-0.000;-' })

# indeces
for row in range(data.shape[0]):
    try:       
        coefficients.write('A{}'.format(row+4),data.index.get_level_values(0)[row],header_format)  
        coefficients.write('B{}'.format(row+4),data.index.get_level_values(1)[row],header_format)
        coefficients.write('C{}'.format(row+4),data.index.get_level_values(2)[row],header_format)
    except:
        pass
    
# columns
for row in range(data.shape[1]):       
    coefficients.write(0,row+3,data.columns.get_level_values(0)[row],header_format)  
    coefficients.write(1,row+3,data.columns.get_level_values(1)[row],header_format)
    coefficients.write(2,row+3,data.columns.get_level_values(2)[row],header_format) 


for row in range(data.shape[0]):
    for col in range(data.shape[1]):
        coefficients.write(row+3,col+3,data.iloc[row,col],coeff_format)   
        
units = workbook.add_worksheet('units')

data = World.units
units.write('C1','unit',header_format)

counter = 2
for key,item in data.items():
    for row in range(item.shape[0]):
        units.write('A{}'.format(counter),key,header_format)
        units.write('B{}'.format(counter),item.index[row],header_format)
        units.write('C{}'.format(counter),item.iloc[row,0])
    
    counter += 1
    

workbook.close()


#%%
World.get_shock_excel(path=r'C:\Users\Gollinucci\Documents\GitHub\MEAT_SSA\MRIO\Inputs\Demand_Shock.xlsx')
