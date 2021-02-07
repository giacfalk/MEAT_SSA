# -*- coding: utf-8 -*-
"""
Created on Wed May 20 11:44:25 2020

@author: nigolred
"""

import pyioa
import pandas as pd
import pymrio
import matplotlib.pyplot as plt
import numpy as np

# Importing and aggregating the most updated Multi Regional Hybrid Database

vH_11 = pyioa.mrio(path=r'C:\Users\Gollinucci\Desktop\Nicolò\Lavoro\FEEM\Databases\EXIOBASE_3.3.18_hsut_2011_custom\Exiobase_MR_HIOT_2011_v3_3_18_by_prod_tech.xlsb', v='H')
vH_11.calc()

#%% Exploring the dataset related to meat products

Meat_prod = ['Products of meat cattle', 'Products of meat pigs', 'Products of meat poultry', 'Meat products nec']
Meat_acti = ['Processing of meat cattle', 'Processing of meat pigs', 'Processing of meat poultry']
f_meat = vH_11.f_dis.loc[:, (slice(None), Meat_acti, slice(None), slice(None))]
f_meat_mean = f_meat.groupby(axis=1, level=1, sort=False).mean()
# Getting the share of import of meat
Import_Share_Africa_Meat = vH_11.Y_dis.loc[(slice(None), Meat_prod, slice(None), slice(None), slice(None)), (['WF','ZA'])]
Tot_demand_Africa = Import_Share_Africa_Meat.groupby(sort=False, axis=0, level=0).sum().sum(axis=1)
shares_tot_demand = Tot_demand_Africa/(Tot_demand_Africa.sum())

e_meat = vH_11.e_dis.groupby(sort=False, axis=0, level=1).sum().loc[:, (slice(None), Meat_acti, slice(None), slice(None))]
#%% Footprints Chart

ext = ['Land use, arable land','Greenhouse gasses','Eutrophication','Fossil Fuels']
reg = ['BR','CN','DE','IT','US','ZA','WA','WF','WM']
reg_lab = ['Brazil','China','Germany','Italy','USA','South Africa','RoW - America', 'RoW - Africa', 'RoW - Middle East']
mea = ['Cattle','Pigs','Poultry']

# pyioa.plot_footprints(footprint=f_meat, regions_code=reg, regions_name=reg_lab, extensions=ext, units=['ha','tonCO2_eq','tonPO4_eq','GJ'], products=Meat_acti)
#%% These are all the dimensions of the scenarios

Year = [2020, 2030, 2040, 2050]
Scenario = ['SSP1', 'SSP2','SSP3','SSP4','SSP5']
Region = ['Central Europe', 'Central Latin America', 'East Asia', 'North Africa and Middle East'] # add 'Median' when updated
Mod = ['pessimistic', 'baseline','optimistic']	
Extensions = ['Land [Mkm2]', 'Water Cons. Blue [BCM]', 'Water Cons. Green [BCM]', 'Fossil Fuels [EJ]','Electricity [TWh]', 'GHG [GtonCO2_eq]', 'CO2_f [GtonCO2_eq]','CH4 [GtonCO2_eq]','N2O [GtonCO2_eq]','CO2_b [GtonCO2_eq]','Eutrop. [MtonPO4_eq]']
Res = pd.DataFrame(0, index=pd.MultiIndex.from_product([Extensions, Region, Scenario, Mod]), columns=pd.MultiIndex.from_product([Year]))

Projections = pd.read_csv('Inputs/From Demand Model/all_projections_2050_SSAfrica_old.csv', index_col =[0,1,2,4])

Y_s = vH_11.Y_dis.copy()
X = vH_11.X_dis
l = pymrio.calc_L(vH_11.z_dis)
vH_11.agg('Aggregations/Aggregation_results.xlsx')

# Preparing leontief matrices

ReferenceScenario = pd.read_excel('Inputs/Support information for MRIO/Scenarios.xlsx', sheet_name='Reference countries', index_col=0)
ChangeRate =  pd.read_excel('Inputs/Support information for MRIO/Scenarios.xlsx', sheet_name='Change rates_v2', index_col=0)

# Build the Frame of Reference
RC = ReferenceScenario.loc['Frame of reference'].ref # Reference Scenario         
zWF = vH_11.z_dis.loc['WF','WF']
zRC = vH_11.z_dis.loc[RC,RC]
eWF = vH_11.e_dis.loc[:,'WF']
eRC = vH_11.e_dis.loc[:,RC]
z_r = vH_11.z_dis.copy()
e_r = vH_11.e_dis.copy()
E_base = vH_11.E_dis.sum(axis=1)

E_r_T_dict = {2020:0,
              2030:0,
              2040:0,
              2050:0}

E_r_T = pd.DataFrame(0, index=e_r.index, columns=Year)
E_change = pd.DataFrame(0, index=e_r.index, columns=Year)

#
for y in Year:
    C_WF = ChangeRate.loc['RoW Africa',y]
    C_ref = ChangeRate.loc['Ref',y]
    zWF_sum = zWF.sum(axis=0)
    zRC_sum = zRC.sum(axis=0)

    e_r.loc[:,'WF'] = C_WF*eWF.values+C_ref*eRC.values
    E_r = pymrio.calc_F(e_r, X)
    E_r_T_dict[y] = E_r
    E_r_T.loc[:,y] = E_r.sum(axis=1)
    E_change.loc[:,y] = (E_r_T.loc[:,y]-E_base)/E_base*100

fig, ax = plt.subplots(figsize=(5,6))
res_lis=['Water Consumption Blue','Water Consumption Green','Eutrophication','Greenhouse gasses','Land use, arable land']
ax.set_title('Change in environmental extension if RoW Africa intensities changes in time tending to '+RC)
ax.set_xticks(Year)
ax.legend(res_lis)
ax.plot(Year, E_change.loc[res_lis,:].T)
ax.legend(res_lis)
ax.set_ylabel('Percentual change with respect to baseline [%]')

# Chosen scenario if runall = False

runall = False

if runall == False:
    Year = [2050]
    Scenario = ['SSP5']
    Region = ['North Africa and Middle East']
    Mod = ['baseline']

for ProjYear in Year:
    for Scen in Scenario:
        for RefCou in Region:
            for mod in Mod:
                NewDemand = Projections.loc[(ProjYear, Scen, RefCou), :].dry_ton
                Y_dis_s = vH_11.Y_dis.copy()
                shares = pd.read_excel('Inputs/Support information for MRIO/Import_shares.xlsx', sheet_name='Scenarios_dis', index_col=[0]) # toglilo dal loop
                RC = ReferenceScenario.loc[RefCou].ref
                C_WF = ChangeRate.loc['RoW Africa',ProjYear]
                C_ref = ChangeRate.loc['Ref',ProjYear]
                eRC = vH_11.e_dis.loc[:,RC]
                e_1 = vH_11.e_dis.copy()
                eWF = vH_11.e_dis.loc[:,'WF']
                e_1.loc[:,'WF'] = C_WF*eWF.values+C_ref*eRC.values
                
                RC = ReferenceScenario.loc[RefCou].ref # Reference Scenario         
                C_WF = ChangeRate.loc['RoW Africa',ProjYear]
                C_ref = ChangeRate.loc['Ref',ProjYear]
                eRC = vH_11.e_dis.loc[:,RC]      
                e_r.loc[:,'WF'] = C_WF*eWF.values+C_ref*eRC.values
                E_r = pymrio.calc_F(e_r, X)
                E_r_T_dict[ProjYear] = E_r
                E_r_T.loc[:,ProjYear] = E_r.sum(axis=1)
                E_change.loc[:,ProjYear] = (E_r_T.loc[:,ProjYear]-E_base)/E_base*100
                
                for r in list(shares.index):
                    if shares.loc[r,mod] != 0:
                        Y_s.loc[(r,'Products of meat cattle'),('WF')] = shares.loc[r,mod]*NewDemand.loc['beef']
                        Y_s.loc[(r,'Products of meat pigs'),('WF')] = shares.loc[r,mod]*NewDemand.loc['pork']
                        Y_s.loc[(r,'Products of meat poultry'),('WF')] = shares.loc[r,mod]*NewDemand.loc['poultry']
                        # Y_s.loc[(r,'Meat products nec'),('WF')] = shares.loc[r,mod]*NewDemand.loc['mutton']
                    else:
                        Y_s.loc[(r,'Products of meat cattle'),('WF')] = 0
                        Y_s.loc[(r,'Products of meat pigs'),('WF')] = 0
                        Y_s.loc[(r,'Products of meat poultry'),('WF')] = 0
                        # Y_dis_s.loc[(r,'Meat products nec'),('WF')] = 0
                        
                
                DeltaDemand_dis = Y_s - vH_11.Y_dis
                X_s = pd.DataFrame(pymrio.calc_x_from_L(l.values, Y_s.sum(axis=1)), index=vH_11.X_dis.index, columns=vH_11.X_dis.columns)
                deltaXele = (X_s-X).set_index(vH_11.Z_index).groupby(axis=0, level=[0,1], sort=False).sum().loc[(slice(None),'Electricity [TJ]'),:]
                E_s = pymrio.calc_F(e_1, X_s)
                deltaF_dis = E_s - E_r_T_dict[ProjYear]
                res_dis = deltaF_dis.sum(axis=1)
                
                Res.loc[('Land [Mkm2]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['Land Use [ha]'] / 10**8
                Res.loc[('Water Cons. Blue [BCM]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['Water Consumption Blue [Mm3]'] / 1000
                Res.loc[('Water Cons. Green [BCM]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['Water Consumption Green [Mm3]'] / 1000
                Res.loc[('Electricity [TWh]', RefCou, Scen, mod), (ProjYear)] = deltaXele.sum().values/3600
                Res.loc[('Fossil Fuels [EJ]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['Fossil Fuels [GJ]'] / 10**9
                Res.loc[('GHG [GtonCO2_eq]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['GHG [tonCO2_eq]'] / 10**9
                Res.loc[('CO2_f [GtonCO2_eq]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['CO2_f [tonCO2_eq]'] / 10**9
                Res.loc[('CH4 [GtonCO2_eq]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['CH4 [tonCO2_eq]'] / 10**9
                Res.loc[('N2O [GtonCO2_eq]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['N2O [tonCO2_eq]'] / 10**9
                Res.loc[('CO2_b [GtonCO2_eq]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['CO2_b [tonCO2_eq]'] / 10**9
                Res.loc[('Eutrop. [MtonPO4_eq]', RefCou, Scen, mod), (ProjYear)] = res_dis.groupby(level=1, sort=False).sum().loc['EUT [tonPO4_eq]'] / 10**6
                
Res.to_csv('Results/old_code_Final_results_tot_new.csv')
# (tot, cow, pig, pou)

#%%
Footprints = pymrio.calc_M(e_1,l.values)
Requirments = pymrio.calc_F_Y(Footprints, DeltaDemand_dis.sum(axis=1))
FactorUse = pymrio.calc_F(e_1, X_s-X)
agg = 'Aggregations/Aggregation_results_ff.xlsx'
vH_11.agg(agg)
Sec_lis = list(pd.read_excel(agg, sheet_name='Sectors').Sec_agg)
Reg_lis = list(pd.read_excel(agg, sheet_name='Regions').Reg_agg)
Z_index = pd.MultiIndex.from_product([Reg_lis, Sec_lis])
Reg_index = pd.MultiIndex.from_product([Reg_lis])
Requirments_agg = Requirments.groupby(axis=0, level=1, sort=False).sum()
Requirments_agg = Requirments_agg.T.set_index(Z_index).groupby(axis=0, level=[0,1], sort=False).sum().T
FactorUse_agg = FactorUse.groupby(axis=0, level=1, sort=False).sum()
FactorUse_agg = FactorUse_agg.T.set_index(Z_index).groupby(axis=0, level=[0,1], sort=False).sum().T

# net
A_agg = pymrio.calc_A(vH_11.Z_agg, pymrio.calc_x(vH_11.Z_agg, vH_11.Y_agg))
x_agg = X_s-X
x_agg = x_agg.set_index(Z_index).groupby(axis=0, level=[0,1], sort=False).sum()
Z_agg = pymrio.calc_Z(A_agg,x_agg)
Y_agg = DeltaDemand_dis.set_index(Z_index).groupby(axis=0, level=[0,1], sort=False).sum().sum(axis=1)
Y_agg.name = 'Final Consumption'

Cons = Z_agg.merge(Y_agg, left_index=True, right_index=True)

Sat = 'Fossil Fuels [GJ]'
Exte = FactorUse_agg.loc[Sat,:]
IBA = Cons.copy()
#
for i in range(len(Cons.index)):
    for j in range(len(Cons.columns)):
        if Cons.iloc[i,:].sum() == 0:
            IBA.iloc[i,j] = 0
        else:
            IBA.iloc[i,j] = Cons.iloc[i,j]/Cons.iloc[i,:].sum()*Exte.iloc[i]

add_row = pd.Series(0, index=[('World','Final Consumption')])
PBA = FactorUse_agg.loc[Sat,:].append(add_row)
CBA = Requirments_agg.loc[Sat,:].append(add_row)
IBA = IBA.sum(axis=0)
IBA.index = CBA.index
Comparison = pd.concat([PBA,IBA,CBA], axis=1, keys=['PBA','IBA','CBA'])
#
FactorUse_agg.loc['Fossil Fuels [GJ]',:].plot(kind='bar',stacked=True)
Requirments_agg.loc['Fossil Fuels [GJ]',:].plot(kind='bar',stacked=True)
FactorUse_agg.plot(kind='bar',stacked=True)
FF = Footprints.loc['Fossil Fuels',:].T.set_index(Z_index).groupby(axis=0, level=[0,1], sort=False).sum().T
FF.T.plot(kind='bar')
#%%%
import matplotlib.pyplot as plt
Comparison.drop(Comparison.tail(1).index,inplace=True)
f = plt.figure()
Comparison.T.plot(kind='bar', stacked=True, title='Fossil Fuels [GJ] use by region and sector with Production-, Input- and Consumption-based approach', figsize=(15,12), colormap='terrain')
plt.legend(loc='center right', bbox_to_anchor=(1.25,0.5))
Comparison.to_csv('Results/Use_of_FF_new.csv')
