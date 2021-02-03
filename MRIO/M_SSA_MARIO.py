# -*- coding: utf-8 -*-
"""
Created on Sat Jan 16 09:29:36 2021

@author: nigolred
"""
# Preparing the database

import MARIO
import support
import pandas as pd
import copy
All = slice(None)

# Enter the path of where you have to stored your local copy of hybrid EXIOBASE 3.3 in txt adding \flows
read_from = r'C:\Users\Gollinucci\Desktop\Nicolò\Lavoro\FEEM\Databases\EXIOBASE_3.3.18_Africa_Meat\flows'
World = MARIO.Database(name='Txt version of MR_HIOT_2011_v3_3_18')

World.parse_from_txt(read_from, 'IOT', 'basic', mode='flows')
World.calc_all(['w','m','M','f','F']) # Main matrices (z,v and e) already computed by default

#%% Select your extensions, products and region and print the footprint explorer

Meat = ['Products of meat cattle', 
        'Products of meat pigs', 
        'Products of meat poultry']

Reg_comp = ['Italy','Germany','Japan','USA','Brazil','RoW Africa']

MEAT_Ext = ['Water Consumption Blue','Water Consumption Green','Fossil Fuels','Greenhouse gases','Land Use','Europhication']
Eco = World.get_index('Factor of production')

for i in ['cba','CBA','PBA','pba']:
    MARIO.plot_footprints(World, i, MEAT_Ext, Reg_comp, Meat, project_name='/Baseline/AM_')
    
#%% Preparing the cases

Years = [2020, 2030, 2040, 2050]
Scenarios = ['SSP1', 'SSP2','SSP3','SSP4','SSP5']
Regions = ['Central Europe', 'Central Latin America', 'East Asia', 'North Africa and Middle East'] # add 'Median' when updated
Extensions = ['Land [Mkm2]', 'Water Cons. Blue [BCM]', 'Water Cons. Green [BCM]', 'Fossil Fuels [EJ]','Electricity [TWh]', 'GHG [GtonCO2_eq]', 
              'CO2_f [GtonCO2_eq]','CH4 [GtonCO2_eq]','N2O [GtonCO2_eq]','CO2_b [GtonCO2_eq]','Eutrop. [MtonPO4_eq]']

Results = pd.DataFrame(0, index=pd.MultiIndex.from_product([Extensions, Regions, Scenarios]), columns=pd.MultiIndex.from_product([Years]))

Ele_sect = ['Electricity by coal','Electricity by gas','Electricity by nuclear','Electricity by hydro','Electricity by wind','Electricity by petroleum and other oil derivatives','Electricity by biomass and waste','Electricity by solar photovoltaic','Electricity by solar thermal','Electricity by tide, wave, ocean','Electricity by Geothermal','Electricity nec']
#%% Updating final demand values
# Import data from the demand model and plotting to ease exploration of input data

Projections = pd.read_csv('Inputs/From Demand Model/all_projections_2050_SSAfrica_new.csv', index_col =[0,1,2,4])
meat = ['beef','pork','poultry']
support.plot_projections(Projections,'total_Kt','Projections')

#Computing dry matter values
Moisture = pd.read_excel('Inputs/Support information for MRIO/Moisture_content.xlsx', index_col=[0])
Projections['Dry_matter_tonnes'] = 0 # adding the column

for i in meat:
    Projections.loc[(All,All,All,i),'Dry_matter_tonnes'] = Projections.loc[(All,All,All,i),'Total_kg']*(1-Moisture.loc[i].values)/1000
    
# Comparing EXIOBASE consumption with the one provided in the first observed period (2015) by the Demand Model
Meat_Cons = {}    
Meat_Cons['Y_abs'] = World.Y.loc[(All,'Sector',Meat),'RoW Africa'].sum(axis=1).to_frame().unstack(0).droplevel(0).T.droplevel(0).T
Meat_Cons['Y_per'] = Meat_Cons['Y_abs'].T / Meat_Cons['Y_abs'].sum(axis=1)
Meat_Cons['Z_abs'] = World.Z.loc[(All,'Sector',Meat),'RoW Africa'].sum(axis=1).to_frame().unstack(0).droplevel(0).T.droplevel(0).T
Meat_Cons['Z_per'] = Meat_Cons['Z_abs'].T / Meat_Cons['Z_abs'].sum(axis=1)

Tot_Meat_Cons = Meat_Cons['Y_abs'].sum(axis=1) + Meat_Cons['Z_abs'].sum(axis=1)
Mean_Meat_Proj = Projections.loc[(2015,All,All,meat)].groupby(level=3).mean().loc[:,'Dry_matter_tonnes']

#%% Preparing the environmental matrices dictionary

ee = {} # Environmental matrices dictionary
rate = pd.read_excel('Inputs/Support information for MRIO/Scenarios.xlsx', sheet_name='Change rates', index_col=[0])
ref_cou = support.reference_countries(Regions)
for yea in Years:
    RoW, Ref = rate.loc['RoW Africa',yea], rate.loc['Ref',yea] # Import the weights for the selected year
    for reg in Regions:
        Countries = ref_cou.loc[reg].loc[:,'EXIOBASE'].values # For every reference region, all the relative EXIOBASE regions are imported
        ee[str(reg)+' - '+str(yea)] = copy.deepcopy(World.e) # Every new satellite account coefficients matrix is a modified version of the baseline one
        for sec in World.get_index('Sector'):
            c=0
            for cou in Countries:
                if World.X.loc[cou,All,sec].values !=0:
                    c=c+1 # To compute the average only on the producing sector: 0 impact coefficeints are present in non-producing sector
            if c!=0:
                print(c)
                ee[str(reg)+' - '+str(yea)].loc[:,('RoW Africa',All,sec)] = (( RoW*World.e.loc[:,('RoW Africa',All,sec)].droplevel(0,1).droplevel(0,1) + Ref*World.e.loc[:,(Countries,All,sec)].groupby(level=2, axis=1).sum()/c ) /2).values
            else:
                print(c)
                ee[str(reg)+' - '+str(yea)].loc[:,('RoW Africa',All,sec)] = World.e.loc[:,('RoW Africa',All,sec)]
            
#%% Running all the Africa Meat (AM) cases

runall = True # In case you need to run a subset of the possible cases

if runall == False:
    Years = [2020]
    Scenarios = ['SSP2']
    Regions = ['Central Latin America']

for yea in Years:
    for sce in Scenarios:
        for reg in Regions:
            AM = copy.deepcopy(World) # Every AM case is a copy of the baseline and has... 
            Y_meat = Meat_Cons['Y_per'] * Projections.loc[(yea,sce,reg,meat),'Dry_matter_tonnes'].values #...a specific demand vector

            for r in AM.get_index('Region'):
                for i in Meat:
                    AM.Y.loc[(r,All,i),'RoW Africa'] = Y_meat.loc[r,i] # Update final demand values cell by cell
            # AM.e = ee[str(reg)+' - '+str(yea)] # Update case-specific satellite account coefficients
                    
            AM.X = MARIO.calc_X_from_w(AM.w, AM.Y.sum(axis=1)) # Compute new production
            AM.E = MARIO.calc_E(AM.e, AM.X) # Compute new enivronmental transactions
            AM.f = MARIO.calc_f(AM.e, AM.w) # Compute new footprints
            AM.F = MARIO.calc_F(AM.f, AM.Y.sum(axis=1)) # Compute new footprints
            Electricity_Change = AM.X.loc[(All,All,Ele_sect),:] - World.X.loc[(All,All,Ele_sect),:].values # Change in electricity production by sector and region
            
            # Writing results
            Results.loc[('Land [Mkm2]', reg, sce), (yea)] = (AM.E.loc['Land Use']-World.E.loc['Land Use']).sum() / 10**8
            Results.loc[('Water Cons. Blue [BCM]', reg, sce), (yea)] = (AM.E.loc['Water Consumption Blue']-World.E.loc['Water Consumption Blue']).sum()/ 1000
            Results.loc[('Water Cons. Green [BCM]', reg, sce), (yea)] = (AM.E.loc['Water Consumption Green']-World.E.loc['Water Consumption Green']).sum()/ 1000
            Results.loc[('Electricity [TWh]', reg, sce), (yea)] = Electricity_Change.sum().values/3600
            Results.loc[('Fossil Fuels [EJ]', reg, sce), (yea)] = (AM.E.loc['Fossil Fuels']-World.E.loc['Fossil Fuels']).sum() / 10**9
            Results.loc[('GHG [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['Greenhouse gases']-World.E.loc['Greenhouse gases']).sum() / 10**9
            Results.loc[('CO2_f [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['Carbon dioxide, fossil to air']-World.E.loc['Carbon dioxide, fossil to air']).sum() / 10**9
            Results.loc[('CH4 [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['CH4 to air']-World.E.loc['CH4 to air']).sum() / 10**9
            Results.loc[('N2O [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['N2O to air']-World.E.loc['N2O to air']).sum() / 10**9
            Results.loc[('CO2_b [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['Carbon dioxide, biogenic to air']-World.E.loc['Carbon dioxide, biogenic to air']).sum() / 10**9
            Results.loc[('Eutrop. [MtonPO4_eq]', reg, sce), (yea)] = (AM.E.loc['Europhication']-World.E.loc['Europhication']).sum() / 10**6

for i in ['cba','CBA','PBA','pba']:
    MARIO.plot_footprints(AM, i, MEAT_Ext, Reg_comp, Meat, project_name='/Central Europe/NewAM_')                  
    

