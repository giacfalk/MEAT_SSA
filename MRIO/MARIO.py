# -*- coding: utf-8 -*-
"""
Created on Sat Jan 16 09:29:36 2021

@author: nigolred
"""
# Preparing the database

import MARIO
import LUIGI
import pandas as pd
import copy
All = slice(None) # to lighten the writing of code

# Enter the path of where you have to stored your local copy of hybrid EXIOBASE 3.3 for Sub-Saharian Africa (SSA) Meat analysis in txt format adding \flows
read_from = r'C:\Users\Gollinucci\Desktop\Nicolò\Lavoro\FEEM\Databases\EXIOBASE_3.3.18_Africa_Meat\flows' # zenodo link for downloading the file
World = MARIO.Database(name='Txt version of MR_HIOT_2011_v3_3_18 updated for Africa Meat analysis')

World.parse_from_txt(read_from, 'IOT', 'basic', mode='flows')
World.calc_all(['w','m','M','f','F']) # Main matrices (z,v and e) already computed by default

#%% Select your parameters

Res_name = 'Total' # Name your result file

Meat = ['Products of meat cattle','Products of meat pigs','Products of meat poultry']
Years = [2020, 2030, 2040, 2050]
Scenarios = ['SSP1', 'SSP2','SSP3','SSP4','SSP5']
Regions = ['Central Europe', 'Central Latin America', 'East Asia', 'North Africa and Middle East'] # add 'Median' when updated
Result_Extensions = ['Land [Mkm2]', 'Water Cons. Blue [BCM]', 'Water Cons. Green [BCM]', 'Fossil Fuels [EJ]','Electricity [TWh]', 'GHG [GtonCO2_eq]', 
              'CO2_f [GtonCO2_eq]','CH4 [GtonCO2_eq]','N2O [GtonCO2_eq]','CO2_b [GtonCO2_eq]','Eutrop. [MtonPO4_eq]']

Extensions = ['Water Consumption Blue','Water Consumption Green','Fossil Fuels','Greenhouse gases','Land Use','Eutrophication'] # Used for plotting with LUIGI

Ele_sect = ['Electricity by coal','Electricity by gas','Electricity by nuclear','Electricity by hydro','Electricity by wind','Electricity by petroleum and other oil derivatives',
            'Electricity by biomass and waste','Electricity by solar photovoltaic','Electricity by solar thermal','Electricity by tide, wave, ocean','Electricity by Geothermal','Electricity nec']

# Import data from the demand model and plotting to ease exploration of input data
Approach = 'New'

if Approach !='Old': # The new environemental coefficients are updated on the basis of a weighted average between actual SSA situation and the median of a subset of EXIOBASE regions per each reference region
    ref_cou = LUIGI.reference_countries(Regions) # UN regions (used for demand forcast) and EXIOBASE regions are reconciled 
    Projections = pd.read_csv('Inputs/From Demand Model/all_projections_2050_SSAfrica_new.csv', index_col =[0,1,2,4]) # More refined values

else:
    ref_cou = pd.read_excel('Inputs/Support information for MRIO/Scenarios.xlsx', sheet_name='Reference countries', index_col=[0]).iloc[:,0].to_frame() # just one EXIOBASE region per each reference region
    Projections = pd.read_csv('Inputs/From Demand Model/all_projections_2050_SSAfrica_old.csv', index_col =[0,1,2,4])


meat = ['beef','pork','poultry'] # Meat names in the projection files
Moisture = pd.read_excel('Inputs/Support information for MRIO/Moisture_content.xlsx', index_col=[0]) # Values hare taken from hybrid EXIOBASE's main authors' paper for computing dry matter values from projections
rate = pd.read_excel('Inputs/Support information for MRIO/Scenarios.xlsx', sheet_name='Change rates', index_col=[0]) # Rate of increase in environmental efficiency

#%% Plotting baseline
Reg_comp = ['Italy','Germany','Japan','USA','Brazil','RoW Africa']

for i in ['cba','CBA','PBA','pba']:
    MARIO.plot_footprints(World, i, Extensions, Reg_comp, Meat, project_name='/Impacts/Baseline_')
    
LUIGI.plot_projections(Projections,'total_Kt',Res_name+'_Projections')

#%% Preparing final demand values

Projections['Dry_matter_tonnes'] = 0 # adding the column
for i in meat:
    Projections.loc[(All,All,All,i),'Dry_matter_tonnes'] = Projections.loc[(All,All,All,i),'Total_kg']*(1-Moisture.loc[i].values)/1000

# Computing meat-specific import shares    
Meat_Cons = {}    
Meat_Cons['Y_abs'] = World.Y.loc[(All,'Sector',Meat),'RoW Africa'].sum(axis=1).to_frame().unstack(0).droplevel(0).T.droplevel(0).T
Meat_Cons['Y_per'] = Meat_Cons['Y_abs'].T / Meat_Cons['Y_abs'].sum(axis=1)
Meat_Cons['Z_abs'] = World.Z.loc[(All,'Sector',Meat),'RoW Africa'].sum(axis=1).to_frame().unstack(0).droplevel(0).T.droplevel(0).T
Meat_Cons['Z_per'] = Meat_Cons['Z_abs'].T / Meat_Cons['Z_abs'].sum(axis=1)

# Old methodology assumed an one-fits-all share of import of meat
All_Meat = ['Products of meat cattle','Products of meat pigs','Products of meat poultry','Meat products nec']
Old_share = World.Y.loc[(All,'Sector',All_Meat),'RoW Africa'].sum(axis=1).to_frame().unstack(0).droplevel(0).T.droplevel(0).T.sum()/(World.Y.loc[(All,'Sector',All_Meat),'RoW Africa'].sum(axis=1).to_frame().unstack(0).droplevel(0).T.droplevel(0).T.sum().sum())
Old_shares = pd.DataFrame(0, index=Old_share.index, columns=Meat)
for i in [0,1,2]:
    Old_shares.iloc[:,i] = Old_share.values

# Comparing hybrid EXIOBASE (2011) consumption with the one provided in the first observed period (2020) by the demand model
Tot_Meat_Cons = Meat_Cons['Y_abs'].sum(axis=1) + Meat_Cons['Z_abs'].sum(axis=1)
Mean_Meat_Proj = Projections.loc[(2020,All,All,meat)].groupby(level=3).mean().loc[:,'Dry_matter_tonnes']

#%% Preparing the environmental matrices dictionary

ee = {} # Environmental matrices dictionary
for yea in Years:
    RoW, Ref = rate.loc['RoW Africa',yea], rate.loc['Ref',yea] # Import the SSA-reference country weights for the selected year
    for reg in Regions:
        ee[str(reg)+' - '+str(yea)] = copy.deepcopy(World.e) # Every new satellite account coefficients matrix is a modified version of the baseline one
        if Approach!='Old':
            Countries = ref_cou.loc[reg].loc[:,'EXIOBASE'].values # For every reference region, all the relative EXIOBASE regions are imported
            for sec in World.get_index('Sector'):
                c=0
                for cou in Countries:
                    if World.X.loc[cou,All,sec].values !=0:
                        c=c+1 # To compute the median only if there is at least one producing sector among the reference countries
                if c!=0:
                    ee[str(reg)+' - '+str(yea)].loc[:,('RoW Africa',All,sec)] = ( RoW*World.e.loc[:,('RoW Africa',All,sec)].droplevel(0,1).droplevel(0,1) + Ref*World.e.loc[:,(Countries,All,sec)].groupby(level=2, axis=1).median() ).values
                else: # If no reference country is producing in this sector the coefficient is not 0 (i.e. the median of a all 0s sample) but the old African one
                    ee[str(reg)+' - '+str(yea)].loc[:,('RoW Africa',All,sec)] = World.e.loc[:,('RoW Africa',All,sec)]

        else: # The old approach used one single EXIOBASE region for each reference region (see Scenario.xlsx)
            Countries = ref_cou.loc[reg].values
            ee[str(reg)+' - '+str(yea)].loc[:,('RoW Africa',All,All)] = RoW*World.e.loc[:,('RoW Africa',All,All)].values + Ref*World.e.loc[:,(Countries,All,All)].values
    

#%% Running all the Africa Meat (AM) cases
runall = False # In case you need to run a subset of the possible cases

if runall == False:
    Res_name = '' # Insert your new name for the result in case you want to print it
    Years = [2050] # Select your years,...
    Scenarios = ['SSP5'] # ... SSPs...
    Regions = ['Central Europe'] #... and reference regions

Results = pd.DataFrame(0, index=pd.MultiIndex.from_product([Result_Extensions, Regions, Scenarios], names=['Environmental Extension','Reference Region','SSP']), columns=pd.MultiIndex.from_product([Years]))

for yea in Years:
    Reference = copy.deepcopy(World)
    for sce in Scenarios:
        for reg in Regions:
            AM = copy.deepcopy(World) # Every AM case is initialized as a copy of the baseline
            Y_meat = pd.DataFrame(0, index=Meat_Cons['Y_per'].index, columns=Meat_Cons['Y_per'].columns)
            for r in AM.get_index('Region'):
                for i in Meat:
                    if Approach!='Old':
                        AM.Y.loc[(r,All,'Products of meat cattle'),'RoW Africa'] = Meat_Cons['Y_per'].loc[r,'Products of meat cattle']* Projections.loc[(yea,sce,reg,'beef'),'Dry_matter_tonnes'] # Update final demand values cell by cell
                        AM.Y.loc[(r,All,'Products of meat pigs'),'RoW Africa'] = Meat_Cons['Y_per'].loc[r,'Products of meat pigs']* Projections.loc[(yea,sce,reg,'pork'),'Dry_matter_tonnes'] # Update final demand values cell by cell
                        AM.Y.loc[(r,All,'Products of meat poultry'),'RoW Africa'] = Meat_Cons['Y_per'].loc[r,'Products of meat poultry']* Projections.loc[(yea,sce,reg,'poultry'),'Dry_matter_tonnes'] # Update final demand values cell by cell
                    else:
                        AM.Y.loc[(r,All,'Products of meat cattle'),'RoW Africa'] = Old_shares.loc[r,'Products of meat cattle']* Projections.loc[(yea,sce,reg,'beef'),'dry_ton'] # Update final demand values cell by cell
                        AM.Y.loc[(r,All,'Products of meat pigs'),'RoW Africa'] = Old_shares.loc[r,'Products of meat pigs']* Projections.loc[(yea,sce,reg,'pork'),'dry_ton'] # Update final demand values cell by cell
                        AM.Y.loc[(r,All,'Products of meat poultry'),'RoW Africa'] = Old_shares.loc[r,'Products of meat poultry']* Projections.loc[(yea,sce,reg,'poultry'),'dry_ton'] # Update final demand values cell by cell
            
            
            AM.e, Reference.e = ee[str(reg)+' - '+str(yea)], ee[str(reg)+' - '+str(yea)] # Update case-specific satellite account coefficients matrices
                    
            AM.X = MARIO.calc_X_from_w(AM.w, AM.Y.sum(axis=1)) # Compute new production
            AM.Z, Reference.Z = MARIO.calc_Z(AM.z, AM.X), MARIO.calc_Z(Reference.z, Reference.X) # and the new matrix of transaction
            AM.E, Reference.E = MARIO.calc_E(AM.e, AM.X), MARIO.calc_E(Reference.e, Reference.X) # Compute new enivronmental transactions
            AM.f = MARIO.calc_f(AM.e, AM.w) # Compute new footprints
            AM.F = MARIO.calc_F(AM.f, AM.Y.sum(axis=1)) # Compute new footprints
            Electricity_Change = AM.X.loc[(All,All,Ele_sect),:] - World.X.loc[(All,All,Ele_sect),:].values # Change in electricity production by sector and region
            
            # Writing results
            Results.loc[('Land [Mkm2]', reg, sce), (yea)] = (AM.E.loc['Land Use']-Reference.E.loc['Land Use']).sum() / 10**8
            Results.loc[('Water Cons. Blue [BCM]', reg, sce), (yea)] = (AM.E.loc['Water Consumption Blue']-Reference.E.loc['Water Consumption Blue']).sum()/ 1000
            Results.loc[('Water Cons. Green [BCM]', reg, sce), (yea)] = (AM.E.loc['Water Consumption Green']-Reference.E.loc['Water Consumption Green']).sum()/ 1000
            Results.loc[('Electricity [TWh]', reg, sce), (yea)] = Electricity_Change.sum().values/3600
            Results.loc[('Fossil Fuels [EJ]', reg, sce), (yea)] = (AM.E.loc['Fossil Fuels']-Reference.E.loc['Fossil Fuels']).sum() / 10**9
            Results.loc[('GHG [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['Greenhouse gases']-Reference.E.loc['Greenhouse gases']).sum() / 10**9
            Results.loc[('CO2_f [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['Carbon dioxide, fossil to air']-Reference.E.loc['Carbon dioxide, fossil to air']).sum() / 10**9
            Results.loc[('CH4 [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['GHG - CH4']-Reference.E.loc['GHG - CH4']).sum() / 10**9
            Results.loc[('N2O [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['GHG - N2O']-Reference.E.loc['GHG - N2O']).sum() / 10**9
            Results.loc[('CO2_b [GtonCO2_eq]', reg, sce), (yea)] = (AM.E.loc['Carbon dioxide, biogenic to air']-Reference.E.loc['Carbon dioxide, biogenic to air']).sum() / 10**9
            Results.loc[('Eutrop. [MtonPO4_eq]', reg, sce), (yea)] = (AM.E.loc['Eutrophication']-Reference.E.loc['Eutrophication']).sum() / 10**6

if Res_name != '':
    Results.to_csv('Results/'+Res_name+'.csv')
    LUIGI.plot_results(Res_name)
#%% Plot last run case

for i in ['cba','CBA','PBA','pba']:
    MARIO.plot_footprints(AM, i, Extensions, Reg_comp, Meat, project_name='/Impacts/Case_'+str(yea)+'_'+sce+'_'+reg+'_')                  
 
#%% Plot impact of a specific sector and environmental extension

f = LUIGI.assess_impact(AM,Reference,agg_path=r'Aggregations/Fossil_Fuels.xlsx',ext='Fossil Fuels',sec='Extraction of fossil fuels')
f.to_csv('Results/Fossil_Fuel_allocation - Case '+str(yea)+'_'+sce+'_'+reg+'.csv')

