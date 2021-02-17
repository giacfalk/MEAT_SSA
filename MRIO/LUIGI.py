# -*- coding: utf-8 -*-
"""
Created on Tue Feb  2 11:53:35 2020

@author: nigolred
"""

def reference_countries(region):
    import pandas as pd

    reg1 = pd.read_excel(r'Inputs\Support Information for MRIO\Countries.xlsx', sheet_name='UN to EXIOBASE', index_col=[0])
    reg2 = pd.read_excel(r'Inputs\Support Information for MRIO\Countries.xlsx', sheet_name='UN to Reference', index_col=[1])
    
    reg3 = pd.DataFrame(None, index=reg1.index, columns=['EXIOBASE'])
    
    # Build mapping vector UN to EXIOBASE
    for i in reg3.index:
        for j in reg1.columns:
            if reg1.loc[i,j] == 1:
                reg3.loc[i,'EXIOBASE'] = j
    
    #Hybrid exio does not have explicit Taiwan
    reg3.loc['Taiwan'] = 'RoW Asia and Pacific'
    
    # Complementing reference regions with respective EXIOBASE region
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
        
    return reg2.reset_index().loc[:,('Region','EXIOBASE')].set_index('Region').loc[region].drop_duplicates()
            

    
def plot_projections(projections,what,title='Projections'):
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots

    years = list(dict.fromkeys(list(projections.index.get_level_values(0))))
    ssp_scen = list(dict.fromkeys(list(projections.index.get_level_values(1))))
    reg_scen = list(dict.fromkeys(list(projections.index.get_level_values(2))))
    meat_type = list(dict.fromkeys(list(projections.index.get_level_values(3))))

    fig = make_subplots(rows=len(meat_type), cols=1, subplot_titles=meat_type, vertical_spacing=0.05)
    colors = ['#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000']
    sl = True
    for s in ssp_scen:
        for r in reg_scen:
            for m in meat_type:
                fig.add_trace(go.Scatter(x=years,
                                         y=projections.loc[(years,s,r,m),what],
                                         mode='lines+markers',
                                         marker_color=colors[ssp_scen.index(s)],
                                         showlegend=sl,
                                         legendgroup=s,
                                         name=str(s)+' - '+str(r)), row=meat_type.index(m)+1, col=1)
    
    fig.update_layout(font_family='Palatino Linotype', title=title)
    fig.update_traces(marker=dict(line=dict(width=1, color='DarkSlateGrey')))
    fig.write_html(r'Inputs/From Demand Model/'+title+'.html')
    
def plot_results(result_file_name):
    import pandas as pd 
    result = pd.read_csv(r'Results/'+result_file_name+'.csv', index_col=[0,1,2])
    
    Ext = ['Land [Mkm2]', 'Water Cons. Blue [BCM]', 'Water Cons. Green [BCM]', 'Fossil Fuels [EJ]','Electricity [TWh]', 'GHG [GtonCO2_eq]', 
                  'CO2_f [GtonCO2_eq]','Eutrop. [MtonPO4_eq]']
    
    reg_scen = list(dict.fromkeys(list(result.index.get_level_values(1))))
    reg_scen.remove('Reference Region')
    ssp_scen = list(dict.fromkeys(list(result.index.get_level_values(2))))
    ssp_scen.remove('SSP')
    years = list(result.columns)
    
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    
    
    fig = make_subplots(rows=2, cols=4, subplot_titles=Ext, vertical_spacing=0.05)
    colors = ['#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000']
    sl = True
    row_= 1
    col_= 1
    for e in Ext:
        for r in reg_scen:
            for s in ssp_scen:
                if Ext.index(e)>3:
                    row_ = 2
                    col_ = -3
                fig.add_trace(go.Scatter(x=years,
                                         y=result.loc[(e,r,s)],
                                         mode='lines+markers',
                                         marker_color=colors[ssp_scen.index(s)],
                                         showlegend=sl,
                                         legendgroup=s,
                                         name=str(s)+' - '+str(r)), row=row_, col=col_+Ext.index(e))
            
    fig.update_layout(font_family='Palatino Linotype', title='Environmental impacts associated with meat demand increase in Sub-Saharian Africa')
    fig.update_traces(marker=dict(line=dict(width=1, color='DarkSlateGrey')))
    fig.write_html(r'Results/'+result_file_name+'_plot.html')

def res_allocation(Case, Ref, agg_path, sec, levels=['Sector','Region'], ext=''):
    import MARIO
    import copy
    import pandas as pd
    
    Delta = copy.deepcopy(Case)
    Delta.X = Case.X-Ref.X.values
    Delta.Y = (Case.Y-Ref.Y)
    Delta.E = MARIO.calc_E(Delta.e, Delta.X) # The main matrices are calculated before aggregating
    Delta.F = MARIO.calc_F(MARIO.calc_f(Delta.e, MARIO.calc_w(Delta.z)), Delta.Y.sum(axis=1))
    Delta.Z = Case.Z-Ref.Z.values
    
    print(Delta.Y)
    
    # Reindexing before aggregation
    new_sec_index = list(pd.read_excel(agg_path, sheet_name=levels[0]).iloc[:,1])
    new_reg_index = list(pd.read_excel(agg_path, sheet_name=levels[1]).iloc[:,1])
    new_col_index = pd.MultiIndex.from_product([new_reg_index,new_sec_index])
    Delta.E.columns = new_col_index
    Delta.F.columns = new_col_index
    Delta.Z.columns, Delta.Z.index, Delta.X.index, Delta.Y.index = new_col_index, new_col_index, new_col_index, new_col_index
    Delta.E = Delta.E.groupby(level=[0,1], axis=1).sum()
    Delta.F = Delta.F.groupby(level=[0,1], axis=1).sum()
    Delta.Z = Delta.Z.groupby(level=[0,1], axis=0).sum()
    Delta.Z = Delta.Z.groupby(level=[0,1], axis=1).sum()
    Delta.Y = Delta.Y.groupby(level=[0,1], axis=0).sum()
    Delta.X = Delta.X.groupby(level=[0,1], axis=0).sum()
    
    Delta.b = MARIO.calc_b(Delta.Z, Delta.X)
    Delta.y = MARIO.calc_y(Delta.Y.sum(axis=1), Delta.X)
    
    deltaE = Delta.E.loc[ext]
    deltaF = Delta.F.loc[ext]
    deltaIBA = (Delta.b.loc[(slice(None),sec),:].T*Delta.E.loc[ext,(slice(None),sec)].values).sum(axis=1)

    imp = pd.concat([deltaE,deltaIBA,deltaF], axis=1)
    imp.columns = ['PBA','IBA','CBA']
    
    return imp,Delta.b.loc[(slice(None),sec),:].T,Delta.E.loc[ext,(slice(None),sec)]
    
    