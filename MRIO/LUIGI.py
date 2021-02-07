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
    
        