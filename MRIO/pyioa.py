# -*- coding: utf-8 -*-
"""
Created on Fri May  8 12:53:35 2020

@author: nigolred
"""

class mrio:
        
    def __init__(self, path, v):
        
        import pandas as pd
        import pymrio
        import datetime
        
        self.v = v
        
        if self.v == 'H':

        # Importing databases if an hybrid MRIO is used
            print('Pyioa is importing an hybrid mrio system. Time='+datetime.datetime.utcnow().strftime("%H:%M:%S"))
        
            self.Z_dis = pd.read_excel(path, sheet_name='HIOT', engine='pyxlsb', header=[0,1,2,3], index_col=[0,1,2,3,4])
            self.Yc_dis = pd.read_excel(path, sheet_name='FD', engine='pyxlsb', header=[0,1,2,3], index_col=[0,1,2,3,4])
            self.E_dis = pd.read_excel(path, sheet_name='AM_extensions_act', engine='pyxlsb', header=[0,1,2,3], index_col=[0,1,2])
            self.EY_dis = pd.read_excel(path, sheet_name='AM_extensions_FD', engine='pyxlsb', header=[0,1,2,3], index_col=[0,1,2])
            
            print('Done. Time='+datetime.datetime.utcnow().strftime("%H:%M:%S"))
        else:
         
        # Importing databases if an hybrid MRIO is used
            print('Pyioa is importing a monetary mrio system. Time='+datetime.datetime.utcnow().strftime("%H:%M:%S"))
           
            if self.v == 3:
                self.data = pymrio.parse_exiobase3(path=path)
            
            if self.v == 2:
                self.data = pymrio.parse_exiobase2(path=path, charact=True, popvector=None)
                
            self.z_dis = self.data.A
            self.Yc_dis = self.data.Y
            self.E_dis = self.data.satellite.F
            
            print('Done. Time='+datetime.datetime.utcnow().strftime("%H:%M:%S"))

          
    def calc(self):
        
        import pandas as pd
        import pymrio
        import datetime
        
        if self.v == 'H':
            
        # Calculatin all the relevant matrices 
            print('Pyioa is computing all the matrices. Time='+datetime.datetime.utcnow().strftime("%H:%M:%S"))
            
            self.Y_dis = self.Yc_dis.groupby(axis=1, level=0, sort=False).sum()
            self.X_dis = pymrio.calc_x(self.Z_dis, self.Y_dis)
            self.z_dis = pymrio.calc_A(self.Z_dis, self.X_dis)
            self.l_dis = pymrio.calc_L(self.z_dis)
            self.e_dis = pymrio.calc_S(self.E_dis, self.X_dis)
            self.f_dis = pd.DataFrame(pymrio.calc_M(self.e_dis.values, self.l_dis), index=self.e_dis.index, columns=self.l_dis.columns).groupby(sort=False, axis=0, level=0).sum()
            self.f_dis_tot = pd.DataFrame(pymrio.calc_M(self.e_dis.values, self.l_dis), index=self.e_dis.index, columns=self.l_dis.columns)
            
            
            print('Done. Time='+datetime.datetime.utcnow().strftime("%H:%M:%S"))

    def agg(self, agg):
        
        import pandas as pd
        
        self.Sec_lis = list(pd.read_excel(agg, sheet_name='Sectors').Sec_agg)
        self.Reg_lis = list(pd.read_excel(agg, sheet_name='Regions').Reg_agg)
        self.Z_index = pd.MultiIndex.from_product([self.Reg_lis, self.Sec_lis])
        self.Reg_index = pd.MultiIndex.from_product([self.Reg_lis])
        self.Z_agg = self.Z_dis.set_index(self.Z_index).groupby(axis=0, level=[0,1], sort=False).sum()
        self.Z_agg = self.Z_agg.T.set_index(self.Z_index).groupby(axis=0, level=[0,1], sort=False).sum().T
        self.Y_agg = self.Y_dis.groupby(axis=1, level=0, sort=False).sum()
        self.Y_agg = self.Y_agg.set_index(self.Z_index).groupby(axis=0, level=[0,1], sort=False).sum()
        self.Y_agg = self.Y_agg.T.set_index(self.Reg_index).groupby(axis=0, level=0, sort=False).sum().T
        self.E_agg = self.E_dis.groupby(axis=0, level=1, sort=False).sum()
        self.E_agg = self.E_agg.T.set_index(self.Z_index).groupby(axis=0, level=[0,1], sort=False).sum().T

    
def plot_footprints(footprint, regions_code, regions_name, extensions, units, products, prod_unit, title='Envirnomental footprint per unit of product', file_name=''):
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    
    n = len(extensions)
   
    fig = make_subplots(rows=n, cols=1, subplot_titles=[str(extensions[i]) + ' ['+str(units[i]) + ']' for i in range(len(extensions))])
    colors = ['#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000']
    
    for e in extensions:
        for j in products:
            if extensions.index(e) ==0:
                sl = True
            else:
                sl = False
            fig.add_trace(go.Bar(name=j+' ['+prod_unit.loc[j]+']', legendgroup=j, x=regions_name, y=footprint.loc[e,(regions_code,'Sector',j)].values, marker_color=colors[products.index(j)], showlegend=sl), row=extensions.index(e)+1, col=1)
            
    fig.update_layout(barmode='group', font_family='Palatino Linotype', title=title)
    fig.update_traces(marker=dict(line=dict(width=1, color='DarkSlateGrey')))
    fig.write_html(r'Results/Footprints'+file_name+'.html')
    
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
    fig.write_html(r'Inputs/'+title+'.html')
    
# def plot_results()

        