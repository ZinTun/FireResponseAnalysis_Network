from igraph import *
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import random
import datetime
import csv

# Read data
df = pd.read_csv("cleaned_data.csv")
df.describe(include="all")
df["ResponseTime_Sec"].describe()
df.isnull().any()
df['location_cat']=df['location_cat'].apply(str)
df["location_cat"].describe()

df['date']=pd.to_datetime(df['date'])
df1=df.loc[:, ['date','location_cat','LOCATIONATASSIGNTIME','ResponseTime_Sec']]
df1.head()
df1.isnull().any()

start=datetime.datetime(2016,5,1)
end=datetime.datetime(2017,6,15)
df1["cat"]="A"
df1["cat"]=np.where(df1['date'] <start, 'B', df1["cat"])
df1["cat"]=np.where((df1['date'] >= start)&(df1['date'] <= end), 'D', df1["cat"])
Degrees= []
Closenesses = []
clusetering_coeffes = []
pageranks = []
eccentricities = []
df_all = []
nodes = []
t=0

for t in range(0,4):
    print(t)

    if(t==0):
        df_2=df1[(df1["cat"]=="B")]
        fil="B"

    elif(t==1):
        df_2=df1[(df1["cat"]=="D")]
        fil="D"

    elif(t==2):
        df_2=df1[(df1["cat"]=="A")]
        fil="A"
        
    elif(t==3):
        df_2=df1
        fil="All"
    

    df_2=df_2.drop(columns=["date","cat"])
    df_3=df_2.groupby(['location_cat','LOCATIONATASSIGNTIME'])['ResponseTime_Sec'].agg(['mean']).reset_index()
    k=pd.concat([df_3[('LOCATIONATASSIGNTIME')],df_3[('location_cat')]])
    node=k.unique()
    edge=df_3[['location_cat','LOCATIONATASSIGNTIME']].values
    G=Graph(directed=True)
    G.add_vertices(node)
    G.add_edges(edge)
    nE=edge.shape[0]
    degree = G.degree()
    nodes.append(node)
    
    Closeness = G.closeness(vertices=None, mode=ALL, cutoff=None, weights=None, normalized=True)
    clusetering_coeff = G.transitivity_local_undirected(vertices=None, mode="zero", weights=None)
    pagerank = G.personalized_pagerank(vertices=None, directed=True, damping=0.85, reset=None, reset_vertices=None, weights=None, implementation="prpack", niter=1000, eps=0.001)
    eccentricity = G.eccentricity(vertices=None, mode=ALL)

    for i in range(nE):
        G.es[i]["avg_time"] = df_3['mean'][i]
    Degrees.append(degree)
    Closenesses.append(Closeness)
    clusetering_coeffes.append(clusetering_coeff)
    pageranks.append(pagerank)
    eccentricities.append(eccentricity)
    
d1=[]    

for t in range(0,4):
    if(t==0):
        fil="B"

    elif(t==1):
        fil="D"

    elif(t==2):
        fil="A"
        
    elif(t==3):
        fil="All"        
    d = pd.DataFrame({'degree':Degrees[t], 'node': nodes[t], 'closeness': Closenesses[t], 
                       'clustering': clusetering_coeffes[t], 'pagerank': pageranks[t], 
                       'eccentricity':eccentricities[t]})
    d['cat']=t+1
    d1.append(d)
   
d2 = pd.concat([d1[0], d1[1],d1[2],d1[3]])
d2.to_csv("network_data.csv", sep=',', encoding='utf-8')
