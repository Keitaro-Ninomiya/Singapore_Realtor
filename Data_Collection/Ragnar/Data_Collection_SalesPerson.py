#!/usr/bin/env python
# coding: utf-8

# In[1]:


import requests, zipfile, io
from io import StringIO
import pandas as pd
import os
from datetime import datetime
now=datetime.now()

url = "https://data.gov.sg/dataset/8f357dd4-96f6-4957-8a68-41b28e5e3f2f/download"
r = requests.get(url,stream=True)
z=zipfile.ZipFile(io.BytesIO(r.content))
z.extractall(r"C:\Users\Keitaro Ninomiya\Box\Research Notes (keitaro2@illinois.edu)\Singapore_Realtor\Raw_Data\Folder")


# In[10]:


def update(date):
    dta=pd.read_csv(r"C:\Users\Keitaro Ninomiya\Box\Research Notes (keitaro2@illinois.edu)\Singapore_Realtor\Raw_Data\CEA_Salesperson_Information\dtaAgents.csv")
    dta_temp=dta.drop(columns="Last_Obs")
    dta1=pd.read_csv(r"C:\Users\Keitaro Ninomiya\Box\Research Notes (keitaro2@illinois.edu)\Singapore_Realtor\Raw_Data\Folder\cea-salesperson-information.csv")

    new=pd.concat([dta_temp,dta1]).drop_duplicates(keep=False)
    new["Last_Obs"]=date
    dta=pd.concat([dta,new])
    
    os.remove(r"C:\Users\Keitaro Ninomiya\Box\Research Notes (keitaro2@illinois.edu)\Singapore_Realtor\Raw_Data\Folder\cea-salesperson-information.csv")
    return dta


# In[11]:


dta=update(now.strftime("%m/%d/%Y, %H:%M:%S"))
dta.to_csv(r"C:\Users\Keitaro Ninomiya\Box\Research Notes (keitaro2@illinois.edu)\Singapore_Realtor\Raw_Data\CEA_Salesperson_Information\dtaAgents.csv")


# In[13]:




