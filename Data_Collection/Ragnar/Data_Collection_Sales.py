#!/usr/bin/env python
# coding: utf-8

# In[1]:


import requests, zipfile, io
from io import StringIO
import pandas as pd
import os

url = "https://data.gov.sg/dataset/4ca1b4ae-a264-4eb2-940f-7d36db2ebbde/download"
r = requests.get(url,stream=True)
z=zipfile.ZipFile(io.BytesIO(r.content))
z.extractall(r"/home/keitaro2/Singapore_Realtor/Raw_Data/Folder")


# In[2]:


dta=pd.read_csv(r"/home/keitaro2/Singapore_Realtor/Raw_Data/CEA_Salespersons_Property_Transaction_Records/dtaSales.csv")


# In[3]:


def update():
    dta=pd.read_csv(r"/home/keitaro2/Singapore_Realtor/Raw_Data/CEA_Salespersons_Property_Transaction_Records/dtaSales.csv")
    dta1=pd.read_csv(r"/home/keitaro2/Singapore_Realtor/Raw_Data/Folder/cea-salespersons-property-transaction-records-residential.csv")

    new=pd.concat([dta,dta1]).drop_duplicates(keep=False)
    dta=pd.concat([dta,new])
    
    os.remove(r"/home/keitaro2/Singapore_Realtor/Raw_Data/Folder/cea-salespersons-property-transaction-records-residential.csv")
    os.remove(r"/home/keitaro2/Singapore_Realtor/Raw_Data/Folder/metadata-cea-salesperson-residential-transaction-record.txt")
    return dta


# In[4]:


dta=update()
dta.to_csv(r"C:\Users\Keitaro Ninomiya\Box\Research Notes (keitaro2@illinois.edu)\Singapore_Realtor\Raw_Data\CEA_Salesperson_Information\dtaSales.csv")
