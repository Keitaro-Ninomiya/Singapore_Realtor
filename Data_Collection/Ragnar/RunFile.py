#!/usr/bin/env python
# coding: utf-8

# In[1]:


import subprocess
import schedule
import time

def job():
    subprocess.call(["python", "Data_Collection_Sales.py"])
    subprocess.call(["python", "Data_Collection_SalesPerson.py"])
    
schedule.every(8).hours.do(job)

while True:
    schedule.run_pending()
    time.sleep(1)

