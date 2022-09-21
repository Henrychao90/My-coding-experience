#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd


# In[21]:


def trade_index(data):
    df = data.copy()
    df = pd.DataFrame(df)
    df.dropna(inplace=True)
    
    ##select the address we need##
    sheet2 = pd.read_csv('sheet2.csv')
    output = pd.DataFrame({'address':sheet2['address']})
    
    global result
    result = pd.DataFrame({'txid':[],'address':[],'type':[],'btc':[],'date':[]})
    
    for ad in output['address']:
        catch = df.loc[df['address']==ad]
        result = pd.concat([result,catch])
        
    ##create No. of tx for each address and  huge duel counter##
    output['n_tx'] = 0
    output['huge_counter'] = 0
    result['tx_add'] = result['txid'] + ',' + result['address']
    full_list = list(set(result['tx_add']))
    full_tx = list(result['txid'])
    
    
    for item in full_list:
        split_list = item.split(',')
        
        if full_tx.count(split_list[0])>10:
            count2 = output.loc[output['address']==split_list[1],'huge_counter']
            count2 +=1 
            output.loc[output['address']==split_list[1],'huge_counter']=count2
            count2 = 0
        
        
        count = output.loc[output['address']==split_list[1],'n_tx']
        count +=1 
        output.loc[output['address']==split_list[1],'n_tx']=count
        count = 0
       
        
    
    ##create overheat warning##
    output['heat'] = 0
    threshold = round((output['n_tx'].mean() + (output['n_tx'].std())*3) , 2)
    output.loc[output['n_tx']>threshold,'heat'] = 1
    
            
            
    
    return output


# In[26]:


d = pd.read_csv('sheet1.csv')
#d2 = d.iloc[1:10000000,:]
a = trade_index(d)


# In[30]:


##output to csv##
a.to_csv('itcost100mins.csv',index=False)

