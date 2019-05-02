
# coding: utf-8

# In[1]:


import numpy as np


# In[2]:


import pandas as pd


# In[3]:


data = pd.read_csv("CIT_Growth_Rate.csv")


# In[4]:


year = data.FY


# In[5]:


cit = data.CIT_REVENUE_Growth_Rate


# In[6]:


exrate = data.EXCHANGE_RATE


# In[7]:


bopin = data.BOP_INFLOWS_Growth_Rate


# In[8]:


bopout = data.BOP_OUTFLOWS_Growth_Rate


# In[9]:


lendrate = data.LENDING_RATE


# In[10]:


taxrate = data.CIT_RATE


# In[11]:


gdpcap = data.GDP_PERCAPITA_Growth_Rate


# In[12]:


inf = data.INFLATION_RATE


# In[13]:


urpopinc = data.URBAN_POPULATION_GROWTH_RATE


# In[14]:


from statsmodels.formula.api import ols


# In[15]:


from statsmodels.stats.anova import anova_lm


# In[16]:


model = ols("cit~exrate + bopin + bopout + lendrate + taxrate + gdpcap + urpopinc",data).fit()


# In[17]:


predict = model.predict()


# In[18]:


df = pd.DataFrame(predict)


# In[19]:


df.to_csv("Predicted_CIT_Growth_Rate.csv")

# In[20]:

anovatable = anova_lm(model)

# In[21]:

summary = model.summary()

# In[22]:
print('ANOVA TABLE:-')
print(anovatable)
print('Results:-')
print(summary)
