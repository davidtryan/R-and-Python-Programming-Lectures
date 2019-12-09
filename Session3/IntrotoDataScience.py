
# coding: utf-8

# In[6]:

## Taken from: http://twiecki.github.io/blog/2014/11/18/python-for-data-science/


# In[1]:

print ('Hello World')


# In[2]:

# http://twiecki.github.io/blog/2014/11/18/python-for-data-science/
from IPython.display import YouTubeVideo
YouTubeVideo('wxVx54ax47s') #Yes, it can also embed youtube videos.


## Pandas

# Normally, people recommend you start by learning NumPy (pronounced num-pie, not num-pee!) which is the library that provides multi-dimensional arrays. Certainly this was the way to go a few years ago but I hardly use NumPy at all today. The reason is that NumPy became more of a core library that's used by other libraries which provide a much nicer interface. Thus, the main library to use for working with data is Pandas. It can input and output data from all kinds of formats (including databases), do joins and other SQL-like functions for shaping the data, handle missing values with ease, support time series, has basic plotting capabilities and basic statistical functionality and much more. There is certainly a learning curve to all its features but I strongly suggest you go through most of the  documentation as a first step. Trust me, the time you invest will be set off a thousand fold by being more efficient in your data munging. Here are a few quick tricks to whet your appetite:

# In[7]:

import pandas as pd

df = pd.DataFrame( { 'A' : 1.,
                     'B' : pd.Timestamp('20130102'),
                     'C' : pd.Series(1, index=list(range(4)),dtype='float32'),
                     'D' : pd.Series([1,2,1,2], dtype='int32'),
                     'E' : pd.Categorical(['test','train','test','train']),
                     'F' : 'foo'})


# In[8]:

df


# In[9]:

df.B


# In[10]:

df.groupby('E').sum().D


## Statistically meaningful plots

# The main plotting library of Python is Matplotlib. However, I don't recommend using it directly for the same reason I don't recommend spending time learning NumPy initially. While Matplotlib is very powerful, it is its own jungle and requires lots of tweaking to make your plots look shiny. So instead, I recommend to start using Seaborn. Seaborn essentially treats Matplotlib as a core library (just like Pandas does with NumPy). I will briefly illustrate the main benefits of seaborn. Specifically, it:
# 
# creates aesthetically pleasing plots by default (for one thing, it does not default to the jet colormap),
# creates statistically meaningful plots, and
# understands the pandas DataFrame so the two work well together.
# While pandas comes prepackaged with anaconda, seaborn is not directly included but can easily be installed with conda install seaborn.

# In[13]:

#IPython magic to create plots within cells
get_ipython().magic(u'matplotlib inline')

%matplotlib inline

# In[16]:

import seaborn as sns

#Load one of the datasets that comes with seaborn
tips = sns.load_dataset("tips")

sns.jointplot('total_bill', 'tip', tips, kind='reg');


# As you can see, with just one line we create a pretty complex statistical plot including the best fitting linear regression line along with confidence intervals, marginals and the correlation coefficients. Recreating this plot in matplotlib would take quite a bit of (ugly) code, including calls to scipy to run the linear regression and manually applying the linear regression formula to draw the line (and I don't even know how to do the marginal plots and confidence intervals from the top of my head). This and the next example are taken from the tutorial on quantitative linear models.

## Dataframes

# Data has structure. Often, there are different groups or categories we are interested in (pandas' groupby functionality is amazing in this case). For example, the tips data set looks like this:

# In[17]:

tips.head()


# We might ask if smokers tip differently than non-smokers. Without seaborn, this would require a pandas groupby together with the complex code for plotting a linear regression. With seaborn, we can provide the column name we wish to split by as a keyword argument to col:

# In[18]:

sns.lmplot('total_bill', 'tip', tips, col='smoker');


# In[ ]:



