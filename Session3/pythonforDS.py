# -*- coding: utf-8 -*-
"""
Created on Thu Feb 05 14:27:17 2015

@author: davidryan
"""

"""
 focus on the 10% of tools that allow you to do 90% of the work. After you mastered these essentials you can browse the long lists of PyData packages to decide which to try next.
 """
 
 
# https://www.kaggle.com/wiki/GettingStartedWithPythonForDataScience
# http://www.analyticsvidhya.com/blog/2014/08/baby-steps-python-performing-exploratory-analysis-python/

# Pandas in python work with dataframes

####################################

## Series and dataframes

#

import pylab as py
import numpy as np
import matplotlib as plt

x = range(-10,11)
y = []

a = 2
b = 3
c = 10

for xi in x:
    y.append(a * xi ** 2 + b * xi + c)
    
plot(x,y)
pylab.plot(x,y)

grid()

xlabel('$x$')
ylabel('$y$')
title('my parabola)