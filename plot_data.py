#!/usr/bin/env python

"""
Call the R GAM function and make the same final plot in python

That's all folks.
"""
__author__ = "Martin De Kauwe"
__version__ = "1.0 (02.05.2016)"
__email__ = "mdekauwe@gmail.com"

import os
import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import rpy2
from rpy2 import robjects as ro
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
pandas2ri.activate()

fn = "HadCRUT_data.txt"
df = pd.read_csv(fn, sep="   ", header=None, usecols=[0,1],
                  names=["year","temp"], engine='python')

df = df[df.year != 2016]

importr("mgcv")
rdf = pandas2ri.py2ri(df)
ro.globalenv['cru'] = rdf
ro.r('nvals = length(cru$year)')
ro.r('fit=gamm(temp ~ s(year, k=20), data=cru)')

# Check if AR1 model is a better fit as each year isn't truly independent
ro.r('g1 <- gamm(temp~s(year, k=20), data=cru)')
ro.r('''g2 <- gamm(temp~s(year, k=20), data=cru,
                   correlation=corARMA(form=~year, p=1))''')

# AR1 model is best.
print ro.r('anova(g1$lme, g2$lme)')

ro.r('gam <- predict(g1$gam, data=cru)')
ro.r('ar1 <- predict(g2$gam, data=cru)')
ro.r('se <- predict(g2$gam, data=cru, se=TRUE)$se.fit')

# get objects back for plotting
lcl = ro.r('ar1 - 1.96 * se')
ucl = ro.r('ar1 + 1.96 * se')
gam = ro.r('gam')

sns.set_style("ticks")
sns.set_style({"xtick.direction": "in","ytick.direction": "in"})
golden_mean = 0.6180339887498949
width = 9
height = width * golden_mean
fig = plt.figure(figsize=(width, height))
ax1 = fig.add_subplot(111)

ax1.plot(df.year, df.temp, "ko")
ax1.plot(df.year, gam, ls="-", lw=2.5, color="royalblue")
ax1.fill_between(df.year, lcl, ucl, color="lightgrey")
ax1.set_xlabel("Year")
ax1.set_ylabel("Temperature anomaly ($^\circ$C; 1961-1990)")
fig.savefig("HadCRU4_temperature_python.pdf", bbox_inches='tight',
            pad_inches=0.1)
