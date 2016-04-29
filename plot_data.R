#!/usr/bin/Rscript

library("ggplot2")
require(mgcv)

golden_ratio <- 1.0 / 1.6180339887

url <- "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/" + "
       "time_series/HadCRUT.4.4.0.0.annual_ns_avg.txt"
cru <- read.table(url, fill=TRUE)

# drop the other cols
cru <- cru[c(1,2)]
names(cru)[1] <- "year"
names(cru)[2] <- "temp"

ggplot(cru, aes(year, temp)) +
       geom_point(size=0.8) +
       xlab("Year") +
       ylab("Temperature anomaly (1961-1990; deg C)") +
       theme(aspect.ratio=golden_ratio) +
       stat_smooth(method="gam", formula=y~s(x), size=1)
