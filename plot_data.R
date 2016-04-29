#!/usr/bin/Rscript

library("ggplot2")
require(mgcv)

url <- "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/" + \
       "time_series/HadCRUT.4.4.0.0.annual_ns_avg.txt"
had_cru <- read.table(url, fill=TRUE)

# drop the other cols
had_cru <- had_cru[c(1,2)]
names(had_cru)[1] <- "year"
names(had_cru)[2] <- "temp"

g <-gam(year~s(temp), data=had_cru)
#gam.check(g)

ggplot(had_cru, aes(year, temp)) +
       geom_point(size=0.8) +
       xlab("Year") +
       ylab("Temperature anomaly (1961-1990) deg C") +
       stat_smooth(method="gam", formula=y~s(x), size=1)
