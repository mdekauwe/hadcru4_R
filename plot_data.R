#!/usr/bin/Rscript

library("ggplot2")

url <- "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/" + \
       "time_series/HadCRUT.4.4.0.0.annual_ns_avg.txt"
had_cru <- read.table(url, fill=TRUE)

# drop the other cols
had_cru <- had_cru[c(1,2)]
names(had_cru)[1] <- "year"
names(had_cru)[2] <- "temp"

require(mgcv)

p <- ggplot(had_cru, aes(year, temp)) +
            geom_point() +
            xlab("Year") +
            ylab("Temperature anomaly (1961-1990) deg C")

p + stat_smooth(method="gam", formula=y~s(x, k=3), size=1)
