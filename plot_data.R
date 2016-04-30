#!/usr/bin/Rscript
library("ggplot2")
require(mgcv)

fn <- "HadCRUT_data.txt"
cru <- read.table(url, fill=TRUE)

# drop the other cols
cru <- cru[c(1,2)]
names(cru)[1] <- "year"
names(cru)[2] <- "temp"

golden_ratio <- 1.0 / 1.6180339887
ggplot(cru, aes(year, temp)) +
       geom_point(size=0.8) +
       xlab("Year") +
       ylab(expression(Temperature~anomaly~(~degree~C)~1961-1990)) +
       stat_smooth(method="gam", formula=y~s(x, k=20), size=0.7) +
       theme_bw() +
       theme(aspect.ratio=golden_ratio,
             panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())
