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
       geom_hline(yintercept=0.0, linetype="dashed", colour="lightgrey") +
       stat_smooth(method="gam", formula=y~s(x, k=20), size=0.7) +
       theme_bw() +
       theme(aspect.ratio=golden_ratio,
             panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())

# Check if AR1 model is a better fit
g1 <- gamm(temp~s(year, k=20), data=cru)
g2 <- gamm(temp~s(year, k=20), data=cru, correlation=corARMA(form=~year, p=1))

# AR1 model is best...god knows how you plot that instead :P
anova(g1$lme, g2$lme)
