#!/usr/bin/Rscript
library("ggplot2")
require(mgcv)
require(gridExtra)

fn <- "HadCRUT_data.txt"
cru <- read.table(url, fill=TRUE)

# drop the other cols
cru <- cru[c(1,2)]
names(cru)[1] <- "year"
names(cru)[2] <- "temp"

golden_ratio <- 1.0 / 1.6180339887
ax1 <- ggplot(cru, aes(year, temp)) +
         geom_point(size=0.8) +
         xlab("") +
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

# AR1 model is best.
anova(g1$lme, g2$lme)

gam <- predict(g1$gam, data=cru)$fit
ar1 <- predict(g2$gam, data=cru)
se <- predict(g2$gam, data=cru, se=TRUE)$se.fit
df <- with(cru, data.frame(year=seq(min(year), max(year), length=length(year)),
                           temp=ar1,
                           lcl=ar1 - 1.96 * se,
                           ucl=ar1 + 1.96 * se))
df2 <- with(cru, data.frame(year=seq(min(year), max(year), length=length(year)),
                           temp=gam))


ax2 <- ggplot() +
         geom_point(data=cru, aes(year, temp), size=0.8) +
         geom_line(data=df, aes(year, temp), size=0.8, colour="blue") +
         geom_line(data=df2, aes(year, temp), size=0.8, colour="lightgreen") +
         geom_ribbon(data=df, aes(year, ymin=lcl, ymax=ucl),
                     fill="grey", alpha=.4) +
         ylab(expression(Temperature~anomaly~(~degree~C)~1961-1990)) +
         geom_hline(yintercept=0.0, linetype="dashed", colour="lightgrey") +
         stat_smooth(method="gam", formula=y~s(x, k=20), size=0.7) +
         theme_bw() +
         theme(aspect.ratio=golden_ratio,
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())

grid.arrange(ax1, ax2, ncol=1)
