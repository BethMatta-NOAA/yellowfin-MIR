###YFS MARGINAL INCREMENT ANALYSIS

library(tidyverse)
library(ggpubr)
library(lubridate)
library(mgcv)
library(gratia)
library(tidymv)


# LOAD DATA ---------------------------------------------------------------

YFS.MIA <- read.csv(file = "./Data/joined_ncf_yfs_data.csv")


# PREPARE DATA FOR ANALYSIS -----------------------------------------------

## Set up dates
YFS.MIA$date_collected <- ymd_hms(YFS.MIA$date_collected)
YFS.MIA$Month <- month(YFS.MIA$Month, label = FALSE, abbr=FALSE)
YFS.MIA$DOY <- yday(YFS.MIA$date_collected)


# DATA EXPLORATION --------------------------------------------------------

#. t-test for differences between dorsal and ventral marginal increment ratio -----

myt <- t.test(YFS.MIA$DMIR, YFS.MIA$VMIR, paired=TRUE)

#. ID outliers ----

boxplot.stats(YFS.MIA$DMIR)$out
boxplot.stats(YFS.MIA$VMIR)$out

#. Re-check specimens with MIR>1 --------
recheck <- YFS.MIA %>% 
  filter(DMIR > 1 | VMIR > 1 | abs_diff >= 0.3) %>% 
  select(structure_id:VMIR, tray_location, barcode, blind_recheck)


#. Graph data -----

DMIR.boxplot <- ggplot(YFS.MIA, aes(factor(Month), DMIR)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="gray", size=2, width=0.3) +
  labs(x="Month", y="Marginal Increment Ratio", title="Dorsal") +
  theme_pubr(border=TRUE) +
  theme(plot.title = element_text(hjust = 0.5))

VMIR.boxplot <- ggplot(YFS.MIA, aes(factor(Month), VMIR)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="gray", size=2, width=0.3) +
  labs(x="Month", y="Marginal Increment Ratio", title="Ventral") +
  theme_pubr(border=TRUE) +
  theme(plot.title = element_text(hjust = 0.5))

DMIR.stage.boxplot <- ggplot(YFS.MIA, aes(factor(Month), DMIR, color=Stage)) +
  facet_wrap(~Stage, ncol=1) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size=2, width=0.3) +
  labs(x="Month", y="Marginal Increment Ratio", title="Dorsal") +
  theme_pubr(legend="right", border=TRUE) +
  theme(plot.title = element_text(hjust = 0.5))

VMIR.stage.boxplot <- ggplot(YFS.MIA, aes(factor(Month), VMIR, color=Stage)) +
  facet_wrap(~Stage, ncol=1) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(size=2, width=0.3) +
  labs(x="Month", y="Marginal Increment Ratio", title="Ventral") +
  theme_pubr(legend="right", border = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))


# GAMs --------------------------------------------------------------------

# Include autocorrelation for temporal effect
## create new date variable so that date is unique within groups (requirement of corCAR1 argument)
set.seed(48)
my.add <- rnorm(n=nrow(YFS.MIA), sd=0.01)
YFS.MIA$date_corr <- YFS.MIA$date_collected + my.add

#CHECK IF NEED TO LOG TRANSFORM
#hist(YFS.MIA$DMIR)
#qqnorm(YFS.MIA$DMIR)
#qqline(YFS.MIA$DMIR, col="steelblue")

#hist(YFS.MIA$DMIR)
#qqnorm(log(YFS.MIA$DMIR))
#qqline(log(YFS.MIA$DMIR), col="steelblue")

#hist(sqrt(YFS.MIA$DMIR))
#qqnorm(sqrt(YFS.MIA$DMIR))
#qqline(sqrt(YFS.MIA$DMIR), col="steelblue")



#.Models with intrinsic variables-------------------------CHECK HOW MANY KNOTS TO USE
m1a <- gamm(log(DMIR) ~ s(DOY, bs="cc"),
            data=YFS.MIA,
            correlation = corCAR1(form = ~date_corr))
m1b <- gamm(log(DMIR) ~ s(DOY, bs="cc") + Stage,
            data=YFS.MIA,
            correlation = corCAR1(form = ~date_corr))
m1c <- gamm(log(DMIR) ~ s(DOY, bs="cc") + final_age,
            data=YFS.MIA,
            correlation = corCAR1(form = ~date_corr))

m1d <- gamm(log(DMIR) ~ s(DOY, bs="cc", by=factor(Stage)) + Stage, #adds interaction between smooth and factor
            data=YFS.MIA,
            correlation = corCAR1(form = ~date_corr))



#.Model comparison----------------------------------------------------
myBIC <- BIC(m1a$lme, m1b$lme, m1c$lme, m1d$lme)
row.names(myBIC) <- c("m1a", "m1b", "m1c", "m1d")
myBIC <- myBIC[order(myBIC$BIC),]
best.model <- rownames(myBIC)[which.min(myBIC$BIC)]
mysum <- summary(get(best.model)$gam)
myresid <- residuals(get(best.model)$lme, type = "normalized")
#myacf <- acf(myresid, xlab="Lag", ylab="ACF", main="", lag.max = 100)
#mypacf <- pacf(myresid, xlab="Lag", ylab="partial ACF", main="", lag.max = 100)
concurvity(get(best.model)$gam)
appraise(get(best.model)$gam)
draw(get(best.model)$gam, select=1, rug=TRUE, pch=19, cex=0.5)
draw(get(best.model)$gam, select=2, rug=TRUE, pch=19, cex=0.5)

summary(m1d$gam)

# .Get fitted values and plots --------------------------------------------

ggplot(YFS.MIA, aes(DOY, DMIR, color=Stage)) +
  geom_point() +
  geom_smooth(method = "gam", 
              formula = y~s(x, bs="cc"))

#Directly from best model----using tidymv
mypredict <-tidymv::predict_gam(get(best.model)$gam, values = list(DOY=seq(1,366,1)))
mypredict$expfit <- exp(mypredict$fit)

fitted.gam <- ggplot(mypredict, aes(DOY, expfit, color=Stage)) +
  geom_line(size=1) +
  labs(x="Day of year", y="Fitted MIR values", color="Stage") +
  theme_pubr(legend = "right")



#.Get min values---------
juv.predict <- mypredict[mypredict$Stage == "Juvenile",]
juv.min <- which.min(juv.predict$fit)
juv.min.date <- as.Date(juv.min, origin="2016-01-01")

ad.predict <- mypredict[mypredict$Stage == "Adult",]
ad.min <- which.min(ad.predict$fit)
ad.min.date <- as.Date(ad.min, origin="2016-01-01")


#.Get max values---------

juv.max <- which.max(juv.predict$fit)
juv.max.date <- as.Date(juv.max, origin="2016-01-01")

ad.max <- which.max(ad.predict$fit)
ad.max.date <- as.Date(ad.max, origin="2016-01-01")


# .Make dataframe of fitted values ----------------------------------------------------

mypredict <- data.frame(mypredict)
mypredict <- mypredict %>% 
  rename("log.fit" = "fit", 
         "se.log.fit" = "se.fit",
         "backtransformed.fit" = "expfit")


