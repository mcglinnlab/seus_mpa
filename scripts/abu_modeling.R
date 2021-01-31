library(readr)
library(tidyr)
#for summarizing data
library(doBy)
#revlaue
library(dplyr)
#Anova
library(car)
#neg binomial glm
library(MASS)
# zero-inflated model
library(pscl)

# plotting
library(ggplot2)
library(GGally)
library(visreg)
## import data ---------------------------------------------------------------
Edisto_Abundance <- read_csv("./data/Edisto_Abundance.csv")
NSC_Abundance <- read_csv("./data/NSC_Abundance.csv")
NF_Abundance <- read_csv("./data/NFL_Abundance.csv")


## clean up data -----------------------------------------------------------
# this needs to be generalized as the same operations are performed three times
Edisto_Abundance$ManagedArea <- 
  factor(Edisto_Abundance$ManagedArea, levels = c("ED","EDCOMP", "ED_PRE"))
Edisto_Abundance$ManagedArea <- 
  revalue(Edisto_Abundance$ManagedArea, c("ED"="Managed", "EDCOMP"="Unmanaged", "ED_PRE" = "Unmanaged"))
Edisto_Abundance$Relief <- ordered(Edisto_Abundance$Relief, levels = 
                                   c("Low", "Moderate", "High"))
Edisto_Abundance$Relief <- revalue(Edisto_Abundance$Relief, 
                             c("Low"=1, "Moderate" = 2, "High" = 3))
factor(Edisto_Abundance$Relief)
Edisto_Abundance$Relief <- as.numeric(Edisto_Abundance$Relief)


Edisto_Abundance$Period <- "Nothing"
Edisto_Abundance$Period[(Edisto_Abundance$Year > 2015)] <- "2016-2018"

Edisto_Abundance$Period[(Edisto_Abundance$Year<2016)&
                                   (Edisto_Abundance$Year>2012)] <- "2013-2015"

Edisto_Abundance$Period[(Edisto_Abundance$Year<2013)&
                                   (Edisto_Abundance$Year>2009)] <- "2010-2012"

Edisto_Abundance$Period[(Edisto_Abundance$Year>2005)&
                                   (Edisto_Abundance$Year<2009)] <- "2006-2008"

Edisto_Abundance$Period[(Edisto_Abundance$Year<2006)&
                                   (Edisto_Abundance$Year>2002)] <- "2003-2005"

Edisto_Abundance$Period[(Edisto_Abundance$Year<2003)] <- "2000-2002"

Edisto_Abundance <- Edisto_Abundance[Edisto_Abundance$Period!="Nothing",]

Edisto_Abundance$Period <- ordered(Edisto_Abundance$Period, levels = 
                                      c("2000-2002", "2003-2005", "2006-2008", 
                                        "2010-2012", "2013-2015", "2016-2018"))

Edisto_Abundance$Topten <- rowSums(Edisto_Abundance[,c(27, 30, 34, 42, 48,
                                                       55, 61, 63, 72, 76)])

Edisto_Abundance$abu <- rowSums(Edisto_Abundance[ , 26:79])
ED_AB_bin <- na.exclude(Edisto_Abundance[,c(3,10,12:13,16,17,18,21,23,24,80:82,84,85,86)])
##

NSC_Abundance$ManagedArea <- factor(NSC_Abundance$ManagedArea, levels = 
                                      c("SC","NSCCOMP", "SC_PRE"))
NSC_Abundance$ManagedArea <- revalue(NSC_Abundance$ManagedArea, 
                                     c("SC"="Managed", "NSCCOMP"="Unmanaged", "SC_PRE" = "Unmanaged"))
NSC_Abundance$Relief <- ordered(NSC_Abundance$Relief, levels = 
                                   c("Low", "Moderate", "High"))
NSC_Abundance$Relief <- revalue(NSC_Abundance$Relief, 
                             c("Low"=1, "Moderate" = 2, "High" = 3))
factor(NSC_Abundance$Relief)
NSC_Abundance$Relief <- as.numeric(NSC_Abundance$Relief)

NSC_Abundance$Period <- "Nothing"
NSC_Abundance$Period[(NSC_Abundance$Year > 2015)] <- "2016-2018"

NSC_Abundance$Period[(NSC_Abundance$Year<2016)&
                                   (NSC_Abundance$Year>2012)] <- "2013-2015"

NSC_Abundance$Period[(NSC_Abundance$Year<2013)&
                                   (NSC_Abundance$Year>2009)] <- "2010-2012"

NSC_Abundance$Period[(NSC_Abundance$Year>2005)&
                                   (NSC_Abundance$Year<2009)] <- "2006-2008"

NSC_Abundance$Period[(NSC_Abundance$Year<2006)&
                                   (NSC_Abundance$Year>2002)] <- "2003-2005"

NSC_Abundance$Period[(NSC_Abundance$Year<2003)] <- "2000-2002"

NSC_Abundance <- NSC_Abundance[NSC_Abundance$Period!="Nothing",]

NSC_Abundance$Period <-  ordered(NSC_Abundance$Period, levels = c("2000-2002", "2003-2005", "2006-2008", "2010-2012", "2013-2015", "2016-2018"))

NSC_Abundance$Topten <- rowSums(NSC_Abundance[,c(28, 30, 33, 34, 42, 45, 51, 
                                                 63, 64, 72)])

NSC_Abundance$abu <- rowSums(NSC_Abundance[ , 27:83])

NSC_AB_bin <- na.exclude(NSC_Abundance[,c(3,10,12:13,16,17,18,19,22,24,25,84:88)])


##
NF_Abundance$ManagedArea <- factor(NF_Abundance$ManagedArea, levels = c("NF","NFCOMP","NF_PRE"))
NF_Abundance$ManagedArea <- revalue(NF_Abundance$ManagedArea, c("NF"="Managed", "NFCOMP"="Unmanaged", "NF_PRE"="Unmanaged"))
NF_Abundance$Relief <- ordered(NF_Abundance$Relief, levels = 
                                   c("Low", "Moderate", "High"))
NF_Abundance$Relief <- revalue(NF_Abundance$Relief, 
                             c("Low"=1, "Moderate" = 2, "High" = 3))
factor(NF_Abundance$Relief)
NF_Abundance$Relief <- as.numeric(NF_Abundance$Relief)


NF_Abundance$Period <- "Nothing"
NF_Abundance$Period[(NF_Abundance$Year > 2015)] <- "2016-2018"

NF_Abundance$Period[(NF_Abundance$Year<2016)&
                                   (NF_Abundance$Year>2012)] <- "2013-2015"

NF_Abundance$Period[(NF_Abundance$Year<2013)&
                                   (NF_Abundance$Year>2009)] <- "2010-2012"

NF_Abundance$Period[(NF_Abundance$Year>2005)&
                                   (NF_Abundance$Year<2009)] <- "2006-2008"

NF_Abundance$Period[(NF_Abundance$Year<2006)&
                                   (NF_Abundance$Year>2002)] <- "2003-2005"

NF_Abundance$Period[(NF_Abundance$Year<2003)] <- "2000-2002"

NF_Abundance <- NF_Abundance[NF_Abundance$Period!="Nothing",]

NF_Abundance$Period <-  ordered(NF_Abundance$Period, levels = c("2000-2002", "2003-2005", "2006-2008", "2010-2012", "2013-2015", "2016-2018"))


NF_Abundance$Topten <- rowSums(NF_Abundance[,c(27, 32, 38, 39, 53, 60, 62, 66, 68, 69)])

NF_Abundance$abu <- rowSums(NF_Abundance[ , 27:70])


NF_AB_bin <- na.exclude(NF_Abundance[,c(3,10,12:13,16,17,18,19,22,24,25,71:75)])

# so sampling was low in NF prior to 2009
NF_AB_bin <- NF_AB_bin[NF_AB_bin$Year > 2009, ]


dat <- data.frame(rbind(ED_AB_bin, NSC_AB_bin, NF_AB_bin))

# define region variable
dat$reg <- substr(dat$ManagedAreaBar,start = 1, stop = 2)
dat$reg <- factor(ifelse(dat$reg == 'NS', 'NSC', dat$reg),
                  levels = c('NSC', 'ED', 'NF'))
# define spatial grouping variable: notMPA (outside MPA) vs MPA (inside MPA) 
dat$loc <- factor(ifelse(grepl('COMP', dat$ManagedAreaBar), 'notMPA', 'MPA'), 
                  levels = c('notMPA', 'MPA'))
# define temporal grouping variable: before (prior to 2009) vs after (after 2009)
dat$time <- factor(ifelse(dat$Year < 2009, 'before', 'after'), 
                   levels = c('before', 'after'))

head(dat)

# graphical exploration ------------------------------------------------------
# topten ------
ggplot(dat, aes(x = loc,  y = Topten)) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = Topten)) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = Topten)) + 
  geom_boxplot(aes(color = reg)) + 
  facet_wrap(~time)
## with log2 + 1 transform
ggplot(dat, aes(x = loc,  y = log2(Topten + 1))) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = log2(Topten + 1))) + 
  geom_boxplot(aes(color = reg))
ggplot(dat, aes(x = loc,  y = log2(Topten + 1))) + 
  geom_boxplot(aes(color = reg)) + 
  facet_wrap(~time)
# all fish -----
ggplot(dat, aes(x = loc,  y = abu)) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = abu)) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = abu)) + 
  geom_boxplot(aes(color = reg)) + 
  facet_wrap(~time)
## with log2 + 1 transform
ggplot(dat, aes(x = loc,  y = log2(abu + 1))) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = log2(abu + 1))) + 
  geom_boxplot(aes(color = reg))
ggplot(dat, aes(x = loc,  y = log2(abu + 1))) + 
  geom_boxplot(aes(color = reg)) + 
  facet_wrap(~time)

## covariates included with both arithmetic and log2 + 1 abundance
dat$Toptenl2 <- log2(dat$Topten + 1)
dat$abul2 <- log2(dat$abu + 1)
ggpairs(dat[ , c('Topten', 'Toptenl2','abu', 'SampleHr', 'Relief', 'SubstrateDensity', 'BiotaDensity',
                 'StationDepth', 'Temp')],
        lower = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na =
    "na"),
  upper = list(continuous = "points", combo = "facethist", discrete = "facetbar", na =
    "na"))

# tells us that none of the covariates really are home runs with abundance or its log transform
# there is some strong correlations between covariates 
# substrate density and biota density are highly correlated (0.85) 
# so including one or the other is probably fine if model needs to be simplified

# model fitting ----------------------------------------------------------------

pseudo_r2 = function(glm_mod) {
    1 -  glm_mod$deviance / glm_mod$null.deviance
}

mod_gau <- glm(Toptenl2 ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, family = gaussian, data=dat)
mod_poi <- glm(Topten ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, family = poisson, data=dat)
mod_ngb <- glm.nb(Topten ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, data=dat)
mod_zip <- zeroinfl(Topten ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp | SampleHr + Temp, data=dat)
AIC(mod_gau)
AIC(mod_poi)
AIC(mod_ngb)
AIC(mod_zip)

# it appears the gaussian model with log2 + 1 transform is best fit according to AIC
par(mfrow=c(2,2))
plot(mod_gau)
# better looking than the other models but still some drammatic violations in 
# regression assumptions. The 

# PO plot
par(mfrow=c(1,1))
plot(predict(mod_gau), dat$Toptenl2)
abline(a=0, b=1)
lines(lowess(predict(mod_gau), dat$Toptenl2), col='red')

par(mfrow=c(3,3))
termplot(mod_gau, partial.resid = T, se = T)

summary(mod_gau)
pseudo_r2(mod_gau)
Anova(mod_gau, type = 3)

## simplify approach further
cov_mod <- glm(Toptenl2 ~ SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                       StationDepth + Temp, family = gaussian, data=dat)
pseudo_r2(cov_mod)
mpa_mod <- glm(residuals(cov_mod) ~ reg + loc + time + loc*time, data = dat)
summary(mpa_mod)
Anova(mpa_mod, type =3)
pseudo_r2(mpa_mod)

## least conservative approach
mpa_mod <-  glm(Toptenl2 ~ reg + loc + time + loc*time, data = dat)
summary(mpa_mod)
Anova(mpa_mod, type =3)
pseudo_r2(mpa_mod)

# only about 1% diff than more conservative model
par(mfrow=c(2,2))
termplot(mpa_mod, partial.resid = T, se = T)


# diagonstic plot
#plot(predict(mod_zip), residuals(mod_zip))
#lines(lowess(predict(mod_zip), residuals(mod_zip)), col = 'red')

###
mod_gau <- glm(abul2 ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, family = gaussian, data=dat)
mod_poi <- glm(abu ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, family = poisson, data=dat)
mod_ngb <- glm.nb(abu ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, data=dat)
mod_zip <- zeroinfl(abu ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp | SampleHr + Temp, data=dat)
AIC(mod_gau)
AIC(mod_poi)
AIC(mod_ngb)
AIC(mod_zip)

# PO plot
par(mfrow=c(1,1))
plot(predict(mod_ngb), dat$abu)
abline(a=0, b=1)
lines(lowess(predict(mod_poi), dat$abu), col='red')


summary(mod_gau)
pseudo_r2(mod_ngb)
Anova(mod_gau, type = 3)
