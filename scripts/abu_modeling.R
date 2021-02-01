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

which(names(Edisto_Abundance) == "Pargus pagrus")

ED_AB_bin <- na.exclude(Edisto_Abundance[,c(3,10,12:13,16,17,18,21,23,24,63,80:82,84,85,86)])
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

NSC_AB_bin <- na.exclude(NSC_Abundance[,c(3,10,12:13,16,17,18,19,22,24,25,64,84:88)])


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


NF_AB_bin <- na.exclude(NF_Abundance[,c(3,10,12:13,16,17,18,19,22,24,25,62, 71:75)])

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
# red porgy -----
ggplot(dat, aes(x = loc,  y = Pagrus.pagrus)) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = Pagrus.pagrus)) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = Pagrus.pagrus)) + 
  geom_boxplot(aes(color = reg)) + 
  facet_wrap(~time)
## with log2 + 1 transform
ggplot(dat, aes(x = loc,  y = log2(Pagrus.pagrus + 1))) + 
  geom_boxplot(aes(color = loc))
ggplot(dat, aes(x = loc,  y = log2(Pagrus.pagrus + 1))) + 
  geom_boxplot(aes(color = reg))
ggplot(dat, aes(x = loc,  y = log2(Pagrus.pagrus + 1))) + 
  geom_boxplot(aes(color = reg)) + 
  facet_wrap(~time)

## covariates included with both arithmetic and log2 + 1 abundance
ggpairs(dat[ , c('Topten', 'Pagrus.pagrus', 'SampleHr', 'Relief', 'SubstrateDensity', 'BiotaDensity',
                 'StationDepth', 'Temp')],
        lower = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na =
    "na"),
  upper = list(continuous = "points", combo = "facethist", discrete = "facetbar", na =
    "na"))

dat$logTopten <- log(dat$Topten + 1)
dat$logRp <- log(dat$Pagrus.pagrus + 1)
ggpairs(dat[ , c('logTopten','logRp', 'SampleHr', 'Relief', 'SubstrateDensity', 'BiotaDensity',
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

norm.loglike4 <- function(model, k, y) {
  # function to calculate log likihood of gaussian model for log(abu + k) transformed data
  #https://sakai.unc.edu/access/content/group/2842013b-58f5-4453-aa8d-3e01bacbfc3d/public/Ecol562_Spring2012/docs/lectures/lecture11.htm#checking
  #MLE of sigma^2
  sigma2 <- (sum(residuals(model)^2))/length(y)
  # probability at y = 0 is calculated differently
  prob <- ifelse(y==0, pnorm(log(y+k+.5), mean=predict(model), sd=sqrt(sigma2)), pnorm(log(y+k+.5), mean=predict(model), sd=sqrt(sigma2)) - pnorm(log(y+k-.5), mean=predict(model), sd=sqrt(sigma2)))
  #calculate log-likelihood
  loglike <- sum(log(prob))
  loglike
}

mod_gau <- lm(log(Topten + 1) ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, data=dat)
mod_poi <- glm(Topten ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, family = poisson, data=dat)
mod_ngb <- glm.nb(Topten ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, data=dat)
mod_zip <- zeroinfl(Topten ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp | SampleHr + Temp, data=dat)

# we have to manually calculate AIC for the log(abu + 1) model
LL_gau <- norm.loglike4(mod_gau, 1, dat$Topten)
LL_gau
AIC_gau <- -2 * LL_gau + 2 * mod_gau$rank


AIC(mod_poi)
AIC(mod_ngb)
AIC(mod_zip)
AIC_gau

summary(mod_ngb)
Anova(mod_ngb, type = 3)
anova(glm.nb(dat$Topten ~ 1), mod_ngb)

# so it appears that neg binomial model is most supported zip model is next 
# but it is a long way off in comparison. 

# let's check diagonstic plots 
par(mfrow=c(2,2))
plot(mod_ngb)
# those look troublesome with divergence form normality and heteroscadistity

# not sure if this is completely appropriate
pseudo_r2(mod_ngb)

# PO plot
par(mfrow=c(1,1))
plot(predict(mod_ngb, type = 'response'), dat$Topten)
abline(a=0, b=1)
lines(lowess(predict(mod_ngb, type = 'response'),
             dat$Topten), col='red')

# this shows that there are a lot of very large abundances our model doesn't 
# get close to capturing. The same is true for the other models as well. 

# Red Porgy models ---------
mod_gau_rp <- lm(log(Pagrus.pagrus + 1) ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, data=dat)
mod_poi_rp <- glm(Pagrus.pagrus ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, family = poisson, data=dat)
mod_ngb_rp <- glm.nb(Pagrus.pagrus ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp, data=dat)
mod_zip_rp <- zeroinfl(Pagrus.pagrus ~ reg + loc + time + loc*time + 
                        SampleHr + Relief + SubstrateDensity + BiotaDensity + 
                        StationDepth + Temp | SampleHr + Temp, data=dat)

# we have to manually calculate AIC for the log(abu + 1) model
LL_gau_rp <- norm.loglike4(mod_gau_rp, 1, dat$Pagrus.pagrus)
LL_gau_rp
AIC_gau_rp <- -2 * LL_gau_rp + 2 * mod_gau_rp$rank


AIC(mod_poi_rp)
AIC(mod_ngb_rp)
AIC(mod_zip_rp)
AIC_gau_rp

# negative binomial model is most supported here

summary(mod_ngb_rp)
Anova(mod_ngb_rp, type = 3)
anova(glm.nb(dat$Pagrus.pagrus ~ 1), mod_ngb_rp)

# let's check diagonstic plots 
par(mfrow=c(2,2))
plot(mod_ngb_rp)
# those look troublesome with divergence form normality and heteroscadistity


# not sure if this is completely appropriate
pseudo_r2(mod_ngb_rp)

# PO plot
par(mfrow=c(1,1))
plot(predict(mod_ngb_rp, type = 'response'), dat$Pagrus.pagrus)
abline(a=0, b=1)
lines(lowess(predict(mod_ngb_rp, type = 'response'),
             dat$Pagrus.pagrus), col='red')


# plot output of models ----------------------------------------------

# this section needs to be improved
# we need to decide to we want predictions of a minimal model without covariates
# if not then we need to compute mean covariate at each grouping level and us
# that to make abundance prediction

newdata <- dat[ , c('reg', 'loc', 'time')] %>%
               expand(reg, loc, time)

pred_ngb <- predict(mod_ngb, newdata, se.fit = TRUE)

dat_pred <- data.frame(newdata, mean = pred_ngb$fit, 
                               se = pred_ngb$se.fit)

g1<-ggplot(dat_pred, aes(x=interaction(loc,time), y=mean,
                                            fill=factor(reg))) +
    geom_bar(stat="identity", position=position_dodge(), width = 0.75) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                position = position_dodge(.75), width = 0.2) +
  scale_fill_manual(labels = c("Northern South Carolina", "Edisto", "North Florida"),
                    values=c("grey30", "grey60", "grey90"))+
  coord_cartesian(ylim = c(0, 10)) +
  annotate("text", x = 1:4, y = - 0.1,
           label = rep(c("Outside", "Inside"), 2)) +
  annotate("text", c(1.5, 3.5), y = - 0.3, label = c("Before", "After")) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
       axis.title.x = element_blank(),
       axis.text.x = element_blank(), 
       axis.text=element_text(size=12))+
  guides(fill=guide_legend("")) + 
  scale_y_continuous(expand = c(0, 0)) 

g2 <- ggplot_gtable(ggplot_build(g1))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g2)




### All Topten
All_final_abundance_summary <- ddply(dat, c("reg", "loc", "time"), summarise,
               N    = length(Topten),
               mean = mean((Topten), na.rm=TRUE),
               sd   = sd((Topten), na.rm =TRUE),
               se   = sd / sqrt(N)
)

All_final_abundance_summary$pred <- dat_pred$mean
All_final_abundance_summary$predsd <- dat_pred$se

dodge <- position_dodge(width=0.9) 

g3<-ggplot(All_final_abundance_summary, aes(x=interaction(loc,time), y=mean, fill=factor(reg))) + geom_bar(stat="identity", position=position_dodge(), width = 0.75)+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge(.75), width = 0.2)+
  scale_fill_manual(labels = c("Northern South Carolina", "Edisto", "North Florida"),values=c("grey30", "grey60", "grey90"))+
  coord_cartesian(ylim = c(0, 10)) +
  annotate("text", x = 1:4, y = - 0.1,
           label = rep(c("Outside", "Inside"), 2)) +
  annotate("text", c(1.5, 3.5), y = - 0.3, label = c("Before", "After")) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
       axis.title.x = element_blank(),
       axis.text.x = element_blank(), 
       axis.text=element_text(size=12))+
  guides(fill=guide_legend("")) + 
  scale_y_continuous(expand = c(0, 0)) 

g4 <- ggplot_gtable(ggplot_build(g3))
g4$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g4)

## Red Porgy graphics ------------------------------------------


