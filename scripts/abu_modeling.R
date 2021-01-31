library(readr)
library(tidyr)
##for summarizing data
library(doBy)
###revlaue
library(plyr)
# plotting
library(ggplot2)
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


ED_AB_bin <- na.exclude(Edisto_Abundance[,c(3,10,12:13,16,17,18,21,23,24,80:82,84,85)])
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



NSC_AB_bin <- na.exclude(NSC_Abundance[,c(3,10,12:13,16,17,18,19,22,24,25,84:87)])


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

NF_AB_bin <- na.exclude(NF_Abundance[,c(3,10,12:13,16,17,18,19,22,24,25,71:74)])

dat <- rbind(ED_AB_bin, NSC_AB_bin, NF_AB_bin)




