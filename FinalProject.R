rm(list=ls())
macro<- read.csv(file = "OliverCanyonData_Macro.csv",header = TRUE)
names(macro)
plot(macro$v)
names(macro)

macro$fsurvey=as.factor(macro$survey)

plot(macro$v~macro$fsurvey)
plot(macro$bg~macro$fsurvey)
plot(macro$l~macro$fsurvey)
hist(macro$v)
hist(macro$bg)
hist(macro$l)

modelv=aov(macro$v~macro$fsurvey)
summary(modelv)
TukeyHSD(modelv)

modelbg=aov(macro$bg~macro$fsurvey)
summary(modelbg)
TukeyHSD(modelbg)

#group species
#species=percent[,8:99]
#species2=percent[2:433,]

#percent cover
percent<-read.csv(file = "OliverCanyonData_PercentCover_T.csv",header = TRUE)
#take columns in percent with n for row 1
speciesdesignation=percent[1,]
#take out that row
percent=percent[-1,]

for(i in 8:98) percent[,i]=as.numeric(percent[,i])
#for each column with na then make 0
for(i in 8:98) percent[which(is.na(percent[,i])),i]=0

#separate native and nonnatives
percent$native=rowSums(percent[,which(speciesdesignation=="n")])
percent$nonnative=rowSums(percent[,which(speciesdesignation=="nn")])
#calculate total and percent cover
percent$total=percent$native+percent$nonnative
percent$nativepercent=percent$native/percent$total
percent$nonnativepercent=percent$nonnative/percent$total

#check that new columns have been added
names(percent)

#look at native and nonnative percent distribution
plot(percent$nativepercent)
hist(percent$nativepercent)
plot(percent$nonnativepercent)
hist(percent$nonnativepercent)
#nonnormal distribution


#season
Jan<-percent[percent$season=="Jan",]
Nov<-percent[percent$season=="Nov",]
C<-percent[percent$season=="c",]
#subset, name of data, variable ==
boxplot(Jan$nativepercent, Nov$nativepercent, C$nativepercent)

#seed pretreatment
PT<-percent[percent$seed.treatment=="pt",]
NPT<-percent[percent$seed.treatment=="npt",]
C2<-percent[percent$seed.treatment=="c",]
boxplot(PT$nativepercent, NPT$nativepercent, C2$nativepercent)
hist(PT$nativepercent)

#tamping
Tamp<-percent[percent$tamping=="t",]
NT<-percent[percent$tamping=="nt",]
C3<-percent[percent$tamping=="c",]
boxplot(Tamp$nativepercent, NT$nativepercent, C3$nativepercent)

#compare survey 1 and 3
survey1<-percent[percent$survey.number=="1",]
boxplot(survey1$nativepercent, survey1$nonnativepercent)

survey3<-percent[percent$survey.number=="3",]
boxplot(survey3$nativepercent, survey3$nonnativepercent, xlab="native, nonnative")



#compare full treatments
boxplot(nativepercent ~ fulltreat, data=percent, main="Native percent cover by treatment")
boxplot(nativepercent ~ fulltreat, data=survey1, main="Native percent cover by treatment")
boxplot(nativepercent ~ fulltreat, data=survey3, main="Native percent cover by treatment")
#looks like high native percent cover in each treatment between surveys 1 and 3

boxplot(nativepercent ~ season, data=percent, main="Native percent cover by season-total")
boxplot(nativepercent ~ season, data=survey1, main="Native percent cover by season-survey1")
boxplot(nativepercent ~ season, data=survey3, main="Native percent cover by season-survey3")

boxplot(nativepercent ~ seed.treatment, data=percent, main="Native percent cover by seed treatment-total")
boxplot(nativepercent ~ seed.treatment, data=survey1, main="Native percent cover by seed treatment-survey1")
boxplot(nativepercent ~ seed.treatment, data=survey3, main="Native percent cover by seed treatment-survey3")

kc####survey 3 only######
#season
Jan2<-survey3[survey3$season=="Jan",]
Nov2<-survey3[survey3$season=="Nov",]
Cb<-survey3[survey3$season=="c",]
#subset, name of data, variable ==
boxplot(Jan2$nativepercent, Nov2$nativepercent, Cb$nativepercent)

#seed pretreatment
PT2<-survey3[survey3$seed.treatment=="pt",]
NPT2<-survey3[survey3$seed.treatment=="npt",]
C2b<-survey3[survey3$seed.treatment=="c",]
#subset, name of data, variable ==
boxplot(PT2$nativepercent, NPT2$nativepercent, C2b$nativepercent)

#Tamping
Tamp2<-percent[percent$tamping=="t",]
NT2<-percent[percent$tamping=="nt",]
C3b<-percent[percent$tamping=="c",]
#subset, name of data, variable ==
boxplot(Tamp2$nativepercent, NT2$nativepercent, C3b$nativepercent)

#order based on 5 most/least prominent species
plot(percent$Deinandra.fasciculata)
dotchart(percent$Deinandra.fasciculata)
qqnorm(percent$Deinandra.fasciculata)
qqline(percent$Deinandra.fasciculata)

#need to pull out percents and presence of native vs nonnative
#want to compare seasonality of all plots
#want to compare tamping in all plots
#want to compare seed pretreatment of all plots
#compare full treatments

###look at this later###


#aggregate-group together by % by full treatment
species=aggregate(percent[,8:99], by=list(percent$fulltreat),sum)


#find the proportion of native to nonnative species in presab
presab<-read.csv(file = "OliverCanyonData_PresAb_T.csv",header = TRUE)
species3=presab[,8:125]


macro2<- read.csv(file = "OliverCanyonData_Macro.csv",header = TRUE)
names(macro2)
macrocover=macro2[1:13,]
names(macrocover)
library(tidyverse)
library(dplyr)

#does not work
#macro2 %>% slice()

#exclude where na
presab.na=presab %>% filter(!is.na(species3))
