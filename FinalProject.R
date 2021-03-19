###############################################
#comparing macro plot data of litter, bareground, vegetation, and rocks at each site
rm(list=ls())
macro<- read.csv(file = "OliverCanyonData_Macro.csv",header = TRUE)
names(macro)
plot(macro$v)

#create grouping factor for each survey
macro$fsurvey=as.factor(macro$survey)

#plot the different cover types over time (survey number)
plot(macro$v~macro$fsurvey)
plot(macro$bg~macro$fsurvey)
plot(macro$l~macro$fsurvey)
#look at distribution of each cover type
hist(macro$v)
hist(macro$bg)
hist(macro$l)

#check if vegetation cover is statisically different from each other
modelv=aov(macro$v~macro$fsurvey)
summary(modelv)
#see which treatments differ from each other
TukeyHSD(modelv)
#all are statistically different from each other

#check if bare ground cover is statistically different from each other
modelbg=aov(macro$bg~macro$fsurvey)
summary(modelbg)
#see which treatments differ from each other
TukeyHSD(modelbg)
#all are statistically different from each other

#check if litter cover is statistically different from each other
modell=aov(macro$l~macro$fsurvey)
summary(modell)
#see which treatments differ from each other
TukeyHSD(modell)
#all are statistically different from each other


#####Percent cover dataset######################
percent<-read.csv(file = "OliverCanyonData_PercentCover_T.csv",header = TRUE)
#take columns in percent with n for row 1
speciesdesignation=percent[1,]
#take out that row
percent=percent[-1,]

for(i in 8:98) percent[,i]=as.numeric(percent[,i])
#for each column with na then make 0
for(i in 8:98) percent[which(is.na(percent[,i])),i]=0

#separate native and nonnatives, creates new columns
percent$native=rowSums(percent[,which(speciesdesignation=="n")])
percent$nonnative=rowSums(percent[,which(speciesdesignation=="nn")])

#calculate total and percent cover, create new columns
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


#season, create matrix of percent values grouped by sowing season
Jan<-percent[percent$season=="Jan",]
Nov<-percent[percent$season=="Nov",]
C<-percent[percent$season=="c",]
#subset, name of data, variable ==
boxplot(Jan$nativepercent, Nov$nativepercent, C$nativepercent)

#seed pretreatment, create matrix of percent values grouped by seed pretreatment
PT<-percent[percent$seed.treatment=="pt",]
NPT<-percent[percent$seed.treatment=="npt",]
C2<-percent[percent$seed.treatment=="c",]
boxplot(PT$nativepercent, NPT$nativepercent, C2$nativepercent)
hist(PT$nativepercent)

#tamping, create matrix of percent values grouped by tamping treatment
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
boxplot(nativepercent ~ fulltreat, data=survey1, main="Native percent cover by treatment, Survey 1")
boxplot(nativepercent ~ fulltreat, data=survey3, main="Native percent cover by treatment, Survey 3")
#looks like high native percent cover in each treatment between surveys 1 and 3

boxplot(nativepercent ~ season, data=percent, main="Native percent cover by season-total")
boxplot(nativepercent ~ season, data=survey1, main="Native percent cover by season-survey1")
boxplot(nativepercent ~ season, data=survey3, main="Native percent cover by season-survey3")

boxplot(nativepercent ~ seed.treatment, data=percent, main="Native percent cover by seed treatment-total")
boxplot(nativepercent ~ seed.treatment, data=survey1, main="Native percent cover by seed treatment-survey1")
boxplot(nativepercent ~ seed.treatment, data=survey3, main="Native percent cover by seed treatment-survey3")

####Survey 3 only######
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
Tamp2<-survey3[survey3$tamping=="t",]
NT2<-survey3[survey3$tamping=="nt",]
C3b<-survey3[survey3$tamping=="c",]
#subset, name of data, variable ==
boxplot(Tamp2$nativepercent, NT2$nativepercent, C3b$nativepercent)

######ANOVA############
###season
#Native % cover
survey3$seasonf=as.factor(survey3$season)
model1=lm(survey3$nativepercent~survey3$seasonf)
anova(model1)
TukeyHSD(aov(model1))

boxplot(survey3$nonnativepercent~survey3$seasonf)
boxplot(survey3$nativepercent~survey3$seasonf)

#There was a significant difference between the native percent cover of seeds sown in jan vs nov.

###seed treatment
#Native %cover
survey3$seed.treatmentf=as.factor(survey3$seed.treatment)
model2=lm(survey3$nativepercent~survey3$seed.treatmentf)
anova(model2)
TukeyHSD(aov(model2))

boxplot(survey3$nativepercent~survey3$seed.treatmentf)
boxplot(survey3$nonnativepercent~survey3$seed.treatmentf)
#Not a significant difference between nonnative and native percent cover between pretreated and not pretreated seeds.

###tamping
survey3$tampingf=as.factor(survey3$tamping)
model3=lm(survey3$nativepercent~survey3$tampingf)
anova(model3)
TukeyHSD(aov(model3))

boxplot(survey3$nativepercent~survey3$tampingf)
boxplot(survey3$nonnativepercent~survey3$tampingf)
#No significant difference in percent cover of natives between tamping and no tamping treatments.

#######look at 2 species#########
#native annual species of interest, assess distribution
plot(percent$Deinandra.fasciculata)
dotchart(percent$Deinandra.fasciculata)
qqnorm(percent$Deinandra.fasciculata)
qqline(percent$Deinandra.fasciculata)

#invasive species of interest, assess distribution
plot(percent$Hirschfeldia.incana)
dotchart(percent$Deinandra.fasciculata)
qqnorm(percent$Deinandra.fasciculata)
qqline(percent$Deinandra.fasciculata)


######compare full treatments######

#aggregate-group together by % by full treatment across 144 plots
species=aggregate(percent[,8:99], by=list(percent$fulltreat),sum)

#the mean percent cover across all plots, grouped by full treatments
speciesmean=aggregate(percent[,8:99], by=list(percent$fulltreat),FUN="mean")

#the mean percent cover across all plots, grouped by full treatment, for survey 3
speciesmean3=aggregate(survey3[,8:99], by=list(survey3$fulltreat),FUN="mean")

#subset group
speciesgroup=survey3[,8:99]
library(vegan)
species.matrix=as.matrix(speciesgroup)
species_nmds=metaMDS(speciesgroup[, -1], k=12, try=20) 

adonis(species.matrix~survey3$fulltreat)
plot(species_nmds, type="p")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Jan_R_T_NPt",display="sites",label=F,col="blue4")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Jan_R_NT_NPt",display="sites",label=F,col="deepskyblue3")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Jan_R_T_Pt",display="sites",label=F,col="darkturquoise")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Jan_R_NT_Pt",display="sites",label=F,col="darkcyan")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Nov_R_T_NPt",display="sites",label=F,col="yellow")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Nov_R_NT_NPt",display="sites",label=F,col="darkred")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Nov_R_T_Pt",display="sites",label=F,col="darkorange1")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Nov_R_NT_Pt",display="sites",label=F,col="purple")
ordispider(species_nmds,groups=survey3$fulltreat,show.groups="Con",display="sites",label=F,col="deeppink2")


#maybe not all different from each other, test which treatments are different
#most likely all will be different from control because seeds were added after weed treatment (glyphosate-based herbicide, and grass specific herbicide)
#cleaner way is to subset the data
#pair.wise adonis- the long way
adonis(species.matrix[which(survey3$fulltreat=="Jan_R_T_NPt"|survey3$fulltreat=="Con"),]~survey3$fulltreat[which(survey3$fulltreat=="Jan_R_T_NPt"|survey3$fulltreat=="Con")])

#cleaner pairwise adonis
#statistically significantly different
survey3_subset=survey3[which(survey3$fulltreat=="Nov_R_T_Pt"|survey3$fulltreat=="Con"),]
adonis(survey3_subset[,8:99]~survey3_subset$fulltreat)
survey3_subset1=survey3[which(survey3$fulltreat=="Nov_R_NT_NPt"|survey3$fulltreat=="Con"),]
adonis(survey3_subset1[,8:99]~survey3_subset1$fulltreat)
survey3_subset2=survey3[which(survey3$fulltreat=="Nov_R_T_NPt"|survey3$fulltreat=="Con"),]
adonis(survey3_subset2[,8:99]~survey3_subset2$fulltreat)
survey3_subset4=survey3[which(survey3$fulltreat=="Jan_R_T_Pt"|survey3$fulltreat=="Con"),]
adonis(survey3_subset4[,8:99]~survey3_subset4$fulltreat)
survey3_subset5=survey3[which(survey3$fulltreat=="Jan_R_NT_NPt"|survey3$fulltreat=="Con"),]
adonis(survey3_subset5[,8:99]~survey3_subset5$fulltreat)
survey3_subset6=survey3[which(survey3$fulltreat=="Jan_R_T_NPt"|survey3$fulltreat=="Con"),]
adonis(survey3_subset6[,8:99]~survey3_subset6$fulltreat)
survey3_subset7=survey3[which(survey3$fulltreat=="Jan_R_NT_Pt"|survey3$fulltreat=="Con"),]
adonis(survey3_subset7[,8:99]~survey3_subset7$fulltreat)

#not significant
survey3_subset8=survey3[which(survey3$fulltreat=="Nov_R_NT_Pt"|survey3$fulltreat=="Con"),]
adonis(survey3_subset8[,8:99]~survey3_subset8$fulltreat)

# All treatment plots but the Nov_R_NT_Pt were significantly different from the control plots.
survey3_subset9=survey3[which(survey3$fulltreat=="Nov_R_T_Pt"|survey3$fulltreat=="Nov_R_NT_Pt"),]
adonis(survey3_subset9[,8:99]~survey3_subset9$fulltreat)
boxplot(survey3$nativepercent~survey3$fulltreat)
#Treatment Nov_R_NT_PT is different from most treatments (besides control), suggesting that this is the least effective treatment.


#######Presab data analysis########################
#find the proportion of native to nonnative species in presab
rm(list=ls())

#load and prepare dataset for analysis
presab<-read.csv(file = "OliverCanyonData_PresAb_T.csv",header = TRUE)
speciespresab=presab[,8:125]

#take columns in percent with n for row 1
speciesdesignation2=presab[1,]
#take out that row
presab=presab[-1,]

for(i in 8:125) presab[,i]=as.numeric(presab[,i])
#for each column with na then make 0
for(i in 8:125) presab[which(is.na(presab[,i])),i]=0

#separate native and nonnatives, creates new columns
presab$native=rowSums(presab[,which(speciesdesignation2=="n")])
presab$nonnative=rowSums(presab[,which(speciesdesignation2=="nn")])

#calculate total and percent cover, create new columns
presab$total=presab$native+presab$nonnative
presab$nativepercent=presab$native/presab$total
presab$nonnativepercent=presab$nonnative/presab$total
survey3presab<-presab[presab$survey.number=="3",]

########all surveys##############
#season, create matrix of percent values grouped by sowing season
Jan<-presab[presab$season=="Jan",]
Nov<-presab[presab$season=="Nov",]
C<-presab[presab$season=="c",]
#subset, name of data, variable ==
boxplot(Jan$nativepercent, Nov$nativepercent, C$nativepercent)
#seed pretreatment, create matrix of percent values grouped by seed pretreatment
PT<-presab[presab$seed.treatment=="pt",]
NPT<-presab[presab$seed.treatment=="npt",]
C2<-presab[presab$seed.treatment=="c",]
boxplot(PT$nativepercent, NPT$nativepercent, C2$nativepercent)
hist(PT$nativepercent)

#tamping, create matrix of percent values grouped by tamping treatment
Tamp<-presab[presab$tamping=="t",]
NT<-presab[presab$tamping=="nt",]
C3<-presab[presab$tamping=="c",]
boxplot(Tamp$nativepercent, NT$nativepercent, C3$nativepercent)

####survey 3 only######
#season
Jan2<-survey3presab[survey3presab$season=="Jan",]
Nov2<-survey3presab[survey3presab$season=="Nov",]
Cb<-survey3presab[survey3presab$season=="c",]
#subset, name of data, variable ==
boxplot(Jan2$nativepercent, Nov2$nativepercent, Cb$nativepercent)

#seed pretreatment
PT2<-survey3presab[survey3presab$seed.treatment=="pt",]
NPT2<-survey3presab[survey3presab$seed.treatment=="npt",]
C2b<-survey3presab[survey3presab$seed.treatment=="c",]
#subset, name of data, variable ==
boxplot(PT2$nativepercent, NPT2$nativepercent, C2b$nativepercent)

#Tamping
Tamp2<-survey3presab[survey3presab$tamping=="t",]
NT2<-survey3presab[survey3presab$tamping=="nt",]
C3b<-survey3presab[survey3presab$tamping=="c",]
#subset, name of data, variable ==
boxplot(Tamp2$nativepercent, NT2$nativepercent, C3b$nativepercent)

######ANOVA############
###season
survey3presab$seasonf=as.factor(survey3presab$season)
model1=lm(survey3presab$nativepercent~survey3presab$seasonf)
anova(model1)
TukeyHSD(aov(model1))

###seed treatment
survey3presab$seed.treatmentf=as.factor(survey3presab$seed.treatment)
model2=lm(survey3presab$nativepercent~survey3presab$seed.treatmentf)
anova(model2)
TukeyHSD(aov(model2))

###tamping
survey3presab$tampingf=as.factor(survey3presab$tamping)
model3=lm(survey3presab$nativepercent~survey3presab$tampingf)
anova(model3)
TukeyHSD(aov(model3))

#Tamping, season sowed, and seed treatment did not make a difference between the presence of native species in experimental plots (pvalues>0.05)
#All treatments compared to control had significantly different number of native species with p-value>0.05.
##Adding seeds altered the presence of native species but the treatments didn't matter.


################NMDS/ADONIS###########
speciesgrouppresab=survey3presab[,8:125]
library(vegan)
species.matrixb=as.matrix(speciesgrouppresab)
species_nmds_presab=metaMDS(speciesgrouppresab[, -1],k=12) 


adonis(species.matrixb~survey3presab$fulltreat)
plot(species_nmds_presab, type="p")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Jan_R_T_NPt",display="sites",label=F,col="blue4")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Jan_R_NT_NPt",display="sites",label=F,col="deepskyblue3")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Jan_R_T_Pt",display="sites",label=F,col="darkturquoise")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Jan_R_NT_Pt",display="sites",label=F,col="darkcyan")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Nov_R_T_NPt",display="sites",label=F,col="yellow")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Nov_R_NT_NPt",display="sites",label=F,col="darkred")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Nov_R_T_Pt",display="sites",label=F,col="darkorange1")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Nov_R_NT_Pt",display="sites",label=F,col="purple")
ordispider(species_nmds_presab,groups=survey3presab$fulltreat,show.groups="Con",display="sites",label=F,col="deeppink2")

#pair.wise adonis
#statistically significantly diferent
survey3_subset=survey3presab[which(survey3presab$fulltreat=="Nov_R_T_Pt"|survey3presab$fulltreat=="Con"),]
adonis(survey3_subset[,8:99]~survey3_subset$fulltreat)
survey3_subset1=survey3presab[which(survey3presab$fulltreat=="Nov_R_NT_NPt"|survey3presab$fulltreat=="Con"),]
adonis(survey3_subset1[,8:99]~survey3_subset1$fulltreat)
survey3_subset2=survey3presab[which(survey3presab$fulltreat=="Nov_R_T_NPt"|survey3presab$fulltreat=="Con"),]
adonis(survey3_subset2[,8:99]~survey3_subset2$fulltreat)
survey3_subset4=survey3presab[which(survey3presab$fulltreat=="Jan_R_T_Pt"|survey3presab$fulltreat=="Con"),]
adonis(survey3_subset4[,8:99]~survey3_subset4$fulltreat)
survey3_subset5=survey3presab[which(survey3presab$fulltreat=="Jan_R_NT_NPt"|survey3presab$fulltreat=="Con"),]
adonis(survey3_subset5[,8:99]~survey3_subset5$fulltreat)
survey3_subset6=survey3presab[which(survey3presab$fulltreat=="Jan_R_T_NPt"|survey3presab$fulltreat=="Con"),]
adonis(survey3_subset6[,8:99]~survey3_subset6$fulltreat)
survey3_subset7=survey3presab[which(survey3presab$fulltreat=="Jan_R_NT_Pt"|survey3presab$fulltreat=="Con"),]
adonis(survey3_subset7[,8:99]~survey3_subset7$fulltreat)

boxplot(survey3presab$nativepercent~survey3presab$fulltreat)
#All treatments were significantly different from the control treatments.
#only seed addition had an effect on native presence.

