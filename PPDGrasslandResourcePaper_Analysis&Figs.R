#Packages needed
#install.packages("tidyr")
library(tidyr) #for reshaping data
library(ggplot2) #for graphing
library(dplyr)
#install.packages("gridExtra")
library(gridExtra) #needed for plotting fig 5 - one panel per date "type" (start, peak, end)

#####FUNCTIONS - READ IN BEFORE USING CODE: ####

error.bar <- function(y, x, upper, lower=upper, length=0.01,...){
  if(length(x) != length(y) | length(x) !=length(lower) | length(lower) != length(upper))  
    stop("vectors must be same length")
  arrows(x-upper, y, x+lower, y, angle=90, code=3, length=0.07, lwd=5) # length=length # angle = 90  is modified to make horizontal
} #alternate (previous) option: length=length as bar whiskers can decide , also lwd=2

#Figure 1 - Grassland Photo
#Figure 2 - Concept/Theory overview diagram
#Figure 3 - ####
setwd("/Users/rachaelolliff/Box Sync/Projects/PepperwoodData")
Temp<-read.csv("GrasslandPlotTemps2016_2018.csv")#created in PPDGrasslandAbioticData.R
PollResDates<-read.csv("PollResDates.csv")
p<-c("BHS", "BHN", "DPS", "DPN", "FRS", "FRN", "TPN", "TPS")
#head(Temp)
AvMonthTemp<- na.omit(Temp) %>% #creating df of average montly temps
  filter(PLOT %in% p, DataType=="plot") %>% 
  group_by(YEAR,PLOT) %>% 
  summarise(TempJan=mean(TEMP[DOY<31]), #Note: In most years data was only collected Mar-Jun
            TempFeb=mean(TEMP[DOY>=31 & DOY<59]), 
            TempMar=mean(TEMP[DOY>=59 & DOY<90]),
            TempApr=mean(TEMP[DOY>=90 & DOY<120]),
            TempMay=mean(TEMP[DOY>=120 & DOY<151]),
            TempJun=mean(TEMP[DOY>=151 & DOY<181]),
            TempJul=mean(TEMP[DOY>=181 & DOY<212]),
            TempAug=mean(TEMP[DOY>=212 & DOY<243]),
            TempSep=mean(TEMP[DOY>=243 & DOY<273]),
            TempOct=mean(TEMP[DOY>=273 & DOY<304]),
            TempNov=mean(TEMP[DOY>=304 & DOY<334]),
            TempDec=mean(TEMP[DOY>=334 & DOY<367]))
#TO DO - CREATE A FIGURE WITH AVERAGE TEMP X AVE PHENO 
AvMonthTemp_long <- AvMonthTemp %>% gather(Month, MoTemp,TempJan:TempDec) #need tidyr loaded for gather function
AvMonthTemp_long<-na.omit(AvMonthTemp_long)

AMTLsub<-AvMonthTemp_long %>% 
  mutate(SITE=substr(PLOT,1,2), ASPECT=substr(PLOT,3,3), MONTH=substr(Month,5,7))%>% #splitting "plot" into site & aspect, cleaning up "month" substring numbers - 1st one to show, last one to show e.g. "1,2" shows characters 1-2 (starting from the left)
  select("SITE", "ASPECT","YEAR","MONTH","MoTemp")

PhenoTemp<-merge(PollResDates,AMTLsub, by=c("YEAR", "SITE", "ASPECT"))
FSmo<-c("Mar", "Apr", "May") #flowering season months 
PhenoAVERAGES<-PhenoTemp%>%
  filter(MONTH%in%FSmo) %>% #filter to only Flowering season months (peak has already been calculated, so this only affects temp data)
  group_by(YEAR, ASPECT) %>% 
  mutate(YrAspMean=mean(peak), YAspAvTemp=mean(MoTemp)) %>% 
  group_by(YEAR, ASPECT, MONTH) %>% 
  mutate(MoAvTemp=mean(MoTemp)) %>% 
  ungroup() %>% 
  group_by(YEAR, SITE, ASPECT) %>% 
  mutate(FSAvTemp=mean(MoTemp)) #average temp across growing season (by year & Apect)


summary(lm(PhenoAVERAGES$peak~PhenoAVERAGES$FSAvTemp))
summary(lm(PhenoAVERAGES$peak~PhenoAVERAGES$FSAvTemp + PhenoAVERAGES$YEAR + PhenoAVERAGES$SITE)) # Mar-may temp averages 
# slope r2 = 0.53 slope =  -3.2570  # full model with other fixed effects... #Extrap data:  r2=0.56, slope =-3.4
fitlm <-lm(PhenoAVERAGES$peak~PhenoAVERAGES$FSAvTemp+ PhenoAVERAGES$YEAR + PhenoAVERAGES$SITE)
PhenoAVERAGES$predlm = predict(fitlm)

ggplot(PhenoAVERAGES, aes(x = FSAvTemp, y = peak)) +
  geom_smooth(method = "lm", colour="grey", aes(group = c(YEAR)), se=FALSE, size=0.5,alpha=0.2) + #, se=FALSE
  geom_smooth(method = "lm", aes(y = predlm), colour="black", size=0.5,alpha=0.2, se=FALSE) +
  geom_point(aes(pch=YEAR, col=ASPECT))+
  scale_colour_manual(values = c("N" = "navyblue", "S" = "tomato1"))+
  labs(x = "Average Temp Mar-May ( C)", y="Day of 50% Flowering") +
  theme_bw() +
  geom_point(aes(x=YAspAvTemp,y=YrAspMean, pch=YEAR, col=ASPECT), size=5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),panel.border = element_blank(), axis.line=element_line(color="black"), 
        axis.ticks=element_line(color="black"), axis.title.y=element_text(vjust=0.3), axis.title.x=element_text(vjust=0.3))
#end figure 2 ##


#Figure 4 - ####
#fig 4 panel 1 -

#set up bar points - means of start & end & mid flowering (call"peak, but date of 50% flowering)
EndMean <-aggregate(PollResDates$end, by=list( PollResDates$ASPECT, PollResDates$YEAR), 
                    FUN=mean, na.rm=TRUE)

se <- function(x) sqrt(var(x)/length(x))

EndSterr <-aggregate(PollResDates$end, by=list( PollResDates$ASPECT, PollResDates$YEAR), 
                     FUN=se)

oneMean <-aggregate(PollResDates$start, by=list( PollResDates$ASPECT, PollResDates$YEAR), 
                    FUN=mean, na.rm=TRUE)

oneSterr <-aggregate(PollResDates$start, by=list( PollResDates$ASPECT, PollResDates$YEAR), 
                     FUN=se)

peakMean <-aggregate(PollResDates$peak, by=list( PollResDates$ASPECT, PollResDates$YEAR), 
                     FUN=mean, na.rm=TRUE)

peakSterr <-aggregate(PollResDates$peak, by=list(PollResDates$ASPECT, PollResDates$YEAR), 
                      FUN=se)



par(mfrow=c(1,1), xpd=TRUE)
PLOT<- barplot(as.numeric(EndMean$x),cex.axis=1.5, horiz=TRUE, xlim=range(c(65, 150)), yaxs='i',space=c(0,0, 0.5, 0, 0.5,0, 0.5,0),  
               border='transparent', col=c("royalblue4","tomato1", "royalblue4","tomato1", "royalblue4","tomato1", "royalblue4","tomato1"), density=c(NA,30,NA,30,NA,30,NA,30) , angle=c(NA,45,NA,45,NA,45,NA,45))  ##, border='transparent'"dodgerblue4","firebrick1") #col=c("dimgrey","lightgrey", "dimgrey","lightgrey","dimgrey","lightgrey")) #col=c("dodgerblue4","firebrick1", "dodgerblue4","firebrick1", "dodgerblue4","firebrick1", "dodgerblue4","firebrick1"))  ##, border='transparent'"dodgerblue4","firebrick1"

#points(EndMean$x,PLOT, pch=25, bg=1 )#bg=1 makes this point filled, can be filled with different colors (e.g 2= red, or "red", etc.)
PLOT2<-barplot(as.numeric(oneMean$x), horiz=TRUE, space=c(0,0, 0.5, 0, 0.5,0,0.5,0), add=TRUE, col=c("white"), xlim=range(c(65, 150)),yaxs='i',
               border='transparent', las=1, #names.arg=startEnd$COHORT
               cex.names=1.5, cex.lab=1.5,cex.axis=1.5, beside=TRUE, ylab=c("2015        2016        2017        2018"), xlab=c("Day of Year") )
#points(oneMean$x,PLOT, pch=24, bg=1 )#bg=1 makes this point filled, can be filled with different colors (e.g 2= red, or "red", etc.)
legend("bottomright",inset=c(-.05,-0), c( "South","North"),text.font=2 ,cex=2, pch=c(7,15), col=c( "tomato1","royalblue4"), box.lty = 0) #c("Competition", "No Competition"),  pch=15, col=c("light grey", "dimgray")) # lty=1, lwd= 15,
legend("topleft", inset=c(-.05,-0.05), c( "A"),text.font=2 ,cex=1.7, box.lty = 0)
box(bty='l')

#read in error.bar function from above
error.bar(PLOT, EndMean$x,EndSterr$x) #
error.bar(PLOT, oneMean$x,oneSterr$x) #
error.bar(PLOT, peakMean$x,peakSterr$x) #

points(peakMean$x,PLOT, pch=21, bg="white", cex=2)



#Fig 4 panel 2 - 
SpMeansSubtract<-read.csv("SpMeansSubtract.csv") #need to create from PPDPollResources.r? 
SiteComsp<-SpMeansSubtract %>%  #need to load in from above
  group_by(YEAR, SITE) %>%
  summarize(CommonSp=length(unique(NAME))) #ONLY common species at a site...
head(SiteComsp)
SPECIESPollResDates<-read.csv("SPECIESPollResDates.csv")
ASPrich1<-SPECIESPollResDates %>% 
  group_by(YEAR, SITE) %>%
  summarise(TOTSiteRich=length(unique(NAME)))

ASPrich2<-SPECIESPollResDates %>%  #need to load in from above
  group_by(YEAR, SITE, ASPECT) %>%
  summarize(AspectRich=length(unique(NAME)))  # aspect richness...
ASPrich3<-merge(ASPrich1,ASPrich2, by=c("YEAR", "SITE"))
ASPrich<-merge(ASPrich3,SiteComsp, by=c("YEAR", "SITE"))
ASPrich$ASPECT<-as.factor(ASPrich$ASPECT)
ASPrich_short <- spread(ASPrich, ASPECT, AspectRich)
ASPturn<-ASPrich_short%>% 
  mutate(Sunique=(S-CommonSp),Nunique=(N-CommonSp) )

SiteBeta<-ASPturn %>% 
  #mutate(c=(N-S)) %>% #make sure North aspect #s are always higher or = to S #s (this is the case 2015-2018)
  mutate(TurnAbs=(Sunique+Nunique), RelTurn=((Sunique+Nunique)/TOTSiteRich), PropTurn=(Sunique+Nunique)/(S+N) , WhitTurn=(Sunique+Nunique)/((S+N)/2), ßsim=(CommonSp/(CommonSp+min(Sunique,Nunique)))) #min(Sunique,Nunique) on top in some ßsim# other metrics: ßsor=((Sunique+Nunique)/((2*CommonSp)+Sunique+Nunique)), ßnes=(ßsor-ßsim))#%>% #min(Sunique,Nunique) SiteRichness=(Sunique+Nunique+CommonSp)  # min chooses minimum of the two columns! 

BetaExt<-merge(SiteBeta,meansSubtract, by=c("YEAR","SITE"))
head(BetaExt)

BetaExt$YEAR<-as.factor(BetaExt$YEAR)

plot(BetaExt$DaysExtension~BetaExt$TurnAbs, pch=16,ylab=c("# Days Extension"), xlab=c("Absolute Turnover"), cex.lab=1.5, cex=2, cex.axis=1.5)#turnover # col=BetaExt$SITE, #, 
legend("bottomright", inset=c(.1,0.2), c("* R^2 = 0.26"),text.font=2 ,cex=1.3, box.lty = 0)#inset=c(.05,0.05)
mtext("(# species only on 1 aspect)", cex=1, side = 1, line = 4)#higher line # places the label further from the plot

abline(lm(BetaExt$DaysExtension~BetaExt$TurnAbs), lty=1, lwd=3)
#

#Fig 4 panel 3 - 
###numberline visualization of extension with vs. without turnover
a<-meansSubtract %>% 
  mutate(TurnoverExt=(DaysExtension),TurnoverExtPercent=(Extension))%>% #
  select(YEAR, SITE, TurnoverExt,TurnoverExtPercent) 
b<-meansSubtractNT %>% 
  mutate(NoTurnoverExt=(DaysExtension),NoTurnoverExtPercent=(Extension))%>% #
  select(YEAR, SITE, NoTurnoverExt,NoTurnoverExtPercent) #

ComboMS<-merge(a,b, by=c("YEAR", "SITE")) 
head(ComboMS)
dim(ComboMS)

ComboMS2<-ComboMS %>% 
  mutate(NT=0.1, T=0.9) #adding two colums for static y values to create two parallel numberlines... numbers don't mean anything, can change as needed 
head(ComboMS2)

plot(ComboMS2$T~ComboMS2$TurnoverExt, ylim=c(0,1), ylab="", yaxt='n', yaxt='n', pch=16,cex=2, bty="n", xlab=c("Number of Days Extension"), cex.axis=1.5) 
segments(x0 = ComboMS2$TurnoverExt,                            # Draw multiple lines connecting site calculations 
         y0 = ComboMS2$T,
         x1 = ComboMS2$NoTurnoverExt,
         y1 = ComboMS2$NT)

points(ComboMS2$NT~ComboMS2$NoTurnoverExt, ylab="", cex=2,pch = 23, bg="light grey" ) 
legend("bottomright",inset=c(.05,.05), c("Turnover", "No Turnover"), pch=c(16,23), pt.bg=c(1,"light grey"), col=c(1,1), text.font=2 ,cex=1.1, box.lty = 1)

# add additional x axis ticks: 
xtick<-seq(0, 17, by=1)
axis(side=1, at=xtick, cex.axis=1.5) # add additional x axis ticks



#figure 5 - #####
#ggplot2 flowering time difference (N-S) for individual species (in the style of CarraDonna et al. 2014 shift graph)
#plots by type of data - start, mid, diff 
head(SPECIESPollResDates ) #load in here

SmeansCalc<-SPECIESPollResDates%>% 
  filter(ASPECT=="S")%>%
  group_by(YEAR, SITE, NAME )%>% 
  summarise(Sday1mean=mean(start),Speakmean=mean(peak), Sendmean=mean(end))
SpmeansCalc<-merge(SmeansCalc, NmeansCalc, by=c("YEAR","SITE", "NAME")) # NAME mean start, peak & end dates by year and site and aspect

SptestLength<-SpmeansCalc%>%
  group_by(YEAR, SITE, NAME)%>% #NAME
  summarise(TOTlength=LengthTOT(Nday1mean, Sday1mean, Nendmean, Sendmean)) #testing length calculation function in action - this is by year & site & species...

SpMeansCalc2<-merge(SptestLength, SpmeansCalc, by=c("YEAR", "SITE", "NAME")) # merging length calculation with means calc

SpMeansSubtract<-SpMeansCalc2  %>% # now to compare mean North length & S length to Tot length 
  mutate(day1diff=(Nday1mean-Sday1mean), peakdiff=(Npeakmean-Speakmean), enddiff=(Nendmean-Sendmean), Slength=((Sendmean-Sday1mean)+5),Nlength=((Nendmean-Nday1mean)+5), TotLength=(TOTlength)+5) %>% #+5 needed to remove zeros and infinite extension metric... (0/#)
  mutate(Longest = ifelse(Nlength>Slength, Nlength, Slength))%>%
  mutate(Extension=(((TotLength-Longest)/Longest)*100), DaysExtension=(TotLength-Longest)) #percent extension metric

SpMeansSubtract2<-SpMeansSubtract  %>% # now to compare mean North length & S length to Tot length 
  group_by(YEAR, NAME)%>%
  mutate(Aveday1diff=mean(day1diff), Avepeakdiff=mean(peakdiff), Aveenddiff=mean(enddiff), AveDaysExtension=mean(DaysExtension))%>% #averaged for each species by year 
  select(YEAR,SITE, NAME, day1diff, Aveday1diff, peakdiff, Avepeakdiff, enddiff, Aveenddiff, DaysExtension, AveDaysExtension)

SpExMeans<-SpMeansSubtract%>%
  group_by(NAME)%>%
  summarise(SpMeanStartDiff=mean(day1diff),SpMeanPeakDiff=mean(peakdiff),SpMeanEndDiff=mean(enddiff), SpMeanDayEx=mean(DaysExtension) ) %>%
  ungroup() %>%
  summarise(MeanStartDiff=mean(SpMeanStartDiff),MeanPeakDiff=mean(SpMeanPeakDiff),MeanEndDiff=mean(SpMeanEndDiff), MeanDayEx=mean(SpMeanDayEx) ) 

plot1<-ggplot(SpMeansSubtract2,aes(day1diff, NAME,shape=as.factor(YEAR)))+  #w
  geom_point(position="dodge",stat="identity", color="dark gray") +  geom_vline(xintercept=0) +  geom_vline(xintercept=1.07, linetype = "dashed", colour = "red") + #get mean species start difference from SpExMeans above
  geom_point(aes(x=Aveday1diff), color="black") +
  labs(x = "Difference in Start Date (N-S)", y="Species") +
  theme(axis.text.y = element_text(face = "italic",color="black"), axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line=element_line(color="black"), 
        axis.ticks=element_line(color="black"), legend.position="none", axis.title.y=element_text(vjust=0.3), axis.title.x=element_text(vjust=0.3))
plot2<-ggplot(SpMeansSubtract2,aes(peakdiff, NAME,shape=as.factor(YEAR)))+  #color=as.factor()
  geom_point(position="dodge",stat="identity", color="dark gray") +  geom_vline(xintercept=0) + geom_vline(xintercept=3.31, linetype = "dashed", colour = "red") +  
  geom_point(aes(x=Avepeakdiff), color="black") +
  labs(x = "Difference in Mid Date (N-S)", y="") +
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), #axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line=element_line(color="black"), 
        axis.ticks=element_line(color="black"), legend.position="none", axis.title.y=element_blank(), axis.title.x=element_text(vjust=0.3))

plot3<-ggplot(SpMeansSubtract2,aes(enddiff, NAME,shape=as.factor(YEAR)))+  #
  geom_point(position="dodge",stat="identity", color="dark gray") +  geom_vline(xintercept=0) + geom_vline(xintercept=6.6, linetype = "dashed", colour = "red") + 
  geom_point(aes(x=Aveenddiff), color="black") + 
  labs(x = "Difference in End Date (N-S)", y="") +
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line=element_line(color="black"), 
        axis.ticks=element_line(color="black"),legend.position="none", axis.title.y=element_blank(), axis.title.x=element_text(vjust=0.3))# legend.title=element_blank(),

plot4<-ggplot(SpMeansSubtract2,aes(DaysExtension, NAME,shape=as.factor(YEAR)))+  #
  geom_point(position="dodge",stat="identity", color="dark gray") +  geom_vline(xintercept=0) + geom_vline(xintercept=2.6, linetype = "dashed", colour = "red") + 
  geom_point(aes(x=AveDaysExtension), color="black") +
  labs(x = "Days Extension", y="") +   xlim(-10, 25) + 
  theme(axis.text.y = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line=element_line(color="black"), 
        axis.ticks=element_line(color="black"), legend.title=element_blank(), axis.title.y=element_blank(), axis.title.x=element_text(vjust=0.3))

grid.arrange(plot1, plot2, plot3, plot4, ncol=4, widths = c(1.5, 1, 1, 1.25))  # legend 2015= circle, 2016=triangle, 2017 = square
#negative values - are true ( only calculated when species is present across both slopes)

###end figure 4 


###PHENOLOGY DATES - ANOVA Statistics ######
#Start (day of 10% flowering)
StarttestF<-(lm(start ~ ASPECT  + YEAR + SITE, data=PollResDates))
StartnullF<-(lm(start ~   YEAR + SITE, data=PollResDates))
summary(StartnullF)
summary(StarttestF)
anova(StartnullF,StarttestF)  # Aspect is significant ***
AICc(StarttestF,StartnullF) #model with aspect is better

#interaction tests - strategy in writing
a1<-(lm(start ~ ASPECT+YEAR + SITE + ASPECT:YEAR + SITE:ASPECT + SITE:YEAR+ SITE:ASPECT:YEAR,  data=PollResDates)) 
a2<-(lm(start ~ ASPECT+YEAR + SITE + ASPECT:YEAR + SITE:ASPECT + SITE:YEAR , data=PollResDates)) 
anova(a1,a2) # 3 wa interaction NS

#Null: 
anull<-(lm(start ~ ASPECT+YEAR + SITE, data=PollResDates)) 

a3<-(lm(start ~ ASPECT+YEAR + SITE + SITE:YEAR,  data=PollResDates)) 
anova(anull,a3) # site:yr interaction  *- therefore flowering time at a site depends on the year  (makes sense)


a4<-(lm(start ~ ASPECT+YEAR + SITE + ASPECT:YEAR,  data=PollResDates)) 
anova(anull,a4) # aspect:YEAR interaction - ns therefore aspect effect on flowering time does not depend on the year

summary(a4)

a5<-(lm(start ~ ASPECT+YEAR + SITE + SITE:ASPECT, data=PollResDates)) 
anova(anull,a5) # site:aspect interaction * - therefore aspect affects flowering time differently depending on the site! 



#peak tests - (day of 50% flowering)
PeaktestF<-(lm(peak ~ ASPECT  + YEAR + SITE, data=PollResDates)) 
PeaknullF<-(lm(peak ~  YEAR + SITE, data=PollResDates)) 
anova(PeaknullF,PeaktestF) # **
anova(PeaknullF)
AICc(PeaknullF,PeaktestF)
summary(PeaknullF)
summary(PeaktestF)

#peak interaction tests -
b1<-(lm(peak ~ ASPECT+YEAR + SITE + ASPECT:YEAR + SITE:ASPECT + SITE:YEAR+ SITE:ASPECT:YEAR,  data=PollResDates)) 
b2<-(lm(peak ~ ASPECT+YEAR + SITE + ASPECT:YEAR + SITE:ASPECT + SITE:YEAR , data=PollResDates)) 
anova(b1,b2) # 3 wa interaction NS

#Null: 
bnull<-(lm(peak ~ ASPECT+YEAR + SITE, data=PollResDates)) 
b3<-(lm(peak ~ ASPECT+YEAR + SITE + SITE:YEAR,  data=PollResDates)) 
anova(bnull,b3) # site:yr interaction  NS

b4<-(lm(peak ~ ASPECT+YEAR + SITE + ASPECT:YEAR,  data=PollResDates)) 
anova(bnull,b4) # aspect:YEAR interaction - ns therefore aspect effect on flowering time does not depend on the year
summary(b4)

b5<-(lm(peak ~ ASPECT+YEAR + SITE + SITE:ASPECT, data=PollResDates)) 
anova(bnull,b5) # site:aspect interaction *** - therefore aspect affects flowering time differently depending on the site! 


#end tests (day of 90% flowering)
Endtest<-(lm(end ~ ASPECT + YEAR + SITE,data=PollResDates)) 
Endnull<-(lm(end ~  YEAR + SITE, data=PollResDates))
anova(Endtest)
anova(Endnull,Endtest)
AICc(Endnull,Endtest)
summary(Endnull)
summary(Endtest)

Endtestint<-(lm(end ~ ASPECT*YEAR + SITE, data=PollResDates)) 
Endtest<-(lm(end ~ ASPECT + YEAR + SITE, data=PollResDates)) 
anova(Endtestint,Endtest) #NS


#end interaction tests -
c1<-(lm(end ~ ASPECT+YEAR + SITE + ASPECT:YEAR + SITE:ASPECT + SITE:YEAR+ SITE:ASPECT:YEAR,  data=PollResDates)) 
c2<-(lm(end ~ ASPECT+YEAR + SITE + ASPECT:YEAR + SITE:ASPECT + SITE:YEAR , data=PollResDates)) 
anova(c1,c2) # 3 wa interaction NS

#Null: 
cnull<-(lm(end ~ ASPECT+YEAR + SITE, data=PollResDates)) 
c3<-(lm(end ~ ASPECT+YEAR + SITE + SITE:YEAR,  data=PollResDates)) 
anova(cnull,c3) # 

c4<-(lm(end ~ ASPECT+YEAR + SITE + ASPECT:YEAR,  data=PollResDates)) 
anova(cnull,c4) # aspect:YEAR interaction - ns therefore aspect effect on flowering time does not depend on the year

summary(a4)

c5<-(lm(end ~ ASPECT+YEAR + SITE + SITE:ASPECT, data=PollResDates)) 
anova(cnull,c5) # site:aspect interaction * - therefore aspect affects flowering time differently depending on the site! 
summary(c5)
anova(c5)


