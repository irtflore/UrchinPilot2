#October 12, 2022
#Urchin Pilot data
#session: set working directory to whatever file you have the raw data in or do the following

# Basic commands ----------------------------------------------------------
#super wow!
##This reads out the columns
str(UrchinPilot1)
##This summarizes the data
summary(UrchinPilot)
#This deletes variables
rm(list = ("mydata")) 
#Delete rows 
## dataset[-c(row numbers seoparated by commas), (column numbers separted by commas)]
#To view entire column
UrchinPilot1$State.Barren.Kelp.
#This is a form of filtering 
UrchinPilot1[UrchinPilot1$State.Barren.Kelp.=="B",]
#or filter this way
filter(UrchinPilot, State.Barren.Kelp. == "B")
# Select everything EXCEPET the indicated columns: Add a negative sign and the c(). Store as Urchin.cleaned by adding the little arrow and new variable name at the beginning
Urchin.cleaned <- UrchinPilot %>% 
  select(-c(Tank..))
#Filter trials 4-6
UrchinPilot1%>%
  filter(Trial..==c("4","5","6"))
#example of using pipleine to get average
## data<-c(1,2,5,7,4)
##data %>% mean()
#install packages


# Start of work -----------------------------------------------------------


setwd("~/Desktop/R")
library(tidyverse)
library(ggplot2)
library(dplyr)
UrchinPilot<-read.csv("UrchinPilot.csv")

        #  Changing from characters to numbers
UrchinPilot$Size.mm.<-as.numeric(as.character(UrchinPilot$Size.mm.))
UrchinPilot$Wet.weight.g.<-as.numeric(as.character(UrchinPilot$Wet.weight.g.))
UrchinPilot$SetTemp<-as.numeric(as.integer(UrchinPilot$SetTemp))

      #Creating new column(2 ways)
# Mutate to create new columns

UrchinPilot<-UrchinPilot %>% 
  mutate(Grazing_Rate = Kelp.start.weight-Kelp.end.weight )

UrchinPilot$Grazing_Rate<- ifelse(UrchinPilot$Grazing_Rate< 0, 0, UrchinPilot$Grazing_Rate)

UrchinPilot<-UrchinPilot %>% 
  mutate(Grazing_by_Size = (Grazing_Rate)/Size.mm.)

UrchinPilot<-UrchinPilot%>%
  mutate(Grazing_by_weight= Grazing_Rate/Wet.weight.g.)

####   OR
#UrchinPilot$GrazingRate<-UrchinPilot$Kelp.start.weight-UrchinPilot$Kelp.end.weight
#UrchinPilot$Grazing_by_size<-UrchinPilot$GrazingRate/UrchinPilot$Size.mm.

#Deletes rows: These rows had urchins that escaped and NA'S for size
UrchinPilot1<-UrchinPilot[-c(3,47,82,106,130),]

#Grouping & Getting average grazing by state and by temp NOT SEPARATED BY TRIAL
str(UrchinPilot1)

UrchinSumStats<- UrchinPilot1 %>%
  group_by(State.Barren.Kelp.,SetTemp)%>%
  summarize(MeanGrazing = mean(Grazing_by_Size),StdGrazing = sd(Grazing_by_Size))


#UP3<-UrchinPilot1%>%
  #group_by(Trial..,State.Barren.Kelp.,SetTemp)%>%
#summarize(MeanGrazing=mean(Grazing_by_Size),StdGrazing=sd(Grazing_by_Size))

# Making plots ------------------------------------------------------------

library(ggplot2)
#Figures for ALL TRIALS combined 
UrchinSumStats %>%
  ggplot(mapping = aes(x=SetTemp,y=MeanGrazing,color=State.Barren.Kelp.)) + 
  geom_point()+
  geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing)) +
  geom_line()+
  scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))

UrchinSumStats%>%
ggplot(mapping=aes(x=SetTemp,y=MeanGrazing, color=State.Barren.Kelp.))+
  geom_point()+
  facet_grid(.~State.Barren.Kelp.)+
  geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing))+
  scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))
 
#To Change the ticks on the x-axis :scale_x_continuous(limits = c(2500, 6500), breaks = c(seq(2500, 6500, 1000))) # set manual axis ticks
# set the x and y axis range: ylim(180, 220) + #oops cut off some points, need bigger scale #xlim(1000,10000)+ #maybe you know this colony could be bigger or smaller, show the gap 

#Scatteplot separated by trials

ggplot(UrchinPilot1,aes(x=SetTemp,y=Grazing_by_Size, color=State.Barren.Kelp.))+
  geom_point()+
  facet_grid(State.Barren.Kelp.~Trial..)+
  scale_x_continuous(limits = c(11,22), breaks =c(seq(12,21,3)))

##Bar graph= dont need, but this is what i was hoping to get/ 2 boxplots per temnperature
 UrchinSumStats1 %>%
  ggplot(mapping=aes( x=SetTemp,fill=State.Barren.Kelp.,y= MeanGrazing))+
   geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))


# Trials 1 to 4 -----------------------------------------------------------
  #Just focused on Trials 1-4
  UrchinPilot1to4<-UrchinPilot1%>%
    filter(Trial.. %in% c("1","2","3","4"))

  #Group & Create mean and std deviation for trials 1-4 THIS GROUP BY ISNT WORKING ANYMORE
  UP1TO4<-UrchinPilot1to4 %>%
  group_by(State.Barren.Kelp.,SetTemp)%>%
    summarise(MeanGrazing = mean(Grazing_by_Size),
              StdGrazing = sd(Grazing_by_Size))
  
  UPShort<-UrchinPilot1%>%
    filter(Trial..=="1"|Trial..=="2"|Trial..=="3"|Trial..=="4")
  
  
  #Figure for barren vs kelp for trials 1-4
  ggplot(UPShort,aes(x=SetTemp,y=Grazing_by_Size, color=Trial..))+
    geom_point()+
    facet_grid(.~State.Barren.Kelp.)+
  #  geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing))+
    scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))
  

# Trials 4 to 6 -----------------------------------------------------------

  ##Now just looking at trials 4-6   BARREN URCHINS FROM 18C ARE NOT SHOWING UP
  UrchinPilot4to6<-UrchinPilot1 %>%
    filter(Trial..%in% c("4","5","6"))

  
  ##model ran with Emily
  
  model2<-lm(Grazing_by_Size~SetTemp*State.Barren.Kelp.+State.Barren.Kelp.*Trial.., data=UrchinPilot4to6)
  anova(model2)
  
  ##change tank so its reading numbers only not 5A,5B,6A,6B
  model2<-lm(Grazing_by_Size~SetTemp*State.Barren.Kelp.*Trial..+(1|State.Barren.Kelp.:Tank..), data=UrchinPilot4to6)
  
  ggplot(UrchinPilot4to6,aes(x=SetTemp,y=Grazing_by_Size,color=State.Barren.Kelp.))+
    geom_point()+
    geom_smooth(method = lm)
  
  ggplot(UrchinPilot4to6,aes(x=Trial..,y=Grazing_by_Size,))+
    geom_bar(stat ="identity")
  
  
  ## Group & Mean grazing rate by size and standard error NOT WORKING ANYMORE!!!
  UP4to6<-UrchinPilot4to6 %>%
    group_by(State.Barren.Kelp., SetTemp,)%>%
    summarize(MeanGrazing = mean(Grazing_by_Size),
              StdGrazing = sd(Grazing_by_Size))
  
  #Figure for barren vs kelp for trials 1-4
  ggplot(UP4to6,aes(x=SetTemp,y=MeanGrazing, color=State.Barren.Kelp.))+
    geom_point()+
    facet_grid(.~State.Barren.Kelp.)+
    geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing))


# kk added ----------------------------------------------------------------

install.packages("lme4")  
install.packages("lsmeans")
install.packages("emmeans")

library(lme4)
library(lsmeans)
library(emmeans)


model1<-lmer(Grazing_Rate~SetTemp + State.Barren.Kelp. + SetTemp*State.Barren.Kelp. + Wet.weight.g. + (1|Trial..), data=UrchinPilot1to4, REML = TRUE)
summary(model1)


model2<-lmer(Grazing_by_wt~SetTemp + State.Barren.Kelp. + SetTemp*State.Barren.Kelp. + (1|Trial..), data=UrchinPilot1to4, REML = TRUE)
summary(model2)

model3<-lmer(Grazing_by_Size~SetTemp + State.Barren.Kelp. + SetTemp*State.Barren.Kelp. + (1|Trial..), data=UrchinPilot1to4, REML = TRUE)
summary(model3)




# EMILY-----------------------------------------------------------------
#New column that categorizes trials as short or long


AC<-c(rep("s",93),rep("l",46))
UrchinPilot1$Acclimation<-AC

UPShort<-UrchinPilot1%>%
  filter(Trial..=="1"|Trial..=="2"|Trial..=="3"|Trial..=="4")

UPShort<-UP1TO36%>%
  mutate(Grazing_by_wt=Grazing_Rate/Wet.weight.g.)

install.packages("lmerTest")
library(lmerTest)

model2<-lmer(Grazing_by_Size~SetTemp+State.Barren.Kelp. + (1|Trial..), data=UP1TO36)
summary(model2)
anova(model2)

ggplot(data=UP1TO36,aes(x=Size.mm.,y=Grazing_Rate,color=SetTemp,shape=Acclimation))+
  geom_point()

UP1TO36$SetTemp<-as.numeric(UP1TO36$SetTemp)

install.packages("Rmisc")
library(Rmisc)
 Con_Sum1 <- summarySE(data=UP1TO36, measurevar = "Grazing_Rate",
                      groupvars = c("SetTemp","State.Barren.Kelp.","Acclimation"), na.rm = TRUE)


