#October 12, 2022
#Urchin Pilot data
# Basic commands ----------------------------------------------------------

##This reads out the columns
str(UrchinPilot1)
##This summarizes the data
summary(UrchinPilot)
#This deletes variables
rm(list = ("")) 
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
str(UrchinPilot) #view structure

        #  Changing from characters to numbers
UrchinPilot$Size.mm.<-as.numeric(as.character(UrchinPilot$Size.mm.))
UrchinPilot$Wet.weight.g.<-as.numeric(as.character(UrchinPilot$Wet.weight.g.))
UrchinPilot$SetTemp<-as.numeric(as.integer(UrchinPilot$SetTemp))

str(UrchinPilot) #view structure

# Create new column -------------------------------------------------------

UrchinPilot1<-UrchinPilot[-c(3,47,82,106,130),] #Deletes rows: These rows had urchins that escaped and NA'S for size

UrchinPilot2<-UrchinPilot1 %>% #new data set with Grazing rate column
  mutate(Grazing_Rate = Kelp.start.weight-Kelp.end.weight )

UrchinPilot2$Grazing_Rate<- ifelse(UrchinPilot2$Grazing_Rate< 0, 0,  UrchinPilot2$Grazing_Rate) #makes all negative grazing rates zero

UrchinPilot2<-UrchinPilot2 %>%  #grazing by size column
  mutate(Grazing_by_Size = (Grazing_Rate)/Size.mm.)

UrchinPilot2<-UrchinPilot2%>% #grazing by weight column
  mutate(Grazing_by_weight= Grazing_Rate/Wet.weight.g.)


# Grouping all trials by habitat and temp----------------------------------------------------------------

#Grouping & Getting average grazing and standard deviation by state and by temp NOT SEPARATED BY TRIAL

UrchinSumstats<-UrchinPilot2 %>%
  group_by(State.Barren.Kelp.,SetTemp) %>%
  summarize(MeanGrazing = mean(Grazing_by_Size),
            StdGrazing = sd(Grazing_by_Size))

# Making plots ------------------------------------------------------------

library(ggplot2)

colnames(UrchinSumstats)[1]="Habitat" #change column name 

UrchinSumstats %>% #plot with all trials including error bars
  ggplot(mapping = aes(x=SetTemp,y=MeanGrazing,color=Habitat)) + 
  geom_point()+
  geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing)) +
  geom_line()+
  scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))+
  labs(x="Temperature (C)", y="Average grazing")

UrchinSumstats%>% #same plot using facet grid
ggplot(mapping=aes(x=SetTemp,y=MeanGrazing))+
  geom_point()+
  facet_grid(.~Habitat)+
  geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing))+
  scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))+ ##To Change the ticks on the x-axis/set manual axis ticks
  labs(x="Temperature (C)", y="Average grazing")
 
# Scatterplot separated by trials -----------------------------------------

colnames(UrchinPilot2)[7]="Habitat" #change column name 

ggplot(UrchinPilot2,aes(x=SetTemp,y=Grazing_by_Size))+
  geom_point()+
  facet_grid(Habitat~Trial..)+
  scale_x_continuous(limits = c(11,22), breaks =c(seq(12,21,3)))+
  labs(x="Temperature (C)", y="Average grazing by size")

# Bar graph ---------------------------------------------------------------
 UrchinSumstats %>%
  ggplot(mapping=aes(x=SetTemp,fill=Habitat,y= MeanGrazing))+
   geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
   labs(x="Temperature (C)", y="Average grazing by size")+
   scale_x_continuous(limits = c(11,22), breaks =c(seq(12,21,3)))


# Trials 1 to 4 -----------------------------------------------------------
  UrchinPilot1to4<-UrchinPilot2%>%
    filter(Trial.. %in% c("1","2","3","4"))
 #OR UPShort<-UrchinPilot1%>%
  ## filter(Trial..=="1"|Trial..=="2"|Trial..=="3"|Trial..=="4")

  #Group by trial, habitat, and temp Create mean and std deviation for trials 1-4
ShortTrial<-UrchinPilot1to4 %>%
  group_by(Habitat,SetTemp,Trial..) %>%
    summarise(MeanGrazing = mean(Grazing_by_Size),
              StdGrazing = sd(Grazing_by_Size))
  
#grouping by Habitat and temp NOT TRIAL but using the 1-4 trial data set
UP1TO4<-UrchinPilot1to4 %>%
 group_by(Habitat,SetTemp)%>%
  summarise(MeanGrazing = mean(Grazing_by_Size),
            StdGrazing = sd(Grazing_by_Size))


# plots (trials 1-4) -------------------------------
# grouped by trial, temp, and habitat for trials 1-4
  ggplot(ShortTrial,aes(x=SetTemp,y=MeanGrazing, color=Habitat))+
    geom_point()+
    facet_grid(.~Trial..)+
  geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing))+
    scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))+
    scale_color_manual(name="Habitat Type",values=c("coral2","cornflowerblue"))
  
  ggplot(ShortTrial,aes(x=SetTemp,y=MeanGrazing, color=Habitat))+ #same plot without error bars
    geom_point()+
    facet_grid(.~Trial..)+
    scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))
# Trials 4 to 6 -----------------------------------------------------------
#Now just looking at trials 4-6 
  UrchinPilot4to6<-UrchinPilot2 %>%
    filter(Trial..%in% c("4","5","6"))
  
  LongTrial<-UrchinPilot4to6 %>% #grouping by trials (4-6 ), habitat and temp
    group_by(Habitat,SetTemp,Trial..) %>%
    summarise(MeanGrazing = mean(Grazing_by_Size),
              StdGrazing = sd(Grazing_by_Size))

# plots trials 4 to 6 -----------------------------------------------------

   ggplot(LongTrial,aes(x=SetTemp,y=MeanGrazing))+
    geom_point()+
    facet_grid(Habitat~Trial..)+
    geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing))+
    scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))+
    scale_color_manual(name="Habitat Type",values=c("coral2","cornflowerblue"))
# Models with emily------------------------------------------------------------------
  model2<-lm(Grazing_by_Size~SetTemp*Habitat+Habitat*Trial.., data=UrchinPilot4to6)
  anova(model2)
  
  model3<-lm(Grazing_by_Size~SetTemp*Habitat*Trial..+(1|Habitat:Tank..), data=UrchinPilot4to6)
  
# plots with model --------------------------------------------------------------------

  ggplot(UrchinPilot4to6,aes(x=SetTemp,y=Grazing_by_Size,color=Habitat))+
    geom_point()+
    geom_smooth(method = lm)+
    scale_x_continuous(limits = c(12,21), breaks =c(seq(12,21,3)))+
    scale_color_manual(name="Habitat Type",values=c("coral2","cornflowerblue")) 
  
# kk added more models for trials 1-4----------------------------------------------------------------

install.packages("lme4")  
install.packages("lsmeans")
install.packages("emmeans")

library(lme4)
library(lsmeans)
library(emmeans)

#no p value??
model1<-lmer(Grazing_Rate~SetTemp + Habitat + SetTemp*Habitat + Wet.weight.g. + (1|Trial..), data=UrchinPilot1to4, REML = TRUE)

summary(model1)


model4<-lmer(Grazing_by_weight~SetTemp + Habitat + SetTemp*Habitat + (1|Trial..), data=UrchinPilot1to4, REML = TRUE)
summary(model4)

model5<-lmer(Grazing_by_Size~SetTemp + Habitat + SetTemp*Habitat + (1|Trial..), data=UrchinPilot1to4, REML = TRUE)
summary(model5)


# EMILY-----------------------------------------------------------------
#New column that categorizes trials as short or long


AC<-c(rep("s",93),rep("l",46)) #unsure of this 

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


