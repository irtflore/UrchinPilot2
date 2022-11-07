#October 12, 2022
#Urchin Pilot data
#session: set working directory to whatever file you have the raw data in or do the following

# Basic commands ----------------------------------------------------------
#super wow!
##This reads out the columns
str(UrchinPilot)
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
UrchinPilot<-read.csv("UrchinPilot.csv")

        #  Changing from characters to numbers
UrchinPilot$Size.mm.<-as.numeric(as.character(UrchinPilot$Size.mm.))
UrchinPilot$Wet.weight.g.<-as.numeric(as.character(UrchinPilot$Wet.weight.g.))
UrchinPilot$SetTemp<-as.numeric(as.integer(UrchinPilot$SetTemp))
      #Creating new column(2 ways)
# Mutate to create new columns
UrchinPilot<-UrchinPilot %>% 
  mutate(Grazing_by_Size = (Kelp.start.weight - Kelp.end.weight)/Size.mm.)

UrchinPilot<-UrchinPilot %>% 
  mutate(Grazing_Rate = Kelp.start.weight-Kelp.end.weight )

####   OR
#UrchinPilot$GrazingRate<-UrchinPilot$Kelp.start.weight-UrchinPilot$Kelp.end.weight
#UrchinPilot$Grazing_by_size<-UrchinPilot$GrazingRate/UrchinPilot$Size.mm.

#Deletes rows: These rows had urchins that escaped and did not eat
UrchinPilot1<-UrchinPilot[-c(3,20,21,24,28,47,53,82,97,121,106,130),]

##Group and select certain columns
# BarrenUrchin<-UrchinPilot %>% 
#   filter(State.Barren.Kelp. == "B")%>%
#   select(Urchin.ID,Grazing_by_Size,SetTemp,StandardDevSize)
# KelpUrchin<-UrchinPilot %>%
#   filter(State.Barren.Kelp.=="K")%>%
#   select(Urchin.ID, Grazing_by_Size,SetTemp,StandardDevSize)


#Grouping & Getting average grazing by state and by temp
UrchinSumStats <- UrchinPilot1 %>%
  group_by(State.Barren.Kelp., SetTemp)%>%
  summarize(MeanGrazing = mean(Grazing_by_Size),
            StdGrazing = sd(Grazing_by_Size))

UP3<-UrchinPilot1%>%
  group_by(Trial..,State.Barren.Kelp.,SetTemp)%>%
summarize(MeanGrazing=mean(Grazing_by_Size),StdGrazing=sd(Grazing_by_Size))

# Making plots ------------------------------------------------------------


library(ggplot2)
UrchinSumStats %>%
  ggplot(mapping = aes(x=SetTemp,y=MeanGrazing,color=State.Barren.Kelp.)) + 
  geom_point()+
  geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing)) +
  geom_line()+
  scale_x_continuous(limits = c(10,24), breaks =c(seq(12,21,3)))

ggplot(UrchinSumStats,aes(x=SetTemp,y=MeanGrazing, color=State.Barren.Kelp.))+
  geom_point()+
  facet_grid(.~State.Barren.Kelp.)+
  geom_errorbar(aes(ymin = MeanGrazing - StdGrazing, ymax = MeanGrazing + StdGrazing))
 
#To Change the ticks on the x-axis :scale_x_continuous(limits = c(2500, 6500), breaks = c(seq(2500, 6500, 1000))) # set manual axis ticks
# set the x and y axis range: ylim(180, 220) + #oops cut off some points, need bigger scale #xlim(1000,10000)+ #maybe you know this colony could be bigger or smaller, show the gap 

#While we are on facet things, you can also facet by more than two with "+" like: facet_grid(cat1~cat2+cat3) Could use this to show each trial\

ggplot(UrchinPilot1,aes(x=SetTemp,y=Grazing_by_Size, color=State.Barren.Kelp.))+
  geom_point()+
  facet_grid(State.Barren.Kelp.~Trial..)


 UrchinSumStats %>%
  ggplot(mapping=aes( x=SetTemp,fill=State.Barren.Kelp.,y= MeanGrazing))+
   geom_density()
  ##geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))
  
#This graohs 2 boxplots for 1 x value : ggplot(UrchinSumStats, aes(x =SetTemp, fill= State.))+
##I need 2 boxplots per temperature treatment, but this keep combining the mean grazing rates of all urchins from barrens and kelp forest. i want to have 

 UP3%>%
 ggplot(mapping =aes(x=SetTemp,y=MeanGrazing,fill=State.Barren.Kelp.))+
   geom_boxplot()+
   facet_grid(.~Trial..)
 

 
 # NIKI HELP ---------------------------------------------------------------

install.packages("lme4")  
install.packages("lsmeans")


URCH1<-lmer(Grazing_by_Size~UrchinPilot1$SetTemp+UrchinPilot1$State.Barren.Kel.+UrchinPilot1$State.Barren.Kelp.*UrchinPilot1$SetTemp+(1|UrchinPilot1$Trial..),UrchinPilot1)
