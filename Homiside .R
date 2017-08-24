#calling libraries
library(dplyr)
library(ggplot2)
#############################################
DeathRec<-read.csv("/Users/WejdanAthobaiti/Downloads/DeathRecords/DeathRecords.csv",stringsAsFactors = F,header = T)
Races<-read.csv("/Users/WejdanAthobaiti/Downloads/DeathRecords/Race.csv",stringsAsFactors = F,header = T)
AgeType<-read.csv("/Users/WejdanAthobaiti/Downloads/DeathRecords/AgeType.csv",stringsAsFactors = F,header = T)
Edu2003<-read.csv("/Users/WejdanAthobaiti/Downloads/DeathRecords/Education2003Revision.csv",stringsAsFactors = F,header = T)
Marital_table<-read.csv("../input/MaritalStatus.csv",stringsAsFactors = F,header = T)
mannerOfDeath = read.csv("/Users/WejdanAthobaiti/Downloads/DeathRecords/MannerOfDeath.csv")
##############################################
filteredRecords = DeathRec[,c('Id', "MannerOfDeath", "Education2003Revision")]
# Merge together manner of death and educational info
filteredRecords = merge(filteredRecords, mannerOfDeath, by.x="MannerOfDeath", by.y="Code")
filteredRecords$MannerOfDeath = filteredRecords$Description
filteredRecords$Description = NULL
filteredRecords = merge(filteredRecords, Edu2003, by.x="Education2003Revision", by.y="Code")
filteredRecords$Education2003Revision = filteredRecords$Description
filteredRecords$Description = NULL

#Check the distribution of manner of death in the dataset
ggplot(filteredRecords, aes(x=MannerOfDeath, color=MannerOfDeath, fill=MannerOfDeath)) + 
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Death cases and  Manner of Deaths")

#Check the distribution of education in the dataset
ggplot(filteredRecords, aes(x=Education2003Revision, color=Education2003Revision, fill=Education2003Revision)) + 
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Death cases per Education Levels")

##############################################################
#----------Analysing the Homiside MannerofDeath---------#
#############################################################
# Gitting Homiside records when age type is 1 which is>> years
Homiside<-DeathRec %>%
  filter(MannerOfDeath==3, AgeType==1)
#merge data in order to plot it with the descriptions not the codes

Homi= merge(Homiside, Races, by.x="Race", by.y="Code")
Homi$Race = Homiside$Description
Homi$Description = NULL

Homiside = merge(Homiside, Edu2003, by.x="Education2003Revision", by.y="Code")
Homiside$Education2003Revision = Homiside$Description
Homiside$Description = NULL

table(Homiside$AgeType)
View(Homiside)

# the percentage of Humiside case per Sex
#79% of the Himiside cases are men while women represnts only 20%
# the mean of ages in men is 34 while in women is 38
HomidideSex<-Homiside%>%
  group_by(Sex)%>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id),Mean=mean(Age),Std=sd(Age))
#group data by Sex and caclculate mean 
HomisideDist<- Homiside %>%
  group_by(Sex) %>%
  mutate(Mean=mean(Age))

ggplot(HomisideDist,aes(x=Age,y=..density.., fill=Sex))+
  geom_histogram(binwidth = 1)+
  geom_density()+
  geom_vline(aes(xintercept=Mean),color="black", linetype="dashed", size=1)+
  facet_grid(.~Sex)+
  labs(title="  US Homicide Cases Distribution in 2014 ")+
  scale_x_continuous(limits=c(0,100))

#calculate the percentage of each Race in the original data set for all death manners
# and then calculate the percentage of Race in Homicide cases

#The results:  a significant percentage of Homiside cases were black (around 48%),
#although in all the death cases black represent around (around 11%) 
RacePercent<-DeathRec %>%
  group_by(Race) %>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id))

RacePer<-Homiside %>%
  group_by(Race) %>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id))
# the percentage of Homiside cases and age mean per Race
HomidideRace<-Homiside%>%
  group_by(Race)%>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id),Mean=mean(Age),Std=sd(Age))
# the percentage of Homiside case per Race and Sex
  ByRace<-Homiside %>%
         group_by(Race,Sex) %>%
         summarise(Sum=n(),Percentage=100*n()/length(.$Id)) 
       
       ggplot(ByRace,aes(x=factor(Race),y=Percentage,fill=Sex))+
         geom_bar(stat="identity")+
         # facet_grid(.~Sex) +
         scale_x_discrete(name="Race.Code")+
         scale_y_discrete(name="Percentage%",breaks=seq(0, 40, by = 2))+
         labs(title="Percentage of Homicide Cases per Race in 2014")
       
#rename the first column as Code to use it to merge with the lookup table     
       colnames(RacePercent)[1]="Code"
       merge(Races,RacePercent,all=T)
       
       ggplot(Homiside,aes(y=Age,x=factor(Race)))+
         geom_boxplot(aes(color=Sex))+
         facet_grid(.~Sex)+
         scale_x_discrete(name="Race.Code")+
         labs(title="Distribution of Homicide Cases Over Age per Race")+
         scale_y_continuous(limits=c(0,100))
#################################################################     
#--------------------Education Vs Race & Sex--------------------#  
#################################################################
#the percentage of each education level in the original data set for all death manners
EduPercent<-DeathRec %>%
         group_by(Education2003Revision) %>%
         summarise(Cases=n(),Percentage=100*n()/length(.$Id))
      
#rename the first column as Code to use it to merge with the lookup table
       colnames(EduPercent)[1]="Code"
       merge(Edu2003,EduPercent,all=T)

#All Race vs Education level
ByEdu<-Homiside %>%
         group_by(Education2003Revision,Race,Age) %>%
         summarise(Sum=n())
       library(grid)
       ggplot(ByEdu,aes(y=Sum,x=Age,color=Education2003Revision))+ 
         geom_point(aes(color=factor(Education2003Revision),size=Sum),alpha=0.5)+ 
         facet_grid(.~Race)+
         scale_y_continuous(name="Number of Homiside Cases",limits=c(0,100))+
         scale_colour_discrete("Education Level \n 2003 Rev. Code")+
         scale_size_continuous("Number of \n Homiside Cases", breaks=seq(0,300,by=50))+
         theme(legend.position="top", legend.box = "horizontal")+
         labs(title="Number of Homicide Cases in each Race Grouped by Age and Education ")+
         theme(panel.spacing.x = unit(0.09, "lines"))+
         scale_x_continuous(limits=c(0,100))
       
# since the RacePercent of People shows that the most common death by Homiside were White, Black, Amrican indian, and Asian
#I filtered the data accordingly
ra<-Homiside%>%
         filter((Race==1)|(Race==2)|(Race==3)|(Race==68))
       
Race_names <-list('1'="White",
                 '2'="Balck",
                    '3'="Indian",
                      '68'= "Asian")
Race_labeller <- function(variable,value){
         return(Race_names[value])
}

# white,black, Amrican indian, and Asian vs Education level
ByEdu2<-ra %>%
         group_by(Education2003Revision,Race,Age) %>%
         summarise(Sum=n())
       
         library(grid)
ggplot(ByEdu2,aes(y=Sum,x=Age))+ 
         geom_point(aes(color=factor(Education2003Revision),size=Sum),alpha=0.5)+ 
         facet_grid(.~Race,labeller=as_labeller(Race_labeller))+
         scale_y_continuous(name="Number of Homicide Cases",limits=c(0,100))+
         scale_colour_discrete("Education Level \n 2003 Rev. Code")+
         scale_size_continuous("Number of \n Homicide Cases", breaks=seq(0,300,by=50))+
         theme(legend.position="top", legend.box = "horizontal")+
         labs(title="Number of Homicide Cases\n in the 4 Most common Races Grouped by Age and Education ")+
         theme(panel.spacing.x = unit(0.09, "lines"))+
         scale_x_continuous(limits=c(0,100))
       
#Sex and Education level
       #death$Education2003Revision <- factor(death$Education2003Revision)  
       #death$Education2003Revision <- factor(death$Education2003Revision,levels = c("1","2","3","4","5","6","7","8","9"),labels =c("8th grade or less","9 - 12th grade, no diploma","high school graduate or GED completed","some college credit, but no degree","Associate degree","Bachelor's degree","Master's degree","Doctorate or professional degree","Unknown"))
ByEdu3<-Homiside %>%
         group_by(Education2003Revision,Sex,Age) %>%
         summarise(Sum=n())
       
ggplot(ByEdu3,aes(y=Sum,x=Age))+ 
         geom_point(aes(color=factor(Education2003Revision),size=Sum),alpha=0.5)+ 
         facet_grid(.~Sex)+
         scale_y_continuous(name="Number of Homicide Cases")+
         scale_colour_discrete("Education Level \n 2003 Rev. Code")+
         scale_size_continuous("Number of \n Homicide Cases", breaks=seq(0,300,by=50))+
        # theme(legend.position="top", legend.box = "horizontal")+
         labs(title="Number of Homicide Cases in each gender Grouped by Age and Education ")+
         scale_x_continuous(limits=c(0,100))
        
#######################################################
#-------------------MaritalStatus---------------------
#######################################################
ByMarital<-Homiside %>%
         group_by(MaritalStatus,Sex) %>%
         summarise(Cases_Homiside=n(),Percentage_Homiside=100*n()/length(.$Id))
       
ggplot(ByMarital,aes(x=MaritalStatus,y=Percentage_Homiside,fill=Sex))+
         geom_bar(stat="identity")+
         #facet_grid(.~Sex)+
         scale_x_discrete(name="Marital Status",
                          labels=c("Divorced","Married","Single","unknown","Widowed"))+
         theme(axis.text.x=element_text(angle = 90,vjust=1))+
         scale_y_continuous(name="Percentage%")+
         labs(title="Percentage of Homicide Cases Per Marital Status")
       
ByMarital2<-Homiside%>%
         group_by(MaritalStatus,Sex, Race) %>%
         summarise(Cases_Homiside=n(),Percentage_Homiside=100*n()/length(.$Id))
       
ggplot(ByMarital2,aes(x=factor(Race),y=Percentage_Homiside, fill=Sex))+
         geom_bar(stat="identity")+
         facet_grid(.~MaritalStatus)+
         scale_x_discrete(name="Race code")+
         theme(axis.text.x=element_text(angle = 90,vjust=1))+
         scale_y_continuous(name="Percentage%")+
         labs(title="Percentage of Homicide Cases Per Race and Marital Status")

#Percentage of Homicide Cases Per the most 4 common Race and Marital Status
ByMarital0<-ra%>%
  group_by(MaritalStatus,Sex, Race) %>%
  summarise(Cases_Homiside=n(),Percentage_Homiside=100*n()/length(.$Id))

ggplot(ByMarital0,aes(x=MaritalStatus,y=Percentage_Homiside, fill=Sex))+
  geom_bar(stat="identity")+
  facet_grid(.~Race,labeller=as_labeller(Race_labeller))+
  scale_x_discrete(name="Race code")+
  theme(axis.text.x=element_text(angle = 90,vjust=1))+
  scale_y_continuous(name="Percentage%")+
  labs(title="Percentage of Homicide Cases Per\n the 4 Most common Races and Marital Status")
       
# The cases and percentage of Homisde in each marital status
 ByMarital3<-Homiside %>%
         group_by(MaritalStatus) %>%
         summarise(Cases_Homiside=n(),Percentage_Homiside=round((Percentage_Suicide=100*n()/length(.$Id)),2))
# The cases and percentage of  marital status in alll data
ByMarital_All<-DeathRec %>%
         group_by(MaritalStatus) %>%
         summarise(Cases_All=n(),Percentage_All=round((Percentage_All=100*n()/length(.$Id)),2))
# The cases and percentage  marital status in all data    
MaritalData<-merge(ByMarital3,ByMarital_All) %>% 
         merge(Marital_table,by.x=("MaritalStatus"),by.y=("Code"))
# Although the percentage of single in all data is only %12.66,
#it % 64.9 when it comes to Homiside. On the other side Marride ppl are the larger group in the data % 37,
#but they represnt only % 18 when it comes to Homiside
MaritalData
       