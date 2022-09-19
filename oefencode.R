library(dplyr)
library(tidyverse)
library(ggplot2)
library(tibble)
# inital data import
setwd("C:/Users/203043/Documents/cijferanalyse/")
LesGroepenPerSchooljaar<- read.csv("LesgroepenPerSchooljaar.csv", sep=";", dec=",", header=TRUE)
CijfersPerToets4H5V_22232_20220915<- read.csv("CijfersPerToets4H5V_2223_20220915.csv", sep=";", dec=",", header=TRUE)
LesGroepenPerSchooljaar$VakAfk<- as.character(LesGroepenPerSchooljaar$VakAfk)
LesGroepenPerSchooljaar$Leerlingnummer<- as.character(LesGroepenPerSchooljaar$Leerlingnummer)
CijfersPerToets4H5V_22232_20220915$VakAfk<- as.character(CijfersPerToets4H5V_22232_20220915$VakAfk)
CijfersPerToets4H5V_22232_20220915$Leerlingnummer<- as.character(CijfersPerToets4H5V_22232_20220915$Leerlingnummer)
LesGroepenPerSchooljaar<- LesGroepenPerSchooljaar %>% mutate(combinr =paste(Leerlingnummer, VakAfk, sep="_"))
CijfersPerToets4H5V_22232_20220915<- CijfersPerToets4H5V_22232_20220915 %>% mutate(combinr =paste(Leerlingnummer, VakAfk, sep="_"))

VorigJaar<-  CijfersPerToets4H5V_22232_20220915 %>% filter(Examendossier.resultaten.Schooljaar=="2021/2022")
DitJaar<-  CijfersPerToets4H5V_22232_20220915 %>% filter(Examendossier.resultaten.Schooljaar=="2022/2023")
VorigJaarKlas<-  LesGroepenPerSchooljaar %>% filter(Schooljaar=="2021/2022")
DitJaarKlas<-  LesGroepenPerSchooljaar %>% filter(Schooljaar=="2022/2023")


# code om de lesgroep per cijfer toe te voegen
aantalrijen<- dim(VorigJaar)[1]
tempvec<- array(aantalrijen)
i<- 0
while(i< aantalrijen){
  i<- i+1
  temp<- VorigJaar$combinr[i]
  tempvec[i]<- VorigJaarKlas$Lesgroep[which(VorigJaarKlas$combinr==temp)[1]]
}
VorigJaar2<- cbind(VorigJaar, tempvec)

aantalrijen<- dim(DitJaar)[1]
tempvec<- array(aantalrijen)
i<- 0
while(i< aantalrijen){
  i<- i+1
  temp<- DitJaar$combinr[i]
  tempvec[i]<- DitJaarKlas$Lesgroep[which(DitJaarKlas$combinr==temp)[1]]
}
DitJaar2<- cbind(DitJaar, tempvec)
Cohort2023<- rbind(DitJaar2,VorigJaar2)

# selectie vakken
Cohort2023b<-  Cohort2023 %>% filter(VakAfk != "ckv") %>% filter(VakAfk != "combivwo") %>% filter(VakAfk != "anw") %>% filter(VakAfk != "combihavo") %>% filter(VakAfk != "ckv") %>% filter(VakAfk != "lo") %>% filter(VakAfk != "maat") %>% filter(VakAfk != "lv") %>% filter(VakAfk != "pws") %>% filter(VakAfk != "men")%>% filter(VakAfk != "lob") %>% filter(VakAfk != "cam") %>% filter(VakAfk != "ebcl") %>% filter(VakAfk != "R3F")
#Cohort2023b$VakAfk

table(Cohort2023b$VakAfk)
