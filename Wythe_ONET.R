library(readr)
library(tidyverse)
library(readxl)
library(ggalluvial)
library(viridis)
library(forcats)
library(ggrepel)
library(ggthemes)
library(collapsibleTree)

#Import and clean data from ONET
All_Career_Clusters<- read_csv("~/Desktop/All_Career_Clusters.csv")
All_Industries <- read_csv("~/Desktop/All_Industries.csv")
Education_Training_and_Experience <- read_excel("~/Desktop/Education, Training, and Experience.xlsx")
Education_Training_and_Experience_Categories <- read_excel("~/Desktop/Education, Training, and Experience Categories.xlsx")
Knowledge <- read_excel("~/Desktop/Knowledge.xlsx")%>%
  filter(`Scale Name`=="Importance")%>%
  select(Code=`O*NET-SOC Code`,Element_Name=`Element Name`,Importance=`Data Value`)
Projected_openings<-All_Industries%>%
  select(Code,Job_Openings=`Projected Job Openings (2018-2028)`)

#Put data in the proper for for a collapsible tree
Tree<-left_join(All_Career_Clusters,Projected_openings,by="Code")%>%
  left_join(Knowledge,by="Code")%>%
  select(-Code,Career_Cluster=`Career Cluster`,Career_Pathway=`Career Pathway`)

#Fix long names
Tree[(24235:24267),3]<-"Grinding and Buffing Machine Operators"

#code to create tree.  A version of this is used in the shiny app
Tree%>%
  filter(Job_Openings>5300)%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
  filter(Importance>=2.88)%>%
  collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Element_Name"),
                         root="Industries",
                         attribute = "Importance",
                  width=1450,
                  zoomable=F)

#Save Tree data in the Shiny folder
write_csv(Tree,"/Users/adamwells/Project_Wythe_County/ShinyFinalPresentation/Tree")

