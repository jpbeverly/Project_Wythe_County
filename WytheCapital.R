setwd("~/Project_Wythe_County")

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(leaflet)

#show available variables in a particular ACS survey
acs5<-load_variables(2018, "acs5", cache=T)
View(acs5)

acs5_subject <- load_variables(2018, "acs5/subject", cache=T)
View(acs5_subject)

acs5_profile<- load_variables(2018, "acs5/profile", cache=T)
View(acs5_profile)


#FUNCTIONS:

# 1. "acs_tables" calls "get_acs" (from tidycensus) on a vector of table names. It returns a dataframe of 
# all the tables bound together.  The function requires a vector of table names, 
# a census API key, and a geographical unit.  The user can add other parameters as well.

acs_tables<-function(tables,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(tables)){
    data<-get_acs(geography = geography,
                  table = tables[i],
                  key = key,
                  show_call = T,
                  cache_table=T,
                  ...
    )
    acs_data<-rbind(acs_data,data.frame(data))
  }
  return(acs_data)
}

# 2. "acs_wide" cleans the data returned from a census API call.  More specifically, 
# it separates the variable column into separate variables, and it separates "NAME" into 
# different columns with pre-defined column names (NAME_col_names). The function also
# drops the "margin of error" column.

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#3. acs_years retrieves individual variables (or a list of variables) across a series of years.
acs_years<-function(years,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(years)){
    acs<-get_acs(geography = geography,
                 #variables = vars,
                 key = key,
                 year=years[i],
                 output = "wide",
                 show_call = T,
                 geometry = F,
                 ...)
    acs_data<-(rbind(acs_data,data.frame(acs)))
  }
  acs_data<-cbind(acs_data,year=rep((years),each=length(unique(acs_data$GEOID))))
  return(acs_data)
}


#4. "acs_years_tables" uses two previously defined functions (acs_tables and acs_wide) to return multiple 
# variable tables across multiple years in one single tibble.  A couple of notes: the way that 
# get_acs handles variables before 2013 varies, so this function only works for 2013 and after.
# For variable tables before 2013, use acs_tables to pull individual sets of tables.  Also, I have 
# not included "geometry" in the function.  If the user includes geometry, he/she may need 
# to modify the call to acs_wide.


acs_years_tables<-function(tables,years,key,geography,NAME_col_names,...){
  acs_data<-NULL
  for (j in 1:length(years)){
    acs<-acs_tables(tables=tables,year=years[j],key=key,geography = geography,...)
    year<-rep(years[j],times=length(acs$GEOID))
    acs_years2<-cbind(year,data.frame(acs))
    acs_data<-(rbind(acs_data,acs_years2))
  }
  acs_data<-acs_wide(acs_data,NAME_col_names = NAME_col_names)
  return(acs_data)
}


tables<-c("DP04","S2501","S1501","DP03","S2503")
years<-c(2018)
colnames=c("County","State")

Wythe<-acs_tables(tables,
                  .key,
                  geography="county",
                  state="VA",
                  county="Wythe county")

Wythe<-acs_wide(Wythe,colnames)

#Clean data and add relevant variables
Wythe<-Wythe%>%
  #Household size
  mutate("1"=S2501_C01_002)%>%
  mutate("2"=S2501_C01_003)%>%
  mutate("3"=S2501_C01_004)%>%
  mutate("4+"=S2501_C01_005)%>%
  mutate(HouseholdsWithChildren=S2501_C02_032)%>%
  #Level of education
  mutate("Less than HS"=S1501_C01_007+S1501_C01_008)%>%
  mutate("HS Diploma"=S1501_C01_009)%>%
  mutate("Some College (No Degree)"=S1501_C01_010)%>%
  mutate("Associate's Degree"=S1501_C01_011)%>%
  mutate("BA/BS or Higher"=S1501_C01_015)%>%
  #Income
  mutate("<$5,000"=S2503_C01_002)%>%
  mutate("$5,000-$9,999"=S2503_C01_003)%>%
  mutate("$10,000-$14,999"=S2503_C01_004)%>%
  mutate("$15,000-$19,999"=S2503_C01_005)%>%
  mutate("$20,000-$24,999"=S2503_C01_006)%>%
  mutate("$25,000-$34,999"=S2503_C01_007)%>%
  mutate("$35,000-$49,999"=S2503_C01_008)%>%
  mutate("$50,000-$74,999"=S2503_C01_009)%>%
  mutate("$75,000-$99,999"=S2503_C01_010)%>%
  mutate("$100,000-$149,999"=S2503_C01_011)%>%
  mutate(">$150,000"=S2503_C01_012)%>%
  #Home values
  mutate("<$50,000"=DP04_0081)%>%
  mutate("$50,000-$99,999"=DP04_0082)%>%
  mutate("$100,000-$149,999"=DP04_0083)%>%
  mutate("$150,000-$199,999"=DP04_0084)%>%
  mutate("$200,000-$299,999"=DP04_0085)%>%
  mutate("$300,000-$499,999"=DP04_0086)%>%
  mutate("$500,000-$999,999"=DP04_0087)%>%
  mutate(">$1,000,000"=DP04_0088)%>%
  #Employment Industry
  mutate("Ag, forestry, mining"=DP03_0033)%>%
  mutate("Construction"=DP03_0034)%>%
  mutate("Manufacturing"=DP03_0035)%>%
  mutate("Wholesale Trade"=DP03_0036)%>%
  mutate("Retail Trade"=DP03_0037)%>%
  mutate("Transportation, utilities"=DP03_0038)%>%
  mutate("Information"=DP03_0039)%>%
  mutate("Finance, insurance"=DP03_0040)%>%
  mutate("Professional, scientific, management"=DP03_0041)%>%
  mutate("Education, healthcare"=DP03_0042)%>%
  mutate("Arts, recreation"=DP03_0043)%>%
  mutate("Other"=DP03_0044)%>%
  mutate("Public Admin"=DP03_0045)
  
Wythe<-Wythe%>%select(1452:1492)

Variable=c(rep("Household Size",times=4),rep("Level of Education",times=5),
             rep("Household Income",times=11),rep("Home Values",times=7),
             rep("Industry",times=13))  

Wythe_long<-pivot_longer(Wythe,cols = colnames(Wythe))%>%
  filter(name!="HouseholdsWithChildren")%>%
  bind_cols("Variable"=Variable)

Wythe_long$Variable<-as.factor(Wythe_long$Variable)

write_csv(Wythe_long,"/Users/adamwells/Project_Wythe_County/ShinyFinalPresentation/Wythe_long")

