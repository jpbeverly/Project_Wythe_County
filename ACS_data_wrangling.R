setwd("~/Project_Wythe_County")

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)


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


#DATA:

#To request specific variables (rather than tables of variables), use the following code:

#create a vector of variables to return
vars<-c(households_= "B19058_001",SNAP_="B19058_002")

#"get_acs" creates a census api call using the vector of variables specified above
acs<-get_acs(geography = "tract",
             state="VA",
             county = "Wythe county",
             variables = vars,
             survey = "acs5",
             key = .key,
             output = "wide",
             show_call = T,
             geometry = T,
             keep_geo_vars = T)

#Separate the NAME column into Census_tract, County, and State
colnames=c("Census_tract","County","State")
acs<-separate(acs,NAME.y, into=colnames, sep = ", ")

#To make a map:
ggplot(acs, aes(fill = SNAP_E/households_E, color = SNAP_E/households_E)) +
  geom_sf() +
  labs(title="Wythe County",subtitle="Proportion of households receiving SNAP")+
  theme(legend.title = element_blank())

#To request a table or list of tables, use the following code, which returns variables for
#all census tracts in VA:

tables<-c("S2801","S2802") #These tables have to do with broadband
acs_tract<-acs_tables(tables = tables,
                      key = .key,
                      #geographic entity is tract
                      geography = "tract",
                      #data restricted to the state of VA
                      state = "VA")


#Changes the resulting data frame from long to wide format and drops MOE
colnames=c("Census_tract","County","State")
acs_tract_wide<-acs_wide(data=acs_tract,NAME_col_names = colnames)

#This code returns a list of variables for all states in the US.
acs_state<-acs_tables(tables = tables,
                      key = .key,
                      #geographic entity is "state."  Data includes all states.
                      geography = "state")
colnames="State"
acs_state_wide<-acs_wide(data=acs_state,NAME_col_names = colnames)


