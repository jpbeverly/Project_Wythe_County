setwd("~/Project_Wythe_County")

library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(viridis)
library(ggthemes)

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


#PULLING VA DATA:

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



#To request a table or list of tables, use the following code, which returns variables for

#These tables have to do with broadband, healthcare, income, and education
tables<-c("S2801","S2802","S2402","B19013","DP02") 

#All census tracts in VA
acs_VA_tract<-acs_tables(tables = tables,
                      key = .key,
                      #geographic entity is tract
                      geography = "tract",
                      #data restricted to the state of VA
                      state = "VA")

acs_VA_county<-acs_tables(tables = tables,
                         key = .key,
                         #geographic entity is tract
                         geography = "county",
                         #data restricted to the state of VA
                         state = "VA")

#Clean VA data and add relevant variables
colnames=c("Census_tract","County","State")
acs_VA_tract_wide<-acs_wide(data=acs_VA_tract,NAME_col_names = colnames)
acs_VA_tract_wide<-acs_VA_tract_wide%>%
  mutate(PerBroadband=S2801_C02_014)%>%
  mutate(PerHealthcare=round((((S2402_C01_015)/S2402_C01_001)*100),3))%>%
  mutate(MedianIncome=B19013_001)%>%
  mutate(PropAssocPlus=round((((DP02_0063+DP02_0064+DP02_0065)/DP02_0058)*100),3))

#Clean VA county ata and add relevant variables
colnames=c("County","State")
acs_VA_county_wide<-acs_wide(data=acs_VA_county,NAME_col_names = colnames)
acs_VA_county_wide<-acs_VA_county_wide%>%
  mutate(PerBroadband=S2801_C02_014)%>%
  mutate(PerHealthcare=round((((S2402_C01_015)/S2402_C01_001)*100),3))%>%
  mutate(MedianIncome=B19013_001)%>%
  mutate(PropAssocPlus=round((((DP02_0063+DP02_0064+DP02_0065)/DP02_0058)*100),3))%>%
  rename("Code"="GEOID")
acs_VA_county_wide$Code<-str_remove(acs_VA_county_wide$Code,"51")
acs_VA_county_wide<-inner_join(acs_VA_county_wide,FIPS,by="Code")%>%
  select(-c(County.x,State.x))%>%
  rename(County=County.y,
         State=State.y)%>%
  select(Code, County, State,PerBroadband,PerHealthcare,MedianIncome,PropAssocPlus)
 

#MAPPING:

#Get census-level geometry data for Virginia 
VirginiaGeometry<-get_acs(geography = "tract",
                         state="VA",
                         variables = "B19058_002",
                         survey = "acs5",
                         key = .key,
                         year=2018,
                         output = "wide",
                         show_call = T,
                         geometry = T,
                         keep_geo_vars = T)%>%
  select(-c(11:12))

acs_VA_geom<-inner_join(acs_VA_tract_wide,VirginiaGeometry,by="GEOID")%>%
  select(GEOID,Census_tract,County, COUNTYFP, State, STATEFP,PerBroadband,PerHealthcare,MedianIncome,PropAssocPlus,geometry)

#Get county level geometry for VA.
VirginiaCountyGeometry<-get_acs(geography = "county",
                          state="VA",
                          variables = "B19058_002",
                          survey = "acs5",
                          key = .key,
                          year=2018,
                          output = "wide",
                          show_call = T,
                          geometry = T,
                          keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

#PLOTTING

#divide relevant variable into quantiles
quantile.interval = quantile(acs_VA_geom$PerHealthcare, probs=seq(0, 1, by = .2),na.rm = T)
acs_VA_geom$PerHealthcareQuan = cut(acs_VA_geom$PerHealthcare, breaks=quantile.interval, include.lowest = TRUE)
acs_VA_geom$PerHealthcareQuan[is.na(acs_VA_geom$PerHealthcareQuan)]<-"[0,2.72]"

#plot at the state level
ggplot() +
  geom_sf(data=acs_VA_geom,aes(geometry=geometry,fill = PerHealthcareQuan, color = PerHealthcareQuan),show.legend = "fill") +
  geom_sf(data=VirginiaCountyGeometry,fill="transparent",color="black",size=0.5,show.legend = F)+
  geom_sf(data=VirginiaCountyGeometry%>%filter(COUNTYFP==197),fill="transparent",color="white",size=0.5,show.legend = F)+
  labs(title="Virginia",subtitle="Percent of population working in healthcare")+
  scale_fill_viridis(discrete=T,name = "Quantiles", labels = c("1","2","3","4","5"),guide = guide_legend(reverse=TRUE))+
  scale_color_viridis(discrete=T,name = "Quantiles", labels = c("1","2","3","4","5"),guide = guide_legend(reverse=TRUE))

#Plot at the county level.  I manually set the beginning and end of the viridis scale to resemble the scale at the state level for Wythe.
#The scale goes from 0 to 1, and I used the quantiles as a rough guide.
ggplot() +
  geom_sf(data=acs_VA_geom%>%filter(COUNTYFP==197),aes(geometry=geometry,fill = PerHealthcareQuan, color = PerHealthcareQuan),show.legend = "fill") +
  geom_sf(data=VirginiaCountyGeometry%>%filter(COUNTYFP==197),fill="transparent",color="black",size=0.5,show.legend = F)+
  labs(title="Wythe County",subtitle="Percent of population working in healthcare")+
  scale_fill_viridis(discrete=T,begin=0,end=1,name = "Quantiles", labels = c("1","3","4","5"),guide = guide_legend(reverse=TRUE))+
  scale_color_viridis(discrete=T,begin=0,end=1,name = "Quantiles", labels = c("1","3","4","5"),guide = guide_legend(reverse=TRUE))

