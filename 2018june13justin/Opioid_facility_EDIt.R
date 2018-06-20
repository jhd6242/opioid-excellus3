library(shiny)
library(dplyr)
library(sp)
library(raster)
library(maptools)
library(leaflet)
library(RColorBrewer)
library(tigris)
library(rgdal)
library(shinythemes)
library(reshape2)
library(data.table)

#read the public dataset

xx<- read.csv('All_Payer.csv')

#Preprocessing
colnames(xx)[colnames(xx)=='ï..Year']<- 'Year'
colnames(xx)[colnames(xx)=='Patient.County.Name']<- 'NAME_2'

#as.numeric(gsub(",","",xx[,]))

xx[,6:32]<- as.data.frame(lapply(xx[,6:32], function(y) as.numeric(gsub(",", "", y))))


xx[,c(1,3)]<- lapply(xx[,c(1,3)], as.character)

xx[,c(4,5)]<- lapply(xx[,c(4,5)], as.factor)

xx[,c(2,6:32)]<- lapply(xx[,c(2,6:32)], as.numeric)

irrelevant_rows<- c('Other than New York State','Unknown','Statewide Total')
xx<- xx[!xx$NAME_2 %in% irrelevant_rows,]

xx$NAME_2<- gsub('([A-z]+) .*', '\\1', xx$NAME_2)

xx$NAME_2[xx$NAME_2=='New']<- "New York"

xx$NAME_2[xx$NAME_2=="St. Lawrence"]<- "Saint Lawrence"
#-----------------------------------------------------------------

#Combine all facility visits across all the payers
df <- xx %>% group_by(Year, NAME_2) %>% summarise_each(funs(sum), -Rural.Urban,-Payer)
df<- as.data.frame(append(df, list(Payer='All'), after = 3))

Rural.Urban<- xx[xx$Year==2014 & xx$Payer=='Commercial', c('NAME_2', 'Rural.Urban')]

County.Code<- xx[xx$Year==2014 & xx$Payer=='Commercial', c('NAME_2', 'Patient.County.Code')]

df$Rural.Urban<- Rural.Urban[match(paste(df$NAME_2), paste(Rural.Urban$NAME_2)), 'Rural.Urban']

df$Patient.County.Code<- County.Code[match(paste(df$NAME_2), paste(County.Code$NAME_2)), 'Patient.County.Code']

df<- df[,c(1, 3,2, 32, 4:31)]


df[,c(1,3)]<- lapply(df[,c(1,3)], as.character)

df[,c(4,5)]<- lapply(df[,c(4,5)], as.factor)

df[,c(2,6:32)]<- lapply(df[,c(2,6:32)], as.numeric)

df$ER.Rate.Per.1000<- (df$ER.Opioid/ df$ER.Visits)*1000

df$Inpatient.Total.Rate.Per.1000<- (df$Inpatient.Total.Opioid/ df$Inpatient.Total.Discharges )*1000

df$Ambulatory.Surgery.Rate.Per.1000<- (df$Opioid.Ambulatory.Surgery.Visits/df$Ambulatory.Surgery.Visits)*1000


df$Outpatient.Rate.Per.1000<- (df$Outpatient.Opioid/df$Outpatient.Visits)*1000


df$Overall.Rate.Per.1000<- (df$Overall.Opioid/df$Overall.Discharges)*1000


xx<- rbind(xx, df)
#-----------------------------------------------------------------

#Combine all the facility visits across all the years
df <- xx %>% group_by(NAME_2, Payer) %>% summarise_each(funs(sum), -Rural.Urban, -Year)
df<- as.data.frame(append(df, list(Year='All'), after = 1))

df$Rural.Urban<- Rural.Urban[match(paste(df$NAME_2), paste(Rural.Urban$NAME_2)), 'Rural.Urban']

df$Patient.County.Code<- County.Code[match(paste(df$NAME_2), paste(County.Code$NAME_2)), 'Patient.County.Code']

df<- df[,c(2, 4,1, 32, 3, 5:31)]


df[,c(1,3)]<- lapply(df[,c(1,3)], as.character)

df[,c(4,5)]<- lapply(df[,c(4,5)], as.factor)

df[,c(2,6:32)]<- lapply(df[,c(2,6:32)], as.numeric)

df$ER.Rate.Per.1000<- (df$ER.Opioid/ df$ER.Visits)*1000

df$Inpatient.Total.Rate.Per.1000<- (df$Inpatient.Total.Opioid/ df$Inpatient.Total.Discharges )*1000

df$Ambulatory.Surgery.Rate.Per.1000<- (df$Opioid.Ambulatory.Surgery.Visits/df$Ambulatory.Surgery.Visits)*1000


df$Outpatient.Rate.Per.1000<- (df$Outpatient.Opioid/df$Outpatient.Visits)*1000


df$Overall.Rate.Per.1000<- (df$Overall.Opioid/df$Overall.Discharges)*1000


xx<- rbind(xx, df)

#-----------------------------------------------------------------

#load the population dataset
population_df<- read.csv('C:/Users/mpatel2/Desktop/Opioids/Annual_Population_Estimates_for_New_York_State_and_Counties__Beginning_1970.csv')

#Preprocessing
population_df<- population_df[!population_df$ï..FIPS.Code %in% c(36000), ]

population_df$Geography<- gsub('([A-z]+) .*', '\\1', population_df$Geography)

population_df$Geography[population_df$Geography=='New']<- "New York"


population_df$Geography[population_df$Geography=="St. Lawrence"]<- "Saint Lawrence"

population_df$Population<- as.numeric(gsub(",", "", population_df$Population))

population_df[,c(2,3)]<- lapply(population_df[,c(2,3)], as.character)

colnames(population_df)[colnames(population_df)=='Geography']<- 'NAME_2'

#Join population dataset and the facility visit dataset
xx<- left_join(xx, population_df[, c('Year', 'NAME_2','Population')], by=c('Year','NAME_2'))

#Calculate the avg population for all the years
avg_population<- aggregate(Population~NAME_2, xx[xx$Year!='All',], mean)

#Append the population column for 'All'' the years
x<- left_join(xx[xx$Year=='All',], avg_population, by=c('NAME_2'), all.x=TRUE)

xx[xx$Year=='All','Population']<- as.integer(x$Population.y)

xx[is.na(xx)]<- 0
opioid_df<- xx

#Save the global environment
save.image("opioid_facility.Rdata")
