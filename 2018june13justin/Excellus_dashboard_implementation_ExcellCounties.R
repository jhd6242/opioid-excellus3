library(stringr)
library(reshape)
library(readr)
library(plyr)
library(quantmod)

#Read the final_claims file from the source
final_claim <- read_csv("FINAL_CLAIMS.csv")


final_claim <- final_claim[!grepl("Others", final_claim$Facility_visit),]

#REname he columns of the dataset
colnames(final_claim)<- c('Year','County','Payer','Facility_visit', 'Claim_type','count')

#Convert it into a dataframe
final_claim<- data.frame(final_claim)

#Convert County column into defined levels
final_claim$County<- as.factor(final_claim$County)

#Convert Payer column into defined levels
final_claim$Payer<- as.factor(final_claim$Payer)

#Convert Facility_visit column into defined levels
final_claim$Facility_visit<- as.factor(final_claim$Facility_visit)

#Convert Claim_type column into defined levels
final_claim$Claim_type<- as.factor(final_claim$Claim_type)

#Convert Year column into defined levels
final_claim$Year<- as.factor(as.character(final_claim$Year))

#Define a new column 'visit' to flag opioid claims
final_claim$visit<- ifelse(final_claim$Claim_type=='0', paste0(final_claim$Facility_visit,' Counts'),paste0('Opioid ',final_claim$Facility_visit,' Counts') )

#Convert 'visit' column into defined levels
final_claim$visit<- as.factor(final_claim$visit)

#Select columns that are relevant
final_claim<- final_claim[,c(1,2,3,7,6)]

#--------------------------------------------

#Define a grid that will create the missing facility for every county. Every county will not have all facility visits (ER, Inpatient, Outpatient) and Payers(Commercial, Medicaid, Medicare). So counties which have some missing payers or facility visits, the below grid function will create those missing attributes and input NA as their value
missing_Payers_or_Visits <- with(final_claim, expand.grid(Year=levels(Year),County = levels(County), Payer = levels(Payer), visit= levels(visit)))

#merge all the missing payers and counties to the original final_claim dataset
final_claim <- merge(final_claim, missing_Payers_or_Visits, all.y = TRUE)

#Replace all NA values with 0
final_claim$count[is.na(final_claim$count)]<- 0

#Convert every value in the visit column into a new column (ER, Inpatient, Outpatient) by grouping Year, County and Payer
final_claim<-cast(final_claim, Year+County+Payer ~ visit)

#List Excellus regions
# create vectors for the counties in our regions
cny.region <- c("St Lawrence", "Jefferson", "Lewis", "Oswego", "Onondaga", "Cayuga", "Tompkins", "Cortland")
southern.tier.region <- c("Chenango", "Broome", "Tioga", "Chemung", "Schuyler", "Steuben")
rochester.region <- c("Monroe", "Wayne", "Livingston", "Ontario", "Yates", "Seneca")
utica.region <- c("Franklin", "Clinton", "Essex", "Hamilton", "Herkimer", "Fulton", "Oneida", "Madison", "Montgomery", "Otsego", "Delaware")
univera <- c("Allegany", "Cattaraugus", "Chautauqua", "Erie", "Genesee", "Niagara", "Orleans", "Wyoming")


#-------------------------------------------------
#Convert the County column into a charcter column
final_claim$County<- as.character(final_claim$County)

#Rename the St. Lawrence county name
final_claim$County[final_claim$County=="St. Lawrence"]<- "Saint Lawrence"

#Rename the Allegheny county name
final_claim$County[final_claim$County=="Allegheny"]<- "Allegany"

#Aggregate values for ((Alleghany and Alegany) as Allegany) and ((St. Lawrence and Saint Lawrence) as Saint Lawrence)
final_claim<- aggregate(. ~  Year + County + Payer, data = final_claim, sum)


#----Extract Ny state data for opioid facility visits-------

library(RSocrata)

#Download the public data for opioid facility visits
facility_visits_df <- read.socrata(
  "https://health.data.ny.gov/resource/9p95-5ez3.csv",
  app_token = "bjp8KrRvAPtuf809u1UXnI0Z8"
)

#Select thw relevant columns
facility_visits_df<- facility_visits_df[,c("year","patient_county_name","payer", "rural_urban","er_opioid","er_visits","er_rateper1000")]

#Rename the columns
colnames(facility_visits_df)<- c('Year',"County","Payer","Geography","Opioid.ER.Visits","Total.ER.Visits","ER.Visit.Rate")

#Extract the unique set of counties
NY_County<- unique(facility_visits_df$County)

#remove the irrelvant names (county names) in NY_County
#NY_County<- NY_County[!NY_County%in%c('Other than New York State','Unknown','Statewide Total')]

#Keep only the Excellus counitesNM.
NY_County<- NY_County[NY_County%in%c(cny.region,southern.tier.region,rochester.region,utica.region,univera,"St. Lawrence")]



#Keep rows that are being displayed in the "Term significance" tab in the dashboard
facility_visits_df<- facility_visits_df[(facility_visits_df$County %in% c('Monroe','Erie') & facility_visits_df$Year=='2015'),]


#Strip white spaces
NY_County<- gsub('([A-z]+) .*', '\\1', NY_County)

#Stripping white spaces introduces some problems with counties comprising of two words.
#Let's fix that
#NY_County[NY_County=='New']<- "New York"

NY_County[NY_County=="St. Lawrence"]<- "Saint Lawrence"

#Reshuffle the columns
final_claim<- final_claim[,c(1,2,3,6,4,7,5,8,9)]

#Rename the columns
colnames(final_claim)<- c('Year',"County","Payer",
                          "Opioid.ER.visits","Total.ER.visits",
                          "Opioid.Inpatient.visits", "Total.Inpatient.visits",
                          "Opioid.Outpatient.visits", "Total.Outpatient.visits")

#Convert first letter to uppercase and the rest to lower case of County name
final_claim$County<- as.factor(str_to_title(as.character(tolower(final_claim$County))))

#Convert first letter to uppercase and the rest to lower case of Payer name
final_claim$Payer<- as.factor(str_to_title(as.character(tolower(final_claim$Payer))))

#Change some of the payes name into uppercase
final_claim$Payer <- gsub("Direct Pay Hmo", "Direct Pay HMO", final_claim$Payer)
final_claim$Payer <- gsub("Direct Pay Pos", "Direct Pay POS", final_claim$Payer)
final_claim$Payer <- gsub("Ssa", "SSA", final_claim$Payer)
#Select counties that are present in the NY state opioid dataset
final_claim<- final_claim[final_claim$County %in% NY_County,]

final_claim$County<- as.factor(as.character(final_claim$County))

#Only keep the Excellus counties.
final_claim$County <-final_claim$County[final_claim$County%in% c(cny.region,southern.tier.region,rochester.region,utica.region,univera,"Saint Lawrence")]

#Re-assign index of a dataset
rownames(final_claim)<- NULL
#-----------------------------------------------------------------

#Calculate the Opioid ER visit rate
final_claim$ER.visit.rate<- round((final_claim$Opioid.ER.visits/ final_claim$Total.ER.visits)*1000, 3)

#Calculate the Opioid Inpatient visit rate
final_claim$Inpatient.visit.rate<- round((final_claim$Opioid.Inpatient.visits/ final_claim$Total.Inpatient.visits )*1000,3)

#Calculate the Opioid Outpatient visit rate
final_claim$Outpatient.visit.rate<- round((final_claim$Opioid.Outpatient.visits/ final_claim$Total.Outpatient.visits )*1000,3)

#Calculate the total opioid visits that includes the summation of opioid ER visits + opioid Inpatient visits +opioid Outpatient visits
final_claim$Opioid.Overall.visits<- final_claim$Opioid.ER.visits+final_claim$Opioid.Inpatient.visits+final_claim$Opioid.Outpatient.visits

#Calculate the total  visits that includes the summation of ER visits + Inpatient visits + Outpatient visits
final_claim$Total.Overall.visits<- final_claim$Total.ER.visits+final_claim$Total.Inpatient.visits + final_claim$Total.Outpatient.visits

#Calculate the Opioid Overall visit rate
final_claim$Overall.visit.rate<- round((final_claim$Opioid.Overall.visits/final_claim$Total.Overall.visits)*1000,3)
b<- final_claim

#Replace all NA's with 0
final_claim[is.na(final_claim)]<-0

#Reshuffle the columns
final_claim<- final_claim[,c(1:5,10, 6,7,11,8,9,12,13:15)]

#Calculate the indexed version for the ER, Inpatient and Outpatient visits
final_claim$Indexed.ER.rate<- round((final_claim$ER.visit.rate/mean(final_claim$ER.visit.rate))*100,3)

final_claim$Indexed.Inpatient.rate<- round((final_claim$Inpatient.visit.rate/mean(final_claim$Inpatient.visit.rate))*100,3)

final_claim$Indexed.Outpatient.rate<- round((final_claim$Outpatient.visit.rate/mean(final_claim$Outpatient.visit.rate))*100,3)

final_claim$Indexed.Overall.rate<- round((final_claim$Overall.visit.rate/mean(final_claim$Overall.visit.rate))*100,3)

#-----------------------------------------------------------

#Load the final_claim data into a new variable, Excellus that would be used in the dashboard
Excellus<- final_claim
Excellus<- data.frame(Excellus)

# create vectors for the counties in our regions
cny.region <- c("Saint Lawrence", "Jefferson", "Lewis", "Oswego", "Onondaga", "Cayuga", "Tompkins", "Cortland")
southern.tier.region <- c("Chenango", "Broome", "Tioga", "Chemung", "Schuyler", "Steuben")
rochester.region <- c("Monroe", "Wayne", "Livingston", "Ontario", "Yates", "Seneca")
utica.region <- c("Franklin", "Clinton", "Essex", "Hamilton", "Herkimer", "Fulton", "Oneida", "Madison", "Montgomery", "Otsego", "Delaware")
univera <- c("Allegany", "Cattaraugus", "Chautauqua", "Erie", "Genesee", "Niagara", "Orleans", "Wyoming")

#Merge counties falling under a certain Excellus region
Excellus$Region<- ifelse(Excellus$County %in% cny.region, 'CNY Region',ifelse(Excellus$County %in% rochester.region, 'Rochester Region', ifelse(Excellus$County %in% southern.tier.region, 'Southern Tier Region', ifelse(Excellus$County %in% univera, 'Univera Region', ifelse(Excellus$County %in% utica.region, 'Utica Region','Others')))))


#------------------------------------By Region& All payers (lines: facility Visits)---------------------------------------------------
library(dplyr)
library(plyr)
library(quantmod)

#Replace all NA with 0
Excellus[is.na(Excellus)]<- 0

#Summarize various facility visits such as ER, Inpatient, Outpatient across different Excellus regions
Excellus_all_region <- Excellus %>% group_by(Year,Region) %>% summarise_each(funs(sum),-County,-Payer)

#Calculate the ER, Inpatient, Outpatient and overall visit rate
Excellus_all_region$ER.visit.rate<- (Excellus_all_region$Opioid.ER.visits/ Excellus_all_region$Total.ER.visits)*1000

Excellus_all_region$Inpatient.visit.rate<- (Excellus_all_region$Opioid.Inpatient.visits/Excellus_all_region$Total.Inpatient.visits)*1000

Excellus_all_region$Outpatient.visit.rate<- (Excellus_all_region$Opioid.Outpatient.visits/Excellus_all_region$Total.Outpatient.visits)*1000

Excellus_all_region$Overall.visit.rate<- (Excellus_all_region$Opioid.Overall.visits/Excellus_all_region$Total.Overall.visits)*1000


#Reshufle the columns and rename them
Excellus_all_region<- Excellus_all_region[,c(1,2,5,8,11,14)]
colnames(Excellus_all_region)<- c("Year",'Region','ER Visit Rate','Inpatient Visit Rate', 'Outpatient Visit Rate','Overall Visit Rate')

Excellus_all_region<- as.data.frame(Excellus_all_region)

#Convert all the facility visits columns into rows using melt function. This is used for structuring a dataframe so that it can be used for line-plots
Excellus_all_region<- melt(Excellus_all_region, id.vars=c("Region","Year"), measure.vars=colnames(Excellus_all_region)[3:6])

#Round the value attribute to the 4th digit
Excellus_all_region$value<- round(Excellus_all_region$value,4)
#Convert varibale column to a character column
Excellus_all_region$variable<- as.character(Excellus_all_region$variable)

#Sort the rows in the dataframe
Excellus_all_region<- Excellus_all_region[order(Excellus_all_region$Region, Excellus_all_region$variable, Excellus_all_region$Year),]

#--------------------------------data: Excellus Region &Visit (lines: Payers)------------------------------------


#Summarize various Payers such as Commercial, Medicaid, Medicare across different Excellus regions
Excellus_all_payers <- Excellus %>% group_by(Payer, Year) %>% summarise_each(funs(sum),-County,-Region)

#Calculating ER, Inpatient, Outpatient and overall visit rate
Excellus_all_payers$ER.visit.rate<- (Excellus_all_payers$Opioid.ER.visits/ Excellus_all_payers$Total.ER.visits)*1000

Excellus_all_payers$Inpatient.visit.rate<- (Excellus_all_payers$Opioid.Inpatient.visits/Excellus_all_payers$Total.Inpatient.visits)*1000

Excellus_all_payers$Outpatient.visit.rate<- (Excellus_all_payers$Opioid.Outpatient.visits/Excellus_all_payers$Total.Outpatient.visits)*1000

Excellus_all_payers$Overall.visit.rate<- (Excellus_all_payers$Opioid.Overall.visits/Excellus_all_payers$Total.Overall.visits)*1000


#Reshuffle columns and rename columns
Excellus_all_payers<- Excellus_all_payers[,c(1,2,5,8,11,14)]

colnames(Excellus_all_payers)<- c('Payer',"Year",'ER Visit Rate','Inpatient Visit Rate', 'Outpatient Visit Rate','Overall Visit Rate')

Excellus_all_payers<- as.data.frame(Excellus_all_payers)

#Convert all the facility visits columns into rows using melt function. This is used for structuring a dataframe so that it can be used for line-plots
Excellus_all_payers<- melt(Excellus_all_payers, id.vars=c("Payer", "Year"), measure.vars=colnames(Excellus_all_payers)[3:6])

#Round the value attribute to the 4th digit
Excellus_all_payers$value<- round(Excellus_all_payers$value,4)
#Convert the variable column into a character column
Excellus_all_payers$variable<- as.character(Excellus_all_payers$variable)

#Sort rows of a dataset
Excellus_all_payers<- Excellus_all_payers[order(Excellus_all_payers$Payer, Excellus_all_payers$variable, Excellus_all_payers$Year), ]


#---------------Calculating Percent Change in Opioid rate for Excellus Regions-------------

Excellus_all_region<- ddply(Excellus_all_region, c('Region','variable'), transform, DeltaCol=Delt(value))

Excellus_all_region$Delt.1.arithmetic<- as.numeric(Excellus_all_region$Delt.1.arithmetic)

colnames(Excellus_all_region)<- c('Region' ,'Year','variable', 'value', 'Percent Change')

Excellus_all_region$`Percent Change`<- ifelse(is.nan(Excellus_all_region$`Percent Change`), 0, ifelse(is.infinite(Excellus_all_region$`Percent Change`),0, Excellus_all_region$`Percent Change`))

Excellus_all_region$`Percent Change`<- 100*Excellus_all_region$`Percent Change`

Excellus_all_region$`Percent Change`<- round(Excellus_all_region$`Percent Change`,2)


#-----------Calculating Percent Change in Opioid rate for Payers----------

Excellus_all_payers<- ddply(Excellus_all_payers, c('Payer','variable'), transform, DeltaCol=Delt(value))

Excellus_all_payers$Delt.1.arithmetic<- as.numeric(Excellus_all_payers$Delt.1.arithmetic)

colnames(Excellus_all_payers)<- c('Payer' ,'Year','variable', 'value', 'Percent Change')


Excellus_all_payers$`Percent Change`<- ifelse(is.nan(Excellus_all_payers$`Percent Change`), 0, ifelse(is.infinite(Excellus_all_payers$`Percent Change`),0, Excellus_all_payers$`Percent Change`))

Excellus_all_payers$`Percent Change`<- 100*Excellus_all_payers$`Percent Change`

Excellus_all_payers$`Percent Change`<- round(Excellus_all_payers$`Percent Change`,2)

#------------------Load the opioid dataframe dictionary-----------

opioid_df_dictionary<- read.csv('opioid_df_dictionary.csv')

opioid_df_dictionary<- opioid_df_dictionary[!opioid_df_dictionary$Field %in% c("Opioid Ambulatory Surgery Visits", "Ambulatory Surgery Visits","Ambulatory Surgery Rate Per 1000"),]

Excellus<- Excellus[,-grep("Region", colnames(Excellus))]

#------------cOMBINE ALL PAYERS------------------------------------------------------

#Combine all facility visits across all the payers
combine_all_payers_Excellus <- Excellus %>% group_by(Year, County) %>% summarise_each(funs(sum), -Payer)

#Append a column of Payers whose value will be "All"
combine_all_payers_Excellus<- as.data.frame(append(combine_all_payers_Excellus, list(Payer='All'), after = 2))

#Append this to the original facility visits dataframe
Excellus<- rbind(Excellus, combine_all_payers_Excellus)


#------------cOMBINE ALL YEARS------------------------------------------------------

#Combine all facility visits across all the payers
combine_all_years_Excellus <- Excellus %>% group_by(County,Payer) %>% summarise_each(funs(sum), -Year)

#Append a column of Payers whose value will be "All"
combine_all_years_Excellus<- as.data.frame(append(combine_all_years_Excellus, list(Year='All'), after = 0))

#Append this to the original facility visits dataframe
Excellus<- rbind(Excellus, combine_all_years_Excellus)

#-------------CALCULATE VISIT RATES---------------------

Excellus$ER.visit.rate<- (Excellus$Opioid.ER.visits/ Excellus$Total.ER.visits)*1000

Excellus$Inpatient.visit.rate<- (Excellus$Opioid.Inpatient.visits/Excellus$Total.Inpatient.visits)*1000

Excellus$Outpatient.visit.rate<- (Excellus$Opioid.Outpatient.visits/Excellus$Total.Outpatient.visits)*1000

Excellus$Overall.visit.rate<- (Excellus$Opioid.Overall.visits/Excellus$Total.Overall.visits)*1000

#--------------CREATE NY COUNTY SPATIAL POLYGON--------------

library(raster)
library(maptools)

# get county level spatial polygons for the United States
counties_spatial_df <- getData("GADM", country = "USA", level = 2)

# filter down to just New York State Counties
counties_spatial_df <- counties_spatial_df[counties_spatial_df@data$NAME_1 == "New York",]

#Remove the Lake Ontario County
counties_spatial_df <- counties_spatial_df[!counties_spatial_df@data$NAME_2 %in% c("Lake Ontario"),]



# create vectors for the counties in our regions
cny.region <- c("Saint Lawrence", "Jefferson", "Lewis", "Oswego", "Onondaga", "Cayuga", "Tompkins", "Cortland")
southern.tier.region <- c("Chenango", "Broome", "Tioga", "Chemung", "Schuyler", "Steuben")
rochester.region <- c("Monroe", "Wayne", "Livingston", "Ontario", "Yates", "Seneca")
utica.region <- c("Franklin", "Clinton", "Essex", "Hamilton", "Herkimer", "Fulton", "Oneida", "Madison", "Montgomery", "Otsego", "Delaware")
univera <- c("Allegany", "Cattaraugus", "Chautauqua", "Erie", "Genesee", "Niagara", "Orleans", "Wyoming")

# create a region variable in the spatial data frame
counties_spatial_df@data$region <- ifelse(counties_spatial_df@data$NAME_2 %in% cny.region, "CNY", ifelse(counties_spatial_df@data$NAME_2 %in% southern.tier.region, "Southern Tier", ifelse(counties_spatial_df@data$NAME_2 %in% rochester.region, "Rochester", ifelse(counties_spatial_df@data$NAME_2 %in% utica.region, "Utica", ifelse(counties_spatial_df@data$NAME_2 %in% univera, "Univera", "Others")))))

#Get Spatial information about the Excellus regions
counties_region <- counties_spatial_df[counties_spatial_df@data$NAME_2 %in% c(cny.region,southern.tier.region,rochester.region,utica.region,univera),]

#Create polygons for Excellus regions
Excellus_regions.polygons <- unionSpatialPolygons(counties_region, counties_region@data$region)

#--------Load the opioid-related ICD codes used for data extraction------

#Load ICD codes csv file

library(readxl)
ICD_codes <- read_excel("ICD codes.xlsx")
colnames(ICD_codes)<- ICD_codes
ICD_codes<- ICD_codes

#--------Select objects to keep in Rdata file----------------

objects_to_keep=c("Excellus_regions.polygons", "counties_spatial_df", "Excellus_all_payers", "Excellus_all_region","Excellus", "opioid_df_dictionary", "facility_visits_df","ICD_codes")

rm(list=setdiff(ls(), objects_to_keep))

#save.image('C:/Users/mpatel2/Desktop/Excellus_data_preprocessing/final.Rdata')
save.image('final.RData')
