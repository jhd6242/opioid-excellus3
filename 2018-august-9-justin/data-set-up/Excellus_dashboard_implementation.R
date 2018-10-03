library(stringr)
library(reshape)
library(readr)
library(plyr)
library(quantmod)
library(dplyr)
#Read the final_claims file from the source
#final_claim <- read_csv("opioid_total_claims_nm_m9.csv")
final_claim <- read_csv("./data/final_claim_parsed.csv")
#changed csv file remove dependency column 
#Exclue Unknown Payer Unknown
final_claim <- final_claim[,-1]
final_claim <- filter(final_claim, BSNS_SGMNT != "<UNKNOWN>")
#REname he columns of the dataset
colnames(final_claim)<- c('Year','County','Payer','Facility_visit', 'Claim_type','count','PainandAbuseDia')

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

# Convert PainandAbuse column into defined levels
final_claim$PainandAbuseDia<- as.factor(final_claim$PainandAbuseDia)

#Convert Year column into defined levels
final_claim$Year<- as.factor(as.character(final_claim$Year))

#Define a new column 'visit' to flag opioid claims
#final_claim$visit<- ifelse(final_claim$Claim_type=='Other', paste0(final_claim$Facility_visit,' Counts'),paste0('Opioid ',final_claim$Facility_visit,' Counts') )
# Pain and abuse and diagnosis are subcategories of opioid I will have to change the code later on but this might work best
# Instead of creating a whole new file and referencing it for the dashboard. When pain and abuse in diagnosis are added together they create the variable of opioid

final_claim$visit <-
  ifelse(
    final_claim$PainandAbuseDia == 'PainandAbuse',
    paste0('PainandAbuse ', final_claim$Facility_visit, ' Counts'),
    ifelse(
      final_claim$PainandAbuseDia == 'Diagnosis',
      paste0('Diagnosis ', final_claim$Facility_visit, ' Counts'),
      (paste0(final_claim$Facility_visit, ' Counts'))
    )
  )


#Convert 'visit' column into defined levels
final_claim$visit<- as.factor(final_claim$visit)

#Select columns that are relevant
final_claim<- final_claim[,c(1,2,3,8,6)]

#--------------------------------------------

#Define a grid that will create the missing facility for every county. Every county will not have all facility visits (ER, Inpatient, Outpatient) and Payers(Commercial, Medicaid, Medicare). So counties which have some missing payers or facility visits, the below grid function will create those missing attributes and input NA as their value
missing_Payers_or_Visits <- with(final_claim, expand.grid(Year=levels(Year),County = levels(County), Payer = levels(Payer), visit= levels(visit)))

#merge all the missing payers and counties to the original final_claim dataset
final_claim <- merge(final_claim, missing_Payers_or_Visits, all.y = TRUE)

#Replace all NA values with 0
final_claim$count[is.na(final_claim$count)]<- 0

#Convert every value in the visit column into a new column (ER, Inpatient, Outpatient) by grouping Year, County and Payer
final_claim<-cast(final_claim, Year+County+Payer ~ visit,sum,value = 'count')
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
colnames(facility_visits_df)<- c('Year',"County","Payer","Geography","Opioid.ER.visits","Total.ER.visits","ER.visit.rate")

#Extract the unique set of counties
NY_County<- unique(facility_visits_df$County)

#remove the irrelvant names (county names) in NY_County
NY_County<- NY_County[!NY_County%in%c('Other than New York State','Unknown','Statewide Total')]

#Keep rows that are being displayed in the "Term significance" tab in the dashboard
facility_visits_df<- facility_visits_df[(facility_visits_df$County %in% c('Albany','Bronx') & facility_visits_df$Year=='2015') ,c('Year','County', 'Geography', 'Payer',"Opioid.ER.visits","Total.ER.visits","ER.visit.rate")]


#Strip white spaces
NY_County<- gsub('([A-z]+) .*', '\\1', NY_County)

#Stripping white spaces introduces some problems with counties comprising of two words.
#Let's fix that
NY_County[NY_County=='New']<- "New York"

NY_County[NY_County=="St. Lawrence"]<- "Saint Lawrence"

#Reshuffle the columns
#final_claim<- final_claim[,c(1,2,3,6,4,7,5,8,9)]
#final_claim<- final_claim[,c(1,2,3,6,4,7,5,8,10,9,11)]

final_claim<- final_claim[,c("Year", "County", "Payer",
                             "PainandAbuse Emergency Room Counts","Diagnosis Emergency Room Counts", "Emergency Room Counts",
                             "PainandAbuse Inpatient Counts","Diagnosis Inpatient Counts", "Inpatient Counts",
                             "PainandAbuse Outpatient Counts","Diagnosis Outpatient Counts", "Outpatient Counts",
                             "PainandAbuse Professional Counts","Diagnosis Professional Counts", "Professional Counts",
                             "PainandAbuse Other Counts","Diagnosis Other Counts", "Other Counts")]

#Rename the columns
colnames(final_claim)<- c('Year',"County","Payer",
                          "PainandAbuse.ER.visits","Diagnosis.ER.visits","Total.ER.visits",
                          "PainandAbuse.Inpatient.visits", "Diagnosis.Inpatient.visits","Total.Inpatient.visits",
                          "PainandAbuse.Outpatient.visits", "Diagnosis.Outpatient.visits","Total.Outpatient.visits",
                          "PainandAbuse.Professional.visits","Diagnosis.Professional.visits", "Total.Professional.visits",
                          "PainandAbuse.Other.visits", "Diagnosis.Other.visits","Total.Other.visits")

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

#Re-assign index of a dataset
rownames(final_claim)<- NULL
# Combined pain and abuse and diagnosis visits
final_claim$PainAbuseDiagnosis.ER.visits <- final_claim$PainandAbuse.ER.visits+final_claim$Diagnosis.ER.visits

final_claim$PainAbuseDiagnosis.Inpatient.visits  <- final_claim$PainandAbuse.Inpatient.visits + final_claim$Diagnosis.Inpatient.visits

final_claim$PainAbuseDiagnosis.Outpatient.visits  <- final_claim$PainandAbuse.Outpatient.visits + final_claim$Diagnosis.Outpatient.visits

final_claim$PainAbuseDiagnosis.Professional.visits  <- final_claim$PainandAbuse.Professional.visits + final_claim$Diagnosis.Professional.visits

final_claim$PainAbuseDiagnosis.Other.visits  <- final_claim$PainandAbuse.Other.visits + final_claim$Diagnosis.Other.visits


#-----------------------------------------------------------------

#Calculate the PainandAbuse ER visit rate
final_claim$PainandAbuse.ER.visit.rate <-
  round((
    final_claim$PainandAbuse.ER.visits / final_claim$Total.ER.visits
  ) * 1000,
  3)

#Calculate the Diagnosis ER visit rate
final_claim$Diagnosis.ER.visit.rate <-
  round((
    final_claim$Diagnosis.ER.visits / final_claim$Total.ER.visits
  ) * 1000,
  3)

#Calculate the PainAbuseDiagnosis ER visit rate
final_claim$PainAbuseDiagnosis.ER.visit.rate <- final_claim$PainandAbuse.ER.visit.rate+final_claim$Diagnosis.ER.visit.rate

#Calculate the PainandAbuse Inpatient visit rate
final_claim$PainandAbuse.Inpatient.visit.rate <-
  round((
    final_claim$PainandAbuse.Inpatient.visits / final_claim$Total.Inpatient.visits
  ) * 1000,
  3
  )

#Calculate the Diagnosis Inpatient visit rate
final_claim$Diagnosis.Inpatient.visit.rate <-
  round((
    final_claim$Diagnosis.Inpatient.visits / final_claim$Total.Inpatient.visits
  ) * 1000,
  3
  )

#Calculate the PainAbuseDiagnosis Inpatient visit rate
final_claim$PainAbuseDiagnosis.Inpatient.visit.rate <- final_claim$PainandAbuse.Inpatient.visit.rate+final_claim$Diagnosis.Inpatient.visit.rate


#Calculate the PainandAbuse Outpatient visit rate
final_claim$PainandAbuse.Outpatient.visit.rate <-
  round((
    final_claim$PainandAbuse.Outpatient.visits / final_claim$Total.Outpatient.visits
  ) * 1000,
  3
  )

#Calculate the Diagnosis Outpatient visit rate
final_claim$Diagnosis.Outpatient.visit.rate <-
  round((
    final_claim$Diagnosis.Outpatient.visits / final_claim$Total.Outpatient.visits
  ) * 1000,
  3
  )

#Calculate the PainAbuseDiagnosis Outpatient visit rate
final_claim$PainAbuseDiagnosis.Outpatient.visit.rate <- final_claim$PainandAbuse.Outpatient.visit.rate+final_claim$Diagnosis.Outpatient.visit.rate

#Added for the other beside In, out, and ER
#Calculate the PainandAbuse  Professional visit rate
final_claim$PainandAbuse.Professional.visit.rate <-
  round((
    final_claim$PainandAbuse.Professional.visits / final_claim$Total.Professional.visits
  ) * 1000,
  3
  )

#Calculate the Diagnosis  Professional visit rate
final_claim$Diagnosis.Professional.visit.rate <-
  round((
    final_claim$Diagnosis.Professional.visits / final_claim$Total.Professional.visits
  ) * 1000,
  3
  )

#Calculate the PainAbuseDiagnosis Professional visit rate
final_claim$PainAbuseDiagnosis.Professional.visit.rate <- final_claim$PainandAbuse.Professional.visit.rate+final_claim$Diagnosis.Professional.visit.rate

#Calculate the PainandAbuse  Other visit rate
final_claim$PainandAbuse.Other.visit.rate <-
  round((
    final_claim$PainandAbuse.Other.visits / final_claim$Total.Other.visits
  ) * 1000,
  3)

#Calculate the Diagnosis  Other visit rate
final_claim$Diagnosis.Other.visit.rate <-
  round((
    final_claim$Diagnosis.Other.visits / final_claim$Total.Other.visits
  ) * 1000,
  3)

#Calculate the PainAbuseDiagnosis Other visit rate
final_claim$PainAbuseDiagnosis.Other.visit.rate <- final_claim$PainandAbuse.Other.visit.rate+final_claim$Diagnosis.Other.visit.rate

#Calculate the total PainandAbuse visits that includes the summation of PainandAbuse ER visits + PainandAbuse Inpatient visits +PainandAbuse Outpatient visits
final_claim$PainandAbuse.Overall.visits <-
  final_claim$PainandAbuse.ER.visits + final_claim$PainandAbuse.Inpatient.visits + final_claim$PainandAbuse.Outpatient.visits +
  final_claim$PainandAbuse.Professional.visits + final_claim$PainandAbuse.Other.visits

#Calculate the total Diagnosis visits that includes the summation of Diagnosis ER visits + Diagnosis Inpatient visits +Diagnosis Outpatient visits
final_claim$Diagnosis.Overall.visits <-
  final_claim$Diagnosis.ER.visits + final_claim$Diagnosis.Inpatient.visits + final_claim$Diagnosis.Outpatient.visits +
  final_claim$Diagnosis.Professional.visits + final_claim$Diagnosis.Other.visits

#Calculate the total Diagnosis and PainandAbuse visits that includes the summation of Diagnosis ER visits + Diagnosis Inpatient visits +Diagnosis Outpatient visits

final_claim$PainAbuseDiagnosis.Overall.visits  <- final_claim$PainandAbuse.Overall.visits + final_claim$Diagnosis.Overall.visits

#Calculate the total  visits that includes the summation of ER visits + Inpatient visits + Outpatient visits
final_claim$Total.Overall.visits <-
  final_claim$Total.ER.visits + final_claim$Total.Inpatient.visits + final_claim$Total.Outpatient.visits + final_claim$Total.Professional.visits + final_claim$Total.Other.visits

#Calculate the PainandAbuse Overall visit rate
final_claim$PainandAbuse.Overall.visit.rate<- round((final_claim$PainandAbuse.Overall.visits/final_claim$Total.Overall.visits)*1000,3)

#Calculate the Diagnosis Overall visit rate
final_claim$Diagnosis.Overall.visit.rate<- round((final_claim$Diagnosis.Overall.visits/final_claim$Total.Overall.visits)*1000,3)

#Calculate the PainAbuseDiagnosis Overall visit rate
final_claim$PainAbuseDiagnosis.Overall.visit.rate <- final_claim$PainandAbuse.Overall.visit.rate + final_claim$Diagnosis.Overall.visit.rate

b<- final_claim

#Replace all NA's with 0
final_claim[is.na(final_claim)]<-0

#Reshuffle the columns
#final_claim<- final_claim[,c(1:5,10, 6,7,11,8,9,12,13:15)]
final_claim <- final_claim[, c(
  'Year',
  "County",
  "Payer",
  "PainandAbuse.ER.visits",
  "Diagnosis.ER.visits","PainAbuseDiagnosis.ER.visits",
  "Total.ER.visits",
  "PainandAbuse.ER.visit.rate",
  "Diagnosis.ER.visit.rate","PainAbuseDiagnosis.ER.visit.rate",
  "PainandAbuse.Inpatient.visits",
  "Diagnosis.Inpatient.visits","PainAbuseDiagnosis.Inpatient.visits",
  "Total.Inpatient.visits",
  "PainandAbuse.Inpatient.visit.rate",
  "Diagnosis.Inpatient.visit.rate","PainAbuseDiagnosis.Inpatient.visit.rate",
  "PainandAbuse.Outpatient.visits",
  "Diagnosis.Outpatient.visits","PainAbuseDiagnosis.Outpatient.visits",
  "Total.Outpatient.visits",
  "PainandAbuse.Outpatient.visit.rate",
  "Diagnosis.Outpatient.visit.rate","PainAbuseDiagnosis.Outpatient.visit.rate",
  "PainandAbuse.Professional.visits",
  "Diagnosis.Professional.visits","PainAbuseDiagnosis.Professional.visits",
  "Total.Professional.visits",
  "PainandAbuse.Professional.visit.rate",
  "Diagnosis.Professional.visit.rate","PainAbuseDiagnosis.Professional.visit.rate",
  "PainandAbuse.Other.visits",
  "Diagnosis.Other.visits","PainAbuseDiagnosis.Other.visits",
  "Total.Other.visits",
  "PainandAbuse.Other.visit.rate",
  "Diagnosis.Other.visit.rate","PainAbuseDiagnosis.Other.visit.rate",
  "PainandAbuse.Overall.visits",
  "Diagnosis.Overall.visits","PainAbuseDiagnosis.Overall.visits",
  "Total.Overall.visits",
  "PainandAbuse.Overall.visit.rate",
  "Diagnosis.Overall.visit.rate","PainAbuseDiagnosis.Overall.visit.rate"
)]


#final_claim<- final_claim[,c(1:5,12, 6,7,13,8,9,15,10,11,14,16:18)]

#Calculate the indexed version for the PainandAbuse and Diagnosis and PainAbuseDiagnosis for ER, Inpatient and Outpatient visits

#Indexed version of PainandAbuse and Diagnosis and PainAbuseDiagnosis for ER
final_claim$PainandAbuse.Indexed.ER.rate<- round((final_claim$PainandAbuse.ER.visit.rate/mean(final_claim$PainandAbuse.ER.visit.rate))*100,3)

final_claim$Diagnosis.Indexed.ER.rate<- round((final_claim$Diagnosis.ER.visit.rate/mean(final_claim$Diagnosis.ER.visit.rate))*100,3)

final_claim$PainAbuseDiagnosis.Indexed.ER.rate<- round((final_claim$PainAbuseDiagnosis.ER.visit.rate/mean(final_claim$PainAbuseDiagnosis.ER.visit.rate))*100,3)

#Indexed version of PainandAbuse and Diagnosis and PainAbuseDiagnosis for Inpatient
final_claim$PainandAbuse.Indexed.Inpatient.rate<- round((final_claim$PainandAbuse.Inpatient.visit.rate/mean(final_claim$PainandAbuse.Inpatient.visit.rate))*100,3)

final_claim$Diagnosis.Indexed.Inpatient.rate<- round((final_claim$Diagnosis.Inpatient.visit.rate/mean(final_claim$Diagnosis.Inpatient.visit.rate))*100,3)

final_claim$PainAbuseDiagnosis.Indexed.Inpatient.rate<- round((final_claim$PainAbuseDiagnosis.Inpatient.visit.rate/mean(final_claim$PainAbuseDiagnosis.Inpatient.visit.rate))*100,3)

#Indexed version of PainandAbuse and Diagnosis and PainAbuseDiagnosis for Outpatient
final_claim$PainandAbuse.Indexed.Outpatient.rate<- round((final_claim$PainandAbuse.Outpatient.visit.rate/mean(final_claim$PainandAbuse.Outpatient.visit.rate))*100,3)

final_claim$Diagnosis.Indexed.Outpatient.rate<- round((final_claim$Diagnosis.Outpatient.visit.rate/mean(final_claim$Diagnosis.Outpatient.visit.rate))*100,3)

final_claim$PainAbuseDiagnosis.Indexed.Outpatient.rate<- round((final_claim$PainAbuseDiagnosis.Outpatient.visit.rate/mean(final_claim$PainAbuseDiagnosis.Outpatient.visit.rate))*100,3)

#Add the others and Professional for PainandAbuse and Diagnosis intothe mix of IN, OUT and ER
#Indexed version of PainandAbuse and Diagnosis and PainAbuseDiagnosis for Professional
final_claim$PainandAbuse.Indexed.Professional.rate<- round((final_claim$PainandAbuse.Professional.visit.rate/mean(final_claim$PainandAbuse.Professional.visit.rate))*100,3)

final_claim$Diagnosis.Indexed.Professional.rate<- round((final_claim$Diagnosis.Professional.visit.rate/mean(final_claim$Diagnosis.Professional.visit.rate))*100,3)

final_claim$PainAbuseDiagnosis.Indexed.Professional.rate<- round((final_claim$PainAbuseDiagnosis.Professional.visit.rate/mean(final_claim$PainAbuseDiagnosis.Professional.visit.rate))*100,3)

#Indexed version of PainandAbuse and Diagnosis and PainAbuseDiagnosis for Other
final_claim$PainandAbuse.Indexed.Other.rate<- round((final_claim$PainandAbuse.Other.visit.rate/mean(final_claim$PainandAbuse.Other.visit.rate))*100,3)

final_claim$Diagnosis.Indexed.Other.rate<- round((final_claim$Diagnosis.Other.visit.rate/mean(final_claim$Diagnosis.Other.visit.rate))*100,3)

final_claim$PainAbuseDiagnosis.Indexed.Other.rate<- round((final_claim$PainAbuseDiagnosis.Other.visit.rate/mean(final_claim$PainAbuseDiagnosis.Other.visit.rate))*100,3)

#Indexed version of PainandAbuse and Diagnosis and PainAbuseDiagnosis for Overall
final_claim$PainandAbuse.Indexed.Overall.rate<- round((final_claim$PainandAbuse.Overall.visit.rate/mean(final_claim$PainandAbuse.Overall.visit.rate))*100,3)

final_claim$Diagnosis.Indexed.Overall.rate<- round((final_claim$Diagnosis.Overall.visit.rate/mean(final_claim$Diagnosis.Overall.visit.rate))*100,3)

final_claim$PainAbuseDiagnosis.Indexed.Overall.rate<- round((final_claim$PainAbuseDiagnosis.Overall.visit.rate/mean(final_claim$PainAbuseDiagnosis.Overall.visit.rate))*100,3)

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
Excellus$Region<- ifelse(Excellus$County %in% cny.region, 'CNY Region',ifelse(Excellus$County %in% rochester.region, 'Rochester Region', ifelse(Excellus$County %in% southern.tier.region, 'Southern Tier Region', ifelse(Excellus$County %in% univera, 'Univera Region', ifelse(Excellus$County %in% utica.region, 'Utica Region','Other')))))


#------------------------------------By Region& All payers (lines: facility Visits)---------------------------------------------------
library(dplyr)
library(plyr)
library(quantmod)

#Replace all NA with 0
Excellus[is.na(Excellus)]<- 0

#Summarize various facility visits such as ER, Inpatient, Outpatient across different Excellus regions
Excellus_all_region <- Excellus %>% group_by(Year,Region) %>% summarise_each(funs(sum),-County,-Payer)

#Calculate the ER, Inpatient, Outpatient and overall visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
#ER visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_region$PainandAbuse.ER.visit.rate<- (Excellus_all_region$PainandAbuse.ER.visits/Excellus_all_region$Total.ER.visits)*1000

Excellus_all_region$Diagnosis.ER.visit.rate<- (Excellus_all_region$Diagnosis.ER.visits/Excellus_all_region$Total.ER.visits)*1000

Excellus_all_region$PainAbuseDiagnosis.ER.visit.rate<- (Excellus_all_region$PainAbuseDiagnosis.ER.visits/Excellus_all_region$Total.ER.visits)*1000


#Inpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_region$PainandAbuse.Inpatient.visit.rate<- (Excellus_all_region$PainandAbuse.Inpatient.visits/Excellus_all_region$Total.Inpatient.visits)*1000

Excellus_all_region$Diagnosis.Inpatient.visit.rate<- (Excellus_all_region$Diagnosis.Inpatient.visits/Excellus_all_region$Total.Inpatient.visits)*1000

Excellus_all_region$PainAbuseDiagnosis.Inpatient.visit.rate<- (Excellus_all_region$PainAbuseDiagnosis.Inpatient.visits/Excellus_all_region$Total.Inpatient.visits)*1000


#Outpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_region$PainandAbuse.Outpatient.visit.rate<- (Excellus_all_region$PainandAbuse.Outpatient.visits/Excellus_all_region$Total.Outpatient.visits)*1000

Excellus_all_region$Diagnosis.Outpatient.visit.rate<- (Excellus_all_region$Diagnosis.Outpatient.visits/Excellus_all_region$Total.Outpatient.visits)*1000

Excellus_all_region$PainAbuseDiagnosis.Outpatient.visit.rate<- (Excellus_all_region$PainAbuseDiagnosis.Outpatient.visits/Excellus_all_region$Total.Outpatient.visits)*1000


#Add the Professional Other into the ix of IN, OUT and ERfor PainandAbuse and Diagnosis and PainAbuseDiagnosis
#Professional visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_region$PainandAbuse.Professional.visit.rate<- (Excellus_all_region$PainandAbuse.Professional.visits/Excellus_all_region$Total.Professional.visits)*1000

Excellus_all_region$Diagnosis.Professional.visit.rate<- (Excellus_all_region$Diagnosis.Professional.visits/Excellus_all_region$Total.Professional.visits)*1000

Excellus_all_region$PainAbuseDiagnosis.Professional.visit.rate<- (Excellus_all_region$PainAbuseDiagnosis.Professional.visits/Excellus_all_region$Total.Professional.visits)*1000

#Other visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_region$PainandAbuse.Other.visit.rate<- (Excellus_all_region$PainandAbuse.Other.visits/Excellus_all_region$Total.Other.visits)*1000

Excellus_all_region$Diagnosis.Other.visit.rate<- (Excellus_all_region$Diagnosis.Other.visits/Excellus_all_region$Total.Other.visits)*1000

Excellus_all_region$PainAbuseDiagnosis.Other.visit.rate<- (Excellus_all_region$PainAbuseDiagnosis.Other.visits/Excellus_all_region$Total.Other.visits)*1000


#Overall visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_region$PainandAbuse.Overall.visit.rate<- (Excellus_all_region$PainandAbuse.Overall.visits/Excellus_all_region$Total.Overall.visits)*1000

Excellus_all_region$Diagnosis.Overall.visit.rate<- (Excellus_all_region$Diagnosis.Overall.visits/Excellus_all_region$Total.Overall.visits)*1000

Excellus_all_region$PainAbuseDiagnosis.Overall.visit.rate<- (Excellus_all_region$PainAbuseDiagnosis.Overall.visits/Excellus_all_region$Total.Overall.visits)*1000


#Reshufle the columns and rename them
#Excellus_all_region<- Excellus_all_region[,c(1,2,5,8,11,14)]

#NM March6, 2018
#This part is changed to meet the new others into the IN/OUT and ER
Excellus_all_region<- Excellus_all_region[,c(1,2,7,8,9,14,15,16,21,22,23,28,29,30,35,36,37,42,43,44)]
#colnames(Excellus_all_region)<- 
#c("Year",'Region','PainandAbuse ER visit Rate',' Diagnosis ER visit Rate',
#'PainandAbuse Inpatient visit Rate', 'Diagnosis Inpatient visit Rate',
#'PainandAbuse Outpatient visit Rate',' Diagnosis Outpatient visit Rate',
#'PainandAbuse Professional visit Rate', 'Diagnosis Professional visit Rate',
#'PainandAbuse Other visit Rate', 'Diagnosis Other visit Rate',
#'PainandAbuse Overall visit Rate','Diagnosis Overall visit Rate')
#NM March6, 2018
colnames(Excellus_all_region)<- c("Year",'Region','Pain and Abuse ER Visit Rate','Poisoning by Opiates ER Visit Rate','PainAbuseDiagnosis ER Visit Rate',
                                  'Pain and Abuse Inpatient Visit Rate', 'Poisoning by Opiates Inpatient Visit Rate','PainAbuseDiagnosis Inpatient Visit Rate',
                                  'Pain and Abuse Outpatient Visit Rate','Poisoning by Opiates Outpatient Visit Rate','PainAbuseDiagnosis Outpatient Visit Rate',
                                  'Pain and Abuse Professional Visit Rate', 'Poisoning by Opiates Professional Visit Rate','PainAbuseDiagnosis Professional Visit Rate',
                                  'Pain and Abuse Other Visit Rate', 'Poisoning by Opiates Other Visit Rate','PainAbuseDiagnosis Other Visit Rate',
                                  'Pain and Abuse Overall Visit Rate','Poisoning by Opiates Overall Visit Rate','PainAbuseDiagnosis Overall Visit Rate')

Excellus_all_region<- as.data.frame(Excellus_all_region)

#Convert all the facility visits columns into rows using melt function. This is used for structuring a dataframe so that it can be used for line-plots
Excellus_all_region<- melt(Excellus_all_region, id.vars=c("Region","Year"), measure.vars=colnames(Excellus_all_region)[3:20])

#Round the value attribute to the 4th digit
Excellus_all_region$value<- round(Excellus_all_region$value,4)
#Convert varibale column to a character column
Excellus_all_region$variable<- as.character(Excellus_all_region$variable)

#Sort the rows in the dataframe
Excellus_all_region<- Excellus_all_region[order(Excellus_all_region$Region, Excellus_all_region$variable, Excellus_all_region$Year),]

#--------------------------------data: Excellus Region &Visit (lines: Payers)------------------------------------


#Summarize various Payers such as Commercial, Medicaid, Medicare across different Excellus regions
Excellus_all_payers <- Excellus %>% group_by(Payer, Year) %>% summarise_each(funs(sum),-County,-Region)

#Calculate the ER, Inpatient, Outpatient and overall visit rate for Pain and Abuse and Diagnosis and PainAbuseDiagnosis
#ER visit rate for Pain and Abuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_payers$PainandAbuse.ER.visit.rate<- (Excellus_all_payers$PainandAbuse.ER.visits/Excellus_all_payers$Total.ER.visits)*1000

Excellus_all_payers$Diagnosis.ER.visit.rate<- (Excellus_all_payers$Diagnosis.ER.visits/Excellus_all_payers$Total.ER.visits)*1000

Excellus_all_payers$PainAbuseDiagnosis.ER.visit.rate<- (Excellus_all_payers$PainAbuseDiagnosis.ER.visits/Excellus_all_payers$Total.ER.visits)*1000


#Inpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_payers$PainandAbuse.Inpatient.visit.rate<- (Excellus_all_payers$PainandAbuse.Inpatient.visits/Excellus_all_payers$Total.Inpatient.visits)*1000

Excellus_all_payers$Diagnosis.Inpatient.visit.rate<- (Excellus_all_payers$Diagnosis.Inpatient.visits/Excellus_all_payers$Total.Inpatient.visits)*1000

Excellus_all_payers$PainAbuseDiagnosis.Inpatient.visit.rate<- (Excellus_all_payers$PainAbuseDiagnosis.Inpatient.visits/Excellus_all_payers$Total.Inpatient.visits)*1000


#Outpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_payers$PainandAbuse.Outpatient.visit.rate<- (Excellus_all_payers$PainandAbuse.Outpatient.visits/Excellus_all_payers$Total.Outpatient.visits)*1000

Excellus_all_payers$Diagnosis.Outpatient.visit.rate<- (Excellus_all_payers$Diagnosis.Outpatient.visits/Excellus_all_payers$Total.Outpatient.visits)*1000

Excellus_all_payers$PainAbuseDiagnosis.Outpatient.visit.rate<- (Excellus_all_payers$PainAbuseDiagnosis.Outpatient.visits/Excellus_all_payers$Total.Outpatient.visits)*1000


#Add the Professional Other into the ix of IN, OUT and ERfor PainandAbuse and Diagnosis and PainAbuseDiagnosis
#Professional visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_payers$PainandAbuse.Professional.visit.rate<- (Excellus_all_payers$PainandAbuse.Professional.visits/Excellus_all_payers$Total.Professional.visits)*1000

Excellus_all_payers$Diagnosis.Professional.visit.rate<- (Excellus_all_payers$Diagnosis.Professional.visits/Excellus_all_payers$Total.Professional.visits)*1000

Excellus_all_payers$PainAbuseDiagnosis.Professional.visit.rate<- (Excellus_all_payers$PainAbuseDiagnosis.Professional.visits/Excellus_all_payers$Total.Professional.visits)*1000

#Other visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_payers$PainandAbuse.Other.visit.rate<- (Excellus_all_payers$PainandAbuse.Other.visits/Excellus_all_payers$Total.Other.visits)*1000

Excellus_all_payers$Diagnosis.Other.visit.rate<- (Excellus_all_payers$Diagnosis.Other.visits/Excellus_all_payers$Total.Other.visits)*1000

Excellus_all_payers$PainAbuseDiagnosis.Other.visit.rate<- (Excellus_all_payers$PainAbuseDiagnosis.Other.visits/Excellus_all_payers$Total.Other.visits)*1000


#Overall visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus_all_payers$PainandAbuse.Overall.visit.rate<- (Excellus_all_payers$PainandAbuse.Overall.visits/Excellus_all_payers$Total.Overall.visits)*1000

Excellus_all_payers$Diagnosis.Overall.visit.rate<- (Excellus_all_payers$Diagnosis.Overall.visits/Excellus_all_payers$Total.Overall.visits)*1000

Excellus_all_payers$PainAbuseDiagnosis.Overall.visit.rate<- (Excellus_all_payers$PainAbuseDiagnosis.Overall.visits/Excellus_all_payers$Total.Overall.visits)*1000

Excellus_all_payers[is.na(Excellus_all_payers)] <- 0
#Reshuffle columns and rename columns
#Excellus_all_payers<- Excellus_all_payers[,c(1,2,5,8,11,14)]

#colnames(Excellus_all_payers)<- c('Payer','Year', 'ER Visit Rate','Inpatient Visit Rate', 'Outpatient Visit Rate', 'Professional Visit Rate','Other Visit Rate' ,'Overall Visit Rate')

#NM March 6, 2018
#This part is changed to meet the new others into the IN/OUT and ER
Excellus_all_payers<- Excellus_all_payers[,c(1,2,7,8,9,14,15,16,21,22,23,28,29,30,35,36,37,42,43,44)]

#NM March6, 2018
colnames(Excellus_all_payers)<- c("Payer",'Year','Pain and Abuse ER Visit Rate','Poisoning by Opiates ER Visit Rate','PainAbuseDiagnosis ER Visit Rate',
                                  'Pain and Abuse Inpatient Visit Rate', 'Poisoning by Opiates Inpatient Visit Rate','PainAbuseDiagnosis Inpatient Visit Rate',
                                  'Pain and Abuse Outpatient Visit Rate','Poisoning by Opiates Outpatient Visit Rate','PainAbuseDiagnosis Outpatient Visit Rate',
                                  'Pain and Abuse Professional Visit Rate', 'Poisoning by Opiates Professional Visit Rate','PainAbuseDiagnosis Professional Visit Rate',
                                  'Pain and Abuse Other Visit Rate', 'Poisoning by Opiates Other Visit Rate','PainAbuseDiagnosis Other Visit Rate',
                                  'Pain and Abuse Overall Visit Rate','Poisoning by Opiates Overall Visit Rate','PainAbuseDiagnosis Overall Visit Rate')


Excellus_all_payers<- as.data.frame(Excellus_all_payers)

#Convert all the facility visits columns into rows using melt function. This is used for structuring a dataframe so that it can be used for line-plots
Excellus_all_payers<- melt(Excellus_all_payers, id.vars=c("Payer", "Year"), measure.vars=colnames(Excellus_all_payers)[3:20])

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

opioid_df_dictionary<- read.csv('data/opioid_df_dictionary.csv')

opioid_df_dictionary<- opioid_df_dictionary[!opioid_df_dictionary$Field %in% c("Opioid Ambulatory Surgery Visits", "Ambulatory Surgery Visits","Ambulatory Surgery Rate Per 1000"),]

Excellus<- Excellus[,-grep("Region", colnames(Excellus))]

#------------cOMBINE ALL PAYERS------------------------------------------------------

#Combine all facility visits across all the payers
combine_all_payers_Excellus <- Excellus %>% group_by(Year, County) %>% summarise_each(funs(sum), -Payer)

#Append a column of Payers whose value will be "All"
combine_all_payers_Excellus<- as.data.frame(append(combine_all_payers_Excellus, list(Payer='All'), after = 2))

#Append this to the original facility visits dataframe
Excellus<- rbind(Excellus, combine_all_payers_Excellus)


#------------COMBINE ALL YEARS------------------------------------------------------

#Combine all facility visits across all the payers
combine_all_years_Excellus <- Excellus %>% group_by(County,Payer) %>% summarise_each(funs(sum), -Year)

#Append a column of Payers whose value will be "All"
combine_all_years_Excellus<- as.data.frame(append(combine_all_years_Excellus, list(Year='All'), after = 0))

#Append this to the original facility visits dataframe
Excellus<- rbind(Excellus, combine_all_years_Excellus)

#-------------CALCULATE VISIT RATES---------------------
#ER visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus$PainandAbuse.ER.visit.rate<- (Excellus$PainandAbuse.ER.visits/ Excellus$Total.ER.visits)*1000

Excellus$Diagnosis.ER.visit.rate<- (Excellus$Diagnosis.ER.visits/ Excellus$Total.ER.visits)*1000

Excellus$PainAbuseDiagnosis.ER.visit.rate<- (Excellus$PainAbuseDiagnosis.ER.visits/ Excellus$Total.ER.visits)*1000
#Inpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus$PainandAbuse.Inpatient.visit.rate<- (Excellus$PainandAbuse.Inpatient.visits/Excellus$Total.Inpatient.visits)*1000

Excellus$Diagnosis.Inpatient.visit.rate<- (Excellus$Diagnosis.Inpatient.visits/Excellus$Total.Inpatient.visits)*1000

Excellus$PainAbuseDiagnosis.Inpatient.visit.rate<- (Excellus$PainAbuseDiagnosis.Inpatient.visits/Excellus$Total.Inpatient.visits)*1000

#Outpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus$PainandAbuse.Outpatient.visit.rate<- (Excellus$PainandAbuse.Outpatient.visits/Excellus$Total.Outpatient.visits)*1000

Excellus$Diagnosis.Outpatient.visit.rate<- (Excellus$Diagnosis.Outpatient.visits/Excellus$Total.Outpatient.visits)*1000 

Excellus$PainAbuseDiagnosis.Outpatient.visit.rate<- (Excellus$PainAbuseDiagnosis.Outpatient.visits/Excellus$Total.Outpatient.visits)*1000 
#FP March11, 2018
#Outpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus$PainandAbuse.Professional.visit.rate<- (Excellus$PainandAbuse.Professional.visits/Excellus$Total.Professional.visits)*1000

Excellus$Diagnosis.Professional.visit.rate<- (Excellus$Diagnosis.Professional.visits/Excellus$Total.Professional.visits)*1000

Excellus$PainAbuseDiagnosis.Outpatient.visit.rate<- (Excellus$PainAbuseDiagnosis.Outpatient.visits/Excellus$Total.Outpatient.visits)*1000 

#Outpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus$PainandAbuse.Other.visit.rate<- (Excellus$PainandAbuse.Other.visits/Excellus$Total.Other.visits)*1000

Excellus$Diagnosis.Other.visit.rate<- (Excellus$Diagnosis.Other.visits/Excellus$Total.Other.visits)*1000

Excellus$PainAbuseDiagnosis.Other.visit.rate<- (Excellus$PainAbuseDiagnosis.Other.visits/Excellus$Total.Other.visits)*1000

#Outpatient visit rate for PainandAbuse and Diagnosis and PainAbuseDiagnosis
Excellus$PainandAbuse.Overall.visit.rate<- (Excellus$PainandAbuse.Overall.visits/Excellus$Total.Overall.visits)*1000

Excellus$Diagnosis.Overall.visit.rate<- (Excellus$Diagnosis.Overall.visits/Excellus$Total.Overall.visits)*1000

Excellus$PainAbuseDiagnosis.Overall.visit.rate<- (Excellus$PainAbuseDiagnosis.Overall.visits/Excellus$Total.Overall.visits)*1000

Excellus[is.na(Excellus)]<- 0 
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
counties_spatial_df@data$region <- ifelse(counties_spatial_df@data$NAME_2 %in% cny.region, "CNY", ifelse(counties_spatial_df@data$NAME_2 %in% southern.tier.region, "Southern Tier", ifelse(counties_spatial_df@data$NAME_2 %in% rochester.region, "Rochester", ifelse(counties_spatial_df@data$NAME_2 %in% utica.region, "Utica", ifelse(counties_spatial_df@data$NAME_2 %in% univera, "Univera", "Other")))))

#Get Spatial information about the Excellus regions
counties_region <- counties_spatial_df[counties_spatial_df@data$NAME_2 %in% c(cny.region,southern.tier.region,rochester.region,utica.region,univera),]

#Create polygons for Excellus regions
Excellus_regions.polygons <- unionSpatialPolygons(counties_region, counties_region@data$region)

#--------Load the opioid-related ICD codes used for data extraction------

#Load ICD codes csv file

library(readxl)
ICD_codes <- read_excel("data/ICD codes for Opioids.xlsx")
colnames(ICD_codes)<- ICD_codes[1,]
ICD_codes<- ICD_codes[-1,]

##Load the SAS logic
sas_logic <- read.csv("data/testtable.csv")

#--------Select objects to keep in Rdata file----------------

objects_to_keep=c("Excellus_regions.polygons", "counties_spatial_df", "Excellus_all_payers", "Excellus_all_region","Excellus", "opioid_df_dictionary", "facility_visits_df","ICD_codes", "sas_logic")

rm(list=setdiff(ls(), objects_to_keep))

#save.image('C:/Users/mpatel2/Desktop/Excellus_data_preprocessing/final.Rdata')
save.image('dev-app/final.RData')
head('final.RData')

