# R DAPT 621 Assignment 2
# Dana Mark
# November 20, 2018

#-----------------------------------------------------------------#
# SET UP ----
#-----------------------------------------------------------------#
# Clean out the Environment
rm(list = ls())

# -- !!! SET OWN DIRECTORY HERE !!!!! ----------------------------#
# Set working directory
wdpath <- 'C:/Users/dmark/Documents/DAPT/R/Assign 2/'
# ----------------------------------------------------------------#

setwd(wdpath)

# load packages (install if needed)
required_packages <- c('openxlsx', 'data.table', 'tidyr')

# get the list of packages that are not yet installed on your machine
missing_packages <- required_packages[!(required_packages %in% installed.packages()[ , 'Package'])]

# install the missing packages
if(length(missing_packages) > 0) install.packages(missing_packages)

library(openxlsx)
library(data.table)
library(tidyr)

# Import Dataset

data <- read.xlsx('Dr. Harinder Dhindsa GEMS and HEMS Response 9-13-2018.xlsx',
                  sheet = ' HEMS 911 Responses', detectDates = TRUE)

#-----------------------------------------------------------------#
# QUESTION 1 ----
#-----------------------------------------------------------------#

# Drop columns not needed
keep <- c('Agency', 
          'Incident.Complaint.Reported.By.Dispatch..eDispatch.01.', 
          'Incident.Unit.Notified.By.Dispatch.Date.Time..eTimes.03.', 
          'Scene.Incident.City.Name..eScene.17.', 
          'Scene.Incident.County.Name..eScene.21.', 
          'Situation.Complaint.Type..eSituation.03.', 
          'Situation.Initial.Patient.Acuity..eSituation.13.', 
          'Vitals.Signs.Taken.Date.Time..eVitals.01.', 
          'Vitals.Systolic.Blood.Pressure.SBP..eVitals.06.', 
          'Vitals.Total.Glasgow.Coma.Score.GCS..eVitals.23.', 
          'Incident.Patient.Arrived.At.Destination.Date.Time..eTimes.11.'
          )

hems <- data[, keep]

# Convert hems to data table
hems <- data.table(hems)

# See Updated Data
View(hems)

#-----------------------------------------------------------------#
# QUESTION 2 ----
#-----------------------------------------------------------------#

# Rename columns
newcolnames <- c('AGENCY', 
              'INCIDENT', 
              'NOTIFIED_TIME', 
              'CITY', 
              'COUNTY', 
              'COMPLAINT_TYPE', 
              'PATIENT_ACUITY',
              'VITALS_TAKEN_TIME',
              'VITALS_BP',
              'VITALS_COMA_SCORE',
              'ARRIVAL_DEST_TIME'
              )

colnames(hems) <- newcolnames

# See Updated Data
View(hems)

#-----------------------------------------------------------------#
# QUESTION 3 ----
#-----------------------------------------------------------------#

# Modify AGENCY names
# Identify old names and new names in vectors
oldnames <-  c('AIRCARE' , 'MedStar/MID-ATLANTIC AIR TRANSPORT SERVICE', 'CARILION LIFE-GUARD MED-TRANS CORP',                          
'PHI AIR MEDICAL VIRGINIA', 'NIGHTINGALE AIR AMBULANCE', 'AIR METHODS INC/LifeEvac', 'WINGS AIR RESCUE', 
'University of Virginia Health System, Medical Transport Network', 'FAIRFAX COUNTY POLICE DEPARTMENT', 
'VIRGINIA STATE POLICE - MEDFLIGHT II', 'Air Evac EMS', 'VIRGINIA STATE POLICE - MEDFLIGHT I')

newnames <- c('Aircare', 'MedStar_MidAtl', 'Carilion', 'Phi Air', 'Nightingale', 'AirMethods_LifeEvac', 'Wings Air',
              'UVA', 'Fairfax', 'MedFlight II', 'Air Evacs', 'MedFlight I')

# Replace old names with new names with a loop
i = 0
for (i in 1:length(oldnames)) {
  hems$AGENCY[hems$AGENCY == oldnames[i]] <- newnames[i]
}

# See Updated Data
View(hems)

#-----------------------------------------------------------------#
# QUESTION 4 ----
#-----------------------------------------------------------------#

# Calculate total duration between incident notification time and patient arrival time at destination
# First convert columns NOTIFIED_TIME and ARRIVAL_DEST_TIME to datetime (POSIXct) format
hems$NOTIFIED_TIME <- convertToDateTime(hems$NOTIFIED_TIME)
hems$ARRIVAL_DEST_TIME <- convertToDateTime(hems$ARRIVAL_DEST_TIME)

# Create new column for total duration
hems$TOT_DURATION <- as.numeric(difftime(hems$ARRIVAL_DEST_TIME, hems$NOTIFIED_TIME, units = 'mins'))

# Check that values are reasonable
# Check if any negative
checkneg.TOT_DUR <- hems[TOT_DURATION < 0]

# Check for outliers
plot(hems$TOT_DURATION)

# See Updated Data
View(hems)

#-----------------------------------------------------------------#
# QUESTION 5 ----
#-----------------------------------------------------------------#

# Calculate the reaction time between the incident notification time and time when vital signs were taken
# First convert column VITALS_TAKEN_TIME to datetime (POSIXct) format
hems$VITALS_TAKEN_TIME <- convertToDateTime(hems$VITALS_TAKEN_TIME)

# Create new column for reaction time
hems$REACT_TIME <- as.numeric(difftime(hems$VITALS_TAKEN_TIME, hems$NOTIFIED_TIME, units = 'mins'))

# Check that values are reasonable
# Check if any negative
checkneg.REACT_TIME <- hems[REACT_TIME < 0]

# Check for outliers
plot(hems$REACT_TIME)

# See Updated Data
View(hems)


#-----------------------------------------------------------------#
# QUESTION 6 ----
#-----------------------------------------------------------------#

# Calculate the following metrics for each agency:
#   Total number of incidents
#   Average reaction time (median)
#   Average total duration (median)

# Create aggregated dataset using data.table package
# I noticed there are negative values for reaction time and total duration as well as some large values,
#   but for the purposes of this R homework, I'll leave them in my calculations
# There are some NAs, so "na.rm = TRUE" argument allows the calculation to ignore these values
hems_agg <- hems[, 
                    .(
                      NUM_INCIDENTS = .N,
                      MED_REACTION_TIME = median(REACT_TIME, na.rm = TRUE),
                      MED_TOT_DURATION = median(TOT_DURATION, na.rm = TRUE)
                    ), 
                    by = AGENCY][order(-NUM_INCIDENTS)]

# See new dataset
View(hems_agg)

# Which agency has highest number of incidents?
# Dataset already sorted by highest to lowest incidents, so can pull first row of dataset
# Phi Air has the highest number of incidents
Q6_numinc <- hems_agg[, .(AGENCY, NUM_INCIDENTS)][1]
Q6_numinc

# Does the same agency also have the shortest median reaction time? 
# Can sort by AVG_REACTION_TIME ascending to pull shortest. 
# Fairfax has the shortest reaction time (not the same agency w/ the highest number of incidents)
Q6_reactiontime <- hems_agg[, .(AGENCY, NUM_INCIDENTS, MED_REACTION_TIME)][order(MED_REACTION_TIME)][1]
Q6_reactiontime

#-----------------------------------------------------------------#
# QUESTION 7 ----
#-----------------------------------------------------------------#

# Find most commonly occuring incident by county (distribution)

# first get number of incidents by county
x <- hems[, 
          .(
          NUM_INCIDENTS = .N
          ), 
          by = .(COUNTY)][order(COUNTY)]

# number of incidents by incident type by county
hems_county <- hems[,
                    .(
                      NUM_INC_BY_TYPE = .N
                    ),
                    by = .(COUNTY, INCIDENT)][order(COUNTY, -NUM_INC_BY_TYPE)]

# add rank for incident type by county (easier to identify most common incidents later)
# ties.method = 'dense' allows ties to be numbered the same
hems_county <- hems_county[, INC_RANK := frank(.SD, -NUM_INC_BY_TYPE, ties.method = 'dense'), by = COUNTY]

# add column of total incidents by county to hems_county
hems_county <- merge(hems_county, x, by = c('COUNTY'))

# add column to calculate proportion of incident type by county
hems_county$INCIDENT_DISTN <- hems_county$NUM_INC_BY_TYPE / hems_county$NUM_INCIDENTS

# add column to identify most common incident by county
# first determine if there are multiple incidents with the highest distribution per county
count_topinc <- hems_county[INC_RANK == '1', .(NUM_TOP_INC = .N), by = COUNTY]

# Separate counties with more than one common incident and bring in name of common incident (inner join)
count_topinc_1 <- merge(count_topinc[NUM_TOP_INC == '1'], hems_county[INC_RANK == '1', .(COUNTY, INCIDENT)])
count_topinc_Mult <- count_topinc[NUM_TOP_INC != '1']
count_topinc_Mult$INCIDENT <- 'Multiple'
count_topinc <- rbind(count_topinc_Mult, count_topinc_1)
names(count_topinc)[names(count_topinc) == 'INCIDENT'] <- 'MOST_COM_INC'

# Add MOST_COM_INC to hems_county
hems_county <- merge(hems_county, count_topinc, by = c('COUNTY'))

# View data with county, total incidents, incident prevalence, and most common incident type
View(hems_county)

#-----------------------------------------------------------------#
# QUESTION 8 ----
#-----------------------------------------------------------------#

# Determine counties with > 4 incidents and this type of incident makes up all incidents for county
# Output shows that there are 13 counties that meet this criteria with Fauquier with the highest number of incidents
hems_county[NUM_INC_BY_TYPE > 4 & INCIDENT_DISTN == 1, .(COUNTY, INCIDENT, NUM_INCIDENTS, INCIDENT_DISTN)][order(-NUM_INCIDENTS)]

#-----------------------------------------------------------------#
# QUESTION 9 ----
#-----------------------------------------------------------------#

# Calculate the percentage of missing values for blood pressure by patient acuity

# Calculate number of NAs by PATIENT_ACUITY
bp_NA_count <- hems[is.na(VITALS_BP), .(NUM_MISSING = .N), by = .(PATIENT_ACUITY)]

# Calculate total records by PATIENT_ACUITY
bp_count <- hems[, .(COUNT = .N), by = .(PATIENT_ACUITY)]

# left join on count
bp_NA_count <- merge(bp_count, bp_NA_count, by = c('PATIENT_ACUITY'), all.x = TRUE)

# calculate percent
# Change NAs to zero (= PATIENT_ACUITY w/ no missing values) and order by highest percentage
bp_NA_count$PCT_MISSING <- bp_NA_count$NUM_MISSING / bp_NA_count$COUNT
bp_NA_count <- bp_NA_count[, lapply(.SD, function(x){ifelse(is.na(x), 0, x)})][order(-PCT_MISSING)]

# View dataset
View(bp_NA_count)

#-----------------------------------------------------------------#
# QUESTION 10 ----
#-----------------------------------------------------------------#

# Which one of the following imputation values would you recommend for patient blood pressure: 
#   mean, median, mode, zero, or other? 
# Impute missing values using preferred method and provide reasoning

# Calculate mean, median, and mode
bp_mean <- mean(hems$VITALS_BP, na.rm = TRUE)
bp_median <- median(hems$VITALS_BP, na.rm = TRUE)

# Mode - no built in option so create function
mode_funct <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

bp_mode <- mode_funct(hems$VITALS_BP)

# Mean, median, and mode do not vary significantly from one another so choosing any option is reasonable
# Mode makes sense in this situation since it is the most common BP for patients in this dataset

# Replace missing values with mode
hems$VITALS_BP[is.na(hems$VITALS_BP)] <- bp_mode

# Check no NAs
hems[is.na(hems$VITALS_BP), .N]

# View updated dataset
View(hems)

#-----------------------------------------------------------------#
# QUESTION 11 ----
#-----------------------------------------------------------------#

# Extract the hour of day from the incident notification time column
# Use groupings:
# a. Morning: 6 to 9 
# b. Daytime:  10 to 16 
# c. Evening: 17 to 19 
# d. Night: 20 to 22 
# e. Late night: 23 to 1 
# f. Overnight: 2 to 5 

# Add column for Notified hour and make type factor
hems$NOTIFIED_HR <- factor(hour(hems$NOTIFIED_TIME))

# Define levels for various groupings
levels(hems$NOTIFIED_HR) <- list(
  Morning = c('6', '7', '8', '9'),
  Daytime = c('10', '11', '12', '13', '14', '15', '16'),
  Evening = c('17', '18', '19'),
  Night = c('20', '21', '22'),
  LateNight = c('23', '24', '0', '1'),
  Overnight = c('2', '3', '4', '5')
)

# View updated dataset
View(hems)

#-----------------------------------------------------------------#
# QUESTION 12 ----
#-----------------------------------------------------------------#

# For each day part from the data frame created above, calculate the percentage of incidents by complaint type
# First get total incidents by Complaint Type/Notified Hour
hems_complaint <- hems[,
             .(
               NUM_INCIDENTS = .N
             ), by = .(NOTIFIED_HR, COMPLAINT_TYPE)]

# Next group by notified hr and divide by total to get percentage
hems_complaint <- hems_complaint[, PCT_INCIDENTS := NUM_INCIDENTS / sum(NUM_INCIDENTS), by = NOTIFIED_HR]

# View new dataset
View(hems_complaint)

#-----------------------------------------------------------------#
# QUESTION 13 ----
#-----------------------------------------------------------------#

# Transpose hems_complaint data
hems_complaint_T <- spread(hems_complaint[, -c('NUM_INCIDENTS')], COMPLAINT_TYPE, PCT_INCIDENTS)

# Remove NAs
hems_complaint_T$Other[is.na(hems_complaint_T$Other)] <- 0

# View new dataset
View(hems_complaint_T)


#-----------------------------------------------------------------#
# CLEAN UP ----
#-----------------------------------------------------------------#

# remove temporary objects from environment
rm(bp_count, checkneg.REACT_TIME, checkneg.TOT_DUR, count_topinc, count_topinc_1, count_topinc_Mult, data, x,
   i, keep, missing_packages, newcolnames, newnames, oldnames, required_packages, wdpath, mode_funct)