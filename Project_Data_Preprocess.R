#################################################
#  Company    : Stevens Tech 
#  Project    : Group Project
#  Purpose    : 
#  First Name : Sayan
#  Last Name  : Mukherjee
#  Id			    : 10430998
#  Date       : 04/20/2018
#  Comments   :

rm(list=ls())
#################################################

# Load 2017 Strom Details data
storm_data_all <- read.csv("StormEvents_details_2017.csv", na.strings = "")

# Select non-redundandant and relevant features for analysis
#relevant_columns <- c("BEGIN_DAY", "BEGIN_TIME", "END_DAY", "END_TIME", "STATE", "MONTH_NAME", "EVENT_TYPE", "CZ_TIMEZONE", 
#                      "INJURIES_DIRECT", "INJURIES_INDIRECT",	"DEATHS_DIRECT",	"DEATHS_INDIRECT",	"DAMAGE_PROPERTY",
#                      "DAMAGE_CROPS",	"SOURCE", "BEGIN_LOCATION", "END_LOCATION")

relevant_columns <- c("BEGIN_DAY", "END_DAY", "STATE", "MONTH_NAME", "EVENT_TYPE", 
                      "INJURIES_DIRECT", "INJURIES_INDIRECT",	"DEATHS_DIRECT",	"DEATHS_INDIRECT",	"DAMAGE_PROPERTY",
                      "DAMAGE_CROPS",	"SOURCE", "MAGNITUDE",	"MAGNITUDE_TYPE")

storm_data_relevant <- storm_data_all[relevant_columns]

# Utility for viewing data
View(storm_data_relevant)

# combine injuries
storm_data_relevant[,"INJURIES"] <- storm_data_relevant[,"INJURIES_DIRECT"] +  storm_data_relevant[,"INJURIES_INDIRECT"]
storm_data_relevant[,"DEATHS"] <- storm_data_relevant[,"DEATHS_DIRECT"] +  storm_data_relevant[,"DEATHS_INDIRECT"]
storm_data_relevant <- storm_data_relevant[,-which(names(storm_data_relevant) == "INJURIES_DIRECT")]
storm_data_relevant <- storm_data_relevant[,-which(names(storm_data_relevant) == "INJURIES_INDIRECT")]
storm_data_relevant <- storm_data_relevant[,-which(names(storm_data_relevant) == "DEATHS_DIRECT")]
storm_data_relevant <- storm_data_relevant[,-which(names(storm_data_relevant) == "DEATHS_INDIRECT")]

# Process NA or missing data
storm_data_relevant[is.na(storm_data_relevant$DAMAGE_PROPERTY), "DAMAGE_PROPERTY"] <- "0.00K"
storm_data_relevant[is.na(storm_data_relevant$DAMAGE_CROPS), "DAMAGE_CROPS"] <- "0.00K"

?gsub
storm_data_relevant$DAMAGE_CROPS <- gsub(".*K.*", "Low", storm_data_relevant$DAMAGE_CROPS)
storm_data_relevant$DAMAGE_CROPS <- gsub(".*M.*", "Medium", storm_data_relevant$DAMAGE_CROPS)
storm_data_relevant$DAMAGE_CROPS <- gsub(".*B.*", "High", storm_data_relevant$DAMAGE_CROPS)

storm_data_relevant$DAMAGE_PROPERTY <- gsub(".*K.*", "Low", storm_data_relevant$DAMAGE_PROPERTY)
storm_data_relevant$DAMAGE_PROPERTY <- gsub(".*M.*", "Medium", storm_data_relevant$DAMAGE_PROPERTY)
storm_data_relevant$DAMAGE_PROPERTY <- gsub(".*B.*", "High", storm_data_relevant$DAMAGE_PROPERTY)

# If still empty records exist, remove them
storm_data_relevant <- na.omit(storm_data_relevant)

# summary of relevant data. No records with missing values
summary(storm_data_relevant)
# check number of discrete unique values before categorisation
rapply(storm_data_relevant,function(x)length(unique(x)))
curve(sapply(storm_data_relevant,function(x)length(unique(x))))
?unique
unique(storm_data_relevant$DAMAGE_PROPERTY)