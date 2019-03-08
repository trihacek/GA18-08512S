##############################################################################
# Code for project: GA18-08512S
# Version: 2019-03-04
# Legend: This script loads data from an Excel file and creates three datasets:
#         dc  = "data clinical", a raw data set
#         dcw = a wide-format data set with computed scale scores, without 
#               individual scale items
#         dcl = dcw reshaped in long format, with time-varying variables 
#               centered to individual initial values and with lag 1 variables
##############################################################################

library(openxlsx)
library(psych)
source("mups_functions.R")
options(max.print=3000)


# Loading raw data --------------------------------------------------------

demo_sheet <- read.xlsx(xlsxFile="data_clinical.xlsx", sheet="DEMO", 
                           skipEmptyRows=F, skipEmptyCols=F, na.strings=c(""," ","NA","N/A"))
rows <- nrow(demo_sheet)+1
dc <- as.data.frame(demo_sheet)
rm(demo_sheet)
sheetNames <- c("PRE",paste("W",seq(from=2,to=12,by=1),sep=""),"POST","FU6","FU12")
for (i in 1:length(sheetNames)) {
  sheet <- read.xlsx(xlsxFile="data_clinical.xlsx", sheet=sheetNames[i], 
                      rows=c(1:rows), skipEmptyRows=F, skipEmptyCols=F, na.strings=c(""," ","NA","N/A"))
  dc <- cbind(dc,as.data.frame(sheet[,-1]))
}


# Compute NEQ scores
for (id in 1:nrow(dc)) {
  for (var in 1:32) {
    # Set intensity = 0 where an event did not occure
    if ( !is.na(dc[id,paste0("neq_",var,"_presence")]) )
      if (dc[id,paste0("neq_",var,"_presence")] == 0 )
        dc[id,paste0("neq_",var,"_intensity")] <- 0
    # Create Event_therapy a and Event_extra variables
    if ( !is.na(dc[id,paste0("neq_",var,"_therapy")]) ) {
      dc[id,paste0("neqPOST_",var)] <- dc[id,paste0("neq_",var,"_intensity")]
      if ( dc[id,paste0("neq_",var,"_therapy")] %in% c(1,2) ) #Event attributed to therapy alone or both
        dc[id,paste0("neqtPOST_",var)] <- dc[id,paste0("neq_",var,"_intensity")]
      if ( dc[id,paste0("neq_",var,"_therapy")] %in% c(0,2) ) #Event attributed to extratherapeutic alone or both
        dc[id,paste0("neqePOST_",var)] <- dc[id,paste0("neq_",var,"_intensity")]
    }   
  }
}


# Creating a wide dataset ---------------------------------------------------
# dcw contains demographic and computed variables stored in a wide format

# Loading measures table
measures <- read.xlsx(xlsxFile="measures.xlsx", sheet="measures")

# Copying demographic variables
dcw <- dc[,c(1:match("treatment_length",names(dc)))]

# Computing variables
for (i in 1:nrow(measures)) {
  items <- eval( parse( text=measures[i,"items"] ) )
  reverseItems <- eval( parse( text=measures[i,"reverseItems"] ) )

  for (j in 1:length(sheetNames)) {
    #In case the variable was measured at that time point
    if ( !is.na( measures[ i, sheetNames[j] ] ) ) {
      varNames <- paste0( measures[i,"measure"], sheetNames[j], "_", items )
      varSubset <- dc[ , paste0( measures[i,"measure"], sheetNames[j], "_", seq( 1:measures[i,"numItems"] ) ) ]
      
      #Reverse items
      if ( !is.na( reverseItems[1] )) {
        varSubset[ , reverseItems ] <- abs( varSubset[ , reverseItems ] - 
          ( measures[i,"max"] + measures[i,"min"]) )
      }
      
      #Create a subset containing measure items only
      varSubset <- dc[ , varNames ]
      
      #Missing values: if more than 10%, score not computed
      allowedNAs <- round( length(items) / 10, 0 )
      NAs <- rowSums( is.na( varSubset ) )
      

      #Sum score
      dcw[ , paste0( measures[ i, "scale" ], sheetNames[j] ) ] <- 
        rowSums( varSubset, na.rm=T )
      
      ##REMOVE PATIENTS BASED ON MISSING VALUES
      #dcw[ NAs > allowedNAs, paste0( measures[ i, "scale" ], sheetNames[j])] <- NA
      dcw[ NAs == length(items), paste0( measures[ i, "scale" ], sheetNames[j])] <- NA
      
      #Mean score
      if ( measures[ i, "score" ] == "mean" ) {
        dcw[ , paste0( measures[ i, "scale" ], sheetNames[j] ) ] <- 
          dcw[ , paste0( measures[ i, "scale" ], sheetNames[j] ) ] / 
          ( length(items) - NAs )
      }
    }
  }
}
rm(i,j,items,reverseItems,NAs,allowedNAs,varSubset,varNames)

# Add other variables
dcw$somPRE_functioning <- dc$somPRE_functioning
dcw$depPRE_functioning <- dc$depPRE_functioning
dcw$gadPRE_functioning <- dc$gadPRE_functioning
dcw$somPOST_functioning <- dc$somPOST_functioning
dcw$depPOST_functioning <- dc$depPOST_functioning
dcw$gadPOST_functioning <- dc$gadPOST_functioning

# Add MUPS
for ( patient in 1:nrow(dc) ) {
  isMUPS <- FALSE
  listMUPS <- c()
  numReason <- 0
  for ( symptom in 1:17 ) {
    #If the symptom fulfills MUPS self-report criteria
    if ( #!is.na( dc[ patient, paste0( "somPRE_", symptom, "_reason" ) ] ) & 
         !is.na( dc[ patient, paste0( "somPRE_", symptom, "_duration" ) ] ) & 
         !is.na( dc[ patient, paste0( "somPRE_", symptom, "_mups" ) ] ) & 
         #dc[ patient, paste0( "somPRE_", symptom, "_reason" ) ] == 1 & 
         dc[ patient, paste0( "somPRE_", symptom, "_duration" ) ] == 1 & 
         dc[ patient, paste0( "somPRE_", symptom, "_mups" ) ] == 1 ) {
      listMUPS <- append( listMUPS, symptom )
    }
    if ( !is.na( dc[ patient, paste0( "somPRE_", symptom, "_reason" ) ] ) & 
          dc[ patient, paste0( "somPRE_", symptom, "_reason" ) ] == 1 ) {
      numReason <- numReason + 1
    }
  }
  numMUPS <- length( listMUPS )
  
  if ( numMUPS > 0 )
    isMUPS <- TRUE
  
  #Compute MUPS intensity across all measurement points
  for ( i in 1:length(sheetNames) ) {
    varSubset <- dc[ , paste0( "som", sheetNames[i], "_", seq(1:17) ) ]
    if ( numMUPS == 0 )
      dcw[ patient, paste0( "mups", sheetNames[i] ) ] <- NA
    else if ( numMUPS == 1 ) 
      dcw[ patient, paste0( "mups", sheetNames[i] ) ] <- varSubset[ patient, listMUPS ]
    else {
      dcw[ patient, paste0( "mups", sheetNames[i] ) ] <- rowSums( varSubset[ patient , listMUPS ], na.rm=T )
      NAs <- rowSums( is.na( varSubset[ patient , listMUPS ] ) )
      #Prevent division by zero
      if ( length(listMUPS) - NAs != 0 )
        dcw[ patient, paste0( "mups", sheetNames[i] ) ] <- 
          dcw[ patient, paste0( "mups", sheetNames[i] ) ] / 
          ( length(listMUPS) - NAs )
      else
        dcw[ patient, paste0( "mups", sheetNames[i] ) ] <- NA
    }
  }
  
  dcw[ patient , "is_mups"] <- isMUPS
  dcw[ patient , "num_mups"] <- numMUPS
  dcw[ patient , "num_reason"] <- numReason
}
rm(patient,symptom,numMUPS,isMUPS,listMUPS,varSubset,NAs)

# Compute individual means
vars_mean <- c("sacip_resource","sacip_problem","sacip_mastery","sacip_clarification","gsrs","gcs")
suffix <- c(paste0("W", c(2:12)), "POST")
for (i in 1:length(vars_mean)) {
  dcw[,paste0(vars_mean[i],"MEAN")] <- apply(dcw[,paste0(vars_mean[i],suffix)], 1, function(x){
    mean(x, na.rm=T)
  })
}

# Functioning
dcw$functioningPRE <- rowSums(dcw[,c("depPRE_functioning","gadPRE_functioning","somPRE_functioning")])/3
dcw$functioningPOST <- rowSums(dcw[,c("depPOST_functioning","gadPOST_functioning","somPOST_functioning")])/3

# Compute individual difference scores
vars_diff <- c("som","mups","ors","dep","gad","who","functioning",
          "maia_total","maia_noticing","maia_nDistracting","maia_nWorrying","maia_attRegulation",
          "maia_emRegulation","maia_selfRegulation","maia_bodyListening","maia_trusting",
          "ersq_total","ersq_understanding","ersq_clarity","ersq_bodyAwareness","ersq_resilience",
          "ersq_acceptance","ersq_selfSupport","ersq_regulation","ersq_confrontReadiness","ersq_attention",
          "rns_total","rns_authenticity","rns_support","rns_impact","rns_shared","rns_initOther",
          "cpaq_total","cpaq_actEngagement","cpaq_symptWillingness")
dcw[,paste0(vars_diff,"DIFF")] <- dcw[,paste0(vars_diff,"POST")] - dcw[,paste0(vars_diff,"PRE")]

# Days from the beginning of therapy
origin <- "2000-01-01"
for (i in 1:length(sheetNames))
  dcw[,paste0("days_from_start",sheetNames[i])] <- as.numeric( as.Date(dc[,paste0("date",sheetNames[i])], origin=origin) - 
  as.Date(dc[,"datePRE"], origin=origin) )

# Dose of therapy
#dcw$dose <- rowSums(dcw[,paste0("week",seq(1:12))])
#V nekterych polickach jsou pismena, proto zatim nefunguje

# Find last measurement for SOM, MUPS, and ORS
## SOM
dcw$somLAST <- apply(dcw, 1, function(x) {
  findLastValue(x[1], "som")
  })
dcw$somLAST_days <- apply(dcw, 1, function(x) {
  findLastMeasurement(x[1], "som")
})
## MUPS
dcw$mupsLAST <- apply(dcw, 1, function(x) {
  findLastValue(x[1], "mups")
})
dcw$mupsLAST_days <- dcw$somLAST_days
## ORS
dcw$orsLAST <- apply(dcw, 1, function(x) {
  findLastValue(x[1], "ors")
})
dcw$orsLAST_days <- apply(dcw, 1, function(x) {
  findLastMeasurement(x[1], "ors")
})

# Number of ORS nad MUPS measurements per patient
dcw$num_measurements_ors <- rowSums(!is.na(dcw[,paste0("ors",c("PRE", paste0("W", c(2:12)), "POST"))]))
dcw$num_measurements_mups <- rowSums(!is.na(dcw[,paste0("mups",c("PRE", paste0("W", c(2:12)), "POST"))]))


# Creating a long dataset ---------------------------------------------------
# dcl contains demographic and computed variables stored in a long (person-period) format

suffix <- c("PRE", paste0("W", c(2:12)), "POST")
dcw[,c("weekPRE","weekPOST","sacip_resourcePRE","sacip_problemPRE","sacip_masteryPRE","sacip_clarificationPRE","gsrsPRE","gcsPRE")] <- NA

dcl <- reshape(dcw, 
               varying=list(
                 paste0("days_from_start",suffix),
                 paste0("week",c("PRE", c(2:12), "POST")),
                 paste0("ors",suffix),
                 paste0("som",suffix),
                 paste0("mups",suffix),
                 
                 paste0("maia_total",suffix),
                 paste0("maia_noticing",suffix),
                 paste0("maia_nDistracting",suffix),
                 paste0("maia_nWorrying",suffix),
                 paste0("maia_attRegulation",suffix),
                 paste0("maia_emRegulation",suffix),
                 paste0("maia_selfRegulation",suffix),
                 paste0("maia_bodyListening",suffix),
                 paste0("maia_trusting",suffix),

                 paste0("ersq_total",suffix),
                 paste0("ersq_understanding",suffix),
                 paste0("ersq_clarity",suffix),
                 paste0("ersq_bodyAwareness",suffix),
                 paste0("ersq_resilience",suffix),
                 paste0("ersq_acceptance",suffix),
                 paste0("ersq_selfSupport",suffix),
                 paste0("ersq_regulation",suffix),
                 paste0("ersq_confrontReadiness",suffix),
                 paste0("ersq_attention",suffix),
                 
                 paste0("rns_total",suffix),
                 paste0("rns_authenticity",suffix),
                 paste0("rns_support",suffix),
                 paste0("rns_impact",suffix),
                 paste0("rns_shared",suffix),
                 paste0("rns_initOther",suffix),
                 
                 paste0("cpaq_total",suffix),
                 paste0("cpaq_actEngagement",suffix),
                 paste0("cpaq_symptWillingness",suffix),
                 
                 paste0("sacip_resource",suffix),
                 paste0("sacip_problem",suffix),
                 paste0("sacip_mastery",suffix),
                 paste0("sacip_clarification",suffix),
                 paste0("gsrs",suffix),
                 paste0("gcs",suffix)
               ), 
               v.names=c("time_day","attendance","ors","som","mups",
                         "maia_total","maia_noticing","maia_nDistracting","maia_nWorrying","maia_attRegulation",
                         "maia_emRegulation","maia_selfRegulation","maia_bodyListening","maia_trusting",
                         "ersq_total","ersq_understanding","ersq_clarity","ersq_bodyAwareness","ersq_resilience",
                         "ersq_acceptance","ersq_selfSupport","ersq_regulation","ersq_confrontReadiness","ersq_attention",
                         "rns_total","rns_authenticity","rns_support","rns_impact","rns_shared","rns_initOther",
                         "cpaq_total","cpaq_actEngagement","cpaq_symptWillingness",
                         "sacip_resource","sacip_problem","sacip_mastery","sacip_clarification",
                         "gsrs","gcs"
                         ), 
               timevar="time_week", 
               times=c(1:length(suffix)),
               idvar="patient", 
               direction="long", 
               drop=c("nonclinical","mail","secondary_use","somatic_disorders_text"
               )
              )

dcw[,c("weekPRE","weekPOST","sacip_resourcePRE","sacip_problemPRE","sacip_masteryPRE","sacip_clarificationPRE","gsrsPRE","gcsPRE")] <- NULL

# Recenter time_week to PRE = 0
dcl$time_week <- dcl$time_week-1


# Adding lag 1 variables for all time-varying variables
# Variables are centered to individual initial values; initial values are kept in time-invariant "PRE" variables
vars <- c("som","mups","ors","maia_total","maia_noticing","maia_nDistracting","maia_nWorrying","maia_attRegulation",
               "maia_emRegulation","maia_selfRegulation","maia_bodyListening","maia_trusting",
               "ersq_total","ersq_understanding","ersq_clarity","ersq_bodyAwareness","ersq_resilience",
               "ersq_acceptance","ersq_selfSupport","ersq_regulation","ersq_confrontReadiness","ersq_attention",
               "rns_total","rns_authenticity","rns_support","rns_impact","rns_shared","rns_initOther",
               "cpaq_total","cpaq_actEngagement","cpaq_symptWillingness")
dcl[,paste0(vars,"PRE")] <- NA
dcl[,paste0(vars,"LAG1")] <- NA
for (i in 1:nrow(dcw)) {
  
  # Create time-invariant PRE variables
  dcl[dcl$patient == dcw[i,"patient"],paste0(vars,"PRE")] <- dcw[i,paste0(vars,"PRE")]
  
  # Recenter vars to individual PRE values
  for (week in 0:max(dcl$time_week)) {
    dcl[ dcl$patient == dcw[i,"patient"] & dcl$time_week == week, vars ] <-
      dcl[ dcl$patient == dcw[i,"patient"] & dcl$time_week == week, vars ] - dcw[i,paste0(vars,"PRE")]
  }
  
  # Move  mediators to lag 1
  for (week in max(dcl$time_week):1) {
    dcl[ dcl$patient == dcw[i,"patient"] & dcl$time_week == week, paste0(vars,"LAG1") ] <-
      dcl[ dcl$patient == dcw[i,"patient"] & dcl$time_week == (week-1), vars ]
  }
  dcl[ dcl$patient == dcw[i,"patient"] & dcl$time_week == 0, paste0(vars,"LAG1") ] <- NA
}


# Other -------------------------------------------------------------------

# List of patients who have been interviewed in the context of WP3
interview <- c(
  "HP-002",
  "HP-011",
  "HP-015",
  "KA-005",
  "PS-002",
  "PS-003",
  "PS-004",
  "PS-016",
  "PS-017",
  "PS-018",
  "PS-019",
  "PS-021",
  "PS-041",
  #scheduled
  "HP-025",
  "HP-028",
  "PS-051",
  "PS-032",
  "PS-044"
)
dcw$interview <- FALSE
dcw[dcw$patient %in% interview, "interview"] <- TRUE