# Computing RCI
findRCI <- function( scale ) {
  measureInfo <- measures[ measures$scale==scale, ]
  items <- eval( parse( text=measureInfo$items ) )
  reverseItems <- eval( parse( text=measureInfo$reverseItems ) )
  
  #Compute SD
  sd <- sd( dcw[ , paste0( measureInfo$scale, "PRE" ) ], na.rm=T)
  
  #Compute Cronbach's alpha
  varNames <- paste0( measureInfo$measure, "PRE_", items )
  #Create a subset containing measure items only
  varSubset <- dc[ , paste0( measureInfo$measure, "PRE_", seq( 1:measureInfo$numItems ) ) ]
  #Reverse items
  if ( !is.na( reverseItems[1] )) {
    varSubset[ , reverseItems ] <- abs( varSubset[ , reverseItems ] - 
                                          ( measureInfo$max + measureInfo$min ) )
  }
  alpha <- psych::alpha( varSubset[ , items ], warnings=F )
  alpha <- alpha$total[[1]]
  
  #Compute RCI
  rci <- 1.96 * sqrt(2) * sd * sqrt(1 - alpha)
  return(rci)
}

# Identify last measurement value
findLastValue <- function( patient, variable ) {
  last <- NA
  suffix <- c("PRE", paste0("W",seq(1:11)+1), "POST")
  vars <- paste0(variable, suffix)
  for (i in length(vars):1) {
    x <- dcw[dcw$patient==patient,vars[i]]
    if ( !is.na(x) ) {
      last <- x
      break
    }
  }
  return(last)
}

# Identify last but one measurement value
findLastButOneValue <- function( patient, variable ) {
  lastButOne <- NA
  suffix <- paste0("W",c(2:12))
  vars <- paste0(variable, suffix)
  for (i in length(vars):1) {
    x <- dcw[dcw$patient==patient,vars[i]]
    if ( !is.na(x) ) {
      lastButOne <- x
      break
    }
  }
  return(lastButOne)
}

# Identify last measurement - gives number of days from the start
findLastMeasurement <- function( patient, variable ) {
  last <- NA
  suffix <- c("PRE", paste0("W",seq(1:11)+1), "POST")
  vars <- paste0(variable, suffix)
  for (i in length(vars):1) {
    if ( !is.na( dcw[dcw$patient==patient,vars[i]] ) ) {
      last <- dcw[dcw$patient==patient,paste0("days_from_start",suffix[i])]
      break
    }
  }
  return(last)
}

# Identify last measurement - gives measurement name (for the use in individual reports)
findLastMeasurement2 <- function( patient, variable ) {
  last <- NA
  suffix <- c("PRE", paste0("W",seq(1:11)+1), "POST")
  vars <- paste0(variable, suffix)
  for (i in length(vars):1) {
    if ( !is.na(dcw[dc$patient==patient,vars[i]]) ) {
      last <- suffix[i]
      break
    }
  }
  return(last)
}
