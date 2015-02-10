rankall <- function (outcome, num = 'best') {
  # read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', colClasses = 'character');
  
  # Check that the state and outcome are valid
  validOutcomes <- c('heart attack', 'heart failure', 'pneumonia');
  validStates <- sort(unique(outcomeData[, 7]))

  # Check that the state and outcome are valid
  if (!outcome %in% validOutcomes) {
    stop('invalid outcome');
  }


  if (!state %in% validStates) {
    stop('invalid state');
  }

  columnNames <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')
  columnPosition <- match(outcome, validOutcomes);
  fullColumnName <- columnNames[columnPosition];

  for (index in seq_along(validStates)) {
    stateData <- outcomeData[outcomeData$State == validStates[index], ]
    sortedStateData <- stateData[order(as.numeric(stateData[[fullColumnName]]), stateData[['Hospital.Name']], decreasing=FALSE, na.last=NA), ]

    this.num = num
    if (this.num == 'best' ) {
      this.num = 1
    }

    if (this.num == 'worst') {
      this.num = nrow(sortedStateData)
    }

    hospital[index] <- sortedStateData[this.num, 'Hospital.Name']
  }

  data.frame(hospital=hospital, state=validStates, row.names = validStates)
}
