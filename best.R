best <- function (state, outcome) {
  # The hospital name is the name provided in the Hospital.Name variable
  #Read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', colClasses = 'character');

  # set up valid parameters
  validOutcomes <- c('heart attack', 'heart failure', 'pneumonia');
  validStates <- unique(outcomeData[, 7])

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

  # Return hospital name in that state with the lowest 30-day death rate
  chosenStateInfo <- outcomeData[outcomeData$State == state];
  stateInfoForColumn <- chosenStateInfo[,fullColumnName]
  stateInfoForColumn <- as.double(stateInfoForColumn)
  lowestRateIndex <- which.min(stateInfoForColumn)
  hospitalName <- chosenStateInfo[lowestRateIndex, 'Hospital.Name']

  hospitalName
}

