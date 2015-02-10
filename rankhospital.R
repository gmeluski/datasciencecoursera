rankhospital <- function (state, outcome, num='best') {
  # read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', colClasses = 'character');

  # Check that the state and outcome are valid
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

  # Return the hospital name in that state with given rank 30-day death rate
  chosenStateInfo <- outcomeData[outcomeData$State == state]; 
  dataByState <- chosenStateInfo[order(as.numeric(chosenStateInfo[[fullColumnName]]), chosenStateInfo[['Hospital.Name']], decreasing=FALSE, na.last=NA)]

  if (num == 'best') {
    num = 1
  }
  if (num == 'worst') {
    num = nrow(dataByState)
  }

  dataByState[num, 'Hospital.Name']
}

