# @param {Character Vector} location of the CSV file
# @param {Character Vector} the name of the pollutant to search for
# @param {Integer Vector} the monitor id numbers to be used
# @return {Number} the mean of the pollutant across all monitors
pollutantMean <- function (directory, pollutantToFind, ids) {
    pollutionFrame <- data.frame()
    pollutionFiles <- list.files(directory, full.name=TRUE)
    for (i in ids) {
        pollutionFrame <- rbind(pollutionFrame, read.csv(pollutionFiles[i]))    
    }
    
    mean(pollutionFrame$sulfate, na.rm=TRUE)
}
