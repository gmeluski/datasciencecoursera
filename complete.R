complete <- function (directory, ids) {
    reportFrame <- data.frame()
    reportFiles <- list.files(directory, full.name=TRUE)
    for (i in ids) {
        # get the number of obeserved cases, sum the ones up 
        # complete cases have both sulfate and nitrate columns
        currentFile = reportFiles[i]
        reportFrame <- rbind(reportFrame, read.csv(currentFile))    
        good <- reportFrame[complete.cases(reportFrame),]
        print(nrow(good))
    }
}
