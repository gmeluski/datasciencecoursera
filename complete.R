complete <- function (directory, ids) {
    finalFrame <- data.frame()
    reportFiles <- list.files(directory, full.name=TRUE)
    for (i in ids) {
        # get the number of obeserved cases, sum the ones up 
        # complete cases have both sulfate and nitrate columns
        reportFrame <- data.frame()
        currentFile = reportFiles[i]
        reportFrame <- rbind(reportFrame, read.csv(currentFile))    
        good <- reportFrame[complete.cases(reportFrame),]
        finalFrame <- rbind(finalFrame, c(i, nrow(good)))
    }
    colnames(finalFrame) <- c('id', 'nobs')
    print(finalFrame)
}
