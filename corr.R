corr <- function (directory, threshold = 0) {
    correlationFiles <- list.files(directory, full.name=TRUE)
    acceptedVector = c()
    s <- vector()
    i = 1
    for (i in 1:length(correlationFiles)) {
        # for each file, return the rows that have x > threshold observed cases on both vars.
        currentFile <- read.csv(correlationFiles[i])
        names(currentFile)
        
        # get the complete cases from the current file
        completeCases <- currentFile[complete.cases(currentFile), ]
        numberOfCompleteCases = nrow(completeCases)
        if (numberOfCompleteCases > threshold) {
            currentCorrelation <- cor(completeCases$sulfate, completeCases$nitrate)
            acceptedVector[i - 1] = currentCorrelation
            
        }
    }
    acceptedVector
}
