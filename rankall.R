rankall <- function(outcome, num = "best"){
    inputdata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    outvar <- switch(outcome,
                     "heart attack" = 11,
                     "heart failure" = 17,
                     "pneumonia" = 23,
                     stop("invalid outcome"))
    outdata <- inputdata %>% select(2,7,outvar) #%>% na.omit()
    names(outdata)[3] <- "Rate"
    
    outdata <- outdata %>% arrange(outdata$Rate, outdata$Hospital.Name)
    outdata <- split(outdata, as.factor(outdata$State))
    
    #outdata <- lapply(outdata, function(x) x %>% arrange(x$Rate, x$Hospital.Name))
    
    rank <- switch(num,
                   "best" = 1,
                   "worst" = nrow(outdata),
                   num)
    
    #df <- lapply(outdata, do.call(rbind,[rank,c(1:2)]))
    #print(df)
}