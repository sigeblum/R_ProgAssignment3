rankhospital <- function(state, outcome, num = "best"){
    inputdata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    # check which outcome is selected
    outvar <- switch(outcome,
                     "heart attack" = 11,
                     "heart failure" = 17,
                     "pneumonia" = 23,
                     stop("invalid outcome"))
    outdata <- inputdata %>% select(2,7,outvar) %>% na.omit()
    # Error if invalid stat is selected
    if(!is.element(state, outdata$State)){stop("invalid state")}
    outdata <- outdata %>% filter(State == state)
    # sort by culumn 3
    names(outdata)[3] <- "Rate"
    
    outdata <- outdata %>% arrange(outdata$Rate, outdata$Hospital.Name)
    
    rank <- switch(num,
                   "best" = 1,
                   "worst" = nrow(outdata))
    if(is.null(rank)){rank <- num}
    if(rank > nrow(outdata)){return(NA)}
    print(outdata$Hospital.Name[rank])
    
}