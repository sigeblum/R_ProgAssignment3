best <- function(state, outcome){
    inputdata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    # check which outcome is selected
    outvar <- switch(outcome,
                     "heart attack" = 11,
                     "heart failure" = 17,
                     "pneumonia" = 23,
                     stop("invalid outcome"))
    # select data according to outcome
    outdata <- inputdata %>% select(2,7,outvar) %>% na.omit()
    # Error if invalid stat is selected
    if(!is.element(state, outdata$State)){stop("invalid state")}
    outdata <- outdata %>% filter(State == state)
    # sort by culumn 3
    names(outdata)[3] <- "daystodeath"
    
    outdata <- outdata %>% arrange(outdata$daystodeath, outdata$Hospital.Name)
    print(outdata$Hospital.Name[1])
    
}