rankall <- function(outcome,num="best"){
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    states <- vector("character",54)
    hnames <- vector("character",54)

    if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
        stop("invalid outcome")
    if (outcome=="heart attack") dfcol <-11
        else if (outcome=="heart failure") dfcol <-17
            else dfcol <-23
    st.terr <- c(state.abb,"DC","VI","GU","PR")
    j = 0
    if (num=="best") idx <-1
        else if(is.numeric(num)) idx <- num
            else if(num=="worst") {} #assign state-by-state
                else stop("invalid rank")    
    for (state in st.terr){
        j = j+1
        olist<-order(outcomes[outcomes$State==state,dfcol],
                     outcomes[outcomes$State==state,2],na.last=NA)
        clean_oc <-na.omit(outcomes[outcomes$State==state,dfcol])
        if(num=="worst") idx <- length(clean_oc)
        hnames[j] <- outcomes[outcomes$State==state,2][olist[idx]]
        states[j]<-state
    }
    resdf <- data.frame(states,hnames)
    st_order <- order(resdf[,1])
    resdf[st_order,]
    
}