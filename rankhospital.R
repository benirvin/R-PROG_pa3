rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    st.terr <- c(state.abb,"DC","VI","GU","PR")
    if (!state %in% st.terr)
        stop("invalid state")
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
        stop("invalid outcome")
    if (outcome=="heart attack") dfcol <-11
        else if (outcome=="heart failure") dfcol <-17
            else dfcol <-23
    olist<-order(outcomes[outcomes$State==state,dfcol],
                 outcomes[outcomes$State==state,2],na.last=NA)
    clean_oc <-na.omit(outcomes[outcomes$State==state,dfcol])
    if (num=="best") num <-1
        else if(num=="worst") num <- length(clean_oc)
            else if(is.numeric(num)) {}
                else stop("invalid rank")
    outcomes[outcomes$State==state,2][olist[num]]

}