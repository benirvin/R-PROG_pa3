best <- function(state,outcome) 
{
    mind <- vector()
    if (!state %in% state.abb)
        stop("invalid state")
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
        stop("invalid outcome")
    if (outcome=="heart attack") dfcol <-11
        else if (outcome=="heart failure") dfcol <-17
            else dfcol <-23
    minrate <- min(outcomes[which(outcomes$State==state),dfcol],na.rm = TRUE)
    j <- 0
    for (i in outcomes[which(outcomes$State==state),dfcol]) {
        j <- j + 1
        if (is.na(i)) next
        if (i==minrate) {
            hosp <- outcomes[which(outcomes$State==state),2][j]
            mind <- append(mind,hosp) 
        }
    }
    print(length(mind))
    min(mind)
    
}