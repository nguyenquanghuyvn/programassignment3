# Ranking the hospital in state
rankhospital <- function(state, outcome, rank) {
        outcomedata <- read.csv("hospitaldata/outcome-of-care-measures.csv", colClasses ="character")
        decease <- data.frame(a = c(1, 2, 3), b = c("heart attack", 'heart failure', "pneumonia"))
        if(any(any(state == outcomedata[, 7]), any(outcome == decease[, 2]))) {
            if(any(outcome == decease[, 2])) { 
                if(any(state == outcomedata[, 7])) { a <- c("go ahead")} else {
                    stop("Invalid state")} 
                } else {stop ("Invalid outcome")}
            } else {stop("Invalid outcome and state")}
        #Type function here
        ha <- as.numeric(outcomedata[, 11])
        hf <- as.numeric(outcomedata[, 17])
        pn <- as.numeric(outcomedata[, 23])
        bha <- cbind( outcomedata[,c(1,2,7)],ha)
        good  <- complete.cases(bha)
        bnha <- bha[good, ]
        sha <- split(bnha,bnha[[3]])
        bhf <- cbind( outcomedata[,c(1,2,7)],hf)
        good  <- complete.cases(bhf)
        bnhf <- bhf[good, ]
        shf <- split(bnhf,bnhf[[3]])
        bpn <- cbind( outcomedata[,c(1,2,7)],pn)
        good  <- complete.cases(bpn)
        bnpn <- bpn[good, ]
        spn <- split(bnpn,bnpn[[3]])
        datstate_orderha <- lapply(sha, function(x) x[order(x$ha, x$Hospital.Name), ])
        resultha <- datstate_orderha[[state]]
        datstate_orderhf <- lapply(shf, function(x) x[order(x$hf, x$Hospital.Name), ])
        resulthf <- datstate_orderhf[[state]]
        datstate_orderpn <- lapply(spn, function(x) x[order(x$pn, x$Hospital.Name), ])
        resultpn <- datstate_orderpn[[state]]
        ## If for best and worst
        if (rank == "best") {
        rank1 <- 1
        } else { rank1 <- rank}
         ##Writing code for ranking hospital by State
        if (outcome == "heart attack") {
        if (rank == "worst") {
        rank1 <- nrow(resultha)
        resultha[rank1, "Hospital.Name"]
        } else { resultha[rank1, "Hospital.Name"]}  
        }
        else { 
            if (outcome == "heart failure") {
             if ( rank == "worst") {
             rank1 <- nrow(resulthf)
             resulthf[rank1, "Hospital.Name"]
            } else { resulthf[rank1, "Hospital.Name"]}
             } else {
             if ( rank == "worst") {
             rank1 <- nrow(resultpn)
             resultpn[rank1, "Hospital.Name"]
            resultpn[rank1, "Hospital.Name"]
            } else {resultpn[rank1, "Hospital.Name"]}
          }
     }
 }