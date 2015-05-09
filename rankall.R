# create rankall function for call the hospitals satisfied rank from all States
rankall <- function(outcome, num = "best") {
## Read the data
    outcomedata <- read.csv("hospitaldata/outcome-of-care-measures.csv", colClasses ="character")
## Check the validity of outcome
    decease <- data.frame(a = c(1, 2, 3), b = c("heart attack", 'heart failure', "pneumonia"))
    if(all(outcome != decease[, 2])) {
            stop("Invalid outcome")} 
## The rankall main codes
    ha <- as.numeric(outcomedata[, 11])
    hf <- as.numeric(outcomedata[, 17])
    pn <- as.numeric(outcomedata[, 23])
    bha <- cbind( outcomedata[,c(1,2,7)],ha)
    good  <- complete.cases(bha)
    bnha <- bha[good, ]
    bhf <- cbind( outcomedata[,c(1,2,7)],hf)
    good  <- complete.cases(bhf)
    bnhf <- bhf[good, ]
    bpn <- cbind( outcomedata[,c(1,2,7)],pn)
    good  <- complete.cases(bpn)
    bnpn <- bpn[good, ]
    sha <- split(bnha,bnha[[3]])
    shf <- split(bnhf,bnhf[[3]])
    spn <- split(bnpn,bnpn[[3]])
    datorder_has <- lapply(sha, function(x) x[order(x$ha, x$Hospital.Name), ])
    datorder_hfs <- lapply(shf, function(x) x[order(x$hf, x$Hospital.Name), ])
    datorder_pns <- lapply(spn, function(x) x[order(x$pn, x$Hospital.Name), ])
## Rank by state
    orderank_ha <- lapply(datorder_has, function(x) cbind(x, ra = rank(x$ha, ties.method = "first")))
    orderank_hf <- lapply(datorder_hfs, function(x) cbind(x, ra= rank(x$hf, ties.method = "first")))
    orderank_pn <- lapply(datorder_pns, function(x) cbind(x, ra= rank(x$pn, ties.method = "first")))
## Create a data frame including ranking column:
    rankha <- do.call(rbind, orderank_ha)
    rankhf <- do.call(rbind, orderank_hf)
    rankpn <- do.call(rbind, orderank_pn)
## Change name of column
    colnames(rankha) <- c( "Number", "hospital", "state", "ha", "ra")
    colnames(rankhf) <- c( "Number", "hospital", "state", "hf", "ra")
    colnames(rankpn) <- c( "Number", "hospital", "state", "pn", "ra")
## Call function
if (num == "best") {
    num <- 1
} 
if (outcome == "heart attack") {
    if(num == "worst"){ 
        dat <- data.frame()
        for(i in 1:54) {
            num1 <- nrow(orderank_ha[[i]])
            t <- orderank_ha[[i]]
            colnames(t) <- c("Number", "hospital", "state", "ha","ra")
            dat1 <- t[t$ra == num1, c("hospital", "state")]
            dat <- rbind(dat, dat1)}
        dat
    } else {
        hospital<- as.character(sapply(orderank_ha, function(x) x[x$ra == num, c("Hospital.Name")]))
        state<- as.character(sapply(orderank_ha, function(x) x[x$ra == 1, c("State")]))
        result <- data.frame(hospital, state)
        for (i in 1:54) {
            if (result[i,1] == "character(0)") {
                result[i,1] <- "<NA>"
            }   
        }
        result
    }
} else {
if (outcome == "heart failure") {
    if(num == "worst"){ 
        dat <- data.frame()
        for(i in 1:54) {
            num <- nrow(orderank_hf[[i]])
            t <- orderank_hf[[i]]
            colnames(t) <- c("Number", "hospital", "state", "hf","ra")
            dat1 <- t[t$ra == num, c("hospital", "state")]
            dat <- rbind(dat, dat1)}
        dat
    } else {
        hospital<- as.character(sapply(orderank_hf, function(x) x[x$ra == num, c("Hospital.Name")]))
        state<- as.character(sapply(orderank_hf, function(x) x[x$ra == 1, c("State")]))
        result <- data.frame(hospital, state)
        for (i in 1:54) {
            if (result[i,1] == "character(0)") {
                result[i,1] <- "<NA>"
            }   
        }
        result
    }
} else {

    if(num == "worst"){ 
        dat <- data.frame()
        for(i in 1:54) {
            num <- nrow(orderank_pn[[i]])
            t <- orderank_pn[[i]]
            colnames(t) <- c("Number", "hospital", "state","pn", "ra")
            dat1 <- t[t$ra == num, c("hospital", "state")]
            dat <- rbind(dat, dat1)}
        dat
    } else {
        hospital<- as.character(sapply(orderank_pn, function(x) x[x$ra == num, c("Hospital.Name")]))
        state<- as.character(sapply(orderank_pn, function(x) x[x$ra == 1, c("State")]))
        result <- data.frame(hospital, state)
        for (i in 1:54) {
            if (result[i,1] == "character(0)") {
                result[i,1] <- "<NA>"
            }   
        }
        result
    }
} }
}
