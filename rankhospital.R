rankhospital <- function(state,rate,rank.input) {
  x <- outcome # outcome is data from outcome-of-care-measures.csv file
  x[,c(2,7,11,17,23)] <- outcome[,c(2,7,11,17,23)]
  raw.rows <- nrow(x)
  # print(c("raw.rows",raw.rows))
  badrows <- NULL
  for (k in 1:raw.rows){
    if ((x[k,11] == "Not Available") || (x[k,17] == "Not Available") || (x[k,23] == "Not Available")) {badrows <- rbind(badrows,k)}
  }
  # print(c("badrows",nrow(badrows)))
  # print(badrows)
  x <- x[-c(badrows),]
 # Next will ensure all columns of interest are numeric
 x[,2] <- as.character(x[,2])
 x[,7] <- as.character(x[,7])
 x[,11] <- as.numeric(x[,11])
  x[,17] <- as.numeric(x[,17])
  x[,23] <- as.numeric(x[,23])
  # Next will remove all rows not from state
  revised.rows <- nrow(x)
  # print(c("revised.rows",revised.rows))
  badrows.revised <- NULL
  for (w in 1:revised.rows){
    if ((x[w,7] != state)) {badrows.revised <- rbind(badrows.revised,w)}
  }
  x <- x[-c(badrows.revised),] # All rows not from selected state removed
  
  # print(typeof(x[,17]))
  # print(typeof(x[,23]))
  # print(c(typeof(x[,2]),typeof(x[,7]),typeof(x[,11]),typeof(x[,17]),typeof(x[,23])))

 # Below, will end function if input rank is greater than the number of hospitals in that state
 # Function will end of if no valid data from state

 # Function will end of if not enough valid data from state for requested rank
# Below, set value of rank, or end function
if (rank.input=="best") {rank <- 1}
if (rank.input=="worst") {rank <- nrow(x)}
if (is.numeric(rank.input)) {rank <- rank.input}
if (rank > nrow(x)) {stop("NA")}
if(is.na(match(state, x[,7]))) {
  stop("Error in best(",dQuote(state),",",dQuote(rate),") : invalid state")
}
if(!(rate == "heart attack" || rate == "heart failure" || rate == "pneumonia" )) {
  stop("Error in best(",dQuote(state),",",dQuote(rate),") : invalid outcome")
}

# Set column to be sorted
if(rate == "heart attack") {rate.col <- 11}
if(rate == "heart failure") {rate.col <- 17}
if(rate == "pneumonia") {rate.col <- 23}
# Rename sort column and hospital column
# names(x)[rate.col] <- "Rate" 
# Hospitals in state ranked by desired rate and by hospital name
# print(x[1:5,c(2,rate.col)])
# print("pre-sorted")
 x.sorted <-  x[order(x[,rate.col],x[,2]),]
# print(c("sorted for rank",rank))
# print(x.sorted[1:5,c(2,rate.col)])
print(x.sorted[rank,2]) # Ranked hospital, assuming valid rank and available data
} # End of function
