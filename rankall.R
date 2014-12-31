rankall <- function(rate,rank.input = "best") {
  if(!(rate == "heart attack" || rate == "heart failure" || rate == "pneumonia" )) {
    stop("invalid outcome")
  }
  
  # Set column to be sorted
  x <- outcome
 # print(rate)
  if(rate == "heart attack") {rate.col <- 11}
  if(rate == "heart failure") {rate.col <- 17}
  if(rate == "pneumonia") {rate.col <- 23}
 # print(c("rate.co.",rate.col,"rate",rate))

  raw.rows <- nrow(x)
  # print(c("raw.rows",raw.rows))
  x.execute <- NULL
  badrows <- NULL
  for (k in 1:raw.rows){
    if ((x[k,rate.col] == "Not Available")||(is.na(x[k,rate.col]))) {badrows <- rbind(badrows,k)}
  }
  # print(c("badrows",nrow(badrows)))
  # print(badrows)
  x <- x[-c(badrows),]
  # Next will ensure all columns of interest are numeric
  x[,2] <- as.character(x[,2])
  x[,7] <- as.character(x[,7])
  x[,rate.col] <- as.numeric(x[,rate.col])

 # print(x.execute[1:4,c(2,rate.col)])
  # Below will create a data frame with row names = state names, col1 = state names
  #  and default hosptial names = NA
  rank.matrix <- NULL

  state.vector <- unique(x[,7])
 #print(state.vector)
  state.sorted <- sort(state.vector)
  state.default <- rep("NA",each=length(state.sorted))
  rank.matrix <- data.frame(hospital=state.default,state=state.sorted, row.names=state.sorted)
  rank.matrix[,1] <- as.character(rank.matrix[,1])
 rank.matrix[,2] <- as.character(rank.matrix[,2])
 # print(head(rank.matrix))
  # Next loop will run for all states in the vector
  numstates <- length(state.sorted)
 # print(numstates)

  s <- 1
 # numstates
  for (s in 1:numstates){
    x.sorted <- NULL
    x.execute <- x

  state <- state.sorted[s]
  # print(state)
  # print(state.sorted)
  # Next will remove all rows not from state
  revised.rows <- nrow(x.execute)
  # print(c("revised.rows",revised.rows))

  w <- 1
 # print(state)
 badrows.revised <- NULL
  for (w in 1:revised.rows){
    if ((x.execute[w,7] != state)) {badrows.revised <- rbind(badrows.revised,w)}
  }
  x.execute <- x.execute[-c(badrows.revised),] # All rows not from selected state removed
  

  # Function will end of if not enough valid data from state for requested rank

  # Below, set value of rank
 rank <- rank.input
  if (rank.input=="best") {rank <- 1}
  if (rank.input=="worst") {rank <- nrow(x.execute)}
 
# print(typeof(rank.input))
# print(rank)
# print(c(state,nrow(x.execute),rank))
  # if (rank > nrow(x)) {stop("NA")} used in other parts of assignment 3

  x.sorted <- x.execute[order(x.execute[,rate.col],x.execute[,2]),]
  # print(c(s,x.sorted[rank,2],nrow(x.sorted)))
  if (nrow(x.sorted) >= rank){
    rank.matrix[s,1] <-  x.sorted[rank,2]  # x.execute[rank,2]
  # Ranked hospital, assuming valid rank and available data
  }
# if(state=="WY"){print(x.sorted[,c(2,rate.col)])}
} # End of massive loop for each state with index 's'

return(rank.matrix)
} # End of function
