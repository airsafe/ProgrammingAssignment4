best <- function(state,rate) {
  # First reduce input to a five-column list with key information
  # testmat <- NULL
  x <- outcome # outcome is data from outcome-of-care-measures.csv file
  x[,c(2,7,11,17,23)] <- outcome[,c(2,7,11,17,23)]
  # print(nrow(x))
  # print(nrow(outcome))
  # x <- na.omit(x)
  # print(nrow(x))
  #print(is.data.frame(x))
  # x <- na.omit(outcome) # Get rid of rows with NA numeric values
  # print(nrow(x))
  # print(nrow(outcome))
  # Following lines plus loop will delete rows with "Not Available" values
  raw.rows <- nrow(x)
  # print(c("raw.rows",raw.rows))
  badrows <- NULL
  for (k in 1:raw.rows){
    if ((x[k,11] == "Not Available") || (x[k,17] == "Not Available") || (x[k,23] == "Not Available")) {badrows <- rbind(badrows,k)}
  }
  # print(c("badrows",nrow(badrows)))
  # print(badrows)
  x <- x[-c(badrows),]
  # print(c("revised rows",nrow(x)))
  # print(typeof(x[,2]))
  # print(typeof(x[,7]))
  # print(typeof(x[,11]))
  # print(typeof(x[,17]))
  # print(typeof(x[,23]))
  # print(length(x[,2]))
  # print(length(x[,7]))
  # print(length(x[,11]))
  # print(length(x[,17]))
  # print(length(x[,23]))
  # typeof(x[,11])
  # typeof(x[,17])
  # typeof(x[,23])
  # x[,2] <- as.character(x[,2])
  # print(x[1:100,2])
  # print(x[1:100,7])
  x[,2] <- as.character(x[,2])
  x[,7] <- as.character(x[,7])
  x[,11] <- as.numeric(x[,11])
  #  print(typeof(x[,11]))
  x[,17] <- as.numeric(x[,17])
  x[,23] <- as.numeric(x[,23])
  # print(typeof(x[,11]))
  # print(typeof(x[,17]))
  # print(typeof(x[,23]))
  # print(c(typeof(x[,2]),typeof(x[,7]),typeof(x[,11]),typeof(x[,17]),typeof(x[,23])))
  # pneumonia.vector <- as.numeric(x[,23])
  # testmat <- cbind(hosptital.vector,state.vector,heart.attack.vector,heart.failure.vector,pneumonia.vector)
  # testmat <- data.frame(hosptital.vector,state.vector,heart.attack.vector,heart.failure.vector,pneumonia.vector)
  # testmat <- data.frame(x[,2],x[7],x[11],x[17],x[23])
  # colnames(testmat) <- c("Hospital","State","heart attack","heart failure","pneumonia")
  # testmat <- na.omit(testmat)
  # testmat <- testmat
  #testmat$Hospital <- as.character(testmat$Hospital) # Ensure first column is of type character
  # testmat$"heart attack" <- as.numeric(testmat$"heart attack") # Ensure column is of type numeric
  # testmat$"heart failure" <- as.numeric(testmat$"heart failure") # Ensure column is of type numeric
  # testmat$pneumonia <- as.numeric(testmat$pneumonia) # Ensure column is of type numeric
  # Determine if state is correct two-character configuration
  if(is.na(match(state, x[,7]))) {
    stop("Error in best(",dQuote(state),",",dQuote(rate),") : invalid state")
  }
  if(!(rate == "heart attack" || rate == "heart failure" || rate == "pneumonia" )) {
    stop("Error in best(",dQuote(state),",",dQuote(rate),") : invalid outcome")
  }
  if((!is.na(match(state, x[,7])) && (rate == "heart attack" || rate == "heart failure" || rate == "pneumonia" ))) {
    # print("Input is valid and there are hospitals in this state to compare")
    # Run a loop to extract minimum value for state
    hosp.rows <- nrow(x)
    minrate <- 99999
    rate.col <- 99999
    top.hosp <- "Zzzzz"
    if(rate == "heart attack") {rate.col <- 11}
    if(rate == "heart failure") {rate.col <- 17}
    if(rate == "pneumonia") {rate.col <- 23}
    n <- 1
    for (n in 1:hosp.rows){
      #   if(x[n,7]==state){ 
      # print(c(x[n,2],x[n,rate.col],typeof(x[n,rate.col])))
      #  }
      if((x[n,7]==state) && (x[n,rate.col] <= minrate)){
        last.minrate <- minrate
        minrate <- x[n,rate.col] # minrate update
        # print(c(minrate, last.minrate, "minrate and last.minrate"))
        last.top.hosp <- top.hosp
        top.hosp <- x[n,2] # top.hosp update
        # print(c(top.hosp, last.top.hosp,"top.hosp and last.top.hosp"))
        if((last.minrate == minrate) && (last.top.hosp < top.hosp)) {top.hosp <- last.top.hosp} # tiebreaker
        #  print(c(top.hosp,minrate,"##"))
      }
      
    } # end of loop for case of valid state with at least one hospital
    # print(c("Best hospital in ", state,"for", rate, "is", top.hosp, "with rate of", minrate))
    print(top.hosp)
  # Note: took out a bracket 

  } # End of determine if state is correct
} # End of function
