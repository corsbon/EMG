#This code is modified from Vini's Matlab code for the Thexton Run's test for determining threshold
#From Thexton 1996: A randomisation method for discriminating between signal and noise in recordings of rhythmic electromyographic activity
#The code currently (as of 20160302) performs 5 iterations of the runs and uses a weighted average threshold.
# "CHECKED" means that I have confirmed that the code does as stated, usually via some sort of logical operator 

#Dependency = randtests

ThextonRuns2 <- function(y, num_increments, ChanStart, StartThex, EndThex, numRepeats, normalize = TRUE, reset = TRUE){
  #y is the filtered, rectified, RMS data
  #num_increments is the number of threshold increments to take
  #This assumeds that the data are from ProCapture, which has Index and Time
  #ChanStart is the column number on which the channels start (all other variables should be lower numbers)
  #for the first 2 variables 
  
  #downsample the data to 10ms intervals, similar to Thexton 1996 data
  #i <- 0
  #down_samp <- Hz * downsamp_interval_in_seconds
  #x <- y[1:(i + down_samp)==(i+down_samp), ChanStart:ncol(y)]
    if ((ChanStart - 1) > 0){
  index <- y[, 1:(ChanStart-1)]
    }
  x <- y[StartThex:EndThex, ChanStart:ncol(y)]
  #Identify minimum to maximum (i.e., range) for each channel
  emg_max <- data.frame(lapply(x, max))
  emg_min <- data.frame(lapply(x, min))
  #CHECKED
  #emg_min[1] == min(x[, 1])
  #emg_max[1] == max(x[, 1])
  
  #Create exmpty data.frame to receive equally spaced thresholds from minimum to maximum for each channel
  thresholds <- data.frame(matrix(nrow = num_increments, ncol = ncol(x)))
  
  #Create a place for the row identity of the threshold for each channel for each iteration
threshold_ID <- data.frame(matrix(nrow = numRepeats, ncol = ncol(x)))
  
  for (i in 1:ncol(x)){
    
    #create a vector for the incremented thresholds 
  thresholds[, i] <- transform(seq(emg_min[, i], emg_max[, i], length = num_increments))
    }
  #CHECKED
  #max(thresholds[, 1]) == emg_max[1]
  #min(thresholds[, 1]) == emg_min[1]
    
  pb <- txtProgressBar(min = 1, max = numRepeats, style = 3)
  
  for (iii in 1:numRepeats){
  #Create randomized data
  xr <- data.frame(lapply(x, sample, size = nrow(x), replace = FALSE))

  #randomized data were checked using == to return TRUE. Column 4 returned false
  #when initially writing this code with 2015-12-01_11-33_Evt69, but the means
  #were the same to 8 decimal places
  
  numruns_o <- data.frame(matrix(nrow = num_increments, ncol = ncol(x)))
  numruns_r <- numruns_o
  
  #Calculate number of runs at each threshold for ordered data
  for (i in 1:ncol(x)){
    for (ii in 1:nrow(thresholds)){
    test <- runs.test(x[[i]], threshold = thresholds[ii, i])
    numruns_o[ii, i] <- test$runs
    }
  }
  
  #Calculate number of runs at each threshold for randomized data
  for (i in 1:ncol(xr)){
    for (ii in 1:nrow(thresholds)){
      test <- runs.test(xr[[i]], threshold = thresholds[ii, i])
      numruns_r[ii, i] <- test$runs
    }
  }
  

  
  #Calculate the threshold value with the maximum distance between the two vectors
  numruns_c <- numruns_r - numruns_o
  
  #Fill the threshold ID for each column 
  for (i in 1:ncol(x)){
    max_runs <- which(numruns_c[[i]] == max(numruns_c[[i]])) 
    threshold_ID[iii, i] <- max_runs[1]
  }
  
  setTxtProgressBar(pb, iii)
  
  }
  

  
  #make a data.frame of the weighted average thresholds (average taken from multiple simulations)
  the_threshold <- data.frame(matrix(nrow = 1, ncol = ncol(x)))
  for (i in 1:ncol(x)){
    the_threshold[, i] <- (mean(thresholds[threshold_ID[, i], i])) 
  }    

  
  #replace values less than the threshold with the threshold
  Thex <- y
  
  for (i in ChanStart:ncol(Thex)){
    Thex[i][Thex[i] < the_threshold[1, (i-(ChanStart - 1))]] <- the_threshold[1, (i-(ChanStart - 1))]
  }
  
  #add back the time and index vectors
  Thex_complete <- data.frame(Thex)
  Thex_complete_n <- Thex_complete
  
  if (reset == TRUE){
  for (i in ChanStart:ncol(Thex_complete)){
    Thex_complete_n[,i] <- Thex_complete_n[,i]-min(Thex_complete_n[,i])
  }
  }
  
  if (normalize == TRUE){
  #normalize the data
  for (i in ChanStart:ncol(Thex_complete)){
    Thex_complete_n[,i] <- Thex_complete_n[,i]/max(Thex_complete_n[,i])
  }
  }
  return(Thex_complete_n)
  }

