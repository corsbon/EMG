#Note that this formula comes from Neuromechanics of Human Movement, 4th Ed by Roger Enoka, pg 202
#This RMS code also decreases the data size to match the frame rate of XROMM by taking the RMS for the 
#entire window for which there is XROMM data 
RMSbon_5 <- function(x, fenster, fps, Hz){
  ###
  fenster2 <- Hz/(1000/fenster)
  rms_data <- x[1:(nrow(x)*(1000/fenster)/Hz), ]
  pb <- txtProgressBar(min = 3, max = ncol(x), style = 3)
  
  #Note that it starts at column 3 because the first two outputs from ProCapture are the Index and Time
  for(i in 3:length(x)){
        
    #Define the frames where a new window begins
    v <- c(1:nrow(x))
    v <- v[seq(1, length(v), fenster2)]
    
    for(ii in 1:length(v)){
      #Take all values within the window
       rms_fenster <- x[(v[ii]):(v[ii] + (fenster2 - 1)), i]
       #Take the root mean squared of those values and place them in the current rms_data slot
       rms_data[(v[ii] + (fenster2 - 1))/fenster2, i] <- sqrt(sum(rms_fenster^2) / fenster2)

    }
    setTxtProgressBar(pb, i)
    
  }
  for(i in 1:2){
    for(ii in 1:length(v)){
      rms_data[(v[ii] + (fenster2 - 1))/fenster2, i] <- x[(v[ii]), i]
    }
  }
  return(rms_data)
}

