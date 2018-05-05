#Note that this formula comes from Neuromechanics of Human Movement, 4th Ed by Roger Enoka, pg 202
RMSbon <- function(x, fenster){
  ###
  rms_data <- x
  ende_1 <- length(x)
  pb <- txtProgressBar(min = 3, max = ende_1, style = 3)
  
  for(i in 3:length(x)){
    
    buffer1 <- x[1, i]
    buffer2 <- x[nrow(x), i]
    buffer1 <- rep(buffer1, fenster)
    buffer2 <- rep(buffer2, fenster)
    buffed <- append(x[, i], buffer1, after = 0)
    buffed <- append(buffed, buffer2)
    buffed <- transform(buffed)
    ende_2 <- nrow(x) + fenster
        
    #pb2 <- txtProgressBar(min = fenster, max = ende_2, style = 3)
    #for(ii in nrow(buffer_start):(nrow(buffer_start)*2)){
    for(ii in fenster:ende_2){
     
       rms_fenster <- c(buffed[(ii - (0.5 * (fenster))):(ii + (0.5 * (fenster - 1))), ])
       rms_data[ii, i] <- sqrt(sum(c(rms_fenster)^2) / fenster)
     
      #filter the data (magic happens here)
      #setTxtProgressBar(pb2, (ii - fenster))
    }
    setTxtProgressBar(pb, i)
  }
  rms_data_final <- rms_data[(fenster + 1):ende_2, ]
  return(rms_data_final)
}
