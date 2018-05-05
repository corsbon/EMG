#This code is for making a wide version of the long feeding kinematics 
#dataset from Oreo and Youke.

#If working on midway, drop "/Volumes" from the directory references below

#home computer

orsbutter_high <- function(x, poles, f, g){
filter <- x
  # Loop through all UniqueIDs
for(i in 1:length(x)){
  # Make the filter
  
  #This code used to be plugged in manually; it is now in the arguments of the code
  #number of poles
  #poles <- 4
  #cutoff frequency
  #f <-15
  #Macacamulatta average chew frequency = 2.1 or 1/0.476, per Ross et al 2009
  #recording frequency
  #g <- 200
  
  #filter
  bf <- butter(poles,f/(g/2),"high","z")
  
  #The following applies the filter to each column of feeding 
  #kinematics and then puts them back into the dlist_filter dataset
  #Filter data
  #add a new row to put in buffer data (to prevent tailing)
  buffer1 <- x[1,i]
  buffer2 <-x[nrow(x),i]
  buffer1 <- rep(buffer1,nrow(x))
  buffer2 <- rep(buffer2,nrow(x))
  buffed <-append(x[,i],buffer1,after=0)
  buffed <-append(buffed,buffer2)
  buffed <- transform(buffed)
  
  #filter the data (magic happens here)
  buffed_filter <- transform(filtfilt(bf, buffed$X_data))
  
  #take the buffers off of the data
  buffed_filter2 <- buffed_filter[(length(buffer1)+1):(length(buffed_filter$X_data)-length(buffer2)),]
  
  # Put filtered values in new data.frame
  filter[, i] <- buffed_filter2
}
return(filter)
}