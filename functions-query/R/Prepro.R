##
###
#changing the outliers to the mean of each column 
parkinsons$RPDE <- replace(parkinsons$RPDE , parkinsons$RPDE>0.86, mean(parkinsons$RPDE))
parkinsons$Shimmer <- replace(parkinsons$Shimmer , parkinsons$Shimmer>0.2, mean(parkinsons$Shimmer))
parkinsons$Jitter.PPQ5 <- replace(parkinsons$Jitter.PPQ5 , parkinsons$Jitter.PPQ5>0.03, mean(parkinsons$Jitter.PPQ5))
parkinsons$Jitter.DDP <- replace(parkinsons$Jitter.DDP , parkinsons$Jitter.DDP>0.10, mean(parkinsons$Jitter.DDP))
parkinsons$Jitter.RAP <- replace(parkinsons$Jitter.RAP , parkinsons$Jitter.RAP>0.03, mean(parkinsons$Jitter.RAP))
parkinsons$Jitter.Abs. <- replace(parkinsons$Jitter.Abs. , parkinsons$Jitter.Abs.>0.00032, mean(parkinsons$Jitter.Abs.))
parkinsons$Shimmer.APQ3 <- replace(parkinsons$Shimmer.APQ3 , parkinsons$Shimmer.APQ3>0.135, mean(parkinsons$Shimmer.APQ3))
parkinsons$Shimmer.APQ11 <- replace(parkinsons$Shimmer.APQ11 , parkinsons$Shimmer.APQ11>0.20, mean(parkinsons$Shimmer.APQ11))
parkinsons$Shimmer.DDA <- replace(parkinsons$Shimmer.DDA , parkinsons$Shimmer.DDA>0.38, mean(parkinsons$Shimmer.DDA))

#normalising the matrix
parkinsons_prepro[,3] <- scale(parkinsons_prepro[,3])
parkinsons_prepro[,5:23] <- scale(parkinsons_prepro[,5:23])

#delete the first column that is an index 
parkinsons <- parkinsons[,-1]