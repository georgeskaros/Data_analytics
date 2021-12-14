#remove the target values from the table 
parkinsons_hier_T <- parkinsons[,-5]
parkinsons_hier_score_T <- parkinsons[,5]
parkinsons_hier_M <- parkinsons[,-4]
parkinsons_hier_score_M <- parkinsons[,4]

#find the distances of the table
d_T <- dist(parkinsons_hier_T, method = "euclidean")
d_M <- dist(parkinsons_hier_M, method = "euclidean")


#build a hierarchical clustering model
H_fit_T <- hclust(d_T, method = "ward.D")
H_fit_M <- hclust(d_M, method = "ward.D")

#plot the model in order to find the correct number of clusters
plot(H_fit_T)
plot(H_fit_M)
rect.hclust(H_fit_T, k=3, border="red")
rect.hclust(H_fit_M, k=3, border="red")
#cut the clusters 
groups_T <- cutree(H_fit_T, k=3)
groups_M <- cutree(H_fit_M, k=3)


#convert the target variable to 3 bins (equal to the number of clusters that we found) 
parkinsons_hier_score_M <-cut(parkinsons$motor_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))
parkinsons_hier_score_T <-cut(parkinsons$total_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))

#confusion matrices 
table(parkinsons_hier_score_M, groups_M)/nrow(parkinsons)
table(parkinsons_hier_score_T, groups_T)/nrow(parkinsons)




