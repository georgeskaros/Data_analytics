#install.packages("cluster")
#install.packages("factoextra")
#install.packages("NbClust")
library(factoextra)
library(cluster)
library(NbClust)
#excluding the target column
parkinsons_clust<-as.vector(parkinsons_ARM)

#tarkget column

parkinsons_score_col <- as.vector(parkinsons_ARM[,4:5])
parkinsons_score_col_M <- parkinsons[,4]
parkinsons_score_col_T <- as.data.frame(parkinsons[,5])

parkinsons_clust<- parkinsons[,-(4:5)]
parkinsons_clust_M<- parkinsons[,-4]
parkinsons_clust_T<- parkinsons[,-5]


#making bins out of the two target columns 
parkinsons_score_col_M <-cut(parkinsons$motor_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))
parkinsons_score_col_T <-cut(parkinsons$total_UPDRS, 3 , include.lowest = TRUE, labels=c("1","2","3"))


#ways to determine K in kmeans
        
        
        # Silhouette method
        fviz_nbclust(parkinsons_clust, kmeans, method = "silhouette")+
          labs(subtitle = "Silhouette method")
        
        
        NbClust(data = parkinsons_clust, diss = NULL, distance = "euclidean",
                min.nc = 2, max.nc = 15, method = "kmeans")
        
  
#kmeans models for motor and total
set.seed(20)
clusters_M <- kmeans(parkinsons_clust_M,3)
clusters_T <- kmeans(parkinsons_clust_T,3)


print(clusters_M)
print(clusters_T)

#clusters
table(clusters_T$cluster)
table(clusters_M$cluster)

#cluster labeling
 table(parkinsons_score_col_M, clusters_M$cluster)/nrow(parkinsons_clust)
 table(parkinsons_score_col_T, clusters_T$cluster)/nrow(parkinsons_clust)
 
















