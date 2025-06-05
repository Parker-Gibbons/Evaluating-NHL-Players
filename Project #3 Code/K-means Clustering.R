#Clustering code for Hockey Project

#Import data
forwards <- read.csv("~/Downloads/For.csv") 
defense <- read.csv("~/Downloads/Def.csv") 
goalies <- read.csv("~/Downloads/Goa.csv") 

#K-means clustering
# Loading package 
library(ClusterR) 
library(cluster) 
library(factoextra)


#Create subset of data for clustering
subsetclust1 <- forwards[, c("icetime", "shifts", "gameScore", "I_F_xGoals", "Assists","I_F_points","I_F_goals","I_F_rebounds","I_F_hits","I_F_takeaways","I_F_giveaways","lowDangerGoaAbvExp","medDangerGoaAbvExp","highDangerGoaAbvExp","faceoffsWon","penalityMinutes","penalityMinutesDrawn","shotsBlockedByPlayer")]
subsetclust2 <- defense[, c("icetime", "shifts", "gameScore", "I_F_xGoals", "Assists","I_F_points","I_F_goals","I_F_rebounds","I_F_hits","I_F_takeaways","I_F_giveaways","lowDangerGoaAbvExp","medDangerGoaAbvExp","highDangerGoaAbvExp","faceoffsWon","penalityMinutes","penalityMinutesDrawn","shotsBlockedByPlayer")]
subsetclust3 <- goalies[, c("icetime", "GoalsSavedAbvExp", "xGoals", "goals", "ReboundsAbvExp","XPlayConAbvExp","lowDangerGoalAbvExp","medDangerGoalAbvExp","highDangerGoalAbvExp","penalityMinutes")]


#Elbow Method for finding the optimal number of clusters
fviz_nbclust(
  subsetclust3,
  kmeans,
  method = "wss",
  k.max = 25,
  verbose = FALSE
)



# Fitting K-Means clustering Model  
# to training dataset 
set.seed(240) # Setting seed 
kmeans.re <- kmeans(subsetclust1, centers = 4, nstart = 20) 
kmeans.re 

kmeans.re <- kmeans(subsetclust2, centers = 4, nstart = 20) 
kmeans.re 

kmeans.re <- kmeans(subsetclust3, centers = 3, nstart = 20) 
kmeans.re 

#Adding cluster numbers to initial dataset
DataFor <- forwards
DataFor$subsetclust1 <- kmeans.re$cluster

DataDef <- defense
DataDef$subsetclust2 <- kmeans.re$cluster

DataGoa <- goalies
DataGoa$subsetclust3 <- kmeans.re$cluster

# Cluster identification for  
# each observation 
kmeans.re$cluster 


## Visualizing clusters 
y_kmeans <- kmeans.re$cluster 
clusplot(subsetclust1[, c("X23.payroll", "City.Population")], 
         y_kmeans, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels = 4, 
         plotchar = FALSE, 
         span = TRUE, 
         main = paste("Cluster Groupings"), 
         xlab = 'X23.payroll', 
         ylab = 'City.Population') 

#Export data with cluster number attached to each observation
write.csv(DataFor, file = "CLUSTEREDfor.csv")
write.csv(DataDef, file = "CLUSTEREDdef.csv")
write.csv(DataGoa, file = "CLUSTEREDgoa.csv")
