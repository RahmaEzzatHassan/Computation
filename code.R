library(corrgram)
library(cluster)
library(dplyr)
library(ggplot2)
library(factoextra)
library(GGally)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(caTools)
library(class)
library(dbscan)

#load data
data <- read.csv("C:/Users/hamdy/Downloads/marketing_campaign.csv" , sep = "\t")
head(data)

str(data)

names(data)

#Data Cleaning
sum(is.na(data))
#handel missing values
data <- na.omit(data)
sum(is.na(data))

# Remove duplicate rows (all columns)
#data <- data %>% distinct()
# Remove duplicate rows
data <- data[!duplicated(data), ]

#Derive Age from Year_Birth
data$Age <- 2023 - data$Year_Birth
#extract the year as string from Dt_Customer then convert it to numeric
print(min(data$Dt_Customer))
print(max(data$Dt_Customer))
data$Customer_Year <- as.character(data$Dt_Customer)
data$Customer_Year <- substr(data$Customer_Year , start = 7 , stop=10)
data$Customer_Year <- as.numeric(data$Customer_Year)
head(data)
#calculate the seniority of customer from Customer_Year
data$seniority <- 2023 - data$Customer_Year

boxplot(data$Age, main = "customer Age", xlab = "Age",
        col = "green", horizontal = T, notch = T )

#convert Education to UG & PG categories
unique(data$Education)
data$Education[data$Education  == "2n Cycle"] = "UG"
data$Education[data$Education  == "Basic"] = "UG"
data$Education[data$Education  == "Graduation"] = "PG"
data$Education[data$Education  == "Master"] = "PG"
data$Education[data$Education  == "PhD"] = "PG"

ggplot(data = data, aes(Age, fill = Education)) + geom_histogram()

ggplot(data , aes(Age)) + geom_density()

#convert Material_Status to Single & Couple categories
unique(data$Marital_Status)
data$Marital_Status[data$Marital_Status == "Divorced"] = "Single"
data$Marital_Status[data$Marital_Status == "Absurd"] = "Single"
data$Marital_Status[data$Marital_Status == "YOLO"] = "Single"
data$Marital_Status[data$Marital_Status == "Widow"] = "Single"
data$Marital_Status[data$Marital_Status == "Together"] = "Couple"
data$Marital_Status[data$Marital_Status == "Married"] = "Couple"
data$Marital_Status[data$Marital_Status == "Alone"] = "Single"


#Calculate the total number of children 
data$Child <- data$Kidhome + data$Teenhome

ggplot(data = data,  aes(Child, fill = Marital_Status)) + geom_histogram(position = "dodge" )

ggplot(data , aes(Martial_Status , fill = Martial_Status)) + geom_histogram(stat = "conut")

#sum up the total expenses and total accepted campaign for each customers
data$Total_Expenses <- data$MntWines + data$MntFruits + data$MntMeatProducts +
  data$MntFishProducts + data$MntSweetProducts + data$MntGoldProds

data$Total_Acc_Cmp <- data$AcceptedCmp1 + data$AcceptedCmp2 + data$AcceptedCmp3 +
  data$AcceptedCmp4 + data$AcceptedCmp5 + data$Response

boxplot(data$Total_Expense, main = "Total Spent by the customer", xlab = "Total Expenses",
        col = "red", horizontal = T, notch = T )

hist(data$Total_Expenses , xlab = "Tatal Expenses" , col = "grey")

plot(data$Income , data$Total_Expenses , xlab = "Income" , ylab = "Spent" , col = "blue")

#Data Transformation
data$Education[data$Education  == "UG"] = 0
data$Education[data$Education  == "PG"] = 1
is.numeric(data$Education)
data$Education <- as.numeric(data$Education)
data$Marital_Status[data$Marital_Status == "Single"] = 0
data$Marital_Status[data$Marital_Status == "Couple"] = 1
data$Marital_Status <- as.numeric(data$Marital_Status)

head(data)

boxplot(data$Income, main = "customer Income", xlab = "Income",
        col = "blue", horizontal = T, notch = T )

ggplot(data , aes(as.factor(seniority) , Total_Expenses , fill = seniority)) + geom_boxplot(col = "black")

df <- data[,!names(data) %in% c ("ID" , "Year_Birth" , "Kidhome" , "Teenhome" ,
                                 "Dt_Customer" , "Recency" ,
                                 "AcceptedCmp3" , "AcceptedCmp4" , "AcceptedCmp5" ,
                                 "AcceptedCmp1" , "AcceptedCmp2" , "Complain" ,
                                 "Z_CostContact" , "Z_Revenue" , "Response" ,
                                 "Customer_Year" , "MntWines" , "MntFruits" , "MntMeatProducts" , 
                                 "MntFishProducts" , "MntSweetProducts" ,"MntGoldProds" , "Income")]
head(df)

corrgram(df, order=T, lower.panel=panel.shade, upper.panel=NULL,
         text.panel=panel.txt, main="Customer Data")

ggpairs(df)

set.seed(12)
kmean <- kmeans(df, 2, nstart = 10)
print((kmean$betweenss/kmean$totss)*100)
fviz_cluster(kmean, df, geom = "point",ellipse.type = "norm",repel = TRUE)

# Define function to find elbow point
kElbow <- function(wss) {
  n <- length(wss)
  ss_diff <- (n - 2):(1 + n %% 2)
  elbow <- ss_diff[which.max(abs(diff(wss[ss_diff])))] + 1
  return(elbow)
}


# Compute within-cluster sum of squares (WSS) for values of k ranging from 1 to 15
wss <- sapply(1:15, function(k) {
  kmeans(df, centers = k, iter.max = 100)$tot.withinss
})

# Plot the elbow curve using ggplot2
elbow_plot <- ggplot(data = data.frame(k = 1:15, wss = wss), aes(x = k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 15, by = 1)) +
  labs(x = "Number of Clusters (k)", y = "Within-Cluster Sum of Squares (WSS)") +
  theme_classic()

# Display the plot
elbow_plot

# Initialize empty vectors to store values
sil_width <- numeric(0)
for (k in 2:15) {
  # Run k-means clustering
  km <- kmeans(df, centers = k, nstart = 25,iter.max = 100)
  
  # Calculate the silhouette width
  sil_width[k-1] <- mean(silhouette(km$cluster, dist(df))[,3])
}

# Find the optimal number of clusters (k) based on silhouette width
optimal_k <- which.max(sil_width) + 1

# Plot the silhouette width for each k
ggplot(data.frame(k = 2:15, sil_width = sil_width), aes(x = k, y = sil_width)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(2, 15, by = 1)) +
  labs(x = "Number of Clusters (k)", y = "Average Silhouette Width") +
  geom_vline(xintercept = optimal_k, color = "red") +
  theme_classic()

set.seed(12)
kmean <- kmeans(df, 3, nstart = 10)
print((kmean$betweenss/kmean$totss)*100)
fviz_cluster(kmean, df, geom = "point",ellipse.type = "norm",repel = TRUE)

#agglomerative clustering 
# Compute with agnes (package cluster) 
hc2 <- agnes(df, method = "complete") 
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
# Agglomerative coefficient 
hc2$ac 
#assigning clusters to the data points  
clust <- cutree(hc2, k = 3) 
#fviz_cluster function from the factoextra package 
#to visualize the result in a scatter plot. 
fviz_cluster(list(data = df, cluster = clust)) 

#divisive clustering 
# compute divisive hierarchical clustering 
hc4 <- diana(df) 
# plot dendrogram 
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana") 
# Divise coefficient 
hc4$dc 
#visualize the clusters inside the dendrogram 
rect.hclust(hc4, k = 3, border = 2:10) 
clust <- cutree(hc4, k = 3) 
fviz_cluster(list(data = df, cluster = clust))


data_normalized <- scale(df)
head(data_normalized)

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix[,12:1])

data.pca <- princomp(corr_matrix)
summary(data.pca)

fviz_eig(data.pca, addlabels = TRUE)

fviz_pca_var(data.pca, col.var = "black")

df["segmant"] = kmean$cluster
split=sample.split(df,SplitRatio=0.7)
train_c1=subset(df,split=="TRUE")
test_c1=subset(df,split=="FALSE")
length(train_c1)
train_scale=scale(train_c1[,1:12])
test_scale=scale(test_c1[,1:12])
length(train_scale)
length(test_scale)

classifier_knn=knn(train = train_scale,
                   test = test_scale,
                   cl=train_c1$segmant,
                   k=14)
classifier_knn

cm=table(test_c1$segmant,classifier_knn)
cm

heatmap(cm,xlab = "Predicted",ylab = "Truth")

misClassifier=mean(classifier_knn !=test_c1$segmant)
print(paste('Accuracy =', 1 -misClassifier))

dbscan::kNNdistplot(df,k=14)
abline(h=35,lty=2)

set.seed(123)
db <- dbscan(df, eps = 35, minPts = 14)
print(db)
fviz_cluster(db, data = df , stand = F,outlier.color = "Red",
             ellipse = T,geom = "point",palette="jco",ggtheme = theme_classic())




Image compression
library(jpeg)
library(magick)
library(ggplot2)
library(factoextra)
library(gridExtra)



img <- readJPEG("C:/Users/hamdy/Pictures/itatchi.jpg")
image_read(img)

dim(img)
red <- img[,,1]
green <- img[,,2]
blue <- img[,,3]

red.pca <- prcomp(red, center=FALSE, scale.=FALSE)
green.pca <- prcomp(green, center=FALSE, scale.=FALSE)
blue.pca <- prcomp(blue, center=FALSE, scale.=FALSE)

list.img.pca <- list(red.pca, green.pca, blue.pca)


f1 <- fviz_eig(red.pca, choice = 'eigenvalue', main = "Red", barfill = "red", ncp = 7, addlabels = TRUE)
f2 <- fviz_eig(green.pca, choice = 'eigenvalue', main = "Green", barfill = "green", ncp = 7, addlabels = TRUE)
f3 <- fviz_eig(blue.pca, choice = 'eigenvalue', main = "Blue", barfill = "blue", ncp = 7, addlabels = TRUE)

grid.arrange(f1, f2, f3, ncol=3)

library(abind)

for (i in c(10,15,30,60,80,120,180,210)) {
  new_image <- abind(red.pca$x[,1:i] %*% t(red.pca$rotation[,1:i]),
                     green.pca$x[,1:i] %*% t(green.pca$rotation[,1:i]),
                     blue.pca$x[,1:i] %*% t(blue.pca$rotation[,1:i]),
                     along = 3)
  writeJPEG(new_image, paste0('Compressed_image_with_',i, '_components.jpg'))
}

image_plot <- function(path, plot_name) {
  require('jpeg')
  img <- readJPEG(path)
  d <- dim(img)
  plot(0,0,xlim=c(0,d[2]),ylim=c(0,d[2]),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  title(plot_name, line = -0.5)
  rasterImage(img,0,0,d[2],d[2])
}

par(mfrow = c(2,2), mar = c(0,0,1,1))
for (i in c(10,15,30,60,80,120,180,210)) {
  image_plot(paste0('Compressed_image_with_',i, '_components.jpg'), 
             paste0(round(i,0), ' Components'))
}

library(knitr)

table <- matrix(0,9,3)
colnames(table) <- c("Number of components", "Image size (kilobytes)", "Saved Disk Space (kilobytes)")
table[,1] <- c(10,15,30,60,80,120,180,210,"Original itatchi image")
table[9,2:3] <- round(c(file.info('C:/Users/hamdy/Pictures/itatchi.jpg')$size/1024, 0),2)
for (i in c(1:8)) {
  path <- paste0('Compressed_image_with_',table[i,1], '_components.jpg')
  table[i,2] <- round(file.info(path)$size/1024,2)
  table[i,3] <- round(as.numeric(table[9,2]) - as.numeric(table[i,2]),2)
}

kable(table)
