library(cluster)
library(dplyr)
library(ggplot2)
library(Rtsne)
data <- read.table("Jeu-1-projet.csv",sep=",",header = T)
data[,c(1,2,4:6)] <- lapply(data[,c(1,2,4:6)],as.factor)

gower_dist <- daisy(data2, metric = "gower")
gower_mat <- as.matrix(gower_dist)
#str_detect()
#Print most similar clients
data[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

#' Print most dissimilar clients
data[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

sil_width <- c(NA)
for(i in 2:20){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:20, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, sil_width)

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- data %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) #%>%
  #do(the_summary = summary(.))
#pam_results$the_summary


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering)) %>% 
  mutate(ipsource = data$ipsrc)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

test <- (pam_results$the_summary)
test2 <- data.frame(unclass(pam_results$the_summary), check.names = FALSE, stringsAsFactors = FALSE)
test3 <- do.call(cbind, lapply(mydf, summary))
test[,3]
