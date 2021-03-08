tsne_base <- function(data,k){
  data <- data[sample(nrow(data), 1000), ]
  data[,c(1,2,4,5)] <- lapply(data[,c(1,2,4,5)],as.factor)
  
  gower_dist <- daisy(data, metric = "gower")
  gower_mat <- as.matrix(gower_dist)
  
  pam_fit <- pam(gower_dist, diss = TRUE, k)
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  tsne_data <- tsne_obj$Y %>% data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering)) %>% 
    mutate(ipsource = data$ipsrc)
  
  pam_results <- data %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  resume <- pam_results$the_summary
  
  p <- ggplot(aes(x = X, y = Y), data = tsne_data) +
       geom_point(aes(color = cluster))

  return_list <- list(p, resume)
  return(return_list)
}  


databis <- function(data,k){
  data2 <- sqldf("select ipsrc, count(*) as nombre, count(distinct ipdst) as cnbripdst, count(distinct dstport) as cndstport,
                  sum( case when action  like 'PERMIT' then 1 ENd) as permit,
                  sum( case when action  like 'PERMIT' and dstport < 1024 then 1 ENd) as inf1024permit,
                  sum( case when action  like 'PERMIT' and dstport >= 1024 then 1 ENd) as sup1024permit,
                  sum( case when action  like 'PERMIT' and (dstport = 21 OR dstport = 22 OR dstport = 3389 OR dstport = 3306) then 1 ENd) as adminpermit,
                  sum(case when action like 'DENY' then 1 ENd) as deny,
                  sum(case when action like 'DENY' and dstport < 1024  then 1 ENd) as inf1024deny,
                  sum(case when action like 'DENY' and dstport >= 1024  then 1 ENd) as sup1024deny,
                  sum( case when action  like 'DENY' and (dstport = 21 OR dstport = 22 OR dstport = 3389 OR dstport = 3306) then 1 ENd) as admindeny
                  from data  group by ipsrc ")
  
  data2[is.na(data2)] <- 0
  
  rownames(data2) <- data2[,1]
  data2 <- data2[,-1]
  data2[,1:11] <- lapply(data2[,1:11] , as.integer)
  data2[,-c(2,6)] <- scale(data2[,-c(2,6)])
  
  return(data2)
  
} 

km_graph1 <- function(data){
  
  ggplot(data, aes(x=cndstport, fill=cluster)) + geom_area(stat ="bin", alpha=0.6) +coord_cartesian(xlim =c(0, 10), ylim = c(0, 100000))
  

}