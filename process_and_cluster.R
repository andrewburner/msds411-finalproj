# Data Processing
df_pca <- df %>% select(-name,-artists, -id,
                        -release_date, -explicit,
                        -key, -mode, -year)




#Clustering
data_kmeans <- df %>% select(-name,-artists,-id, -release_date, -explicit, -key, -mode, -year)

#Scale
data_scale <- scale(data_kmeans)

#Identify Optimal Clusters
library(psych)
fa.parallel(correlation, n.obs=169909, fa="both", n.iter=100, show.legend=TRUE,main="Scree Plot (Parallel Analysis)")
fa(correlation, n.obs=169909, n.iter=100, show.legend=TRUE)

#Kmeans
kmeans_data <- kmeans(data_scale, centers = 6, nstart = 25)
kmeans_data

#Cluster Plot
fviz_cluster(kmeans_data, data = data_scale, geom="point")

#Distributions in Clusters
centers <- data.frame(cluster = rownames(kmeans_data$centers), kmeans_data$centers)
centers <- reshape2::melt(centers)
ggplot(centers, aes(x = reorder(variable, value), y = value)) +
  geom_bar(aes(fill = value > 0), width=0.8, stat = "identity") +
  facet_wrap(~ cluster, nrow=1) +
  coord_flip() +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA), legend.position="none") +
  labs(x = NULL)
