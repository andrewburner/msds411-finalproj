library(recipes)
library(psych)
set.seed(123)

# Data Processing
df_clean <- df %>%
  # Filtering out some songs that we don't want to include
  # Most of these are white noise tracks
  filter(!artists %in% c('[\'Sound Dreamer\']', '[\'Sounds for Life\']',
                         '[\'Lightning, Thunder and Rain Storm\']',
                         '[\'Ocean Sounds\']', '[\'Ocean Waves For Sleep\']',
                         '[\'Sleep\']', '[\'Rain Sounds\']',
                         '[\'Unspecified\']')) %>% 
  filter(!(speechiness == 0 & tempo == 0 & valence == 0)) %>% 
  # Probably controversial but I'm removing tracks nobody listens to
  filter(!popularity == 0) %>% 
  # Removing some of the variables we won't use for PCA
  select(-name,-artists,-id, -release_date, -explicit, -key, -mode, -year)

# Scale and center the data
df_pca <- recipe(popularity ~ ., data=df_clean) %>% 
  step_normalize(all_numeric()) %>% 
  prep() %>% 
  bake(new_data = df_clean) %>% 
  # Going to remove the duration variable because I think
  # the outliers might make the PCA really weird due to outliers
  select(-duration_ms,
         -popularity)

# Look at new distribution of variables
plot_num(df_pca)




#########################################
## this is where I stopped ##
#########################################

pca_data <- prcomp(df_pca)
fviz_eig(pca_data)


#Clustering
#Identify Optimal Clusters

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
