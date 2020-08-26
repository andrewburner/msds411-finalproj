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
  filter(!popularity == 0)

# Scale and center the data
df_pca <- recipe(popularity ~ ., data=df_clean) %>% 
  step_normalize(all_numeric()) %>% 
  prep() %>% 
  bake(new_data = df_clean) %>% 
  # Going to remove the duration variable because I think
  # it might make the PCA really weird due to outliers
  select(-duration_ms, -year, -mode,
         -popularity, -mode, -explicit, -key,
         -name, -artists, -release_date, -id)

# Look at new distribution of variables
plot_num(df_pca)

# Get PCs
pca_data <- princomp(df_pca)
summary(pca_data)
fviz_eig(pca_data)

# Add PCs to the data set
df_clean <- df_clean %>% mutate(PC1 = pca_data$scores[,1],
                            PC2 = pca_data$scores[,2],
                            PC3 = pca_data$scores[,3],
                            PC4 = pca_data$scores[,4],
                            PC5 = pca_data$scores[,5],
                            PC6 = pca_data$scores[,6])

#Clustering
#Identify Optimal Clusters
correlation <- df_clean %>% select_if(is.numeric) %>% cor()

fa.parallel(correlation, fa="both", n.iter=100, show.legend=TRUE,main="Scree Plot (Parallel Analysis)")
fa(correlation, n.obs = 142397, n.iter=100, show.legend=TRUE, fm = 'ml')

#Kmeans
kmeans_data_pca <- df_clean %>% select(PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  kmeans(centers = 7, nstart = 25)

kmeans_data.7 <- df_pca %>% 
  kmeans(centers = 7, nstart = 25)

kmeans_data.3 <- df_pca %>% 
  kmeans(centers = 3, nstart = 25)

#Cluster Plot
fviz_cluster(kmeans_data, data = df_pca, geom="point")

fviz_cluster(kmeans_data_pca, data = df_clean, geom="point")



################################
### This is where I left off ###
################################


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
