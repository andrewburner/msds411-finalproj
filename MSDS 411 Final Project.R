#Setup
library(data.table)
library(dplyr)
library(corrplot)
library(ggplot2)
library(formattable)
library(tidyverse)
library(funModeling)
library(Hmisc)
library(gridExtra)
library(ggcorrplot)
library(timetk)
library(factoextra)

# For PG13's personal Mac use only
# setwd("/Users/pjgarret/Downloads/Spotify Data")

#Load Spotify Data
df <- read.csv("data.csv")

#1: EDA of Spotify Data

basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

basic_eda(df)

#Heatmap
correlation <- df %>% select(-name,-artists,-id, -release_date, -duration_ms, -mode)
correlation <- cor(correlation)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
ggcorrplot(correlation, hc.order = TRUE, type = "lower", lab = TRUE,tl.srt=90) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation of Variables",
       x = "X",
       y = "Y")

# Plotting popularity by year
ggplot(df, aes(x = year, y = popularity)) +
  geom_point() +
  geom_smooth()

# Count of songs by year
ggplot(df, aes(x = year)) +
  geom_histogram(binwidth = 1) +
  ylim(0, 2500)


#Top 10 Popular Artists
df %>% 
  group_by(artists) %>%  
  summarise(Popularity = sum(popularity)) %>%  
  arrange(desc(Popularity)) %>%  
  top_n(10) %>%
  ggplot(aes(x = reorder(artists,desc(Popularity)), y = Popularity)) +  geom_bar(stat = "identity", fill = "steelblue", color = 1) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) + 
  labs(title = "Top 10 Artists",x = NULL,y = "Popularity")

#Top 5 Songs
df %>% 
  group_by(name) %>%  
  summarise(Popularity = mean(popularity)) %>%  
  arrange(desc(Popularity)) %>%  
  top_n(5) %>%
  ggplot(aes(x = reorder(name,desc(Popularity)), y = Popularity)) +  geom_bar(stat = "identity", fill = "steelblue", color = 1) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) + 
  labs(title = "Top 5 Songs",x = NULL,y = "Popularity")

#How X Changes over time (change var)
df %>% group_by(year) %>%
  summarise(acousticness = mean(danceability)) %>%
  plot_time_series(year, acousticness, .smooth = TRUE, .smooth_span = 0.25,
                   .title = "Time Series: Acousticness",.x_lab = "Years",.y_lab = "Measure", .interactive = FALSE)


df %>% 
  group_by(year) %>%
  summarise(energy = mean(energy), acousticness = mean(acousticness),speechiness = mean(speechiness),
            liveness = mean(liveness), valence = mean(valence)) %>%
  ggplot() + 
  geom_line(aes(y = acousticness, x = year,  colour = "darkred")) + 
  geom_line(aes(y = energy , x = year,  colour = "black")) +
  geom_line(aes(y = speechiness, x = year ,  colour = "green")) +
  geom_line(aes(y = liveness, x = year, colour = "violet")) +
  geom_line(aes(y = valence, x = year  ,colour = "steelblue")) + 
  theme_bw() + scale_color_discrete(name = "Audio Characteristics", labels = c("Energy", "Acousticness","Speechiness","Liveness","Valence")) +
  labs(title = "Time Series: Audio Characteristics",x = "Year", y = "Measure")


