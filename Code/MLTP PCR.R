library(tidyverse)
library(ggplot2)
library(ggfortify)
library(plotly)
library(data.table)
library(factoextra)
library(pls)
  
MLTPfiles <- list.files(path = "C:/Users/jays/Desktop/Tagpro/MLTP_Stats/",
                        pattern = "*.tsv", full.names = TRUE)

MLTP_master_stats <- data.frame()

for (lambda in c(1:10)) {
  location <- MLTPfiles[lambda]
  temp <- fread(file = location, 
                sep = "\t", fill = TRUE, header = TRUE) %>% mutate(season = as.factor(lambda + 9))
  
  MLTP_master_stats <- rbind(MLTP_master_stats, temp)
}

str(MLTP_master_stats)

MLTP_normalized_stats<- mutate(MLTP_master_stats, scpm = score/minutes, success_rate = pm/minutes, tpm = tags/minutes, poppm = pops/minutes, droppm = drops/minutes, gpm = grabs/minutes,
                               hpm = hold/minutes, cpm = captures/minutes, ppm = prevent/minutes, rpm = returns/minutes,
                               pupm = pups/minutes, kdr = tags/pops, limp = flaccids/minutes, LHpm = longholds/minutes, 
                               handpm = handoff/minutes, GHpm = goodhandoff/minutes, gorepm = goregrab/minutes, corepm = coregrab/minutes,
                               copm = cohandoff/minutes, inbasepm = retinbase/minutes, quickpm = quickret/minutes, savepm = saves/minutes,
                               HApm = holdagainst/minutes)

MLTP_filtered_stats <- MLTP_normalized_stats %>%
  filter(minutes >= 120) %>%
  select(-c(V28, team, pupscomp, score, minutes, pm, tags, pops, grabs, drops, hold, captures, prevent, returns,
            pups, pupscomp, flaccids, longholds, handoff, goodhandoff, goregrab, gohandoff, cohandoff, retinbase,
            quickret, saves, holdagainst, coregrab))

MLTP_defense_stats <- MLTP_filtered_stats %>%
  filter(ppm > hpm)

MLTP_offense_stats <- MLTP_filtered_stats %>%
  filter(hpm > ppm)

defense_pca <- prcomp(MLTP_defense_stats[ , 3:25],
                      center = TRUE,
                      scale = TRUE,
                      rank. = 5)

autoplot(defense_pca, data = MLTP_defense_stats, colour = 'season',
         loadings = TRUE, loadings.label = TRUE)
summary(defense_pca)
fviz_eig(defense_pca)

scores_defense_pca <- data.frame(defense_pca$x)  

d_pca <- plot_ly(scores_defense_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~MLTP_defense_stats$season, 
             colors = c('#1f77b4',  '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
                        '#bcbd22', '#17becf')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

d_pca

offense_pca <- prcomp(MLTP_offense_stats[ , 3:25],
                      center = TRUE,
                      scale = TRUE,
                      rank. = 5)

autoplot(offense_pca, data = MLTP_offense_stats, colour = 'season',
         loadings = TRUE, loadings.label = TRUE)
summary(offense_pca)
fviz_eig(offense_pca)

scores_offense_pca <- data.frame(offense_pca$x)  

o_pca <- plot_ly(scores_offense_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~MLTP_offense_stats$season, 
             colors = c('#1f77b4',  '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f',
                        '#bcbd22', '#17becf')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

o_pca

#PCR time

set.seed(123)
defense_pcr <- pcr(success_rate ~ ., data = MLTP_defense_stats[ , 3:25], scale = TRUE, 
                   validation = 'CV', ncomp = 10)

summary(defense_pcr)

offense_pcr <- pcr(success_rate ~ ., data = MLTP_offense_stats[ , 3:25], scale = TRUE, 
                   validation = 'CV', ncomp = 10)

summary(offense_pcr)