library(tidyverse)
library(ggplot2)
library(data.table)
library(plotly)

#Read in the .tsv from file path (switched from read.table to fread to fix row deletion issue)
MLTP_S14_Stats <- fread(file = "C:/Users/jays/Desktop/Tagpro/MLTP_Stats/MLTP_S14_majors_cumulative.tsv")

#Check out the structure of the data
str(MLTP_S14_Stats)

#Find out where missing data is
missing_values <- is.na(MLTP_S14_Stats)
missing_values

#Remove the useless columns
MLTP_S14_Stats <- subset(MLTP_S14_Stats, select = -c(team, V28))

#Add columns for defensive per minute stats
MLTP_S14_Stats <- mutate(MLTP_S14_Stats, prevent_per_min = prevent / minutes)
MLTP_S14_Stats <- mutate(MLTP_S14_Stats, tags_per_min = tags / minutes)

#Add columns for offensive per minute stats
MLTP_S14_Stats <- mutate(MLTP_S14_Stats, caps_per_min = captures / minutes)
MLTP_S14_Stats <- mutate(MLTP_S14_Stats, hold_per_min = hold / minutes)


#New data frame with columns not relevant to defense removed
defense_MLTP_S14_Stats <- subset(MLTP_S14_Stats, select = -c(grabs, hold, captures, flaccids, longholds, handoff,
                                                             goodhandoff, goregrab, coregrab, gohandoff))

#Another new data frame with columns not relevant to offense removed
offense_MLTP_S14_Stats <- subset(MLTP_S14_Stats, select = -c(tags, prevent, returns, retinbase, quickret))

#Check structures again to verify changes are correct
str(MLTP_S14_Stats)
str(defense_MLTP_S14_Stats)
str(offense_MLTP_S14_Stats)

#Filter out attackers
D_filtered_MLTP_S14_Stats <- MLTP_S14_Stats %>% 
                            filter(prevent_per_min > 10 , minutes > 120) %>%
                            group_by(prevent_per_min)

D_filtered_MLTP_S14_Stats

#Filter out defenders
O_filtered_MLTP_S14_Stats <- MLTP_S14_Stats %>%
                            filter(hold_per_min > 6, minutes > 120) %>%
                            group_by(hold_per_min)

O_filtered_MLTP_S14_Stats


#Scatter plot showing the Prevent Per Minute stats for each individual defensive player
ggplot(D_filtered_MLTP_S14_Stats, aes(x = player, y = prevent_per_min, color = pm)) + geom_point() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "MLTP S14 Defensive Statistics", subtitle = "Prevent Per Minute", 
                                                                    caption = "Based on data from TagPro League", color = "Cap Differential") +
  geom_text(aes(label = ifelse(prevent_per_min > 30, as.character(pm), ''), hjust = 0, vjust = 0))

#Scatter plot showing the Hold Per Minute stats for each individual offensive player
ggplot(O_filtered_MLTP_S14_Stats, aes(x = player, y = hold_per_min, color = pm)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "MLTP S14 Offensive Statistics", subtitle = "Hold Per Minute", 
                                                                  caption = "Based on data from TagPro League", color = "Cap Differential") +
  geom_text(aes(label = ifelse(hold_per_min > 30, as.character(pm), ''), hjust = 0, vjust = 0))

