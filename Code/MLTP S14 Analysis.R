library(tidyverse)
library(ggplot2)
library(data.table)

#Read in .tsv from file path of data folder
MLTP_S14_stats <- fread(file = "C:/Users/jays/Desktop/Tagpro/MLTP_Stats/MLTP_S14_majors_cumulative.tsv")

#Examine data structure to see what we have to work with
str(MLTP_S14_stats)
summary(MLTP_S14_stats)

#Create "per minute" values for some of the variables, since players have different playing time across the seasons
MLTP_S14_stats <- mutate(MLTP_S14_stats, season = 14, success_rate = pm/minutes, tpm = tags/minutes, gpm = grabs/minutes,
                         hpm = hold/minutes, cpm = captures/minutes, ppm = prevent/minutes, rpm = returns/minutes,
                         pupm = pups/minutes, kdr = tags/pops)

#Remove some of the useless, null, and uninteresting columns
MLTP_S14_stats <- subset(MLTP_S14_stats, select = -c(V28, team, longholds, handoff,goodhandoff, goregrab, coregrab, 
                                                     gohandoff, score))

#Re-examine structure and decide how to partition defensive and offensive stats
str(MLTP_S14_stats)
MLTP_S14_D_stats <- subset(MLTP_S14_stats, select = c(player, minutes, tpm, ppm, hpm, rpm, pupm, kdr, success_rate))
MLTP_S14_O_stats <- subset(MLTP_S14_stats, select = c(player, minutes, gpm, hpm, ppm, cpm, pupm, kdr, success_rate))

#Filter defenders out of the offensive stats; requires knowledge of the season to pick the correct boundary conditions
MLTP_S14_O_stats <- MLTP_S14_O_stats %>%
  filter(hpm > ppm, minutes > 120) %>%
  group_by(hpm)

#Filter attackers out of the defensive stats
MLTP_S14_D_stats <- MLTP_S14_D_stats %>% 
  filter(ppm > hpm , minutes > 120) %>%
  group_by(ppm)

#Visual of each per minute stat vs success_rate for attackers:

#Grabs per minute
ggplot(MLTP_S14_O_stats, aes(x = gpm, y = success_rate)) + geom_point(shape = 21, color = "purple", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Offensive Statistics", subtitle = "Grabs Per Minute vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(gpm >= 1.80, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Hold per minute
ggplot(MLTP_S14_O_stats, aes(x = hpm, y = success_rate)) + geom_point(shape = 21, color = "blue", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Offensive Statistics", subtitle = "Hold Per Minute vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(hpm >= 12, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Caps per minute
ggplot(MLTP_S14_O_stats, aes(x = cpm, y = success_rate)) + geom_point(shape = 21, color = "black", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Offensive Statistics", subtitle = "Caps Per Minute vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(cpm > 0.20, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Powerups per minute
ggplot(MLTP_S14_O_stats, aes(x = pupm, y = success_rate)) + geom_point(shape = 21, color = "green", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Offensive Statistics", subtitle = "Powerups Per Minute vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(pupm >= 0.32, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Kill/Death Ratio
ggplot(MLTP_S14_O_stats, aes(x = kdr, y = success_rate)) + geom_point(shape = 21, color = "firebrick", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Offensive Statistics", subtitle = "Kill/Death Ratio vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(kdr >= 0.55, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)



#Analytical examination of offensive statistics

cor(MLTP_S14_O_stats$gpm, MLTP_S14_O_stats$success_rate)
summary(MLTP_S14_O_stats$gpm)
sd(MLTP_S14_O_stats$gpm)
var(MLTP_S14_O_stats$gpm)

cor(MLTP_S14_O_stats$hpm, MLTP_S14_O_stats$success_rate)
summary(MLTP_S14_O_stats$hpm)
sd(MLTP_S14_O_stats$hpm)
var(MLTP_S14_O_stats$hpm)

cor(MLTP_S14_O_stats$cpm, MLTP_S14_O_stats$success_rate)
summary(MLTP_S14_O_stats$cpm)
sd(MLTP_S14_O_stats$cpm)
var(MLTP_S14_O_stats$cpm)

cor(MLTP_S14_O_stats$pupm, MLTP_S14_O_stats$success_rate)
summary(MLTP_S14_O_stats$pupm)
sd(MLTP_S14_O_stats$pupm)
var(MLTP_S14_O_stats$pupm)

cor(MLTP_S14_O_stats$kdr, MLTP_S14_O_stats$success_rate)
summary(MLTP_S14_O_stats$kdr)
sd(MLTP_S14_O_stats$kdr)
var(MLTP_S14_O_stats$kdr)

#Visual of each per minute stat vs success_rate for defenders:

#Tags per minute
ggplot(MLTP_S14_D_stats, aes(x = tpm, y = success_rate)) + geom_point(shape = 21, color = "purple", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Defensive Statistics", subtitle = "Tags Per Minute vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(tpm >= 1.90, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Prevent per minute
ggplot(MLTP_S14_D_stats, aes(x = ppm, y = success_rate)) + geom_point(shape = 21, color = "blue", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Defensive Statistics", subtitle = "Prevent Per Minute vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(ppm >= 19, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Returns per minute
ggplot(MLTP_S14_D_stats, aes(x = rpm, y = success_rate)) + geom_point(shape = 21, color = "black", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Defensive Statistics", subtitle = "Returns Per Minute vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(rpm >= 1.60, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Powerups per minute
ggplot(MLTP_S14_D_stats, aes(x = pupm, y = success_rate)) + geom_point(shape = 21, color = "green", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Defensive Statistics", subtitle = "Powerups Per Minute vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(pupm >= 0.35, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Kill/Death Ratio
ggplot(MLTP_S14_D_stats, aes(x = kdr, y = success_rate)) + geom_point(shape = 21, color = "firebrick", fill = "white", size = 3, stroke = 3) + 
  labs(title = "MLTP S14 Defensive Statistics", subtitle = "Kill/Death Ratio vs Success Rate", 
       caption = "Based on data from TagPro League") +
  geom_text(aes(label = ifelse(kdr >= 2.50, as.character(player), ''), vjust = 2)) +geom_smooth(method = 'lm', formula = y~x)

#Analytical examination of defensive statistics

cor(MLTP_S14_D_stats$tpm, MLTP_S14_D_stats$success_rate)
summary(MLTP_S14_D_stats$tpm)
sd(MLTP_S14_D_stats$tpm)
var(MLTP_S14_D_stats$tpm)

cor(MLTP_S14_D_stats$ppm, MLTP_S14_D_stats$success_rate)
summary(MLTP_S14_D_stats$ppm)
sd(MLTP_S14_D_stats$ppm)
var(MLTP_S14_D_stats$ppm)

cor(MLTP_S14_D_stats$rpm, MLTP_S14_D_stats$success_rate)
summary(MLTP_S14_D_stats$rpm)
sd(MLTP_S14_D_stats$rpm)
var(MLTP_S14_D_stats$rpm)

cor(MLTP_S14_D_stats$pupm, MLTP_S14_D_stats$success_rate)
summary(MLTP_S14_D_stats$pupm)
sd(MLTP_S14_D_stats$pupm)
var(MLTP_S14_D_stats$pupm)

cor(MLTP_S14_D_stats$kdr, MLTP_S14_D_stats$success_rate)
summary(MLTP_S14_D_stats$kdr)
sd(MLTP_S14_D_stats$kdr)
var(MLTP_S14_D_stats$kdr)