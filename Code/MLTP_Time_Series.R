library(tidyverse)
library(ggplot2)
library(ggfortify)
library(plotly)
library(data.table)
library(factoextra)
library(pls)
library(beeswarm)

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
            quickret, saves, holdagainst, coregrab)) %>%
    mutate(position = ifelse(ppm > hpm, "Defense", "Offense"))

MLTP_defense_stats <- MLTP_filtered_stats %>%
  filter(ppm > hpm)

MLTP_offense_stats <- MLTP_filtered_stats %>%
  filter(hpm > ppm)

ppm_tpm_all <- MLTP_filtered_stats %>%
  plot_ly(
    x = ~ppm, 
    y = ~tpm, 
    size = ~success_rate, 
    color = ~position, 
    frame = ~season, 
    text = ~player, 
    hovertemplate = paste("<b>%{text}</b><br><br>",
                          "%{yaxis.title.text}: %{y}<br>",
                          "%{xaxis.title.text}: %{x}<br>",
                          "<extra></extra>"),
    type = 'scatter',
    mode = 'markers'
  ) %>%
    animation_opts(frame = 1500, easing = NULL, redraw = FALSE)

ppm_tpm_all

ppm_tpm_d <- MLTP_defense_stats %>%
  plot_ly(
    x = ~ppm, 
    y = ~tpm, 
    size = ~success_rate, 
    color = ~position, 
    frame = ~season, 
    text = ~player, 
    hovertemplate = paste("<b>%{text}</b><br><br>",
                    "%{yaxis.title.text}: %{y}<br>",
                    "%{xaxis.title.text}: %{x}<br>",
                    "<extra></extra>"),
    type = 'scatter',
    mode = 'markers'
  ) %>%
  animation_opts(frame = 1500, easing = NULL, redraw = FALSE)

ppm_tpm_d

#Prevent by defenders
boxplot(ppm ~ season, data = MLTP_defense_stats, main = "Prevent Per Minute by MLTP Defenders", 
        xlab = "Season", ylab = "Prevent Per Minute", col = "gray69", outline = FALSE,
        par(bg = 'seashell'))

#Release the bees!
beeswarm(ppm ~ season, data = MLTP_defense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Prevent Per Minute by MLTP Season", xlab = "Season", ylab = "Prevent Per Minute",
         add = TRUE)

#Plotly inbound
prevent <- plot_ly(data= MLTP_defense_stats, x = ~season, y = ~ppm, color = ~season, type = "box", boxpoints = 'all',
             text = ~player) %>%
  layout(scene = list(xaxis = list(title = 'Season'),
                      yaxis = list(title = 'Prevent Per Minute')),
         title = "Prevent Per Minute by MLTP Defenders")

prevent


#Tags by defenders
boxplot(tpm ~ season, data = MLTP_defense_stats, main = "Tags Per Minute by MLTP Defenders", 
        xlab = "Season", ylab = "Tags Per Minute", col = "gray69", outline = FALSE,
        par(bg = 'seashell'))

beeswarm(tpm ~ season, data = MLTP_defense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Tags Per Minute by MLTP Season", xlab = "Season", ylab = "Tags Per Minute",
         add = TRUE)

#Plotly inbound
tags <- plot_ly(data= MLTP_defense_stats, x = ~season, y = ~tpm, color = ~season, type = "box", boxpoints = 'all',
             text = ~player) %>%
  layout(scene = list(xaxis = list(title = 'Season'),
                      yaxis = list(title = 'Tags Per Minute')),
         title = "Tags Per Minute by MLTP Defenders")

tags

#KDR by defenders
boxplot(kdr ~ season, data = MLTP_defense_stats, main = "Kill/Death Ratio by MLTP Defenders", 
        xlab = "Season", ylab = "Kill/Death Ratio", col = "gray69", outline = FALSE,
        par(bg = 'seashell'))

beeswarm(kdr ~ season, data = MLTP_defense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Kill/Death Ratio by MLTP Season", xlab = "Season", ylab = "Kill/Death Ratio",
         add = TRUE)

#Plotly inbound
kdr <- plot_ly(data= MLTP_defense_stats, x = ~season, y = ~kdr, color = ~season, type = "box", boxpoints = 'all',
                text = ~player) %>%
  layout(scene = list(xaxis = list(title = 'Season'),
                      yaxis = list(title = 'Kill/Death Ratio')),
         title = "Kill/Death Ratio by MLTP Defenders")

kdr


#Hold by attackers
boxplot(hpm ~ season, data = MLTP_offense_stats, main = "Hold Per Minute by MLTP Offenders", 
        xlab = "Season", ylab = "Hold Per Minute", col = "gray69", outline = FALSE,
        par(bg = 'seashell'))

beeswarm(hpm ~ season, data = MLTP_offense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Hold Per Minute by MLTP Season", xlab = "Season", ylab = "Hold Per Minute",
         add = TRUE)

#Plotly inbound
hold <- plot_ly(data= MLTP_offense_stats, x = ~season, y = ~hpm, color = ~season, type = "box", boxpoints = 'all',
             text = ~player) %>%
  layout(scene = list(xaxis = list(title = 'Season'),
                      yaxis = list(title = 'Hold Per Minute')),
         title = "Hold Per Minute by MLTP Offenders")

hold


#Caps for attackers
boxplot(cpm ~ season, data = MLTP_offense_stats, main = "Captures Per Minute by MLTP Offenders", 
        xlab = "Season", ylab = "Captures Per Minute", col = "gray69", outline = FALSE,
        par(bg = 'seashell'))

beeswarm(cpm ~ season, data = MLTP_offense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Caps Per Minute by MLTP Season", xlab = "Season", ylab = "Captures Per Minute",
         add = TRUE)

caps <- plot_ly(data= MLTP_offense_stats, x = ~season, y = ~cpm, color = ~season, type = "box", boxpoints = 'all',
                text = ~player) %>%
  layout(scene = list(xaxis = list(title = 'Season'),
                      yaxis = list(title = 'Captures Per Minute')),
         title = "Captures Per Minute by MLTP Offenders")

caps

#Grabs for attackers
boxplot(gpm ~ season, data = MLTP_offense_stats, main = "Grabs Per Minute by MLTP Offenders", 
        xlab = "Season", ylab = "Grabs Per Minute", col = "gray69", outline = FALSE,
        par(bg = 'seashell'))

beeswarm(gpm ~ season, data = MLTP_offense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Grabs Per Minute by MLTP Season", xlab = "Season", ylab = "Grabs Per Minute",
         add = TRUE)

grabs <- plot_ly(data= MLTP_offense_stats, x = ~season, y = ~gpm, color = ~season, type = "box", boxpoints = 'all',
                text = ~player) %>%
  layout(scene = list(xaxis = list(title = 'Season'),
                      yaxis = list(title = 'Grabs Per Minute')),
         title = "Grabs Per Minute by MLTP Offenders")

grabs


#big_pile <- ggplot(MLTP_filtered_stats, aes(x = ppm, y = success_rate, color = season)) + geom_point() + theme(
#  text = element_text(family = "Bookman"),
#  title = element_text(color = "gray25"),
#  plot.caption = element_text(color = "gray30"),
#  plot.subtitle = element_text(size = 12)
#) + facet_wrap(~position) + theme(
#  plot.background = element_rect(fill = "gray95"))