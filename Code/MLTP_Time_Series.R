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

beeswarm(ppm ~ season, data = MLTP_defense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Prevent Per Minute by MLTP Season", xlab = "Season", ylab = "Prevent Per Minute")

beeswarm(tpm ~ season, data = MLTP_defense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Tags Per Minute by MLTP Season", xlab = "Season", ylab = "Tags Per Minute")


beeswarm(hpm ~ season, data = MLTP_offense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Hold Per Minute by MLTP Season", xlab = "Season", ylab = "Hold Per Minute")

beeswarm(cpm ~ season, data = MLTP_defense_stats, pch = 16, col = rainbow(10), method = "hex",
         main = "Caps Per Minute by MLTP Season", xlab = "Season", ylab = "Captures Per Minute")


#big_pile <- ggplot(MLTP_filtered_stats, aes(x = ppm, y = success_rate, color = season)) + geom_point() + theme(
#  text = element_text(family = "Bookman"),
#  title = element_text(color = "gray25"),
#  plot.caption = element_text(color = "gray30"),
#  plot.subtitle = element_text(size = 12)
#) + facet_wrap(~position) + theme(
#  plot.background = element_rect(fill = "gray95"))