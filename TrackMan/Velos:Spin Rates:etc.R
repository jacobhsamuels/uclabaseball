library(readr)
library(tidyverse)
library(ggplot2)
library(magick)
library(stringr)

Axis_Tilt <- function(TrackMan, player) {
  img <- image_read("~/UCLA Baseball/TrackMan/silhouette.jpg")
  rast <- as.raster(image_fill(img, "none"))
  
  TrackMan <- TrackMan %>% 
    mutate(PitchOrder = ifelse(TaggedPitchType == "Fastball",1, 
                               ifelse(TaggedPitchType == "Slider",3,
                                      ifelse(TaggedPitchType == "ChangeUp",4,2))), 
           PitchColor = ifelse(TaggedPitchType == "Fastball","black", 
                               ifelse(TaggedPitchType == "Slider","cornflowerblue",
                                      ifelse(TaggedPitchType == "ChangeUp","chartreuse",
                                             ifelse(TaggedPitchType == "Curveball", "orange", "purple"))))) 
  Name.List <- str_split(player, ", ")
  Name <- str_glue(Name.List[[1]][2], Name.List[[1]][1], .sep = " ")
  
  OneGuy <- TrackMan %>% filter(Pitcher == player)
  
  clock_dec <- sapply(str_split(OneGuy$Tilt,":"),
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      }
  )
  for (i in seq_along(clock_dec)) {
    if(clock_dec[i] > 12) clock_dec[i] = clock_dec[i] - 12
  }
  OneGuy$clock <- clock_dec
  
  player <- OneGuy %>% group_by(TaggedPitchType) %>% 
    summarise(Min.Velo = round(min(RelSpeed, na.rm = T)),
              Max.Velo = round(max(RelSpeed, na.rm = T)),
              Avg.Velocity = round(mean(RelSpeed, na.rm = T)), 
              SpinRate = round(mean(SpinRate, na.rm = T)), 
              Extension = format(round(mean(Extension, na.rm = T),1), nsmall = 1),
              RelHeight = round(mean(RelHeight, na.rm = T),2),
              RelSide = round(mean(RelSide, na.rm = T),2),
              Pitch = mean(PitchOrder), PitchColor = unique(PitchColor)) %>% 
    arrange(Pitch) %>% select(!Pitch)
  
  player$TaggedPitchType <- factor(player$TaggedPitchType, 
                                   levels = c("Fastball", "Curveball", "Cutter", "Slider", "ChangeUp"))
  
  OneGuy$TaggedPitchType <- factor(OneGuy$TaggedPitchType, 
                                   levels = c("Fastball", "Curveball", "Cutter", "Slider", "ChangeUp"))
  
  
  releases <- ggplot(data = player, aes(RelSide, RelHeight, col = TaggedPitchType)) +
    geom_point(cex = 1.6) +
    scale_color_manual(name = "Pitch Type", values= player$PitchColor) +
    xlim(-3,3) + ylim(0,7) +
    annotation_raster(rast, -2,2,0,6) +
    ggtitle(Name) + xlab("Release Point\n(view from behind pitcher)") +
    theme(plot.title = element_text(hjust = .5), axis.title.y = element_blank())
  print(releases)
  
  tilt <- ggplot(OneGuy, aes(clock, 24, col = TaggedPitchType)) +
    geom_jitter(cex=2) +
    ylim(0,27) +
    coord_polar(start = 0) +
    scale_color_manual(name = "Tilt on Baseball\nby Pitch Type", values = player$PitchColor) + 
    scale_x_continuous("", limits = c(0,12), breaks = seq(1,12), labels = seq(1,12)) +
    ggtitle(Name) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title.y = element_blank(), plot.title = element_text(hjust = .5))
  print(tilt)
  
  Finaltable <- player[,1:6]
  names(Finaltable) <- c("Pitch Type", "Min", "Max", "Avg",
                         "Spin Rate", "Extension")
  Finaltable$Pitcher <- Name 
  print(Finaltable)
}