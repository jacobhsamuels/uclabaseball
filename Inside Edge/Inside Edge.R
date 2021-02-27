library(readr)
library(dplyr)
library(tidyverse)

Pitcher_Name <- function(Pitcher) {
  Name.List <- str_split(Pitcher, ", ")
  Name <- str_glue(Name.List[[1]][2], Name.List[[1]][1], .sep = " ")
  return(Name)
}

IE_top_line <- function(Trackman) {
  Outs <- Trackman %>% filter(KorBB == "Strikeout") %>% nrow() + sum(Trackman$OutsOnPlay)
  if (Outs %% 3 == 1) {
    IP <- Outs %/% 3 + .1
  }
  else if (Outs %% 3 == 2) {
    IP <- Outs %/% 3 + .2
  }
  else IP <- Outs/3
  Pitches <- nrow(Trackman) #includes intentional Balls
  PA <- Trackman %>% filter(PlayResult != "Undefined" | KorBB != "Undefined" |
                          PitchCall == "HitByPitch") %>% nrow()
  AB <- Trackman %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                          PlayResult == "Triple" | PlayResult == "HomeRun" | 
                          PlayResult == "Error" | PlayResult == "FieldersChoice" | 
                          PlayResult == "Out" | KorBB == "Strikeout") %>% nrow()
  Hits <- Trackman %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                            PlayResult == "Triple" | PlayResult == "HomeRun") %>% nrow()
  Doubles <- Trackman %>% filter(PlayResult == "Double") %>% nrow()
  Triples <- Trackman %>% filter(PlayResult == "Triple") %>% nrow()
  HR <- Trackman %>% filter(PlayResult == "HomeRun") %>% nrow()
  KS <- Trackman %>% filter(KorBB == "Strikeout", PitchCall == "StrikeSwinging") %>% nrow()
  KC <- Trackman %>% filter(KorBB == "Strikeout", PitchCall == "StrikeCalled") %>% nrow()
  BB <- Trackman %>% filter(KorBB == "Walk", PitchCall == "BallCalled") %>% nrow()
  IBB <- Trackman %>% filter(KorBB == "Walk", PitchCall == "BallIntentional") %>% nrow()
  HBP <- Trackman %>% filter(PitchCall == "HitByPitch") %>% nrow()
  Pitches_perPA <- Pitches/PA
  Info <- c(IP, Pitches, PA, AB, H = Hits, Doubles, Triples, 
                     HR, KS, KC, BB, IBB, HBP, Pitches_perPA)
  cat(Info[1], "IP,", Info[2], "TP's,", Info[3], "PA,", Info[4], "AB,", Info[5], "H,", 
      Info[6], "2B,", Info[7], "3B,", Info[8], "HR,", Info[9], "KS,", Info[10], "KC,", 
      Info[11], "BB,", Info[12], "IBB,", Info[13], "HBP, and", round(Info[14], 1), "Pit's/PA\n\n")
  # colnames(Info)[c(2,6:7,14)] <- c("TP's", "2B","3B","Pit's/PA")
  # grid.table(Info)
}

Avg_Velo_by_Pitch_Count <- function(Trackman, pitch) {
  results <- rep(0, 3)
  Pitch <- Trackman %>% filter(TaggedPitchType == pitch)
  if (nrow(Pitch) != 0) {
    PitchAvg <- round(mean(Pitch$RelSpeed, na.rm = T))
    PitchHigh <- round(max(Pitch$RelSpeed, na.rm = T))
    PitchLow <- round(min(Pitch$RelSpeed, na.rm = T))
    results <- c(PitchAvg, PitchLow, PitchHigh)
    every15 <- integer(0)
    while (nrow(Pitch) > 15) {
      NewAvg <- round(mean(Pitch[1:15,]$RelSpeed, na.rm = T))
      every15 <- c(every15, NewAvg)
      Pitch <- Pitch[-c(1:15),]
    }
    NewAvg <- round(mean(Pitch$RelSpeed, na.rm = T))
    every15 <- c(every15, NewAvg)
    cat(pitch, ": Avg: ", results[1], " Low: ", results[2], " High: ", results[3], sep = "")
    for (i in seq_along(every15)) {
      cat(" ", (i-1)*15 + 1, "-", i*15, ": ", every15[i], sep = "")
    }
    cat("\n\n")
  }
  else cat(pitch, ": \n\n", sep = "")
}

Pitch_breakdown <- function(Trackman, pitch, side) {
  Side <- Trackman %>% filter(BatterSide == side)
  Pitch <- Side %>% filter(TaggedPitchType == pitch)
  First <- Pitch %>% filter(PitchofPA == 1)
  Total_Firsts <- Side %>% filter(PitchofPA == 1) %>% nrow()
  Line <- rep(0, 14)
  if (nrow(Pitch) != 0) {
    Pitches <- nrow(Pitch)
    Pitch_Rate <- round((Pitches/nrow(Side))*100)
    Pitch_K <- Pitch %>% filter(PitchCall == "FoulBall" | PitchCall == "InPlay" | 
                                  PitchCall == "StrikeCalled" | 
                                  PitchCall == "StrikeSwinging") %>% nrow()
    Pitch_K_Rate <- round((Pitch_K/Pitches)*100)
    Pitch_Hits <- Pitch %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                                     PlayResult == "Triple" | PlayResult == "HomeRun") %>% nrow()
    Pitch_AB <- Pitch %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                                   PlayResult == "Triple" | PlayResult == "HomeRun" | 
                                   PlayResult == "Error" | PlayResult == "FieldersChoice" | 
                                   PlayResult == "Out" | KorBB == "Strikeout") %>% nrow()
    Pitch_BA <- round(Pitch_Hits/Pitch_AB, 3)
    if (is.nan(Pitch_BA)) Pitch_BA <- 0
    Line[1:7] <- c(Pitches, Pitch_Rate, Pitch_K, Pitch_K_Rate, Pitch_Hits, Pitch_AB, Pitch_BA)
  }
  if (nrow(First) != 0) {
    Firsts <- nrow(First)
    First_Rate <- round((Firsts/Total_Firsts)*100)
    First_K <- First %>% filter(PitchCall == "FoulBall" | PitchCall == "InPlay" | 
                                  PitchCall == "StrikeCalled" | 
                                  PitchCall == "StrikeSwinging") %>% nrow()
    First_K_Rate <- round((First_K/Firsts)*100)
    First_Hits <- First %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                                     PlayResult == "Triple" | PlayResult == "HomeRun") %>% nrow()
    First_AB <- First %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                                   PlayResult == "Triple" | PlayResult == "HomeRun" | 
                                   PlayResult == "Error" | PlayResult == "FieldersChoice" | 
                                   PlayResult == "Out" | KorBB == "Strikeout") %>% nrow()
    First_BA <- round(First_Hits/First_AB, 3)
    if (is.nan(First_BA)) First_BA <- 0
    Line[8:14] <- c(Firsts, First_Rate, First_K, First_K_Rate, First_Hits, First_AB, First_BA)
  }
  cat(pitch, ": Pitches: ", Line[1], " ", Line[2], "%", " Strikes: ", Line[4], "% (", Line[3], "/", Line[1], 
      ") Opp. BA: ", Line[7], " (", Line[5], "/", Line[6], ")\n\n", "1st Pitch ", pitch, ": Pitches: ", Line[8], 
      " ", Line[9], "%", " Strikes: ", Line[11], "% (", Line[10], "/", Line[8], ") Opp. BA: ", Line[14], 
      " (", Line[12], "/", Line[13], ")\n\n", sep = "")
}

Inside_Edge <- function(Trackman, pitcher) {
  TrackMan <- Trackman %>% filter(Pitcher == pitcher, PitchCall != "Undefined", TaggedPitchType != "Undefined") %>% 
    arrange(Inning, PAofInning, PitchofPA)
  player <- Pitcher_Name(pitcher)
  team <- names(table(TrackMan$PitcherTeam))
  opponent <- names(table(TrackMan$BatterTeam))
  cat(player, team, "vs.", opponent, "\n\n")
  IE_top_line(TrackMan)
  PITCHES <- c("Fastball", "Curveball", "Slider", "ChangeUp", "Other")
  if (player == "Sean Mullen") PITCHES <- c("Fastball", "Cutter", "Slider", "ChangeUp", "Other")
  for (i in seq_along(PITCHES)) {
    Avg_Velo_by_Pitch_Count(TrackMan, PITCHES[i])
  }
  Right_PAs <- TrackMan %>% filter(PlayResult != "Undefined" | KorBB != "Undefined" |
                               PitchCall == "HitByPitch", BatterSide == "Right") %>% nrow()
  Left_PAs <- TrackMan %>% filter(PlayResult != "Undefined" | KorBB != "Undefined" |
                                     PitchCall == "HitByPitch", BatterSide == "Left") %>% nrow()
  cat("RHB's:", Right_PAs, "PA's\n\n")
  for (i in seq_along(PITCHES)) {
    Pitch_breakdown(TrackMan, PITCHES[i], "Right")
  }
  cat("LHB's:", Left_PAs, "PA's\n\n")
  for (i in seq_along(PITCHES)) {
    Pitch_breakdown(TrackMan, PITCHES[i], "Left")
  }
  cat("Spin Rates:\n\n")
  for (i in seq_along(PITCHES)) {
    Type <- TrackMan %>% filter(TaggedPitchType == PITCHES[i])
    if (nrow(Type) != 0) {
      Avg_Spin <- round(mean(Type$SpinRate, na.rm = TRUE))
      Best_Spin <- round(max(Type$SpinRate, na.rm = TRUE))
      cat(PITCHES[i], ": Average Spin: ", Avg_Spin, " Best Spin: ", Best_Spin, "\n\n", sep = "") 
    }
    else cat(PITCHES[i], ":\n\n")
  }
  cat("\n\n\\pagebreak\n")
}
