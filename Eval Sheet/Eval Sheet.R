library(readr)
library(dplyr)
library(stringr)
library(crayon)

Pitcher_Eval <- function(Trackman, player) {
  Name.List <- str_split(player, ", ")
  Name <- str_glue(Name.List[[1]][2], Name.List[[1]][1], .sep = " ")
  
  Game <- Trackman %>% filter(Pitcher == player, PitchCall != "Undefined", 
                              TaggedPitchType != "Undefined") %>% 
    arrange(Date, Inning, PAofInning, PitchofPA)
  Pitches <- nrow(Game)
  Batters_Faced <- Game %>% filter(PitchofPA == "1") %>% nrow()
  Pitches_perPA <- Pitches/Batters_Faced
  
  First_Pitch_K <- Game %>% filter(PitchofPA == "1", PitchCall == "FoulBall" | 
                                     PitchCall == "InPlay" | PitchCall == "StrikeCalled" | 
                                     PitchCall == "StrikeSwinging") %>% nrow()
  First_K_Rate <- First_Pitch_K/Batters_Faced
  
  Ks <- Game %>% filter(PitchCall == "FoulBall" | PitchCall == "InPlay" | 
                          PitchCall == "StrikeCalled" | PitchCall == "StrikeSwinging") %>% nrow()
  Strike_Rate <- Ks/Pitches
  Bs <- Pitches - Ks
  Ball_Rate <- Bs/Pitches
  
  Strikeouts <- Game %>% filter(KorBB == "Strikeout") %>% nrow()
  Walks <- Game %>% filter(KorBB == "Walk") %>% nrow()
  Hits <- Game %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                            PlayResult == "Triple" | PlayResult == "HomeRun") %>% nrow()
  
  First_Pitch_Rows <- integer(0)
  for (i in 1:nrow(Game)) {
    if (Game$PitchofPA[i] == 1) First_Pitch_Rows <- c(First_Pitch_Rows, i)
  }
  Check <- FALSE
  First_K_to_Out <- 0
  for (i in First_Pitch_Rows) {
    if (Check) {
      if (Game$KorBB[i-1] == "Strikeout" | Game$PlayResult[i-1] == "Error" | Game$PlayResult[i-1] == "Out") {
        First_K_to_Out <- First_K_to_Out + 1
      }
      Check <- FALSE
    }
    if (Game$PitchCall[i] == "FoulBall" | Game$PitchCall[i] == "InPlay" | 
        Game$PitchCall[i] == "StrikeCalled" | Game$PitchCall[i] == "StrikeSwinging") Check <- TRUE
  }
  nrow(Game)
  if (Check) {
    if (Game$KorBB[nrow(Game)] == "Strikeout" | Game$PlayResult[nrow(Game)] == "Error" | Game$PlayResult[nrow(Game)] == "Out") {
      First_K_to_Out <- First_K_to_Out + 1
    }
    Check <- FALSE
  }
  First_K_Out_Rate <- round(First_K_to_Out/First_Pitch_K, 2)
  
  First3 <- Game %>% filter(PitchofPA < 4)
  First3Y <- 0
  First3N <- -1
  set <- 0
  for (i in 1:nrow(First3)) {
    if (First3$PitchofPA[i] == 1) {
      ifelse (set >= 2, First3Y <- First3Y + 1, First3N <- First3N + 1) 
      set <- 0
    }
    if (First3$PitchofPA[i] != 3 & First3$PitchCall[i] == "InPlay") {
      set <- 2
    }
    if (First3$PitchCall[i] == "FoulBall" | First3$PitchCall[i] == "InPlay" | 
        First3$PitchCall[i] == "StrikeCalled" | 
        First3$PitchCall[i] == "StrikeSwinging") set <- set + 1
  }
  ifelse (set >= 2, First3Y <- First3Y + 1, First3N <- First3N + 1)
  First2of3 <- First3Y + First3N
  First2of3_Rate <- round(First3Y/First2of3, 2)
  
  FB <- Game %>% filter(TaggedPitchType == "Fastball")
  if (nrow(FB) != 0) {
    FBs <- nrow(FB)
    FB_Rate <- FBs/Pitches
    FB_K <- FB %>% filter(PitchCall == "FoulBall" | PitchCall == "InPlay" | 
                              PitchCall == "StrikeCalled" | 
                              PitchCall == "StrikeSwinging") %>% nrow()
    FB_K_Rate <- FB_K/FBs
    FBVelo <- round(mean(FB$RelSpeed, na.rm = T))
    FBHigh <- round(max(FB$RelSpeed, na.rm = T))
  }
  
  CB <- Game %>% filter(TaggedPitchType == "Curveball" | TaggedPitchType == "Cutter")
  if (nrow(CB) != 0) {
    CBs <- nrow(CB)
    CB_Rate <- CBs/Pitches
    CB_K <- CB %>% filter(PitchCall == "FoulBall" | PitchCall == "InPlay" | 
                            PitchCall == "StrikeCalled" | 
                            PitchCall == "StrikeSwinging") %>% nrow()
    CB_K_Rate <- CB_K/CBs
    CBVelo <- round(mean(CB$RelSpeed, na.rm = T))
    CBHigh <- round(max(CB$RelSpeed, na.rm = T))
    if (nrow(FB) != 0) {
      CB_Bottom <- format(FBVelo - CBHigh, nsmall = 1)
      CB_Top <- format(FBHigh - CBVelo, nsmall = 1)
    }
  }
  
  SL <- Game %>% filter(TaggedPitchType == "Slider")
  if (nrow(SL) != 0) {
    SLs <- nrow(SL)
    SL_Rate <- SLs/Pitches
    SL_K <- SL %>% filter(PitchCall == "FoulBall" | PitchCall == "InPlay" | 
                            PitchCall == "StrikeCalled" | 
                            PitchCall == "StrikeSwinging") %>% nrow()
    SL_K_Rate <- SL_K/SLs
    SLVelo <- round(mean(SL$RelSpeed, na.rm = T))
    SLHigh <- round(max(SL$RelSpeed, na.rm = T))
    if (nrow(FB) != 0) {
      SL_Bottom <- format(FBVelo - SLHigh, nsmall = 1)
      SL_Top <- format(FBHigh - SLVelo, nsmall = 1)
    }
  }
  
  CH <- Game %>% filter(TaggedPitchType == "ChangeUp")
  if (nrow(CH) != 0) {
    CHs <- nrow(CH)
    CH_Rate <- CHs/Pitches
    CH_K <- CH %>% filter(PitchCall == "FoulBall" | PitchCall == "InPlay" | 
                            PitchCall == "StrikeCalled" | 
                            PitchCall == "StrikeSwinging") %>% nrow()
    CH_K_Rate <- CH_K/CHs
    CHVelo <- round(mean(CH$RelSpeed, na.rm = T))
    CHHigh <- round(max(CH$RelSpeed, na.rm = T))
    if (nrow(FB) != 0) {
      CH_Bottom <- format(FBVelo - CHHigh, nsmall = 1)
      CH_Top <- format(FBHigh - CHVelo, nsmall = 1)
    }
  }
  
  PO <- Game %>% filter(TaggedPitchType == "Other")
  if (nrow(PO) != 0) {
    POs <- nrow(PO)
    PO_Rate <- POs/Pitches
    PO_K <- PO %>% filter(PitchCall == "FoulBall" | PitchCall == "InPlay" | 
                            PitchCall == "StrikeCalled" | 
                            PitchCall == "StrikeSwinging") %>% nrow()
    PO_K_Rate <- PO_K/POs
    POVelo <- round(mean(PO$RelSpeed, na.rm = T))
    POHigh <- round(max(PO$RelSpeed, na.rm = T))
    if (nrow(FB) != 0) {
      PO_Bottom <- format(FBVelo - POHigh, nsmall = 1)
      PO_Top <- format(FBHigh - POVelo, nsmall = 1)
    }
  }
  
  CB_Attempts <- 0
  CB_Winners <- 0
  SL_Attempts <- 0
  SL_Winners <- 0
  for (i in First_Pitch_Rows) {
    if (Game$TaggedPitchType[i] == "Curveball" | Game$TaggedPitchType[i] == "Cutter") {
      CB_Attempts <- CB_Attempts + 1
      if (Game$PlayResult[i] == "Error" | Game$PlayResult[i] == "Out" | Game$PitchCall[i] == "FoulBall" | 
          Game$PitchCall[i] == "StrikeCalled" | Game$PitchCall[i] == "StrikeSwinging") {
        CB_Winners <- CB_Winners + 1
      }
    }
    if (Game$TaggedPitchType[i] == "Slider") {
      SL_Attempts <- SL_Attempts + 1
      if (Game$PlayResult[i] == "Error" | Game$PlayResult[i] == "Out" | Game$PitchCall[i] == "FoulBall" |
          Game$PitchCall[i] == "StrikeCalled" | Game$PitchCall[i] == "StrikeSwinging") {
        SL_Winners <- SL_Winners + 1
      }
    }
  }
  CB_Winner_Rate <- round(CB_Winners/CB_Attempts, 2)
  SL_Winner_Rate <- round(SL_Winners/SL_Attempts, 2)
  
  Full_Counts <- Game %>% filter(Balls == 3, Strikes == 2)
  Full_Attempts <- 0
  Full_Winners <- 0
  for (i in seq_len(nrow(Full_Counts))) {
    if (Full_Counts$PitchCall[i] != "FoulBall") {
      Full_Attempts <- Full_Attempts + 1
      if (Full_Counts$KorBB[i] == "Strikeout" | Full_Counts$PlayResult[i] == "Error"
          | Full_Counts$PlayResult[i] == "Out") Full_Winners <- Full_Winners + 1
    }
  }
  Full_Winner_Rate <- round(Full_Winners/Full_Attempts, 2)
  
  Even_Counts <- Game %>% filter(Balls == 1, Strikes == 1)
  Even_Attempts <- 0
  Even_Winners <- 0
  for (i in seq_len(nrow(Even_Counts))) {
    Even_Attempts <- Even_Attempts + 1
    if (Even_Counts$PlayResult[i] == "Error" | Even_Counts$PlayResult[i] == "Out" | 
        Even_Counts$PitchCall[i] == "FoulBall" | Even_Counts$PitchCall[i] == "StrikeCalled"
        | Even_Counts$PitchCall[i] == "StrikeSwinging") Even_Winners <- Even_Winners + 1
  }
  Even_Winner_Rate <- round(Even_Winners/Even_Attempts, 2)
  
  exit_velo <- Game[-which(is.na(Game$ExitSpeed)),] %>% filter(PitchCall == "InPlay")
  Hard_Hit <- 0
  for (i in seq_len(nrow(exit_velo))) {
    if (exit_velo$ExitSpeed[i] >= 95) Hard_Hit <- Hard_Hit + 1
  }
  Ground_Balls <- Game %>% filter(PitchCall == "InPlay", HitType == "GroundBall") %>% nrow()
  Fly_Balls <- Game %>% filter(PitchCall == "InPlay", HitType == "FlyBall") %>% nrow()
  
  Negative_Rows <- integer(0)
  for (i in 1:nrow(Game)) {
    if (Game$PlayResult[i] == "Single"|Game$PlayResult[i] == "Double"|Game$PlayResult[i] == "Triple"|
        Game$PlayResult[i] == "HomeRun"|Game$PlayResult[i] == "Error"|Game$KorBB[i] == "Walk"|
        Game$PitchCall[i] == "HitByPitch") Negative_Rows <- c(Negative_Rows, i)
  }
  Negative_Rows
  Negatives <- length(Negative_Rows)
  Check <- FALSE
  NTP <- 0
  for (i in First_Pitch_Rows) {
    if (Check) {
      if (Game$PAofInning[i-1] == 1) Negatives <- Negatives - 1
      else if (Game$KorBB[i-1] == "Strikeout" | Game$PlayResult[i-1] == "Error" | Game$PlayResult[i-1] == "Out") {
        NTP <- NTP + 1
      }
      Check <- FALSE
    }
    if (any(Negative_Rows == i - 1)) Check <- TRUE
  }
  if (Check) {
    if (Game$PAofInning[nrow(Game)] == 1) Negatives <- Negatives - 1
    else if (Game$KorBB[nrow(Game)] == "Strikeout" | Game$PlayResult[nrow(Game)] == "Error" | Game$PlayResult[nrow(Game)] == "Out") {
      NTP <- NTP + 1
    }
    Check <- FALSE
  }
  Outs <- Game %>% filter(KorBB == "Strikeout") %>% nrow() + sum(Game$OutsOnPlay)
  if (Outs %% 3 == 1) {
    IP <- Outs %/% 3 + .1
  }
  else if (Outs %% 3 == 2) {
    IP <- Outs %/% 3 + .2
  }
  else IP <- Outs/3
  
  cat("**", Name, "**\n\n", sep = "")
  cat(Pitches, "pitches to", Batters_Faced, "batters faced, an average of", round(Pitches_perPA, 2), "pitches per hitter.\n\n")
  cat(First_Pitch_K, "of", Batters_Faced, "first pitches (", round(First_K_Rate, 2), "% ) for Strikes.\n\n")
  cat(Ks, "of", Pitches, "pitches (", round(Strike_Rate, 2), "% ) for Strikes.\n\n")
  cat(Bs, "of", Pitches, "pitches (", round(Ball_Rate, 2), "% ) for Balls.\n\n")
  cat(Strikeouts, "Strikeouts,", Walks, "Walks, and", Hits, "Hits.\n\n")
  if (First_Pitch_K != 0) cat(First_K_to_Out, "of", First_Pitch_K, "at bats with first pitch strikes (", First_K_Out_Rate, "% ) ended in outs.\n\n")
  else cat(First_K_to_Out, "of", First_Pitch_K, "ended in outs.\n\n")
  cat(First3Y, "of", First2of3, "times (", First2of3_Rate, "% ) 2 of the first 3 pitches were strikes.\n\n")
  if (nrow(FB) != 0) {
    cat("FB:\t", FBs, "total Fastballs,", round(FB_Rate, 2), "% of all pitches.\n\n")
    cat("FB:\t", FB_K, "of", FBs, "Fastballs (", round(FB_K_Rate, 2), "% ) for Strikes.\n\n")
    cat("FB:\tAve. Velocity:", FBVelo, "\tHigh:", FBHigh, "\n\n")
  }
  else {
    cat("FB:\n\n")
    cat("FB:\n\n")
    cat("FB:\n\n")
  }
  if (nrow(CB) != 0) {
    cat("CB:\t", CBs, "total Curveballs,", round(CB_Rate, 2), "% of all pitches.\n\n")
    cat("CB:\t", CB_K, "of", CBs, "Curveballs (", round(CB_K_Rate, 2), "% ) for Strikes.\n\n")
    if (nrow(FB) != 0) cat("CB:\tAve. Velocity:", CBVelo, "\tHigh:", CBHigh, "\tDifferential:", CB_Bottom, "-", CB_Top, "\n\n")
    else cat("CB:\tAve. Velocity:", CBVelo, "\tHigh:", CBHigh, "\tDifferential:\n\n")
  }
  else {
    cat("CB:\n\n")
    cat("CB:\n\n")
    cat("CB:\n\n")
  }
  if (nrow(SL) != 0) {
    cat("SL:\t", SLs, "total Sliders,", round(SL_Rate, 2), "% of all pitches.\n\n")
    cat("SL:\t", SL_K, "of", SLs, "Sliders (", round(SL_K_Rate, 2), "% ) for Strikes.\n\n")
    if (nrow(FB) != 0) cat("SL:\tAve. Velocity:", SLVelo, "\tHigh:", SLHigh, "\tDifferential:", SL_Bottom, "-", SL_Top, "\n\n")
    else cat("SL:\tAve. Velocity:", SLVelo, "\tHigh:", SLHigh, "\tDifferential:\n\n")
  }
  else {
    cat("SL:\n\n")
    cat("SL:\n\n")
    cat("SL:\n\n")
  }
  if (nrow(CH) != 0) {
    cat("CH:\t", CHs, "total Changeups,", round(CH_Rate, 2), "% of all pitches.\n\n")
    cat("CH:\t", CH_K, "of", CHs, "Changeups (", round(CH_K_Rate, 2), "% ) for Strikes.\n\n")
    if (nrow(FB) != 0) cat("CH:\tAve. Velocity:", CHVelo, "\tHigh:", CHHigh, "\tDifferential:", CH_Bottom, "-", CH_Top, "\n\n")
    else cat("CH:\tAve. Velocity:", CHVelo, "\tHigh:", CHHigh, "\tDifferential:\n\n")
  }
  else {
    cat("CH:\n\n")
    cat("CH:\n\n")
    cat("CH:\n\n")
  }
  if (nrow(PO) != 0) {
    cat("Other:\t", POs, "total Others,", round(PO_Rate, 2), "% of all pitches.\n\n")
    cat("Other:\t", PO_K, "of", POs, "Others (", round(PO_K_Rate, 2), "% ) for Strikes.\n\n")
    if (nrow(FB) != 0) cat("Other:\tAve. Velocity:", POVelo, "\tHigh:", POHigh, "\tDifferential:", PO_Bottom, "-", PO_Top, "\n\n")
    else cat("Other:\tAve. Velocity:", POVelo, "\tHigh:", POHigh, "\tDifferential:\n\n")
  }
  else {
    cat("Other:\n\n")
    cat("Other:\n\n")
    cat("Other:\n\n")
  }
  if (SL_Attempts == 0) {
    SL_Attempts <- "-"
    SL_Winners <- "\\-"
    SL_Winner_Rate <- "-"
  }
  if (CB_Attempts == 0) {
    CB_Attempts <- "-"
    CB_Winners <- "\\-"
    CB_Winner_Rate <- "-"
  }
  if (Full_Attempts == 0) {
    Full_Attempts <- "-"
    Full_Winners <- "\\-"
    Full_Winner_Rate <- "-"
  }
  if (Even_Attempts == 0) {
    Even_Attempts <- "-"
    Even_Winners <- "\\-"
    Even_Winner_Rate <- "-"
  }
  cat(SL_Winners, "of", SL_Attempts, "0-0 SLs (", SL_Winner_Rate, "% ) were winners.\n\n")
  cat(CB_Winners, "of", CB_Attempts, "0-0 CBs (", CB_Winner_Rate, "% ) were winners.\n\n") 
  cat(Full_Winners, "of", Full_Attempts, "3-2 Counts (", Full_Winner_Rate, "% ) were winners.\n\n")
  cat(Even_Winners, "of", Even_Attempts, "1-1 Counts (", Even_Winner_Rate, "% ) were winners.\n\n")
  cat(Hard_Hit, "Hard Hit Balls,", Ground_Balls, "Ground Balls, and", Fly_Balls, "Fly Balls.\n\n")
  cat(NTP, "out of", Negatives, "NTP chances were successfull.\n\n")
  cat(IP, "Innings Pitched.\n\n")
  cat("\n\n\\pagebreak\n")
}