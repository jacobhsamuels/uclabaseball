library(readr)
library(dplyr)
library(tidyverse)

Pitcher_Name <- function(Pitcher) {
  Name.List <- str_split(Pitcher, ", ")
  Name <- str_glue(Name.List[[1]][2], Name.List[[1]][1], .sep = " ")
  return(Name)
}

mround <- function(x,base){ 
  base*round(x/base) 
} 

BIP_data <- function(Game, pitcher) {
  player <- Pitcher_Name(pitcher)
  team <- names(table(Game$PitcherTeam))
  cat(player, "for", team,"\n\n")
  
  Trackman <- Game %>% filter(Pitcher == pitcher, TaggedPitchType != "Undefined") %>% 
    arrange(Date, Inning, PAofInning, PitchofPA)
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
  BB <- Trackman %>% filter(KorBB == "Walk", PitchCall == "BallCalled") %>% nrow()
  IBB <- Trackman %>% filter(KorBB == "Walk", PitchCall == "BallIntentional") %>% nrow()
  HBP <- Trackman %>% filter(PitchCall == "HitByPitch") %>% nrow()
  
  In_play <- Trackman %>% filter(PitchCall == "InPlay")
  GB <- In_play %>% filter(HitType == "GroundBall" | PitchCall == "Bunt") %>% nrow()
  LD <- In_play %>% filter(HitType == "LineDrive") %>% nrow()
  FB <- In_play %>% filter(HitType == "FlyBall") %>% nrow()
  PU <- In_play %>% filter(HitType == "Popup") %>% nrow()
  BIP <- In_play %>% nrow()
  
  BABIP <- round(Hits/BIP, 3)
  EV <- round(mean(In_play$ExitSpeed, na.rm = T), 1)
  Hard_hits <- In_play %>% filter(ExitSpeed >= 95) %>% nrow()
  
  HH_rate <- round(Hard_hits/BIP, 2)
  GB_rate <- round(GB/BIP, 2)
  LD_rate <- round(LD/BIP, 2)
  FB_rate <- round(FB/BIP, 2)
  PU_rate <- round(PU/BIP, 2)
  
  OBP <- (Hits + BB + IBB + HBP)/PA
  SLG <- (Hits + Doubles + 2*Triples + 3*HR)/AB
  OPS <- round(OBP + SLG, 3)
  
  Info <- c(BABIP, EV, HH_rate, OPS, SLG = round(SLG, 3), Hits, AB, 
            Doubles, Triples, HR, GB_rate, LD_rate, FB_rate, PU_rate)
  cat(Info, "\n\n")
}

pitch_by_pitch <- function(Game, type, splits = FALSE, Reliever = FALSE) {
  Info <- type
  Pitches <- nrow(Game)
  Pitch <- Game %>% filter(TaggedPitchType == type)
  if (nrow(Pitch) != 0) {
    if (splits) {
      BIP <- Pitch %>% nrow()
      Hits <- Pitch %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                                 PlayResult == "Triple" | PlayResult == "HomeRun") %>% nrow()
      BABIP <- round(Hits/BIP, 3)
      EV <- round(mean(Pitch$ExitSpeed, na.rm = T), 1)
      Hard_hits <- Pitch %>% filter(ExitSpeed >= 95) %>% nrow()
      
      HH_rate <- round(Hard_hits/BIP, 2)
      
      two <- Pitch %>% filter(Strikes == 2)
      BIP2 <- two %>% nrow()
      Hits2 <- two %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                                PlayResult == "Triple" | PlayResult == "HomeRun") %>% nrow()
      BABIP2 <- round(Hits2/BIP2, 3)
      
      Info <- c(BABIP, BABIP2, HH_rate, EV)
      Info[is.nan(Info)] <- "NA"
      Info <- c(type, Info)
    }
    
    else if (Reliever) {
      count <- nrow(Pitch)
      Usage <- round(count/Pitches, 2)
      PitchVelo <- round(mean(Pitch$RelSpeed, na.rm = T))
      PitchHigh <- round(summary(Pitch$RelSpeed, na.rm = T)[[5]])
      PitchLow <- round(summary(Pitch$RelSpeed, na.rm = T)[[2]])
      
      AvgSpin <- mround(mean(Pitch$SpinRate, na.rm = T), 50)
      swings <- Pitch %>% filter(PitchCall == "FoulBall" | 
                                   PitchCall == "InPlay" |
                                   PitchCall == "StrikeSwinging")
      misses <- swings %>% filter(PitchCall == "StrikeSwinging")
      sw_str <- round(nrow(misses)/nrow(swings), 2)
      
      left <- Pitch %>% filter(BatterSide == "Left")
      right <- Pitch %>% filter(BatterSide == "Right")
      l_pitches <- Game %>% filter(BatterSide == "Left") %>% nrow()
      r_pitches <- Game %>% filter(BatterSide == "Right") %>% nrow()
      l_usage <- round(nrow(left)/l_pitches, 2)
      r_usage <- round(nrow(right)/r_pitches, 2)
      
      Info <- c(type, PitchVelo, PitchLow, PitchHigh, AvgSpin, 
                Usage, l_usage, r_usage, sw_str)
    }
    
    else {
      count <- nrow(Pitch)
      Usage <- round(count/Pitches, 2)
      PitchVelo <- round(mean(Pitch$RelSpeed, na.rm = T))
      PitchHigh <- round(summary(Pitch$RelSpeed, na.rm = T)[[5]])
      PitchLow <- round(summary(Pitch$RelSpeed, na.rm = T)[[2]])
      
      AvgSpin <- mround(mean(Pitch$SpinRate, na.rm = T), 50)
      MaxSpin <- mround(quantile(Pitch$SpinRate, 0.9, na.rm = T), 50)
      
      height <- round(mean(Pitch$RelHeight, na.rm = T), 1)
      extension <- round(mean(Pitch$Extension, na.rm = T), 1)
      
      In_play <- Pitch %>% filter(PitchCall == "InPlay")
      BIP <- In_play %>% nrow()
      Hits <- In_play %>% filter(PlayResult == "Single" | PlayResult == "Double" | 
                                   PlayResult == "Triple" | PlayResult == "HomeRun") %>% nrow()
      BABIP <- round(Hits/BIP, 3)
      EV <- round(mean(In_play$ExitSpeed, na.rm = T), 1)
      Hard_hits <- In_play %>% filter(ExitSpeed >= 95) %>% nrow()
      
      HH_rate <- round(Hard_hits/BIP, 2)
      LA <- round(mean(In_play$Angle, na.rm = T), 1)
      
      if (is.nan(BABIP) | is.nan(EV)) {
        Info <- c(type, PitchVelo, PitchLow, PitchHigh, 
                  AvgSpin, MaxSpin, Usage, height, extension)
      }
      else Info <- c(type, PitchVelo, PitchLow, PitchHigh, AvgSpin, MaxSpin, 
                     Usage, height, extension, BABIP, HH_rate, EV, LA)
    }
  }
  Info
}

Pitch_Type <- function(Trackman, pitcher) {
  Game <- Trackman %>% filter(Pitcher == pitcher, TaggedPitchType != "Undefined") %>% 
    arrange(Date, Inning, PAofInning, PitchofPA)
  
  FB <- pitch_by_pitch(Game, "Fastball")
  CB <- pitch_by_pitch(Game, "Curveball")
  SL <- pitch_by_pitch(Game, "Slider")
  CH <- pitch_by_pitch(Game, "ChangeUp")
  CT <- pitch_by_pitch(Game, "Cutter")
  SI <- pitch_by_pitch(Game, "Sinker")
  SP <- pitch_by_pitch(Game, "Splitter")
  cat(FB, "\n\n", CB, "\n\n", SL, "\n\n", CH, "\n\n", CT, "\n\n", SI, "\n\n", SP, "\n\n")
}

Batter_Splits <- function(Trackman, pitcher) {
  Game <- Trackman %>% filter(Pitcher == pitcher, TaggedPitchType != "Undefined") %>% 
    arrange(Date, Inning, PAofInning, PitchofPA)
  In_play <- Game %>% filter(PitchCall == "InPlay")
  
  sides <- c("Left", "Right")
  for (i in seq_along(sides)) {
    split <- In_play %>% filter(BatterSide == sides[i])
    FB <- pitch_by_pitch(split, "Fastball", splits = TRUE)
    CB <- pitch_by_pitch(split, "Curveball", splits = TRUE)
    SL <- pitch_by_pitch(split, "Slider", splits = TRUE)
    CH <- pitch_by_pitch(split, "ChangeUp", splits = TRUE)
    CT <- pitch_by_pitch(split, "Cutter", splits = TRUE)
    SI <- pitch_by_pitch(split, "Sinker", splits = TRUE)
    SP <- pitch_by_pitch(split, "Splitter", splits = TRUE)
    cat(FB, "\n\n", CB, "\n\n", SL, "\n\n", CH, "\n\n", CT, "\n\n", SI, "\n\n", SP, "\n\n")
  }
  
  cat("\n\n\\pagebreak\n")
}


Reliever_Report <- function(Trackman, pitcher) {
  Game <- Trackman %>% filter(Pitcher == pitcher, TaggedPitchType != "Undefined") %>% 
    arrange(Date, Inning, PAofInning, PitchofPA)
  
  player <- Pitcher_Name(pitcher)
  team <- names(table(Trackman$PitcherTeam))
  arm <- names(table(Game$PitcherThrows))
  
  if (arm == "Left") cat(player, "for", team, "- (LHP)\n\n")
  else if (arm == "Right") cat(player, "for", team, "- (RHP)\n\n")
  else cat(player, "for", team, "- IDKHP\n\n")
  
  FB <- pitch_by_pitch(Game, "Fastball", Reliever = TRUE)
  CB <- pitch_by_pitch(Game, "Curveball", Reliever = TRUE)
  SL <- pitch_by_pitch(Game, "Slider", Reliever = TRUE)
  CH <- pitch_by_pitch(Game, "ChangeUp", Reliever = TRUE)
  CT <- pitch_by_pitch(Game, "Cutter", Reliever = TRUE)
  SI <- pitch_by_pitch(Game, "Sinker", Reliever = TRUE)
  SP <- pitch_by_pitch(Game, "Splitter", Reliever = TRUE)
  cat(FB, "\n\n", CB, "\n\n", SL, "\n\n", CH, "\n\n", CT, "\n\n", SI, "\n\n", SP, "\n\n")
  
  cat("\n\n\\pagebreak\n")
}
