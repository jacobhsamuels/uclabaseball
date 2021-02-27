swing_rate <- function(player, TrackMan) {
  TrackMan[c(which(is.na(TrackMan$PlateLocHeight))),]
  FALL <- TrackMan[-c(which(is.na(TrackMan$PlateLocHeight))),]
  fall <- FALL %>% filter(Pitcher == player)
  
  fall$Swing <- rep(1, nrow(fall))
  for (i in 1:nrow(fall)) {
    if(fall$PitchCall[i] == "BallCalled") fall$Swing[i] <- 0
    if(fall$PitchCall[i] == "StrikeCalled") fall$Swing[i] <- 0
    if(fall$PitchCall[i] == "HitByPitch") fall$Swing[i] <- 0
    if(fall$PitchCall[i] == "BallIntentional") fall$Swing[i] <- 0
  }
  
  fall$High <- rep(0, nrow(fall))
  fall$Low <- rep(0, nrow(fall))
  fall$Lefty <- rep(0, nrow(fall))
  fall$Righty <- rep(0, nrow(fall))
  fall$StrikeZone <- rep(0, nrow(fall))
  for (i in 1:nrow(fall)) {
    fall$High[i] <- ifelse(fall$PlateLocHeight[i] > 3.5, 1, 0)
    fall$Low[i] <- ifelse(fall$PlateLocHeight[i] < 1.5, 1, 0)
    fall$Lefty[i] <- ifelse(fall$PlateLocSide[i] < -0.8333333, 1, 0)
    fall$Righty[i] <- ifelse(fall$PlateLocSide[i] > 0.8333333, 1, 0)
    fall$StrikeZone[i] <- ifelse(fall$High[i] == 0 && fall$Low[i] 
                        == 0 && fall$Lefty[i] == 0 && fall$Righty[i] == 0, 1, 0)
  }
  
  fall$SwingB <- rep(0, nrow(fall))
  fall$TakeK <- rep(0, nrow(fall))
  fall$SwingK <- rep(0, nrow(fall))
  fall$TakeB <- rep(0, nrow(fall))
  for (i in 1:nrow(fall)) {
    fall$SwingB[i] <- ifelse(fall$StrikeZone[i] == 0 && fall$Swing[i] == 1, 1, 0)
    fall$TakeK[i] <- ifelse(fall$StrikeZone[i] == 1 && fall$Swing[i] == 0, 1, 0)
    fall$SwingK[i] <- ifelse(fall$StrikeZone[i] == 1 && fall$Swing[i] == 1, 1, 0)
    fall$TakeB[i] <- ifelse(fall$StrikeZone[i] == 0 && fall$Swing[i] == 0, 1, 0)
  }
  
  SwingBsum <- sum(fall$SwingB)
  SwingKsum <- sum(fall$SwingK)
  TakeBsum <- sum(fall$TakeB)
  TakeKsum <- sum(fall$TakeK)
  
  swings <- SwingBsum + SwingKsum
  takes <- TakeBsum + TakeKsum
  balls <- SwingBsum + TakeBsum
  strikes <- SwingKsum + TakeKsum
  pitches <- swings + takes
  swingrate <- matrix(c(SwingKsum, SwingBsum, swings, TakeKsum, TakeBsum, 
              takes, strikes, balls, pitches, round(SwingKsum/strikes, 3), 
              round(SwingBsum/balls, 3), round(swings/pitches, 3)), 
              ncol = 3, byrow = TRUE)
  colnames(swingrate) <- c("Strikes", "Balls", "Total")
  rownames(swingrate) <- c("Swings", "Takes", "Total", "Rates")
  swingrate
}

contact_rate <- function(player, TrackMan) {
  TrackMan[c(which(is.na(TrackMan$PlateLocHeight))),]
  FALL <- TrackMan[-c(which(is.na(TrackMan$PlateLocHeight))),]
  
  FALL$Strikeout <- rep(0, nrow(FALL))
  FALL$InPlay <- rep(1, nrow(FALL))
  for (i in 1:nrow(FALL)) {
    if(FALL$KorBB[i] == "Strikeout") FALL$Strikeout[i] <- 1
    if(FALL$PlayResult[i] == "Sacrifice") FALL$InPlay[i] <- 0
    if(FALL$PlayResult[i] == "Undefined") FALL$InPlay[i] <- 0
  }
  fall <- FALL %>% filter(Pitcher == player)
  
  fall$High <- rep(0, nrow(fall))
  fall$Low <- rep(0, nrow(fall))
  fall$Lefty <- rep(0, nrow(fall))
  fall$Righty <- rep(0, nrow(fall))
  fall$StrikeZone <- rep(0, nrow(fall))
  for (i in 1:nrow(fall)) {
    fall$High[i] <- ifelse(fall$PlateLocHeight[i] > 3.5, 1, 0)
    fall$Low[i] <- ifelse(fall$PlateLocHeight[i] < 1.5, 1, 0)
    fall$Lefty[i] <- ifelse(fall$PlateLocSide[i] < -0.8333333, 1, 0)
    fall$Righty[i] <- ifelse(fall$PlateLocSide[i] > 0.8333333, 1, 0)
    fall$StrikeZone[i] <- ifelse(fall$High[i] == 0 && fall$Low[i] 
                       == 0 && fall$Lefty[i] == 0 && fall$Righty[i] == 0, 1, 0)
  }
  
  fall$KZ <- rep(0, nrow(fall))
  fall$KO <- rep(0, nrow(fall))
  fall$IZ <- rep(0, nrow(fall))
  fall$IO <- rep(0, nrow(fall))
  for (i in 1:nrow(fall)) {
    fall$KZ[i] <- ifelse(fall$StrikeZone[i] == 1 && fall$Strikeout[i] == 1, 1, 0)
    fall$KO[i] <- ifelse(fall$StrikeZone[i] == 0 && fall$Strikeout[i] == 1, 1, 0)
    fall$IZ[i] <- ifelse(fall$StrikeZone[i] == 1 && fall$InPlay[i] == 1, 1, 0)
    fall$IO[i] <- ifelse(fall$StrikeZone[i] == 0 && fall$InPlay[i] == 1, 1, 0)
  }
  
  zoneK <- sum(fall$KZ)
  outK <- sum(fall$KO)
  zonePlay <- sum(fall$IZ)
  outPlay <- sum(fall$IO)
  allK <- sum(fall$Strikeout)
  allPlay <- sum(fall$InPlay)
  
  zoneABs <- zoneK + zonePlay
  outABs <- outK + outPlay
  allABs <- allK + allPlay
  
  contactrate <- matrix(c(zonePlay, outPlay, allPlay, zoneK, outK, allK, zoneABs, 
                outABs, allABs, round(zonePlay/zoneABs, 3), round(outPlay/outABs, 3),
                round(allPlay/allABs, 3)), ncol = 3, byrow = TRUE)
  colnames(contactrate) <- c("In Zone", "Out of Zone", "All Pitches")
  rownames(contactrate) <- c("In Play", "Strikeouts", "Total ABs", "Rates")
  contactrate
}
