Umpire_Zone <- function(Trackman, Home = FALSE, Visitor = FALSE) {
  if (Home) Trackman <- Trackman %>% filter(PitcherTeam == "UCLA")
  if (Visitor) Trackman <- Trackman %>% filter(PitcherTeam != "UCLA")
  Umpire <- Trackman[,c(22,41,42)] %>% filter(PitchCall != "BallIntentional")
  Umpire$High <- rep(0, nrow(Umpire))
  Umpire$Low <- rep(0, nrow(Umpire))
  Umpire$Lefty <- rep(0, nrow(Umpire))
  Umpire$Righty <- rep(0, nrow(Umpire))
  Umpire$StrikeZone <- rep(0, nrow(Umpire))
  Umpire$CloseK <- rep(0, nrow(Umpire))
  Umpire$CloseB <- rep(0, nrow(Umpire))
  Umpire$CloseH <- rep(0, nrow(Umpire))
  for (i in 1:nrow(Umpire)) {
    Umpire$High[i] <- ifelse(Umpire$PlateLocHeight[i] > 3.5, 1, 0)
    Umpire$Low[i] <- ifelse(Umpire$PlateLocHeight[i] < 1.5, 1, 0)
    Umpire$Lefty[i] <- ifelse(Umpire$PlateLocSide[i] < -0.8333333, 1, 0)
    Umpire$Righty[i] <- ifelse(Umpire$PlateLocSide[i] > 0.8333333, 1, 0)
    Umpire$StrikeZone[i] <- ifelse(Umpire$High[i] == 0 && Umpire$Low[i] == 0 && Umpire$Lefty[i] == 0 
                                   && Umpire$Righty[i] == 0, 1, 0)
    if(0.5 < Umpire$PlateLocSide[i] && Umpire$PlateLocSide[i] < 0.8333333) Umpire$CloseK[i] <- 1
    if(-0.5 > Umpire$PlateLocSide[i] && Umpire$PlateLocSide[i] > -0.8333333) Umpire$CloseK[i] <- 1
    if(0.8333333 < Umpire$PlateLocSide[i] && Umpire$PlateLocSide[i] < 1.1) Umpire$CloseB[i] <- 1
    if(-0.8333333 > Umpire$PlateLocSide[i] && Umpire$PlateLocSide[i] > -1.1) Umpire$CloseB[i] <- 1
    if(1 < Umpire$PlateLocHeight[i] && Umpire$PlateLocHeight[i] < 4) Umpire$CloseH[i] <- 1
  }
  
  Umpire$KcalledB <- rep(0, nrow(Umpire))
  Umpire$BcalledK <- rep(0, nrow(Umpire))
  Umpire$KcalledK <- rep(0, nrow(Umpire))
  Umpire$BcalledB <- rep(0, nrow(Umpire))
  Umpire$Incorrect <- rep(0, nrow(Umpire))
  Umpire$Correct <- rep(0, nrow(Umpire))
  for (i in 1:nrow(Umpire)) {
    Umpire$KcalledB[i] <- ifelse(Umpire$StrikeZone[i] == 1 && Umpire$PitchCall[i] == "BallCalled", 1, 0)
    Umpire$BcalledK[i] <- ifelse(Umpire$StrikeZone[i] == 0 && Umpire$PitchCall[i] == "StrikeCalled", 1, 0)
    Umpire$KcalledK[i] <- ifelse(Umpire$StrikeZone[i] == 1 && Umpire$PitchCall[i] == "StrikeCalled", 1, 0)
    Umpire$BcalledB[i] <- ifelse(Umpire$StrikeZone[i] == 0 && Umpire$PitchCall[i] == "BallCalled", 1, 0)
    if(Umpire$KcalledB[i] == 1) Umpire$Incorrect[i] <- 1
    if(Umpire$BcalledK[i] == 1) Umpire$Incorrect[i] <- 1
    if(Umpire$KcalledK[i] == 1) Umpire$Correct[i] <- 1
    if(Umpire$BcalledB[i] == 1) Umpire$Correct[i] <- 1
  }

  thislist <- 0
  thislist2 <- 0
  thislist3 <- 0
  thislist4 <- 0
  thislist5 <- 0
  thislist6 <- 0
  for (i in 1:nrow(Umpire)) {
    if(Umpire$Incorrect[i] == 1) thislist <- c(thislist, i)
    if(Umpire$KcalledB[i] == 1) thislist2 <- c(thislist2, i)
    if(Umpire$BcalledK[i] == 1) thislist3 <- c(thislist3, i)
    if(Umpire$Correct[i] == 1) thislist4 <- c(thislist4, i)
    if(Umpire$CloseK[i] == 1 && Umpire$KcalledK[i] == 1 && Umpire$CloseH[i] == 1) thislist5 <- c(thislist5, i)
    if(Umpire$CloseB[i] == 1 && Umpire$Correct[i] == 1 && Umpire$CloseH[i] == 1) thislist6 <- c(thislist6, i)
  }
  TMWrong <- Umpire[thislist[-1],]
  TMKcalledB <- Umpire[thislist2[-1],]
  TMBcalledK <- Umpire[thislist3[-1],]
  TMCorrect <- Umpire[thislist4[-1],]
  TMCloseK <- Umpire[thislist5[-1],]
  TMCloseB <- Umpire[thislist6[-1],]
  
  ggplot(data = TMKcalledB, aes(PlateLocSide*-1, PlateLocHeight)) + geom_point(color = "orange") + 
    geom_point(data = TMBcalledK, aes(PlateLocSide*-1, PlateLocHeight), color = "red") + 
    geom_point(data = TMCloseK, aes(PlateLocSide*-1, PlateLocHeight), color = "blue") + 
    geom_point(data = TMCloseB, aes(PlateLocSide*-1, PlateLocHeight), color = "green") + 
    geom_vline(xintercept = -0.8333333) + geom_vline(xintercept = 0.8333333) + geom_hline(yintercept = 3.5) +
    geom_hline(yintercept = 1.5) + ggtitle("Behind Home View of Umpire's Close Calls") + xlab("") + ylab("") +
    theme(plot.title = element_text(hjust = 0.5))
}

Umpire_Data <- function(Trackman, Home = FALSE, Visitor = FALSE) {
  if (Home) Trackman <- Trackman %>% filter(PitcherTeam == "UCLA")
  if (Visitor) Trackman <- Trackman %>% filter(PitcherTeam != "UCLA")
  Umpire <- Trackman[,c(22,41,42)] %>% filter(PitchCall != "BallIntentional")
  Umpire$High <- rep(0, nrow(Umpire))
  Umpire$Low <- rep(0, nrow(Umpire))
  Umpire$Lefty <- rep(0, nrow(Umpire))
  Umpire$Righty <- rep(0, nrow(Umpire))
  Umpire$StrikeZone <- rep(0, nrow(Umpire))
  Umpire$CloseK <- rep(0, nrow(Umpire))
  Umpire$CloseB <- rep(0, nrow(Umpire))
  Umpire$CloseH <- rep(0, nrow(Umpire))
  for (i in 1:nrow(Umpire)) {
    Umpire$High[i] <- ifelse(Umpire$PlateLocHeight[i] > 3.5, 1, 0)
    Umpire$Low[i] <- ifelse(Umpire$PlateLocHeight[i] < 1.5, 1, 0)
    Umpire$Lefty[i] <- ifelse(Umpire$PlateLocSide[i] < -0.8333333, 1, 0)
    Umpire$Righty[i] <- ifelse(Umpire$PlateLocSide[i] > 0.8333333, 1, 0)
    Umpire$StrikeZone[i] <- ifelse(Umpire$High[i] == 0 && Umpire$Low[i] == 0 && Umpire$Lefty[i] == 0 
                                   && Umpire$Righty[i] == 0, 1, 0)
    if(0.5 < Umpire$PlateLocSide[i] && Umpire$PlateLocSide[i] < 0.8333333) Umpire$CloseK[i] <- 1
    if(-0.5 > Umpire$PlateLocSide[i] && Umpire$PlateLocSide[i] > -0.8333333) Umpire$CloseK[i] <- 1
    if(0.8333333 < Umpire$PlateLocSide[i] && Umpire$PlateLocSide[i] < 1.1) Umpire$CloseB[i] <- 1
    if(-0.8333333 > Umpire$PlateLocSide[i] && Umpire$PlateLocSide[i] > -1.1) Umpire$CloseB[i] <- 1
    if(1 < Umpire$PlateLocHeight[i] && Umpire$PlateLocHeight[i] < 4) Umpire$CloseH[i] <- 1
  }
  
  Umpire$KcalledB <- rep(0, nrow(Umpire))
  Umpire$BcalledK <- rep(0, nrow(Umpire))
  Umpire$KcalledK <- rep(0, nrow(Umpire))
  Umpire$BcalledB <- rep(0, nrow(Umpire))
  Umpire$Incorrect <- rep(0, nrow(Umpire))
  Umpire$Correct <- rep(0, nrow(Umpire))
  for (i in 1:nrow(Umpire)) {
    Umpire$KcalledB[i] <- ifelse(Umpire$StrikeZone[i] == 1 && Umpire$PitchCall[i] == "BallCalled", 1, 0)
    Umpire$BcalledK[i] <- ifelse(Umpire$StrikeZone[i] == 0 && Umpire$PitchCall[i] == "StrikeCalled", 1, 0)
    Umpire$KcalledK[i] <- ifelse(Umpire$StrikeZone[i] == 1 && Umpire$PitchCall[i] == "StrikeCalled", 1, 0)
    Umpire$BcalledB[i] <- ifelse(Umpire$StrikeZone[i] == 0 && Umpire$PitchCall[i] == "BallCalled", 1, 0)
    if(Umpire$KcalledB[i] == 1) Umpire$Incorrect[i] <- 1
    if(Umpire$BcalledK[i] == 1) Umpire$Incorrect[i] <- 1
    if(Umpire$KcalledK[i] == 1) Umpire$Correct[i] <- 1
    if(Umpire$BcalledB[i] == 1) Umpire$Correct[i] <- 1
  }
  
  thislist <- 0
  thislist2 <- 0
  thislist3 <- 0
  thislist4 <- 0
  thislist5 <- 0
  thislist6 <- 0
  for (i in 1:nrow(Umpire)) {
    if(Umpire$Incorrect[i] == 1) thislist <- c(thislist, i)
    if(Umpire$KcalledB[i] == 1) thislist2 <- c(thislist2, i)
    if(Umpire$BcalledK[i] == 1) thislist3 <- c(thislist3, i)
    if(Umpire$Correct[i] == 1) thislist4 <- c(thislist4, i)
    if(Umpire$CloseK[i] == 1 && Umpire$KcalledK[i] == 1 && Umpire$CloseH[i] == 1) thislist5 <- c(thislist5, i)
    if(Umpire$CloseB[i] == 1 && Umpire$Correct[i] == 1 && Umpire$CloseH[i] == 1) thislist6 <- c(thislist6, i)
  }
  TMWrong <- Umpire[thislist[-1],]
  TMKcalledB <- Umpire[thislist2[-1],]
  TMBcalledK <- Umpire[thislist3[-1],]
  TMCorrect <- Umpire[thislist4[-1],]
  TMCloseK <- Umpire[thislist5[-1],]
  TMCloseB <- Umpire[thislist6[-1],]
}

# KBsum <- sum(Umpire$KcalledB)
# BKsum <- sum(Umpire$BcalledK)
# KKsum <- sum(Umpire$KcalledK)
# BBsum <- sum(Umpire$BcalledB)
# accurate <- KKsum + BBsum
# inaccurate <- KBsum + BKsum
# strikes <- KBsum + KKsum
# balls <- BKsum + BBsum
# total <- accurate + inaccurate
# umpire <- matrix(c(KBsum, BKsum, inaccurate, (KKsum), BBsum, accurate, strikes, balls, total,
#                    round(KKsum/strikes, 3), round(BBsum/balls, 3), round(accurate/total, 3)),
#                  ncol = 3, byrow = TRUE)
# colnames(umpire) <- c("Zone", "No Zone", "Total")
# rownames(umpire) <- c("Incorrect", "Correct", "Total", "Accuracy")
# 
# thislist2 <- 0
# for (i in 1:nrow(TMWrong)) {
#   if(TMWrong$StrikeZone[i] == 0) thislist2 <- c(thislist2, i)
# }
# TMBalls <- TMWrong[thislist2[-1],]
# 
# missareaprop <- prop.table(c(sum(TMBalls$High), sum(TMBalls$Low), sum(TMBalls$Lefty), sum(TMBalls$Righty)))
# missarea <- matrix(c(sum(TMBalls$High), sum(TMBalls$Low), sum(TMBalls$Lefty), sum(TMBalls$Righty),
#                      sum(c(TMBalls$High, TMBalls$Low, TMBalls$Lefty, TMBalls$Righty)), round(missareaprop[1], 3),
#                      round(missareaprop[2], 3), round(missareaprop[3], 3), round(missareaprop[4], 3), 1),
#                    nrow = 2, byrow = TRUE)
# colnames(missarea) <- c("Up", "Down", "Lefty In", "Righty In", "Total")
# rownames(missarea) <- c("Misses", "Proportion")
# 
# missarea
# 
