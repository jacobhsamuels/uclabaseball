Fall2019_Winter2020 <- read_csv("UCLA Baseball/Bauer Units/Fall2019-Winter2020.csv", skip = 3)
table(Fall2019_Winter2020$Date)

winter <- Fall2019_Winter2020 %>% filter(Date == "1/10/2020"|Date == "1/14/2020"|Date == "1/17/2020"|Date == "1/18/2020"|
              Date == "1/21/2020"|Date == "1/24/2020"|Date == "1/25/2020"|Date == "1/28/2020"|Date == "1/31/2020") %>% select(RelSpeed, SpinRate, TaggedPitchType)

Bauer_Units <- function(TrackMan, Pitcher) {
  FB <- TrackMan %>% filter(TaggedPitchType == "Fastball")
  FBVelo <- mean(FB$RelSpeed, na.rm = TRUE)
  FBSpin <- mean(FB$SpinRate, na.rm = TRUE)
  FBBauer <- FBSpin/FBVelo
  
  CH <- winter %>% filter(TaggedPitchType == "ChangeUp")
  CHVelo <- mean(CH$RelSpeed, na.rm = TRUE)
  CHSpin <- mean(CH$SpinRate, na.rm = TRUE)
  CHBauer <- CHSpin/CHVelo
}
  


FBBauer
CHBauer
