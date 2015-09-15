d6 <- function(){
  sample(1:6, 1, replace = TRUE)
}
roll2 <- function(){
  sum(d6() + d6())
}
rollbegin <-proc.time()
rolls <-replicate(10^6, roll2())
rollend <- proc.time()
print(rollend - roll begin)
#Set starting positions
squareHistory <-NULL
currentSquare <- 1
mainbegin <- proc.time()
for(i in 1:length(rolls)){
  currentSquare = (currentSquare + rolls[i]) %% 40
  if(identical(currentSquare, 31)){
    squareHistory[i] = currentSquare
    currentSquare = 11
  }
  else {
    squareHistory[i] = currentSquare
  }
}
mainend <- proc.time()
print(mainend - mainbegin)

odds <- NULL
oddstable <- table(squareHistory)
print(oddstable)
print(chisq.test((oddstable)))
oddslist <- as.vector(oddstable)
for(j in 1:length(oddslist)){
  odds[j] <- oddslist[j] / (sum(oddslist) - oddslist[j])
}
print("Odds for each square")
print(odds)
