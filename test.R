library(HydeNet)

net <- HydeNetwork( ~playerPoints | card1 * card2
                   +playerAces | card1 * card2
                   +hit1 | playerPoints * dealerCard1
                   
                   +playerAcesAfterHit1 | playerAces * cardFromHit1
                   +playerPointsAfterHit1 | playerPoints * cardFromHit1 * playerAcesAfterHit1
                   +hit2 | playerPointsAfterHit1 * dealerCard1
                   
                   +playerAcesAfterHit2 | playerAcesAfterHit1 * cardFromHit2
                   +playerPointsAfterHit2 | playerPointsAfterHit1 * cardFromHit2 * playerAcesAfterHit2
                   +hit3 | playerPointsAfterHit2 * dealerCard1
                   
                   +playerAcesAfterHi3 | playerAcesAfterHit2 * cardFromHit3
                   +playerPointsAfterHit3 | playerPointsAfterHit2 * cardFromHit3 * playerAcesAfterHit3

                   +finalPoints | playerPoints * playerPointsAfterHit1 * playerPointsAfterHit2 *
                                  playerPointsAfterHit3 * hit1 * hit2 * hit3
                   
                   +result | finalPoints * dealerCard1
                                  
)

drawProbability <- c(rep(1/13,8), 4/13, 1/13)

#net <- setNode(net, card1, nodeType="dcat", pi = vectorProbs(p=drawProbability,card1), validate=FALSE)
#net <- setNode(net, card2, nodeType="dcat", pi = vectorProbs(p=drawProbability,card2), validate=FALSE)
#net <- setNode(net, cardFromHit1, nodeType="dcat", pi = vectorProbs(p=drawProbability,cardFromHit1), validate=FALSE)
#net <- setNode(net, cardFromHit2, nodeType="dcat", pi = vectorProbs(p=drawProbability,cardFromHit2), validate=FALSE)
#net <- setNode(net, cardFromHit3, nodeType="dcat", pi = vectorProbs(p=drawProbability,cardFromHit3), validate=FALSE)
#net <- setNode(net, dealerCard1, nodeType="dcat", pi = vectorProbs(p=drawProbability, dealerCard1, validate=FALSE))
if(!exists("foo", mode="function")) source("functions.R")

chooseCard(2,4,5,6,12,14)

net <- setNode(net, card1, "determ", define = fromFormula(),
               nodeFormula = card1 ~ chooseCard(dealerCard1))

net <- setNode(net, playerPoints, "determ", define=fromFormula(),
               nodeFormula = playerPoints ~ card1+card2+2)

net <- setNode(net, playerAces, "determ", define=fromFormula(),
               nodeFormula = playerAces ~ ifelse(card1==10, 1, 0) + ifelse(card2==10, 1, 0))

net <- setNode(net, playerAcesAfterHit1, "determ", define=fromFormula(),
               nodeFormula = playerAcesAfterHi1 ~ ifelse(cardFromHit1==10, 1, 0) + playerAces)

net <- setNode(net, playerPointsAfterHit1, "determ", define=fromFormula(),
               nodeFormula=playerPointsAfterHit1 ~ returnPoints(cardFromHit1+1+playerPoints, playerAcesAfterHit1))

net <- setNode(net, playerAcesAfterHit2, "determ", define=fromFormula(),
               nodeFormula = playerAcesAfterHi2 ~ ifelse(cardFromHit2==10, 1, 0) + playerAcesFromHit1)

net <- setNode(net, playerPointsAfterHit2, "determ", define=fromFormula(),
               nodeFormula=playerPointsAfterHit2 ~ returnPoints(cardFromHit2+1+playerPointsFromHit1, playerAcesAfterHit2))

net <- setNode(net, playerAcesAfterHit3, "determ", define=fromFormula(),
               nodeFormula = playerAcesAfterHi3 ~ ifelse(cardFromHit3==10, 1, 0) + playerAcesFromHit2)

net <- setNode(net, playerPointsAfterHit3, "determ", define=fromFormula(),
               nodeFormula=playerPointsAfterHit3 ~ returnPoints(cardFromHit3+1+playerPointsFromHit2, playerAcesAfterHit3))

plot(net)

