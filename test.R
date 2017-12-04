library(HydeNet)
library(rjags)


net <- HydeNetwork(~initialPoints | card1 * card2
                   +initialAces | card1 * card2
                   +hit1LM | dealerUpcard * initialPoints
                   +hit1 | hit1LM
                   +doubleLM | dealerUpcard * initialPoints
                   +double | doubleLM
                   
                   +acesAfterCard3 | initialAces * card3
                   +pointsAfterCard3 | initialPoints * card3 * acesAfterCard3
                   +hit2LM | pointsAfterCard3 * dealerUpcard
                   +hit2 | hit2LM

                   +acesAfterCard4 | acesAfterCard3 * card4
                   +pointsAfterCard4 | pointsAfterCard3 * card4 * acesAfterCard4
                   +hit3LM | pointsAfterCard4 * dealerUpcard
                   +hit3 | hit3LM

                   +acesAfterCard5 | acesAfterCard4 * card5
                   +pointsAfterCard5 | pointsAfterCard4 * card5 * acesAfterCard5
                   
                   +finalPoints | initialPoints * pointsAfterCard3 * pointsAfterCard4 *
                     pointsAfterCard5 * hit1 * hit2 * hit3
                   
                   +dealerOutcome | dealerUpcard
                   
                   +result | finalPoints * dealerOutcome
                   +payoff | result * double 
                   )

drawProbability  <- c(rep(1/13,8), 4/13, 1/13)  # probs. for 2, 3, ..., 9, (10-K), A

net <- setNode(net, dealerUpcard, nodeType="dcat",  pi=vectorProbs(p=drawProbability, dealerUpcard), validate=FALSE)

net <- setNode(net, card1, nodeType="dcat", pi = vectorProbs(p=drawProbability,card1), validate=FALSE)
net <- setNode(net, card2, nodeType="dcat", pi = vectorProbs(p=drawProbability,card2), validate=FALSE)
net <- setNode(net, card3, nodeType="dcat", pi = vectorProbs(p=drawProbability,card3), validate=FALSE)
net <- setNode(net, card4, nodeType="dcat", pi = vectorProbs(p=drawProbability,card4), validate=FALSE)
net <- setNode(net, card5, nodeType="dcat", pi = vectorProbs(p=drawProbability,card5), validate=FALSE)





net <- setNode(net, initialPoints, "determ", define=fromFormula(),
               nodeFormula = initialPoints ~ card1+card2+2)

net <- setNode(net, initialAces, "determ", define=fromFormula(),
               nodeFormula = initialAces ~ ifelse(card1==10, 1, 0) + ifelse(card2==10, 1, 0))

net <- setNode(net, acesAfterCard3, "determ", define=fromFormula(),
               nodeFormula = acesAfterCard3  ~ ifelse(card3==10, 1, 0) + initialAces)

net <- setNode(net, pointsAfterCard3, "determ", define=fromFormula(),
               nodeFormula = pointsAfterCard3 ~
                 ifelse(acesAfterCard3 == 3,
                        13,
                        ifelse(acesAfterCard3 == 2,
                               card1 + card2 + card3 + 3 - 10,
                               ifelse(acesAfterCard3 == 1,
                                      ifelse(card1 + card2 + card3 + 3 > 22,
                                             card1 + card2 + card3 + 3 - 10,
                                             card1 + card2 + card3 + 3),
                                      card1 + card2 + card3 + 3
                               )
                        )
                 )
)

net <- setNode(net, acesAfterCard4, "determ", define=fromFormula(),
               nodeFormula =acesAfterCard4 ~ ifelse(card4==10, 1, 0) + acesAfterCard3)

net <- setNode(net, pointsAfterCard4, "determ", define=fromFormula(),
               nodeFormula = pointsAfterCard4 ~
                 ifelse(acesAfterCard4 == 4,
                        14,
                        ifelse(acesAfterCard4 == 3,
                               ifelse(card1 + card2 + card3 + card4 + 4 > 38,
                                      card1 + card2 + card3 + card4 + 4 - 30,
                                      card1 + card2 + card3 + card4 + 4 - 20
                               ),
                               ifelse(acesAfterCard4 > 0,
                                      ifelse(card1 + card2 + card3 + card4 + 4 > 22,
                                             card1 + card2 + card3 + card4 + 4 - 10,
                                             card1 + card2 + card3 + card4 + 4
                                      ),
                                      card1 + card2 + card3 + card4 + 4
                               )
                        )
                 )
)

net <- setNode(net, acesAfterCard5, "determ", define=fromFormula(),
               nodeFormula = acesAfterCard5 ~ ifelse(card5==10, 1, 0) + acesAfterCard4)

net <- setNode(net, pointsAfterCard5, "determ", define=fromFormula(),
               nodeFormula = pointsAfterCard5 ~ 
                 ifelse(acesAfterCard5 == 5,
                        15,
                        ifelse(acesAfterCard5 == 4,
                               ifelse(card1 + card2 + card3 + card4 + card5 + 5 > 51,
                                      card1 + card2 + card3 + card4 + card5 + 5 - 40,
                                      card1 + card2 + card3 + card4 + card5 + 5 - 30
                               ),
                               ifelse(acesAfterCard5 == 3,
                                      ifelse(card1 + card2 + card3 + card4 + card5 + 5 > 51,
                                             card1 + card2 + card3 + card4 + card5 + 5 - 30,
                                             card1 + card2 + card3 + card4 + card5 + 5 - 20
                                      ),
                                      ifelse(acesAfterCard5 == 2,
                                             ifelse(card1 + card2 + card3 + card4 + card5 + 5 > 31,
                                                    card1 + card2 + card3 + card4 + card5 + 5 - 20,
                                                    card1 + card2 + card3 + card4 + card5 + 5 - 10
                                             ),
                                             ifelse(acesAfterCard5 > 0,
                                                    ifelse(card1 + card2 + card3 + card4 + card5 + 5 > 22,
                                                           card1 + card2 + card3 + card4 + card5 + 5 - 10,
                                                           card1 + card2 + card3 + card4 + card5 + 5
                                                    ),
                                                    card1 + card2 + card3 + card4 + card5 + 5
                                             )
                                      )
                               )
                        )
                 )
)

############DEALER OUTCOME
data(BJDealer)
#BJDealer
dealerOutcome.cpt <- cpt(dealerOutcome ~ dealerUpcard,
                         data = BJDealer,
                         wt = BJDealer$probability)

round(dealerOutcome.cpt, 3)
net<- setNodeModels(net, dealerOutcome.cpt)
dealerOutcome.cpt


hit1Probabilities = read.csv("hit1Probs.csv", sep=";")
hit2Probabilities = read.csv("hit2Probs.csv", sep=";")
hit3Probabilities = read.csv("hit3Probs.csv", sep=";")
#head(hit1Probabilities)

hit1LM <- lm(hit1LM ~ dealerUpcard + initialPoints,
                data = hit1Probabilities)

hit2LM <- lm(hit2LM ~ pointsAfterCard3 + dealerUpcard,
                data = hit2Probabilities)

hit3LM <- lm(hit3LM ~ pointsAfterCard4 + dealerUpcard,
                data = hit3Probabilities)
net<- setNodeModels(net, hit1LM, hit2LM, hit3LM)



net <- setNode(net, hit1, "determ", define=fromFormula(),
               nodeFormula = hit1  ~ ifelse(hit1LM>1.1, 1, 0))
net <- setNode(net, hit2, "determ", define=fromFormula(),
               nodeFormula = hit2  ~ ifelse(hit2LM>1.1, 1, 0))
net <- setNode(net, hit3, "determ", define=fromFormula(),
               nodeFormula = hit3  ~ ifelse(hit3LM>1.1, 1, 0))

##########BETS
doubleLM <- lm(doubleLM ~ dealerUpcard + initialPoints,
               data = hit1Probabilities)
net<- setNodeModels(net, doubleLM)

net <- setNode(net, double, "determ", define=fromFormula(),
               nodeFormula = double  ~ ifelse(doubleLM>1.2, 1, 0))

############Final Points acording to the hit decision and final result
net <- setNode(net, finalPoints, "determ", define=fromFormula(),
               nodeFormula = finalPoints ~ 
                 ifelse(hit1 == 0,
                        initialPoints,
                        ifelse(hit2 == 0 || double == 1,
                               pointsAfterCard3,
                               ifelse(hit3 == 0 , pointsAfterCard4, pointsAfterCard5)
                        )
                 )
)

net<- setNode(net, result, "determ", define=fromFormula(),
              nodeFormula = result ~ ifelse(finalPoints<=21 && hit3==1 &&hit2==1 &&hit1==1, 1,
                                            ifelse(finalPoints > 21, -1,
                                              ifelse(finalPoints == 21,
                                                   ifelse(dealerOutcome == "Blackjack", 0,
                                                          ifelse(dealerOutcome == 7, 0, 1)),
                                                   ifelse(dealerOutcome == "Bust",
                                                          ifelse(finalPoints < 22, 1, -1),
                                                          ifelse(dealerOutcome == "17",
                                                                 ifelse(finalPoints == 17, 0,
                                                                        ifelse(finalPoints > 17, 1, -1)),
                                                                 ifelse(dealerOutcome == "18",
                                                                        ifelse(finalPoints == 18, 0,
                                                                               ifelse(finalPoints > 18, 1, -1)),
                                                                        ifelse(dealerOutcome == "19",
                                                                               ifelse(finalPoints == 19, 0,
                                                                                      ifelse(finalPoints > 19, 1, -1)),
                                                                               ifelse(dealerOutcome == "20",
                                                                                      ifelse(finalPoints == 20, 0,
                                                                                             ifelse(finalPoints > 20, 1, -1)),
                                                                                      ifelse(finalPoints == 21, 0, -1))))))))))


net <- setNode(net, payoff, "determ", define=fromFormula(),
               nodeFormula = payoff ~ result * (2^double))

compiledNet <- compileJagsModel(net)
vars = c("payoff", "double", "hit1", "hit2", "hit3", "doubleLM")
post <- HydePosterior(compiledNet,
                      variable.names = vars,
                      n.iter=100000)

dplyr::sample_n(post, 60)
net <- setDecisionNodes(net, hit1, hit2, hit3)
net <- setUtilityNodes(net, payoff)
plot(net)

sum(post$payoff)