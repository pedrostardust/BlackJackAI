ola <- function(asd){
  return(1)
}

calculatePayoff <- function(dealerPoints, playerPoints){
  
  if(dealerPoints > playerPoints){
    return (1)
  }
  else{
    return (1)
  }
}


hitDecision <- function(dealerPoints, playerPoints){
  if(dealerPoints > playerPoints){
    return (1)
  }
  else{
    return (0)
  }
}


calculateFinalPoints <- function(hit1, hit2, hit3, playerPoints, playerPointsAfterHit1, playerPointsAfterHit2, playerPointsAfterHit3){
  if(hit1==0){
    return (playerPoints)
  } 
  else if(hit2==0){
    return(playerPointsAfterHit1)
  }
  else if(hit3==0){
    return(playerPointsAfterHit2)
  }
  else if(hit3==1){
    return(playerPointsAfterHit3)
  }
}


chooseCard <- function(dealerCard1=0, card1 = 0, card2=0, card3=0, card4=0, card5=0){
  cardValues <- c(rep(2,4), rep(3,4), rep(4,4), rep(5,4), rep(6,4), rep(7,4), rep(8,4), rep(9,4), rep(10,16), 
                  rep(11,4))
  
  for(i in 1:52){
    if(dealerCard1 != 0 && dealerCard1 == cardValues[i]){
      cardValues[i] <- NA
      dealerCard1 = 0
    }
    
    else if(card1 != 0 && card1 == cardValues[i]){
      cardValues[i] <- NA
      card1=0
    }
    else if(card2!=0 && card2 == cardValues[i]){
      cardValues[i] <- NA
      card2=0
    }
    else if(card3!=0 && card3 == cardValues[i]){
      cardValues[i] <- NA
      card3=0
    }
    else if(card4!=0 && card4 == cardValues[i]){
      cardValues[i] <- NA
      card4=0
    }
    else if(card5!=0 && card5 == cardValues[i]){
      cardValues[i] <- NA
      card5=0
    }
  }
  
  cardValues <- cardValues[!is.na(cardValues)]
  
  cardToReturn <- sample(cardValues,1)
  print(cardToReturn)
  return(cardToReturn)
  
}

returnPoints <- function(points, nAces){
  
  if(points < 22){
    return (points)
  }
  else if(points > 21 && nAces==0){
    return(points)
  }
  else if(points < 32){
    if(nAces!=0){
      return(points-10)
    }
  }
  else if(points < 42){
    if(nAces>1){
      return(points-20)
    }
  }
  else if(points<52){
    if(nAces>2){
      return(points-30)
    }
  }
  else if(points<62){
    if(nAces>3){
      return(points-40)
    }
  }
  else{
    return(points)
  }
}


