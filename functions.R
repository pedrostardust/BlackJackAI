
chooseCard <- function(dealerCard1, card1 = 0, card2=0, card3=0, card4=0, card5=0){
  cardValues <- c(rep(2,4), rep(3,4), rep(4,4), rep(5,4), rep(6,4), rep(7,4), rep(8,4), rep(9,4), rep(10,4), 
                  rep(11,4), rep(12,4), rep(13,4), rep(14,4))
  
  for(i in 1:52){
    if(dealerCard1!=0 && dealerCard1 == cardValues[i]){
      cardValues[i] <- NULL
      dealerCard1=0
      print('oi')
    }
    if(card1!=0 && card1 == cardValues[i]){
      cardValues[i] <- NULL
      card1=0
    }
    if(card2!=0 && card2 == cardValues[i]){
      cardValues[i] <- NULL
      card2=0
    }
    if(card3!=0 && card3 == cardValues[i]){
      cardValues[i] <- NULL
      card3=0
    }
    if(card4!=0 && card4 == cardValues[i]){
      cardValues[i] <- NULL
      card4=0
    }
    if(card5!=0 && card5 == cardValues[i]){
      cardValues[i] <- NULL
      card5=0
    }
  }
  
  print(cardValues)
  
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


