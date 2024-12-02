#21

#Functions:
#blackJack() - runs the games
#draw_card() - draws a card
#print.cards() - print cards
#countCards() - counts up card values (accounting for aces being 1 and 11)
#createDeck() - ensures no duplicates

#bust() - can do this or include in the countValues function

create_deck <- function(n = 1) {
  
  face_init <- as.character(c(1:13))
  suit_init <- c("spade", "heart", "club", "diamond")
  card <- list(suit=suit_init[1], face = face_init[1], class = "card")
  sample_data <- list()
  
  for(i in 1:n) {
    for(j in 1:4) {
      for(k in 1:13){
        suit <- suit_init[j]  
        face <- face_init[k]
        face <- switch(face, "1"="ace", "11"="jack", "12"="queen", "13"="king", face)
        card <- structure(list(suit=suit, face=face), class="card")
        sample_data[[52*(i-1) + (13*(j-1) + k)]] <- card
        # Each 52th 
      }
    }
  }
  return(sample_data)
  
}

set.seed(2131)
shuffled_deck <- sample(create_deck())
card_index <- 0

draw_card <- function() {
  card_index <<- card_index + 1
  return(shuffled_deck[[card_index]])
}

print.card <- function(object) {
  if(class(object) == "card"){
    suit <- switch(object$suit, "spade" = "♠", "heart" = "♥", "club" = "♣", "diamond" = "♦", object$suit)
    face <- switch(object$face, "queen" = "👑", object$face)
    cat("
  # # # 
  #",suit,"# 
  #",face,"#
  # # #
  ")
    
  }
}

countValues <- function(cards)
{
  value <- 0
  for(i in 1:length(cards))
  {
    faceValue <- cards[[i]]$face
    
    
    if(faceValue == "king" | faceValue == "queen" | faceValue == "jack")
    {
      faceValue = 10
    }
    else if(faceValue == "ace")
    {
      faceValue = 11
    }
    else
    {
      faceValue = as.numeric(faceValue)
    }
    
    #Switch statement is not working
    #switch(faceValue, "king" = 10, "queen" = 10, "jack" = 10, "ace" = 11, as.numeric(faceValue))
    
    value <- value + faceValue
  }
  
  faceValues <- sapply(cards, function(card) as.character(card$face))
  
  if(value > 21 & any(faceValues == "ace"))
  {
    value - 10
  }
  
  return(value)
}

blackjack <- function()
{
  shuffled_deck <- create_deck()
  card_index <- 0
  player <- list(draw_card(), draw_card()) #Not random cards, in order 
  dealer <- list(draw_card(), draw_card()) #Not random cards, in order 
  
  while(TRUE) 
  {
    print("Your Hand")
    print(player)
    print("Total Value")
    print(countValues(player))
    
    print("Dealer's hand")
    print(dealer[1])
    
    var = readline(prompt = "Hit or Stick : ")
    
    if(var == "Hit" || var == "hit")
    {
      player <- append(player, list(draw_card()))
      print(player)
      value <- countValues(player) 
      if(value > 21)
      {
        print(value)
        print("You have gone Bust")
        break
      }
    }
    else if(var == "Stick" || var == "stick")
    {
      playerValue <- countValues(player) 
      dealerValue <- countValues(dealer)
      if(playerValue > 21)
      {
        print(value)
        print("You have gone Bust")
        break
      }
      else if(playerValue > dealerValue) #Account for draw
      {
        print(paste("Your value: ", playerValue))
        print(paste("Dealers value: ", dealerValue))
        print("Congardulations you have won!!")
        break
      }
      else
      {
        print(paste("Your value: ", playerValue))
        print(paste("Dealers value: ", dealerValue))
        print("House wins, try again next time")
        break
      }
    }
    else
    {
      print("You are talking gibberish")
    }
  }
  
  var = readline(prompt = "Game over, Do you want to play again? (Yes/No)")
  
  if(var == "Yes" || var == "yes")
  {
    blackjack()
  }
  else
  {
    print("Goodbye")
  }
  
  
}

blackjack()





