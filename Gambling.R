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

print_card <- function(object) {
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