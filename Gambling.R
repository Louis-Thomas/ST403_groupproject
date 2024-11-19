

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
        sample_data[[56*(i-1) + (13*(j-1) + k)]] <- card
      }
    }
  }
  return(sample_data)
  
}

set.seed(2131)
deck <- create_deck(1)

shuffled_deck <- sample(deck)
shuffled_deck[[1]]