

create_deck <- function(n = 1) {
  
  face_init <- c(1:13)
  suit_init <- c("spade", "heart", "club", "diamond")
  sample_data <- rep(list(suit=suit_init[1], face = face_init[1], class = "card"), n*52)
  
  for(i in 1:n) {
    for(j in 1:4) {
      for(k in 1:13){
        suit <- suit_init[j]  
        face <- switch(face_init[k], "1"="ace", "11"="jack", "12"="queen", "13"="king", face_init[k])
        card <- structure(list(suit=suit, face=face), class="card")
        sample_data[(j*k)*i] <- card
      }
    }
  }
  return(sample_data)
  
}

set.seed(789)
deck <- create_deck(1)

deck
