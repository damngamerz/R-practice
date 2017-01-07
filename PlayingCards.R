deck=read.table("deck.csv",sep = ",",header = TRUE,stringsAsFactors = FALSE)

setup<-function(deck){
DECK<-deck

DEAL<-function(){
  card<-deck[1,]
  assign("deck",deck[-1,],envir = parent.env(environment()))
  card
}

SHUFFLE<-function(){
  random<-sample(1:52,size=52)
  assign("deck",DECK[random,],envir = parent.env(environment()))
}

list(deal=DEAL,shuffle=SHUFFLE)
}
cards<-setup(deck)
deal<-cards$deal
shuffle<-cards$shuffle

#Game of War
deck2<-deck
deck2$value[c(13,26,39,52)]<-14

#Game of War (If deck was shuffled)
deck3<-shuffle()
deck3$value[deck3$face=="ace"]<-14

#Hearts
deck4<-deck
deck4$value<-0
deck4$vaule[deck4$suit=="hearts"]<-1
queenOfSpades<-deck4$face=="queen"& deck4$suit=="spades"
deck4[queenOfSpades,]<-13

# BlackJack
deck5<-deck
facecard<-deck5$face %in% c("king","queen","jack")
deck5$value[facecard]<-10
deck5$value[deck5$face=="ace"]<-NA

#To understand runtime environments
show_env<-function(){
  a=15
  b=6
  c=4
  list(ran.in=environment(),
       parent=parent.env(environment()),
       objects=ls.str(environment()))
}