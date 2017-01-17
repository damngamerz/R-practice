get_symbols<- function(){
  wheel<- c("DD","7","BBB","BB","B","C","0")
  sample(wheel,size = 3,replace = TRUE,prob = c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}

play<-function(){
  symbols<-get_symbols()
  structure(score1(symbols),symbols=symbols,class="slots")
}
#Not Considering DD as WildCards
score<-function(symbols) {
#identify case
same<- symbols[1]==symbols[2] && symbols[2]==symbols[3]
bars<-symbols %in% c("B","BB","BBB")

#get prize
if(same) {
  payouts<-c("DD"=100,"7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
  prize<- unname(payouts[symbols[1]])
} else if(all(bars)) {
  prize<- 5
} else {
  cherries<-sum(symbols=="C")
  prize<- c(0,2,5)[cherries+1]
}

#adjust for diamonds
diamonds<-sum(symbols=="DD")
prize*2^diamonds
}

#Considering DD as Wildcards
score1 <- function(symbols) {
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  prize * 2^diamonds
}
slot_display<- function(prize) {
  #extract Symbols
  symbols<-attr(prize,"symbols")
  #collapse symbols into single string
  symbols<-paste(symbols,collapse = " ")
  #combine symbol with prize as a regular expression
  #\n is regular expression for a new line 
  string<-paste(symbols,prize,sep="\n$")
  #Display Regular Expression in console without quotes
  cat(string)
}

print.slots<-function(x,...){
  slot_display(x)
}

#Expected Value Default
wheel<- c("DD","7","BBB","BB","B","C","0")
combos<-expand.grid(wheel,wheel,wheel,stringsAsFactors = FALSE)
prob<-c("DD"=0.03,"7"=0.03,"BBB"=0.06,"BB"=0.1,"B"=0.25,"C"=0.01,"0"=0.52)
combos$prob1<-prob[combos$Var1]
combos$prob2<-prob[combos$Var2]
combos$prob3<-prob[combos$Var3]
combos$prob<-combos$prob1*combos$prob2*combos$prob3
combos$prize<-NA
for(i in 1:nrow(combos)){
  symbols<-c(combos[i,1],combos[i,2],combos[i,3])
  combos$prize[i]<-score(symbols)
}
sum(combos$prize*combos$prob)

#Expected Value Considering DD case
for(i in 1:nrow(combos)){
  symbols<-c(combos[i,1],combos[i,2],combos[i,3])
  combos$prize[i]<-score1(symbols)
}
sum(combos$prize*combos$prob)

#Understanding while loops
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}

#Understanding repeat loops
plays_till_broke1 <- function(start_with) {
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + play()
    n <- n + 1
    if (cash <= 0) {
      break
    }
  }
  n
}

#Speed Vectorized Code:
abs_loop<- function(vec){
  for(i in 1:length(vec)){
    if(vec[i]<0){
     vec[i]<- -vec[i] 
    }
  }
  vec
}

abs_sets<- function(vec){
  negs<-vec<0
  vec[negs]<-vec[negs]*-1
  vec
}
long<-rep(c(-1,1),5000000)
system.time(abs_loop(long))
system.time(abs_sets(long))

winnings<-vector(length=1000000)
for(i in 1:1000000){
  winnings[i]<-play()
}
mean(winnings)
