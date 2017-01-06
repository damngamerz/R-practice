roll<-function(){
  die<-1:6
  prob<-c(1/8,1/8,1/8,1/8,1/8,3/8)
  dice<-sample(die,size=2,replace = TRUE,prob)
  sum(dice)
}
rolls<-replicate(1000,roll())
qplot(rolls,binwidth=1)