


getwd()



covid<- read.csv("C:/Users/sctig/Desktop/covid.csv", header = TRUE)

D= as.matrix(covid)
D
D[,2]
death<- D[, seq(1,15,1)]
death
death[3,3]
10*death[3,3]

# Covid 19
a<- 1
b<- 1
symp<- matrix(NA, nrow= 5000, ncol=14)
symp[1, ]<- rep(1,14)

d<- death[,1]
# infection rates of 10 symptons
E<- death[,2:15] # infection rates of Covid 19 by those symptons

for (i in 1:4999) {
  for (j in 1:14){
    
    symp[i+1,j]<- rgamma(1, a+d[6]/10, b+E[6,j]*exp(sum(symp[i,])-symp[i,j])) 
  }
}
symp
mean= matrix(NA, nrow= 1, ncol=14)
for (k in 1:14) {
  mean[,k]<- mean(symp[,k])
}
mean

mean(symp)
fever<- exp(symp[,10])
fever
mean(fever)


