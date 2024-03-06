install.packages("rcompanion")
library("rcompanion")

#preparazione del dataset
setwd("C:/Users/dalto/OneDrive - unige.it/Uni/2° Anno/Statistica Applicata/Teoria del campionamento/airbnb")
londra<-read.table("london_weekends.csv", header=TRUE, sep=",")
attach(londra)

##taglie campionarie
N = seq(500,2500, length = 9)


par(mfrow=c(3,3))
n = rep(0,9)

for (i in N) {
  n = i*2/5 ##per ogni taglia campionaria ne prendo 2/5 per il bootstrap
  campione = sample(realSum, i)
  x = rep(0,n)
  for(j in 1:10000) x[j] = mean(sample(campione, n)) ##bootstrap
  ##questo comando crea l'istogramma di default e ci disegna sopra la campana
  ## di una gaussiana di uguale media e varianza al vettore dei valori
  plotNormalHistogram(x,xlab = paste("Figura ",which(N ==i)), breaks = 80 , col = "#FFF5EE",
                       xlim = c(300,450),main = paste("Taglia campionaria: ", i))
  abline(v = mean(realSum), col = "red", lwd =2 ) ; abline(v = mean(x), col = "green",lwd = 2)
}

Q1 = realSum[realSum<as.vector(quantile(realSum)[2])] ##1° quartile
Q4 = realSum[realSum>as.vector(quantile(realSum)[4])] ##4° quartile
badSample = c(Q1,Q4) 

#resetto la disposizione dei grafici
par(mfrow=c(1,1))

#bootstrappo di nuovo
n1 = 1000
x1 = rep(0,n1)
for(i in 1:10000) x1[i] = mean(sample(badSample, n1))
plotNormalHistogram(x1, breaks = 80 , col = "#FFF5EE", xlim = c(300,450), xlab = "realSum", 
                    main = "Bootstrap di un campione non rappresentativo")
abline(v = mean(realSum), col = "red", lwd =2 ) ; abline(v = mean(x1), col = "green",lwd = 2)
legend("topleft",legend = c("Media vera","Media Bootstrap"),
       col = c("red", "green"), lty = c(1,1))
##abbiamo impostato l'xlim uguale ai grafici sopra, in modo da osservare meglio il bias
y


