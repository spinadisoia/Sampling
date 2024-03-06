#Parte 2 della Realzione di Teoria di Campionamento

#preparazione del dataset
setwd("C:/Users/sonia/OneDrive/Desktop/UNI/2' ANNO/2 SEM/STAT APPL/DATASET/airbnb")
londra<-read.table("london_weekends.csv", header=TRUE, sep=",")
londra$X=NULL #elimino la variabile X perchè conta le righe
attach(londra)

#studio della variabile di interesse
mediaTot=mean(realSum); 
varianzaTot=var(realSum); 
stddevTot=sd(realSum);
summary(realSum)
mediaTot
stddevTot

#installo pacchetto viridis per i colori
install.packages("viridis")
library(viridis)
library(viridisLite)
par(mfrow=c(1,2))
hist(realSum, main='Prezzi per alloggio', col = viridis(15)) #si concentra in valori minori di 500
hist(realSum[realSum<1500], xlab="RealSum", main='Prezzi per alloggio minori di 1500 euro',col = viridis(15))
par(mfrow=c(1,1))
boxplot(realSum, main='Boxplot di RealSum')

#######################################################################################
# CAMPIONAMENTO CASUALE SEMPLICE SENZA RIPETIZIONE  #
#######################################################################################
library(sampling)
N<-length(realSum)
# selezioniamo le taglie di numerosità da 250 a 2500 con salto di 50
n = seq(250,2500,length =46)
mediaCampionaria = rep(0,length(n))
sdCampionaria = rep(0,length(n))

for (i in 1:46) {
  #vettore media campionaria per taglie
  campionamento = srswor(n[i],N)
  campione = londra[campionamento==1, "realSum"]
  mediaCampionaria[i] = mean(campione)
  #vettore standard deviation campionaria per taglie
  sdCampionaria[i] = sd(campione)
}

#Le medie campionarie sono
mediaCampionaria
#La media della popolazione
mediaTot

#plot della media osservata al variare della taglia scelta
plot(n,mediaCampionaria,main = "Media al variare di n", xlab = "Taglia Campionaria", 
     ylab = "Media Osservata",type = "o", col = "black",     lwd = 1, 
     pch = 16,lty=10)
grid(nx = NULL, ny=1, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
abline(h=mediaTot, col = "red")
text(x=200, y=mediaTot+2, expression(mu), col='red')

#plot degli IC per la media al variare di n
lower=mediaCampionaria+qnorm(0.025)*sdCampionaria/sqrt(n)
upper=mediaCampionaria-qnorm(0.025)*sdCampionaria/sqrt(n)
install.packages("plotrix")
library("plotrix")
plotCI(x = n ,y = mediaCampionaria,li = lower,ui = upper, ylim=c(300,500), main="Intervalli di confidenza per la media al variare di n")
abline(h=mediaTot, col = "red")
text(x=200, y=mediaTot+5, expression(mu), col='red')


#Le standard deviation campionarie sono
sdCampionaria
#La standard deviation della popolazione è
stddevTot

#plot della standard deviation osservata
plot(n,sdCampionaria,main = "Standard Dev al variare di n", xlab = "Taglia Campionaria", 
     ylab = "Std Dev",type = "o", col = "black",     lwd = 1, 
     pch = 16,lty=10)
grid(nx = NULL, ny=1, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
abline(h=stddevTot, col="red")
text(x=200, y=stddevTot+15, expression(sigma), col='red')

# scelgo taglia campionaria per v
v = 1000
campionamentoCCS = srswor(v,N)
campioneCCS = londra[campionamentoCCS==1,"realSum"]
XmedioCCS= mean(campioneCCS)
XmedioCCS

######################################################################################
# STRATIFICAZIONE
######################################################################################
# ALLOCAZIONE PROPORZIONALE 
# PER ROOM_TYPE
######################################################################################
# scelgo taglia campionaria v
# ordino per room_type
londra0=londra[order(room_type),]

# conta le numeosità della variabile room_type
st<-table(room_type); st

# taglie campionarie negli strati
alloc=st*v/N
sum( round( alloc ) ) #controlla che sommi a v
nst<-as.vector( round( alloc ) ) 
perc=(nst/st)*100 #percentuale di popolazione calcolata per strato
campione<-strata(londra0,c("room_type"), size=nst, method="srswor")
x<-getdata(londra0, campione)

table(room_type)
table(campione$room_type)

#media e standard deviation osservate per strato
mediaSt<-tapply(x$realSum, x$room_type, mean);
stddevSt<-tapply(x$realSum, x$room_type, sd) * sqrt( (nst)/(nst-1) ); 
#stimatore scelto S
#realizzazione della media  e della standard deviation campionaria stratificata
XmedioST<-sum(st/N * mediaSt)
VARPesST=(st-nst)*st*stddevSt^2/nst
VARST=sum(VARPesST)
sdST=sqrt(VARST)


Prop0 = rbind(nst, perc, mediaSt, stddevSt)
rownames(Prop0) = c("Numerosità", "Percentuale campionata", "Media", "Standard Deviation")
Prop0

######################################################################################
# ALLOCAZIONE DI NEYMAN 
# PER ROOM_TYPE
######################################################################################
nk = round(v *st*stddevSt / sum(st*stddevSt)); nk
perck=(nk/st)*100 #percentuale di popolazione calcolata per strato
campionamentoStNey = strata(londra0, c("room_type"), size = nk, method = "srswor")
campioneStNey = getdata(londra0,campionamentoStNey)

#media e standard deviation osservate per strato
mediaSTNey = tapply(campioneStNey$realSum, campioneStNey$room_type, mean)
stdSTNey = tapply(campioneStNey$realSum, campioneStNey$room_type, sd)  * sqrt( (nk)/(nk-1) )

# realizzazione dello stimatore X medio stratificato con allocazione Neyman
XmedioSTNey = sum(st/N * mediaSTNey) 

Neyman0 = rbind(nk,perck, mediaSTNey, stdSTNey)
rownames(Neyman0) = c("Numerosità",  "Percentuale campionata", "Media", "Standard Deviation")
Neyman0

# Grafici di differenza tra allocazione proporzionale e di neyman
mediePop0=tapply(realSum, room_type, mean)
medieRoomType = rbind(mediaSt,mediaSTNey, mediePop0)
rownames(medieRoomType) = c("Proporzionale", "Neyman", expression(mu[h]))
barplot((medieRoomType), main = "Confronto medie negli strati ottenuti con allocazione", 
        sub = "Room Type", ylim = c(0,600), beside = TRUE,col = viridis(3), border = "black",
        legend.text = c("Proporzionale", "Neyman", expression(mu[h])))

sdPop0=tapply(realSum, room_type, sd)
sdRoomType = rbind(stddevSt, stdSTNey, sdPop0)
rownames(sdRoomType) = c("Proporzionale", "Neyman", expression(sigma[h]))
barplot((sdRoomType), main = "Confronto Std Dev negli strati ottenuti con allocazione"
        ,sub = "Room Type", beside = TRUE,col = viridis(3, option = "C"), border = "black", ylim=c(0,800),
        legend.text = c("Proporzionale","Neyman",expression(sigma[h])))


######################################################################################
# ALLOCAZIONE PROPORZIONALE
# PER PERSON_CAPACITY
######################################################################################
#ordino
londra1=londra[order(person_capacity),]

st1<-table(person_capacity)
st1
# decidere le taglie campionarie negli strati
alloc1=st1*v/N ; alloc1  
sum( round( alloc1 ) ) #controlla che sommi a v
nst1<-as.vector( round( alloc1 ) ) ; nst1
perc1=(nst1/st1)*100 #percentuale di popolazione calcolata per strato
campione1<-strata(londra1, c("person_capacity"), size=nst1, method="srswor") 
x1<-getdata(londra1, campione1)

table(person_capacity)
table(campione1$person_capacity)

# media e standard deviation osservate per strato
mediaSt1<-tapply(x1$realSum, x1$person_capacity, mean);
stddevSt1<-tapply(x1$realSum, x1$person_capacity, sd) * sqrt( (nst1)/(nst1-1) );
# stimatore scelto s

# realizzazione dello stimatore X medio stratificato e varianza con allocazione proporzionale
XmedioST1<-sum(st1/N * mediaSt1)
VARPesST1=(st1-nst1)*st1*stddevSt1^2/nst1
VARST1=sum(VARPesST1)
sdST1=sqrt(VARST1)


Prop1 = rbind(nst1,perc1, mediaSt1, stddevSt1)
rownames(Prop1) = c("Numerosità", "Percentuale campionata","Media", "Standard Deviation")
Prop1


######################################################################################
# ALLOCAZIONE DI NEYMAN 
# PER PERSON_CAPACITY
######################################################################################
nk1 = round(v * st1*stddevSt1 / sum(st1*stddevSt1))
#stimiamo la standard deviation nello strato con la sqrt(s^2) stimata con l'allocazione proporzionale
perck1=(nk1/st1)*100 #percentuale di popolazione calcolata per strato

campionamentoStNey1 = strata(londra1, c("person_capacity"), size = nk1, method = "srswor")
campioneStNey1 = getdata(londra1,campionamentoStNey1)
mediaSTNey1 = tapply(campioneStNey1$realSum, campioneStNey1$person_capacity, mean)
stdSTNey1 = tapply(campioneStNey1$realSum, campioneStNey1$person_capacity, sd)* sqrt(nst1/(nst1-1))

# realizzazione dello stimatore X medio stratificato con allocazione Neyman
XmedioSTNey1 = sum(st1/N * mediaSTNey1)


Neyman1 = rbind(nk1, perck1, mediaSTNey1, stdSTNey1)
rownames(Neyman1) = c("Numerosità", "Percentuale campionaria", "Media", "Stardard Deviation")
Neyman1

NeyProp=cbind(XmedioST1, XmedioSTNey1, mediaTot)
colnames(NeyProp)=c("Proporzionale", "Neyman", "Reale")
NeyProp

VARPesSTNey1=(st1-nk1)*st1*stdSTNey1^2/nk1
VARSTNey1=sum(VARPesSTNey1)
sdSTNey1=sqrt(VARSTNey1)

# Grafici di differenza tra allocazione proporzionale e di neyman
mediaPop1 = tapply(realSum, person_capacity, mean)
mediePersonCapacity = rbind(mediaSt1,mediaSTNey1,mediaPop1)
rownames(mediePersonCapacity) = c("Proporzionale", "Neyman", expression(mu[h]))
barplot((mediePersonCapacity), main = "Confronto medie negli strati ottenuti con allocazione", 
        sub = "Person Capacity", ylim = c(0,1100), beside = TRUE,col = viridis(3), border = "black",
        legend.text = c("Proporzionale", "Neyman", expression(mu[h])), args.legend=list(x="topleft") )

sdPersonCapacity = rbind(stddevSt1, stdSTNey1)
rownames(sdPersonCapacity) = c("Proporzionale", "Neyman")
barplot((sdPersonCapacity), main = "Confronto Std Dev negli strati ottenuti con allocazione"
        ,sub = "Person Capacity", beside = TRUE,col = viridis(2, option = "C"), border = "black", ylim=c(0,2000),
        legend.text = c("Proporzionale","Neyman"), args.legend=list(x="topleft") )


#############################################################################
# MIGLIORE STRATIFICAZIONE PROPORZIONALE
#############################################################################
stime=c(XmedioST,  XmedioST1)
varianze=c(VARST,VARST1)
sd=c(sdST, sdST1)
resultato=cbind(stime,varianze, sd)
rownames(resultato)=c("Room_type", "Person_capacity")
resultato

stime1=c(XmedioST1,  XmedioSTNey1)
varianze1=c(VARST1,VARSTNey1)
sd1=c(sdST1, sdSTNey1)
resultato1=cbind(stime1,varianze1, sd1)
colnames(resultato1)=c("stime", "varianze", "sd")
rownames(resultato1)=c("Proporzionale", "Neyman")
resultato1

#############################################################################
# PLOT FINALI
#############################################################################
Risultati = c(XmedioCCS, XmedioST,XmedioSTNey, XmedioST1, XmedioSTNey1); Risultati
barplot(Risultati, col = c("#8EE5EE","gold2","gold2" ,"orangered1","orangered1"), main = "Confronto delle medie ottenute", names.arg =  c("CCS", "PROP","NEY","PROP","NEY"), ylim=c(0,600))
abline(h = mediaTot, col="red")
text(x=0.1,y=mediaTot+15, expression(mu), col='red', cex=2)
legend("topright", bty="n", legend = c("room_type", "person_capacity"), col = c("gold2","orangered1"), fill= c("gold2","orangered1"))

Risultati = c(sdCCS, sdST,sdST1); Risultati
barplot(Risultati, col = c("#8EE5EE","gold2","gold2" ,"orangered1","orangered1"), main = "Confronto delle medie ottenute", names.arg =  c("CCS", "PROP","NEY","PROP","NEY"), ylim=c(0,600))
abline(h = mediaTot, col="red")
text(x=0.1,y=mediaTot+15, expression(mu), col='red', cex=2)
legend("topright", bty="n", legend = c("room_type", "person_capacity"), col = c("gold2","orangered1"), fill= c("gold2","orangered1"))


barplot(Risultati-mediaTot, col = c("#8EE5EE","gold2","gold2" ,"orangered1","orangered1"), ylim=c(-20,20), main=expression(paste("Differenza delle stime della media ottenute con ",mu)),
        names.arg = c("CCS", "PROP","NEY","PROP","NEY"))

legend("topright", bty="n", legend = c("room_type", "person_capacity"), col = c("gold2","orangered1"), fill= c("gold2","orangered1"))
