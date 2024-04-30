
##############################################################
#
#                   Installare un pacchetto in R
#
##############################################################


# Installare il pacchetto carData

install.packages("carData")

# Caricare il pacchetto carData

library(carData)


##############################################################
#
#                   SET DI DATI presenti in R
#
##############################################################

# In R sono presenti diversi set di dati già caricati,
# uno di questi si chiama Pottery ed è presente nel
# pacchetto carData

# Per visualizzare Pottery

Pottery

# per informazioni sul set di dati

help(Pottery)

# Usare la funzione adatta per confermare che Pottery é un dataframe

is.data.frame(Pottery)

# Visualizzare la struttura di Pottery

str(Pottery)

# Visualizzare la testa di Pottery

head(Pottery)

# Visualizzare la coda di Pottery

tail(Pottery)

# Visualizzare tutto il dataframe in una finestra a parte

View(Pottery)

# Visualizzare la statistica di base per ogni variabile di Pottery

summary(Pottery)

##############################################################
#
#                   GRAFICI A PUNTI
#
##############################################################


# Usare la funzione plot sul subset numerico del dataframe

PotteryNum<-Pottery[,-1]

plot(PotteryNum)

# Visualizzare grafici a punti di diverse variabili di
# (due alla volta) con la funzione plot

plot(Pottery$Al,Pottery$Fe)
plot(Pottery$Al,Pottery$Mg)
plot(Pottery$Al,Pottery$Ca)
plot(Pottery$Al,Pottery$Na)
plot(Pottery$Fe,Pottery$Mg)
plot(Pottery$Fe,Pottery$Ca)
plot(Pottery$Fe,Pottery$Na)
plot(Pottery$Mg,Pottery$Ca)
plot(Pottery$Mg,Pottery$Na)
plot(Pottery$Ca,Pottery$Na)

# Migliorare il grafico con titolo, etichette degli assi, colori, ecc...

plot(Pottery$Al,Pottery$Fe, main="Grafico Allumino e Ferro",
     xlab="Alluminio", ylab="Ferro", col=c(2), pch=16)

# Aggiungere una seconda serie di dati al grafico con le funzioni lines o points

points(Pottery$Mg, pch=16)
lines(Pottery$Mg)

# Colorare i punti a seconda di una colonna di categorie

Color<-Pottery$Site
levels(Color)<-c("red","blue","green", "purple")
Color<-as.character(Color)

plot(Pottery$Al,Pottery$Fe, main="Grafico Allumino e Ferro",
     xlab="Alluminio", ylab="Ferro", col=Color, pch=16) 

# Aggiungere altri elementi come testo, frecce, legenda, griglia

catnames<-levels(Pottery$Site)
legend("topright", legend=catnames, fill= c("red","blue","green", "purple"))

grid(lwd=0.75, col="grey")


# Utilizzare la funzione locator, anche dentro ad altre funzioni (es. posizione legenda)

plot(Pottery$Al,Pottery$Fe, main="Grafico Allumino e Ferro",
     xlab="Alluminio", ylab="Ferro", col=Color, pch=16) 


legend(locator(1), legend=catnames, fill= c("red","blue","green", "purple"))

##############################################################
#
#                   GRAFICI A SCATOLA
#
##############################################################


# Visualizzare grafici a scatola di diverse variabili di
# del dataframe (una alla volta) con la funzione boxplot (e i suoi diversi argomenti)

boxplot(Pottery$Al, data=Pottery, xlab="Al", main="Boxplot Al")
boxplot(Pottery$Fe, data=Pottery, xlab="Fe", main="Boxplot Fe")
boxplot(Pottery$Mg, data=Pottery, xlab="Mg", main="Boxplot Mg")
boxplot(Pottery$Ca, data=Pottery, xlab="Ca", main="Boxplot Ca")
boxplot(Pottery$Na, data=Pottery, xlab="Na", main="Boxplot Na")

# Migliorare il grafico con titolo, etichette degli assi, colori, ecc...

boxplot(Pottery$Al, data=Pottery, xlab="Al", main="Boxplot Al", col=2)
boxplot(Pottery$Fe, data=Pottery, xlab="Fe", main="Boxplot Fe", col=3)
boxplot(Pottery$Mg, data=Pottery, xlab="Mg", main="Boxplot Mg", col=4)
boxplot(Pottery$Ca, data=Pottery, xlab="Ca", main="Boxplot Ca", col=5)
boxplot(Pottery$Na, data=Pottery, xlab="Na", main="Boxplot Na", col=6)

# Produrre un grafico con pi? di un boxplot usando una variabile numerica del dataframe
# contro una categoria

boxplot(Pottery$Al~Pottery$Site, xlab="Site", ylab="Al", main=" Al per ogni sito", 
        col= c("red","blue","green", "purple")) 


##############################################################
#
#                   ISTOGRAMMI
#
##############################################################


# Visualizzare istogrammi di diverse variabili del
# dataframe(una alla volta) con la funzione hist(e i suoi diversi argomenti)

hist(Pottery$Al, main="Grafico Al", freq=F, xlab="Al")
hist(Pottery$Fe, main="Grafico Fe", freq=F, xlab="Fe")
hist(Pottery$Mg, main="Grafico Mg", freq=F, xlab="Mg")
hist(Pottery$Ca, main="Grafico Ca", freq=F, xlab="Ca")
hist(Pottery$Na, main="Grafico Na", freq=F, xlab="Na")

# Aggiungere una funzione gaussiana di densit? di probabilit?
# all'istogramma

# la aggiungo al primo grafico creato
hist(Pottery$Al, main="Grafico Al", freq=F, xlab="Al")

xfit<-seq(min(Pottery$Al),max(Pottery$Al), length=100)
yfit<-dnorm(xfit, mean=mean(Pottery$Al),sd=sd(Pottery$Al))

lines(xfit,yfit, col="red", lwd="2")

# Aggiungere una funzione di densit? kernel di probabilit?
# all'istogramma (provare per diverse funzioni kernel e diverse ampiezze di banda)

hist(Pottery$Al, main="Grafico Al", freq=F, xlab="Al")

d<-density(Pottery$Al, kernel="triangular")
d1<-density(Pottery$Al, kernel="rectangular")
d2<-density(Pottery$Al, kernel="cosine")
d3<-density(Pottery$Al, kernel="biweight")

lines(d, col="blue", lwd="2")
# a partire da questo metodo si sovrappongono 
lines(d, col="green", lwd="2")
lines(d, col="purple", lwd="2")
lines(d, col="yellow", lwd="2")
lines(d, col="orange", lwd="2")

#####################################################################

#                CORRELAZIONE TRA VARIABILI

#####################################################################


# Installare pacchetto corrplot

install.packages("corrplot")

# Caricare il pacchetto

library(corrplot)


# Generare la matrice di correlazione (solo colonne con valori numerici!)

Cr<-cor(PotteryNum)


# Visualizzare la matrice di correlazione in una nuova finestra

View(Cr)

# Salvare la matrice di correlazione su file txt

write.table(Cr, "matrice di correlazione pottery.txt", col.names = T, row.names = F, sep=";", quote = F)


# Visualizzare la matrice di correlazione su grafico

corrplot(Cr, method="ellipse")

windows() #nuova finestra per il grafico #non funziona su mac







