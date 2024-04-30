
#####################################################################

#                Importare i dati

#####################################################################

# Il dataset da importare contiene analisi di 14 composti organici volatili
# di due tipi di whiskey (43 campioni in tutto) di generico tipo 1 e 2 (seconda colonna)

# Scopo dello studio: distinguere i due tipi di whiskey
# utilizzando solo la diversa composizione di composti organici volatili

originale<-read.table("Dataset_whiskey.txt", header=TRUE,sep="\t")

str(originale)
View(originale)


# Esplorazione preliminare dei dati

# Visualizza le statistiche di base per ogni variabile

summary(originale)

# Non sembrano esserci variabili con valori anomali, 
# tuttavia risulta poco utile considerare le statistiche per la variabile sample e la variabile type,
# vosto che indicano rispettivamente il numero del campione da cui derivano i dati di quella riga e il tipo di whiskey

###################################################################

#                       PCA

###################################################################


# Preparare il dataset da utilizzare nell'analisi PCA

# Eliminiamo le colonne "sample" e "type" dal dataset e lo salviamo come matrice
dataset<-as.matrix(originale[,-c(1,2)])

# Salviamo le colonne "type" e "sample" in dei vettori

Type<-originale$type
Sample<-originale$sample

#Procediamo con la PCA

PCA<-prcomp(dataset, center = T, scale = T) # centriamo e normalizziamo 

#-- Estrazione delle varianze e della varianza cumulata

varianze <- PCA$sdev^2

varianze_cum <- cumsum(varianze/sum(varianze)*100)

#-- Scree plot

plot(varianze, pch=16, type= "o", main="Scree plot")
abline(h=1,col="gray")

#-- Varianza cumulata %

plot(varianze_cum, pch=16, type= "o", main="Varianza cumulata %")
abline(h=c(50,75),col="gray")

#-- Quante componenti principali sono sufficienti per descrivere la varianza del dataset?

# Dai grafici deduciamo che sono sufficienti 3 componenti per descrivere la varianza del dataset. 
# Osservando lo Scree plot notiamo un gomito tra la terza e la quarta componente, le ultime a mostrare varianza \> 1. 
# Dal plot della varianza cumulata notiamo che tra le 2 e le 3 componeneti sono sufficienti 
# a spiegare tra il 50 % e il 75 % della varianza del dataset.

#-- Plot loadings

plot(PCA$rotation[,1], PCA$rotation[,2], pch=16, main= "Plot loadings")
text(PCA$rotation[,1], PCA$rotation[,2], labels=colnames(dataset), cex=0.8, pos=3)
abline(h=0, col="gray")
abline(v=0, col="gray")

#-- Plot scores (utilizzare 2 colori diversi per identificare i tipi di whiskey nel plot)

Color<-as.factor(Type)
levels(Color)<-c("red","blue")
Color<-as.character(Color)

plot(PCA$x[,1], PCA$x[,2], type="n", main="Plot scores")
points(PCA$x[,1], PCA$x[,2], col=Color, cex=1.2, pch=16)
abline(h=0,col="gray")
abline(v=0,col="gray")

legend(locator(1), legend=c(1:2),fill=levels(as.factor(Color)))


#-- Biplot

biplot(PCA, choices =c(1,2), xlabs=Type, main="Biplot")
abline(h=0,col="gray")
abline(v=0,col="gray")

#-- Grafico del peso delle variabili sperimentali per ogni PC scelta

plot(PCA$rotation[,1],type="h",xaxt="n",xlab="",ylab="Weights",main="PC1")
axis(1,at=seq(1,ncol(dataset),1), labels=colnames(dataset),cex.axis=0.6, las=2)
abline(h=0,col="gray")

plot(PCA$rotation[,2],type="h",xaxt="n",xlab="",ylab="Weights",main="PC2")
axis(1,at=seq(1,ncol(dataset),1), labels=colnames(dataset),cex.axis=0.6, las=2)
abline(h=0,col="gray")

plot(PCA$rotation[,3],type="h",xaxt="n",xlab="",ylab="Weights",main="PC3")
axis(1,at=seq(1,ncol(dataset),1), labels=colnames(dataset),cex.axis=0.6, las=2)
abline(h=0,col="gray")



#####################################################################

#                CLUSTERING GERARCHICO

#####################################################################

# Continuiamo ad utilizzare lo stesso dataset, visto che già all'inizio abbiamo rimosso le colonne non numeriche

#-- Normalizzazione per variabile (E' NECESSARIA!!!)

dataNorm<-scale(dataset, center = TRUE, scale = TRUE)

#-- Clustering gerarchico (usare il metodo "complete")

Dist<-dist(dataNorm, method="euclidean")

HcC<- hclust(Dist, method="complete")

#-- Grafico con il dendrogramma

DendC<-as.dendrogram(HcC)
plot(DendC, main="Complete")

#--- Selezione numero cluster (provare 2 cluster)

Cut<-cutree(HcC, k= 2)

Newdata<-data.frame(K2= Cut, originale)
View(Newdata)

#--- Esplorazione risultati (verificare se effettivamente si riconoscono i 2 tipi di whiskey)

Box<-boxplot(Newdata$A~Newdata$K2, main="Variabile A", col=c("blue", "red"), xlab="K2", ylab = "A")

Cluster1<-Newdata[which(Newdata$K2==1),]

Cluster1

#I due tipi di whiskey vengono riconosciuti, ma ci sono comunque alcuni errori nell'assegnazione ai due diversi cluster.

###########################################################################################

#                            K-MEANS CLUSTERING

###########################################################################################


#-- Normalizzazione

dataNorm<-scale(dataset, center = TRUE, scale = TRUE)

#-- Kmeans selezione numero cluster

Tot<-NULL
for(i in c(1:6)) {set.seed(7); km<- kmeans(dataNorm, centers=i,iter.max=50);
Tot<-c(Tot,km$tot.withinss)}


#-- Elbow plot

plot(Tot, xlab="K",ylab="Somma dei quadrati delle distanze intra-cluster", pch=16, type="o",
     main="Elbow plot")

#-- In quanti cluster vengono divisi i campioni? Esplorare i risultati


KM2<- kmeans(dataNorm, centers=2,iter.max=50)

Newdata2<-data.frame(KM2=KM2$cluster, originale)

View(Newdata2)

# I campioni sono divisi in due cluster, abbiamo selezionato due centroidi, in questo caso la performance di k-means
# risulta essere migliore di quella del clustering gerarchico: quasi tutti i campioni sono nei cluster corretti.


