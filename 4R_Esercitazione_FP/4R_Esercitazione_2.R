
# Seguendo quanto fatto a lezione nello script "4_Esempio guidato" 
# analizzare i dati contenuti in "Dataset alimenti.txt" con PCA, HCA e KM 
# produrre un breve report con i risultati inserendo i grafici ottenuti 
# commentando i risultati come mostrato a lezione.

originale<-read.table("Dataset alimenti.txt", header=TRUE,sep="\t")

str(originale)
View(originale)

# Esplorazione preliminare dei dati

summary(originale) 

# Non sembrano esserci valori anomali, quindi proseguiamo 

###################################################################

#                       PCA

###################################################################

# Prepariamo il dataset per la PCA rimuovendo le colonne non numeriche

dataset<-as.matrix(originale[,-c(1:2)])

# salviamo il contenuto delle colonne che abbiamo escluso in dei vettori
Alimento<-originale$Alimento
Categoria<-originale$Categoria

# Procediamo con la PCA

PCA<-prcomp(dataset, center= TRUE, scale=TRUE)

#-- Estrazione delle varianze e della varianza cumulata

varianze<-PCA$sdev^2

varianzecum<-cumsum(varianze/sum(varianze)*100) 

#-- Scree plot

plot(varianze,pch=16,type="o", main="Scree plot")
abline(h=1,col="gray")

#-- Varianza cumulata %

plot(varianzecum, pch=16, type="o", main="Varianza cumulata %")
abline(h=c(50,75),col="gray")


#-- Plot loadings

plot(PCA$rotation[,1], PCA$rotation[,2], pch=16, main= "Plot Loadings")
text(PCA$rotation[,1], PCA$rotation[,2],labels=colnames(dataset),cex=0.8,pos=3)
abline(h=0,col="gray")
abline(v=0,col="gray")

#-- Plot scores

plot(PCA$x[,1], PCA$x[,2],type="n")
text(PCA$x[,1], PCA$x[,2],labels=Categoria,cex=0.8)
abline(h=0,col="gray")
abline(v=0,col="gray")

#-- Plot scores a colori

Color<-as.factor(Categoria)
levels(Color)<-c("red","blue","green","purple","orange", "pink", "black", "yellow")
Color<-as.character(Color)

plot(PCA$x[,1], PCA$x[,2], type="n", main= "Plot scores")
points(PCA$x[,1], PCA$x[,2],col=Color,cex=1.2,pch=16)
abline(h=0,col="gray")
abline(v=0,col="gray")

legend(locator(1), legend=c(levels(as.factor(Categoria))), fill=levels(as.factor(Color)))


#-- Biplot

biplot(PCA, choices=c(1,2), xlabs=Categoria, main= "Biplot")
abline(h=0,col="gray")
abline(v=0,col="gray")


#-- Peso delle variabili sperimentali su una PC

# PC1
plot(PCA$rotation[,1],type="h",xaxt="n",xlab="",ylab="Weights",main="PC1")
axis(1,at=seq(1,ncol(dataset),1), labels=colnames(dataset),cex.axis=0.6, las=2)
abline(h=0,col="gray")

# PC2
plot(PCA$rotation[,2],type="h",xaxt="n",xlab="",ylab="Weights",main="PC2")
axis(1,at=seq(1,ncol(dataset),1), labels=colnames(dataset),cex.axis=0.6, las=2)
abline(h=0,col="gray")

# PC3
plot(PCA$rotation[,3],type="h",xaxt="n",xlab="",ylab="Weights",main="PC3")
axis(1,at=seq(1,ncol(dataset),1), labels=colnames(dataset),cex.axis=0.6, las=2)
abline(h=0,col="gray")



#####################################################################

#                CLUSTERING GERARCHICO

#####################################################################

dataset<-as.matrix(originale[,-c(1:2)]) # Seleziono solo la parte di dataset con valori numerici

Alimento<-originale$Alimento
Categoria<-originale$Categoria


#-- Normalizzazione per variabile (E' NECESSARIA!!!)

dataNorm<-scale(dataset, center = TRUE, scale = TRUE)

#-- Clustering gerarchico

Dist<-dist(dataNorm, method="euclidean")

HcC<- hclust(Dist, method="complete")
HcS<- hclust(Dist, method="single")
HcA<- hclust(Dist, method="average")
HcW<- hclust(Dist, method="ward.D")

DendC<-as.dendrogram(HcC)
DendS<-as.dendrogram(HcS)
DendA<-as.dendrogram(HcA)
DendW<-as.dendrogram(HcW)

plot(DendC,main="Complete")
plot(DendS,main="Single")
plot(DendA,main="Average")
plot(DendW,main="Ward's method")

#--- Selezione numero cluster

Cut<-cutree(HcW, k= 7)

Newdata<-data.frame(K7= Cut, originale)
View(Newdata)

#--- Esplorazione risultati

Box<-boxplot(as.factor(Newdata$Acqua)~Newdata$K7, xlab="K7", ylab="Acqua", main="Acqua")

Cluster1<-Newdata[which(Newdata$K7==1),]


###########################################################################################

#                            K-MEANS CLUSTERING

###########################################################################################

#-- Normalizzazione

dataNorm<-scale(dataset, center = TRUE, scale = TRUE)


#-- Kmeans selezione numero cluster

Tot<-NULL
for(i in c(1:10)) {set.seed(7); km<- kmeans(dataNorm, centers=i,iter.max=50);
Tot<-c(Tot,km$tot.withinss)}


plot(Tot, xlab="K",ylab="Somma dei quadrati delle distanze intra-cluster", pch=16, type="o",
     main="Elbow plot")

# --  Numero di Cluster?

KM7_1<- kmeans(dataNorm, centers=7,iter.max=50)

Newdata2<-data.frame(KM7_1=KM7_1$cluster,originale)
View(Newdata2)


##############################################################

# Risultato HCA proiettato sul grafico degli score della PCA

##############################################################


#-- Plot scores a colori

Color<-as.factor(Categoria)
levels(Color)<-c("red","blue","green","purple","orange", "pink", "black", "yellow")
Color<-as.character(Color)

plot(PCA$x[,1], PCA$x[,2],type="n")
points(PCA$x[,1], PCA$x[,2],col=Color,cex=1.2,pch=16)
text(PCA$x[,1], PCA$x[,2],labels=Newdata$K7,cex=0.75,pos=3,offset=0.5)# con questo si rappresenta il risultato dell'HCA
abline(h=0,col="gray")
abline(v=0,col="gray")

legend(locator(1), legend=c(levels(as.factor(Categoria))),fill=levels(as.factor(Color)))

##############################################################

# Risultato KM proiettato sul grafico degli score della PCA

##############################################################


#-- Plot scores a colori

Color<-as.factor(Categoria)
levels(Color)<-c("red","blue","green","purple","orange", "pink", "black", "yellow")
Color<-as.character(Color)


plot(PCA$x[,1], PCA$x[,2],type="n")
points(PCA$x[,1], PCA$x[,2],col=Color,cex=1.2,pch=16)
text(PCA$x[,1], PCA$x[,2],labels=Newdata2$KM7_1,cex=0.75,pos=3,offset=0.5) # con questo si rappresenta il risultato del KM
abline(h=0,col="gray")
abline(v=0,col="gray")

legend(locator(1), legend=c(levels(as.factor(Categoria))), fill=levels(as.factor(Color)))



