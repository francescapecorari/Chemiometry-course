# Cancellare tutti gli oggetti gi? generati
# Pulire la Console

rm(list = ls())
cat("\014")  

########################################################################
#   CREARE OGGETTI CHE SERVONO PER I CALCOLI SUCCESSIVI (eseguire tutto)
########################################################################

AA<-c(5:11)
a<-2
BB<-AA*a
CC<-BB/10
DD<-c(CC,a,BB[c(1,3,5)],AA[2:5])


##############################################################
#      Funzioni statistiche su VETTORI
##############################################################

#Per gli esercizi seguenti utilizzare il vettore DD:

#Calcolare il valore minimo di DD

min(DD)

#Calcolare il valore massimo di DD

max(DD)


#Calcolare la somma degli elementi di DD

sum(DD)
 
#Calcolare la media di DD

mean(DD)

# Generare un vettore degli scarti Sc

Sc<- DD-mean(DD)

# Verificare che la somma degli scarti dia un numero prossimo a zero

sum(Sc) # il risulatato è 7.105427e-15, un numero molto vicino a zero

#Calcolare la mediana di DD

median(DD)

# Calcolare il 1? quartile

quantile(DD, probs=0.25)

# Calcolare il 3? quartile

quantile(DD, probs=0.75)

# Calcolare la varianza

var(DD)

# Calcolare la deviazione standard

sd(DD) 

#oppure

sqrt(var(DD))

##############################################################
#  Individuare l'indice di POSIZIONE di elementi secondo criteri
##############################################################

#Per gli esercizi seguenti utilizzare il vettore DD:

#Individuare l'indice di POSIZIONE degli elementi uguali a 2

which(DD==2)

#Individuare l'indice di POSIZIONE degli elementi diversi da 2

which(DD!=2)

#Individuare l'indice di POSIZIONE degli elementi maggiori di 14

which(DD>14)

#Individuare l'indice di POSIZIONE degli elementi minori di 1.8

which(DD<1.8)

#Individuare l'indice di POSIZIONE degli elementi minori di 1.4 e maggiori di 10

c(which(DD<1.4), which(DD >10)) 

#Individuare l'indice di POSIZIONE degli elementi compresi tra 6 e 14

which(DD>6 & DD<14)

##############################################################
#
#                            MATRICI
#
##############################################################


#Creare una matrice di nome MMM a 5 righe e 3 colonne unendo
#con la  funzione cbind 3 vettori a 5 elementi creati a 
#partire da sottoinsiemi (da 5 elementi a piacere) dei vettori AA, CC e DD

EE<-c(AA[c(1,2)],CC[3:5])
FF<-c(DD[5:7],CC[1],AA[7])
GG<-c(CC[4], DD[13:15], AA[4])

MMM<-cbind(EE,FF,GG)

#Visualizzare il contenuto di MMM

MMM

#Visualizzare la classe di MMM con l'opportuna funzione

class(MMM)

#Visualizzare le dimensioni di MMM con l'opportuna funzione

dim(MMM)

#Visualizzare il numero di righe di MMM con l'opportuna funzione

nrow(MMM)

#Visualizzare il numero di colonne di MMM con l'opportuna funzione

ncol(MMM)

#Visualizzare il nome delle colonne di MMM

colnames(MMM)

#Attribuire alle colonne di MMM i nomi V1, V2 e V3

colnames(MMM)<-c("V1","V2","V3")

#Attribuire alle righe di MMM i nomi s1, s2, s3, s4 e s5

rownames(MMM)<-c("s1", "s2", "s3", "s4", "s5")

#Visualizzare l'elemento di MMM presente alla riga 3 e colonna 2

MMM[3,2]

#Visualizzare la riga 4 di MMM

MMM[4,]

#Visualizzare la colonna 2 di MMM

MMM[,2]

#Visualizzare le colonne 1 e 3 di MMM

MMM[, c(1,3)]

#Visualizzare le colonne da 1 a 2 di MMM

MMM[, c(1:2)]

#Visualizzare le righe 1 e 5 di MMM

MMM[c(1,5),]

#Visualizzare le righe da 2 a 4 di MMM

MMM[c(2:4), ]

#Visualizzare il sottoinsieme di elementi presenti nelle righe
# 2 e 5 e nelle colonne da 2 a 3

MMM[c(2,5), c(2:3)]


##############################################################
#
#                            DATAFRAME
#
##############################################################

#Creare un dataframe di nome DAF a partire dai seguenti vettori con intestazioni di colonna
#rispettivamente:"Animale","v1","v2","v3"

aa<-c("cane","gatto","topo","cane","gatto","topo")
bb<-c(1,6,8,9,4,3)
cc<-c(-0.5,-0.9,-0.1,-0.3,-0.9,-0.4)
dd<-c(12,32,12,23,12,11)

DAF<-data.frame(Animale=aa,v1=bb,v2=cc,v3=dd)

#Visualizzare il contenuto di DAF

View(DAF)
# oppure
DAF

#Visualizzare la struttura di DAF

str(DAF)

#Visualizzare la "testa" di DAF

head(DAF)

#Visualizzare la "coda" di DAF

tail(DAF)

#Visualizzare la statistica di base per colonna di DAF

summary(DAF)

#Visualizzare la classe di DAF con l'opportuna funzione

class(DAF)

#Visualizzare le dimensioni di DAF con l'opportuna funzione

dim(DAF)

#Visualizzare il numero di righe di DAF con l'opportuna funzione

nrow(DAF)

#Visualizzare il numero di colonne di DAF con l'opportuna funzione

ncol(DAF)

#Visualizzare il nome delle colonne di DAF

colnames(DAF)

##############################################################
#       Individuare elementi nei DATAFRAME
##############################################################


#Visualizzare l'elemento di DAF presente alla riga 5 e colonna 1

DAF[5,1]

#Visualizzare la riga 3 di DAF

DAF[3,]

#Visualizzare la colonna 1 di DAF con uso del simbolo $

DAF$Animale

##############################################################
#  Individuare elementi di un dataframe secondo criteri
##############################################################

#Individuare righe che soddisfano la condizione colonna 1 = "cane"

DAF[which(DAF$Animale=="cane"),]

#Individuare righe che soddisfano la condizione colonna 1 = "cane"
#e la condizione colonna 3 = -0.5

DAF[which(DAF$Animale=="cane" & DAF$v2==-0.5),]

#Individuare righe che soddisfano la condizione colonna 4 < 12
#o colonna 4 > 23

DAF[which(DAF$v3<12 | DAF$v3>23),]



