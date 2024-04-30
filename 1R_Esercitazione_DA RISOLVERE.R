# Cancellare tutti gli oggetti gi? generati
# Pulire la Console

rm(list = ls())
cat("\014")  

#---------------#---------------#---------------#---------------
#---------------#---------------#---------------#---------------

#Creare un vettore di nome AA che contiene elementi da 5 a 11 con passo 1:

AA<-c(5:11)

#Visualizzare il contenuto di AA

AA 

#Creare un oggetto  di nome a che contenga il valore 2

a<-2

#Visualizzare il contenuto di a

a

#Creare un vettore BB che sia risultato della moltiplicazione di AA con a

BB<-AA*a

#Visualizzare il contenuto di BB

BB


#Creare il vettore CC che sia risultato della divisione per 10 di BB

CC<-BB/10

#Visualizzare il contenuto di CC

CC

#Creare il vettore DD come concatenazione del vettore CC con la variabile a,
#con gli elementi 1-3-5 di BB e con gli elementi da 2 a 5 di AA

DD<-c(CC,a,BB[c(1,3,5)],AA[2:5])

#Visualizzare il contenuto di DD

DD

#Calcolare il numero di elementi di DD (lunghezza)

length(DD)

#Individuare tutti gli elementi di DD>10

DD>10 # genera un vettore di valori booleani in cui True corrisponde a valori >10

which(DD>10) # ci dice le posizioni degli elementi di DD maggiori di 10

DD[DD>10]

#Individuare tutti gli elementi di DD<1.5

which(DD<1.5)
DD[DD<1.5]

#Individuare tutti gli elementi di DD<2 o >9

which(DD<2 | DD>9)
DD[DD<2 | DD>9]

#Individuare tutti gli elementi di compresi tra 5 e 10

which(5<DD & DD<10)
DD[DD>5 & DD<9]


#---------------#---------------#---------------#---------------
#---------------#---------------#---------------#---------------

#Visualizzare la documentazione della funzione max

?max # in Rstudio
help(max)

#Visualizzare gli argomenti previsti da una funzione

args(max)

# Creare e visualizzare il contenuto di un vettore numerico a piacere AV

AV<-c(1:10, 11, 13, 15.5, 17)

AV

# Creare e visualizzare il contenuto di un vettore numerico a piacere BV
# contenente un valore "non disponibile"

BV<- c(20:30, 31, 37,  41.5, NA)

BV

# Utilizzare la funzione max su AV

max(AV)

# Utilizzare la funzione max su BV

max(BV, na.rm=T) # aggiungo l'argomento na.rm=T per non ottenere come risultato NA

# Creare e visualizzare il contenuto di un vettore di caratteri a piacere CV

CV<- c("a", "b", "c", "d", "e", "f", "g", "gd", "ga")

# Utilizzare la funzione max su CV (cosa succede?)

max(CV)

# La funzione ritorna il carattere "massimo" in ordine alfabetico, 
# ossia, quello che ordinando il vettore in ordine alfabetico risulterebbe essere l'ultimo

# Creare e visualizzare il contenuto di un vettore logical a piacere DV

DV<- c(T,F, T,T,T,F,F,F,T)

# Utilizzare la funzione max su DV (cosa succede?)

max(DV)

# Ritorna il valore 1 (corrispondente a True), in quanto valori booleani T e F non possono avere un effettivo massimo;
# sono considerati come valori 0 e 1.



