### ESERCITAZIONE FINALE - GIUSEPPE RIZZO 999272 ###



chooseCRANmirror()

setwd("C:\\Users\\giuse\\OneDrive\\Documenti\\new UNITO\\Albano\\Laboratorio Tomatis R")
getwd()

install.packages("foreign")
library(foreign)

install.packages("car")
library(car)

install.packages("labstatR")
library ("labstatR")

install.packages("fBasics")
library ("fBasics")

install.packages("lm.beta")
library(lm.beta)

install.packages("psych")
library(psych)

install.packages("GPArotation")
library(GPArotation)




##1. ESERCIZIO##



RELAZIONE<-read.spss('RELAZIONE.sav',use.value.label=TRUE, to.data.frame=TRUE)

dim(RELAZIONE)
str(RELAZIONE)


#si preferisce cambiare il nome della variabile Genere in Sesso, in quanto è sociologicamente più consono alla variabile oggetto di studio - come si evince anche dalla traccia della presente Esercitazione#
#"(attenzione: nel dataset la variabile sesso viene denominata impropriamente GENERE)"


names(RELAZIONE)
colnames(RELAZIONE)[which(colnames(RELAZIONE) == 'Genere')] <- 'Sesso'
#controllo che il cambio nome sia stato svolto correttamente, verificando l'integrità dei dati nella variabile
table(RELAZIONE$Sesso)



#Prendere in considerazione le seguenti variabili:
#1.	sesso (Genere)
#2.	anzianità lavorativa (Anz_lav)
#3.	uso tecnologie (Uso_tec)
#4.	stipendio medio mensile (Stip_mens)
#5.	entusiasmo per il proprio lavoro (Entus_lav) 1 Always ----- 4 Never
#6.	lavoro distribuito in modo equo (Equi_lav) 1 Strongly agree --- 5 Strongly disagree
#7.	lavoro utile (Utile_lav) 1 Always ----- 4 Never


table(RELAZIONE$Sesso)

table(RELAZIONE$Anz_lav)

table(RELAZIONE$Uso_tec)

table(RELAZIONE$Stip_mens)

table(RELAZIONE$Entus_lav)

table(RELAZIONE$Equi_lav)

table(RELAZIONE$Utile_lav)



#PER RENDERE MAGGIORMENTE LEGGIBILI I RISULTATI INVERTIRE LE MODALITA' di Entus_lav, Equi_lav e Utile_lav#


RELAZIONE$Entus_lav2 = recode(RELAZIONE$Entus_lav, "1=4; 2=3; 3=2; 4=1")

RELAZIONE$Equi_lav2 = recode(RELAZIONE$Equi_lav, "1=5; 2=4; 3=3; 4=2; 5=1")

RELAZIONE$Utile_lav2 = recode(RELAZIONE$Utile_lav, "1=4; 2=3; 3=2; 4=1")


table(RELAZIONE$Entus_lav, RELAZIONE$Entus_lav2)

table(RELAZIONE$Equi_lav, RELAZIONE$Equi_lav2)

table(RELAZIONE$Utile_lav, RELAZIONE$Utile_lav2)



#OSSERVARE LE DISTRIBUZIONI DI FREQUENZA PERCENTUALI di tutte le VARIABILI CATEGORIALI#


#Sesso
length(RELAZIONE$Sesso)
table(RELAZIONE$Sesso)

SessoPERCENT<-round(table(RELAZIONE$Sesso)/length(RELAZIONE$Sesso)*100,2)
SessoPERCENT


#Uso_tec
length(RELAZIONE$Uso_tec)
table(RELAZIONE$Uso_tec)

Uso_tecPERCENT<-round(table(RELAZIONE$Uso_tec)/length(RELAZIONE$Uso_tec)*100,2)
Uso_tecPERCENT


#Entus_lav2

length(RELAZIONE$Entus_lav2)
table(RELAZIONE$Entus_lav2)

Entus_lav2PERCENT<-round(table(RELAZIONE$Entus_lav2)/length(RELAZIONE$Entus_lav2)*100,2)
Entus_lav2PERCENT


#Equi_lav2

length(RELAZIONE$Equi_lav2)
table(RELAZIONE$Equi_lav2)

Equi_lav2PERCENT<-round(table(RELAZIONE$Equi_lav2)/length(RELAZIONE$Entus_lav2)*100,2)
Equi_lav2PERCENT


#Utile_lav2

length(RELAZIONE$Utile_lav2)
table(RELAZIONE$Utile_lav2)

Utile_lav2PERCENT<-round(table(RELAZIONE$Utile_lav2)/length(RELAZIONE$Utile_lav2)*100,2)
Utile_lav2PERCENT



#OSSERVARE LE DISTRIBUZIONI DELLE VARIABILI CARDINALI#


length(RELAZIONE$Anz_lav)
table(RELAZIONE$Anz_lav)
Anz_lavPERCENT<-round(table(RELAZIONE$Anz_lav)/length(RELAZIONE$Anz_lav)*100,2)
Anz_lavPERCENT
quantile(RELAZIONE$Anz_lav)


length(RELAZIONE$Stip_mens)
table(RELAZIONE$Stip_mens)
Stip_mensPERCENT<-round(table(RELAZIONE$Stip_mens)/length(RELAZIONE$Stip_mens)*100,2)
Stip_mensPERCENT
quantile(RELAZIONE$Stip_mens)
barplot(Stip_mensPERCENT, main="Distribuzione Stipendio Mensile", col="goldenrod1", border = "black", ylab="% individui", xlab="Stipendio mensile")
mean(RELAZIONE$Stip_mens)



#OSSERVARE LA ASIMMETRIA E LA CURTOSI DELLE VARIABILI CARDINALI#


#Anz_lav#
#asimmetria#
skewness(RELAZIONE$Anz_lav)
#asimmetria leggermente >0 , ciò indica che la media e' maggiore della mediana, dunque lieve asimmetria dx#

#curtosi#
kurtosis(RELAZIONE$Anz_lav)
#curtosi <0 = forma platicurtica#


#Stip_mens
#asimmetria#
skewness(RELAZIONE$Stip_mens)
#asimmetria leggermente <0 , ciò indica che la media e' minore della mediana, dunque lieve asimmetria sx#

#curtosi#
kurtosis(RELAZIONE$Stip_mens)
#curtosi <0 = forma platicurtica#




##2.ESERCIZIO##



#RICODIFICARE TUTTE LE VARIABILI SECONDO LE ISTRUZIONI#
#CONTROLLARE CHE LE RICODIFICHE SIANO ANDATE A BUON FINE!#

##N.B.: La traccia dell'esercizio richiede, per la v.i. Equi_lav2, la dicotomizzazione secondo i seguenti valori: 1-2 = "Lavoro distribuito in modo non Equo" e 3-4 = "Distrbuito in modo Equo"; Ma, questo sarebbe incongruente con la variabile;##
##La variabile, infatti, presenta non 4 stati, ma 5; Dunque, scelgo di ricodificare la variabile in questo modo: 1-2 = "Lavoro distribuito in modo non Equo", e 4-5 = "Distribuito in modo Equo", lasciando fuori dalla dicotomia il valore 3 in quanto centrale e dunque riferibile ad una posizione neutrale.##


#Anz_lav#

RELAZIONE$Anz_lavCAT[RELAZIONE$Anz_lav<=7] = "breve"
RELAZIONE$Anz_lavCAT[RELAZIONE$Anz_lav>7 & RELAZIONE$Anz_lav<=15] = "media"
RELAZIONE$Anz_lavCAT[RELAZIONE$Anz_lav>15] = "lunga"
RELAZIONE$Anz_lavCAT<-ordered(RELAZIONE$Anz_lavCAT, levels=c("breve", "media", "lunga"))
RELAZIONE$Anz_lavCAT<-factor(RELAZIONE$Anz_lavCAT)
str(RELAZIONE)
table(RELAZIONE$Anz_lavCAT, RELAZIONE$Anz_lav)


#Uso_tec dicotomia#

RELAZIONE$Uso_tec_dico [RELAZIONE$Uso_tec=="All of the time"]="Almeno 3/4 del tempo"
RELAZIONE$Uso_tec_dico [RELAZIONE$Uso_tec=="Almost all of the time"]="Almeno 3/4 del tempo"
RELAZIONE$Uso_tec_dico [RELAZIONE$Uso_tec=="Around 3/4 of the time"]="Almeno 3/4 del tempo"
RELAZIONE$Uso_tec_dico [RELAZIONE$Uso_tec=="Around half of the time"]="Meno di 3/4 del tempo"
RELAZIONE$Uso_tec_dico [RELAZIONE$Uso_tec=="Around 1/4 of the time"]="Meno di 3/4 del tempo"
RELAZIONE$Uso_tec_dico [RELAZIONE$Uso_tec=="Almost never"]="Meno di 3/4 del tempo"
RELAZIONE$Uso_tec_dico [RELAZIONE$Uso_tec=="Never"]="Meno di 3/4 del tempo"
RELAZIONE$Uso_tec_dico<-factor(RELAZIONE$Uso_tec_dico)
str(RELAZIONE)


#Stip_mens dicotomia#

median(RELAZIONE$Stip_mens)
RELAZIONE$Stip_mens_dico [RELAZIONE$Stip_mens<=1300] = "sotto la mediana"
RELAZIONE$Stip_mens_dico [RELAZIONE$Stip_mens>1300] = "sopra la mediana"
RELAZIONE$Stip_mens_dico<-factor(RELAZIONE$Stip_mens_dico)
str(RELAZIONE)
table(RELAZIONE$Stip_mens, RELAZIONE$Stip_mens_dico)


#Entus_lav2 dicotomia#

RELAZIONE$Entus_lav2_dico [RELAZIONE$Entus_lav2=="1"] = "NON ENTUSIASTA"
RELAZIONE$Entus_lav2_dico [RELAZIONE$Entus_lav2=="2"] = "NON ENTUSIASTA"
RELAZIONE$Entus_lav2_dico [RELAZIONE$Entus_lav2=="3"] = "ENTUSIASTA"
RELAZIONE$Entus_lav2_dico [RELAZIONE$Entus_lav2=="4"] = "ENTUSIASTA"
RELAZIONE$Entus_lav2_dico<-factor(RELAZIONE$Entus_lav2_dico)
str(RELAZIONE)


#Equi_lav2 dicotomia#

RELAZIONE$Equi_lav2_dico [RELAZIONE$Equi_lav2=="1"] = "lavoro distribuito in modo non equo"
RELAZIONE$Equi_lav2_dico [RELAZIONE$Equi_lav2=="2"] = "lavoro distrubuito in modo non equo"
RELAZIONE$Equi_lav2_dico [RELAZIONE$Equi_lav2=="4"] = "distribuito in modo equo"
RELAZIONE$Equi_lav2_dico [RELAZIONE$Equi_lav2=="5"] = "distribuito in modo equo"
RELAZIONE$Equi_lav2_dico<-factor(RELAZIONE$Equi_lav2_dico)
str(RELAZIONE)


#Utile_lav2 dicotomia#

RELAZIONE$Utile_lav2_dico [RELAZIONE$Utile_lav2=="1"] = "NO"
RELAZIONE$Utile_lav2_dico [RELAZIONE$Utile_lav2=="2"] = "NO"
RELAZIONE$Utile_lav2_dico [RELAZIONE$Utile_lav2=="3"] = "SI"
RELAZIONE$Utile_lav2_dico [RELAZIONE$Utile_lav2=="4"] = "SI"
RELAZIONE$Utile_lav2_dico<-factor(RELAZIONE$Utile_lav2_dico)
str(RELAZIONE)



#COSTRUIRE UN GRAFICO A BARRE ROSSO CON BORDO NERO DELLA VARIABILE ANZIANITÀ LAVORATIVA (a 3 categorie) e CHIAMARE IL GRAFICO “ANZIANITÀ LAVORATIVA”#


length(RELAZIONE$Anz_lavCAT)
perc<-round(table(RELAZIONE$Anz_lavCAT)/473*100,2)
perc

perc1<-paste(perc, "%",sep="")
perc1

graficoabarreAnz_lavCAT<-barplot(perc, main="ANZIANITÀ LAVORATIVA", names.arg=c("Breve", "Media", "Lunga"), horiz=FALSE, col="red", border = "black", ylab="% individui", xlab="Classi di anzianità lavorativa")
text(x = graficoabarreAnz_lavCAT , y=perc/2,labels =perc1)

jpeg("Rizzo_G_graficobarre_anzianita_lav.jpg")
barplot(perc, main="ANZIANITÀ LAVORATIVA", names.arg=c("Breve", "Media", "Lunga"), horiz=FALSE, col="red", border = "black", ylab="% individui, per classi", xlab="Classi di anzianità lavorativa")
text(x = graficoabarreAnz_lavCAT , y=perc/2,labels =perc1)
dev.off()



#COME SI DISTRIBUISCE LO STIPENDIO MENSILE (Variabile ORIGINALE) in MEDIA RISPETTO AL SESSO#


table(RELAZIONE$Stip_mens)
table(RELAZIONE$Sesso)

distr2<-table(RELAZIONE$Stip_mens, RELAZIONE$Sesso)
distr2

distr3<-by(RELAZIONE$Stip_mens, RELAZIONE$Sesso, summary)
distr3

boxplot(RELAZIONE$Stip_mens~RELAZIONE$Sesso, main="Distribuzione Stipendio Mensile rispetto al Sesso", ylab="Stip_mens", xlab="Sesso")




##3.ESERCIZIO##

#Prendere in considerazione le seguenti variabili:
#Variabile dipendente: Entusiasmo per il proprio lavoro (ORIGINALE)
#Variabili indipendenti:
#1.	Sesso
#2.	Anzianità lavorativa (A TRE CATEGORIE)
#3.	Stipendio medio mensile (ORIGINALE)
#4.	Lavoro distribuito in modo equo (ORIGINALE)
#5.	Lavoro utile (ORIGINALE)

table(RELAZIONE$Entus_lav2)
#si utilizzano le variabili originali, come da indicazione, ma del tipo che abbiamo invertito inizialmente, per osservarle alla luce di un ordine crescente, più consono all'interpretazione della regressione#
table(RELAZIONE$Sesso)
table(RELAZIONE$Anz_lavCAT)
table(RELAZIONE$Stip_mens)
table(RELAZIONE$Equi_lav2)
table(RELAZIONE$Utile_lav2)



#COSTRUIRE UN GRAFICO SCATTERPLOT CON LE VARIABILI STIPENDIO MENSILE (ORIGINALE) E ANZIANITÀ LAVORATIVA (ORIGINALE) e CALCOLARE LA CORRELAZIONE#

scatterplot(RELAZIONE$Anz_lav, RELAZIONE$Stip_mens)

#o anche nel modo seguente

scatterplot(RELAZIONE$Stip_mens~RELAZIONE$Anz_lav)
abline(lm(RELAZIONE$Stip_mens~RELAZIONE$Anz_lav), col="firebrick3", lwd=1)


cor(RELAZIONE$Anz_lav, RELAZIONE$Stip_mens)


#COSTRUIRE IL MODELLO DI REGRESSIONE (con le variabili esposte all’inizio dell’esercitazione e per anzianità lavorativa utilizzare come categoria di riferimento “breve”)#


#per variabili originali esposte all'inizio, ho inteso che ci si riferisca alle variabili che abbiamo trasformato, per avere un ordine crescente nel grado di "misurazione" e non decrescente#
#Dunque, per un'interpretazione più lineare ed aritmetica della regressione, inserisco le variabili originali che abbiamo trasposto all'inizio#


#RELAZIONE$Entus_lav2<-factor(RELAZIONE$Entus_lav2)
#Si inserisce, per completezza d'informazione, la riga di comando precedente, ma preceduta dal simbolo # in quanto la v. era gia' stata resa factor precedentemente#

RELAZIONE$anzlavbreve = "0"
RELAZIONE$anzlavbreve[RELAZIONE$Anz_lavCAT=="breve"] = "1"
table(RELAZIONE$anzlavbreve)

RELAZIONE$anzlavmedia = "0"
RELAZIONE$anzlavmedia[RELAZIONE$Anz_lavCAT=="media"] = "1"
table(RELAZIONE$anzlavmedia)

RELAZIONE$anzlavlunga = "0"
RELAZIONE$anzlavlunga[RELAZIONE$Anz_lavCAT=="lunga"] = "1"
table(RELAZIONE$anzlavlunga)



#costruisco il modello di regressione#


modello1<-lm(RELAZIONE$Entus_lav2~RELAZIONE$Sesso+RELAZIONE$anzlavmedia+RELAZIONE$anzlavlunga+RELAZIONE$Stip_mens+RELAZIONE$Equi_lav2+RELAZIONE$Utile_lav2)
modello1



#COMMENTARE I RISULTATI OTTENUTI dalla regressione (soglia p-value per considerare statisticamente significativi i valori è 0.05)#
#CONTROLLARE LA COLLINEARITÀ#


summary(modello1)

vif(modello1)

#indice di tolleranza
1/vif(modello1)



#CALCOLARE GLI INTERVALLI DI CONFIDENZA#

confint(modello1)


#CALCOLARE I PESI BETA#

beta.modello1<-lm.beta(modello1)
beta.modello1
confint(beta.modello1)




##4. ESERCIZIO##


#Prendere in considerazione le seguenti variabili:
#Variabile dipendente: Entusiasmo per il proprio lavoro (DICOTOMIZZATA)
#Variabili indipendenti:
#1.	Sesso
#2.	Anzianità lavorativa (ORIGINALE)
#3.	Uso tecnologie (DICOTOMICA)
#4.	Lavoro utile (DICOTOMICA)
#5.	Stipendio medio mensile (ORIGINALE)
#6.	Lavoro distribuito in modo equo (ORIGINALE)

table(RELAZIONE$Entus_lav2_dico)
table(RELAZIONE$Sesso)
table(RELAZIONE$Anz_lav)
table(RELAZIONE$Uso_tec_dico)
table(RELAZIONE$Utile_lav2_dico)
table(RELAZIONE$Stip_mens)
table(RELAZIONE$Equi_lav2)



#CALCOLARE CHI QUADRATO e PHI QUADRATO tra Lavoro utile (DICOTOMICA) e Sesso


TAB.CONTINGENZA<-table(RELAZIONE$Utile_lav2_dico, RELAZIONE$Sesso)
TAB.CONTINGENZA

summary(TAB.CONTINGENZA)

N<-sum(table(RELAZIONE$Utile_lav2_dico))
PHISQ<-2.7304/N
PHISQ



#COSTRUIRE IL MODELLO DI REGRESSIONE (soglia p-value per considerare statisticamente significativi i valori è 0.05)


RELAZIONE$Entus_lav2_dico<-factor(RELAZIONE$Entus_lav2_dico)
str(RELAZIONE)

modello2<-glm(RELAZIONE$Entus_lav2_dico~RELAZIONE$Sesso+RELAZIONE$Anz_lav+RELAZIONE$Uso_tec_dico+RELAZIONE$Utile_lav2_dico+RELAZIONE$Stip_mens+RELAZIONE$Equi_lav2, family=binomial())
modello2

summary(modello2)



#CALCOLARE GLI ODDS RATIO del MODELLO DI REGRESSIONE


exp(modello2$coefficients)


#CALCOLARE GLI INTERVALLI DI CONFIDENZA DEGLI ODDS RATIO


exp(confint(modello2))




##ESERCIZIO 5 - ANALISI FATTORIALE##


RELAZIONE2<-read.csv('AFE.csv' ,sep=",", header=TRUE)
#eseguo il successivo comando, che risultera' molto lungo nell'output, solamente per visualizzare il dataframe completo ed intuirne la struttura in maniera piu' diretta#
RELAZIONE2
str(RELAZIONE2)
describe(RELAZIONE2)
dim(RELAZIONE2)
names(RELAZIONE2)
summary(RELAZIONE2)



#COSTRUIRE UN MODELLO DI ANALISI FATTORIALE CON DUE FATTORI (rotazione oblimin, metodo di estrazione Unweighted Least Squares)


R<-lowerCor(RELAZIONE2)
R

modelloAFE<-fa(R, nfactors = 2, n.obs = 300, rotate = "oblimin", fm = "uls")
modelloAFE



#Osservare la caduta degli autovalori (anche graficamente) e dire quanti fattori estrarrebbe Cattell e quanti ne estrarrebbe Harman (v. Slides lezione 18)


#col successivo comando print, R fa comparire le variabili in maniera riordinata rispetto a prima#
print(modelloAFE, sort=TRUE)
print(modelloAFE$values)
plot(modelloAFE$values, type="b", main="Scree plot Caduta degli Autovalori", col="royalblue4")

##Secondo il criterio di Cattell, dovremmo estrarre i valori che precedono il punto "di gomito" - quest'ultimo compreso, dunque sarebbero tre; Harman invece suggerisce di fermarsi al fattore precedente al gomito, in questo caso i fattori da estrarre sarebbero due;##



#Costruire il grafico del modello e commentare tutti i risultati ottenuti, dando un nome ai due fattori


loadings(modelloAFE)
fa.diagram(modelloAFE)


#dando un nome ai due fattori

#ai due fattori che conducono agli autovalori potrebbero essere ricondotti i seguenti nomi:
#ULS1: Propensione alle Materie Umanistiche
#ULS2: Propensione alle Materie Scientifiche

table(RELAZIONE$Anz_lavCAT)




###Giuseppe Rizzo - 999272###
###Università degli Studi di Torino - CdL Magistrale in Sociologia###
###Metodi Quantitativi per la Ricerca Sociale - Prof. Albano Roberto, Dott.ssa Tomatis Francesca###










