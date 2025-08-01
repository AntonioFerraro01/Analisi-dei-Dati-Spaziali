library(readxl)
library(terra)
library(tripack)    
library(spdep)
library(moments)
library(lmtest)
library(nortest)
library(car)
library(spatialreg)
library(leaflet)
library(RColorBrewer)
library(spData)
library(ggplot2)
library(sf)           
library(rcompanion)
library(mapview)

####CARICO I FILE####
Incidenti_Stradali = read_excel("File/Dataset_Incidenti_Stradali.xlsx")
View(Incidenti_Stradali)
attach(Incidenti_Stradali)

path=file.choose() # QUI BISOGNA CARICARE IL FILE .shp ALL'INTERNO DELLA CARTELLA "File"
path
shp=st_read(path)
mapview(shp)
View(shp)

dataset=merge(shp, Incidenti_Stradali, by="NUTS_ID")
class(dataset)
View(dataset)
mapview(dataset)
attach(dataset)

####DATI DI SINTESI####
summary(`Incidenti Stradali`)

par(mfrow=c(1,2))
plotNormalHistogram(`Incidenti Stradali`, main="Incidenti Stradali - Frequenze",  xlab="Incidenti Stradali",ylab="Frequenze")
boxplot(`Incidenti Stradali`, main = "BoxPlot Incidenti Stradali", ylab="Incidenti Stradali")
par(mfrow=c(1,1))

Palette=brewer.pal(5,"OrRd")
mapview(dataset, zcol="Incidenti Stradali", layer.name="Livelli Incidenti Stradali", at=quantile(`Incidenti Stradali`), col.regions=Palette)

####MATRICE DI CONTIGUITÁ DI ORDINE 1####
#QUEEN
Lista_Contiguità_Q=poly2nb(dataset,queen = T)
Lista_Contiguità_Q
View(Lista_Contiguità_Q)
class(Lista_Contiguità_Q)
summary(Lista_Contiguità_Q)
Centroidi=st_centroid(st_geometry(dataset),of_largest_polygon = T)
Centroidi
plot(st_geometry(dataset), col="lightgrey", main = "Matrice di Contiguità del 1° Ordine")
plot(Lista_Contiguità_Q, coords=Centroidi, add=T, col="blue", lwd=2)

#LISTA DEI PESI BINARI
Lista.B_Contiguità_Q=nb2listw(Lista_Contiguità_Q, style = "B", zero.policy = T)
View(Lista.B_Contiguità_Q)
#MATRICE DEI PESI BINARI
Matrice_Contiguità_Q=nb2mat(Lista_Contiguità_Q, style = "B", zero.policy = T)
View(Matrice_Contiguità_Q)
#MATRICE DEI PESI BINARI STANDARDIZZATA PER RIGA
Matrice.S_Contiguità_Q=nb2mat(Lista_Contiguità_Q, style = "W", zero.policy = T)
View(Matrice.S_Contiguità_Q)

####MATRICE DELLE DISTANZE####
Centroidi=st_centroid(st_geometry(dataset), of_largest_polygon = T)
Matrice_Banda_Distanza=dnearneigh(Centroidi,0,200000)  #200KM
summary(Matrice_Banda_Distanza)
plot(st_geometry(dataset),col="lightgrey", main = "Matrice a Bande di Distanza (200Km)")
plot(Matrice_Banda_Distanza, coords=Centroidi, add=T, col="red", lwd=1)

#RAPPRESENTAZIONE GRAFICA
par(mfrow=c(1,2))
plot(st_geometry(dataset), col="lightgrey", main = "Matrice di Contiguità del 1° Ordine")
plot(Lista_Contiguità_Q, coords=Centroidi, add=T, col="blue", lwd=2)
plot(st_geometry(dataset),col="lightgrey", main = "Matrice a Bande di Distanza (200Km)")
plot(Matrice_Banda_Distanza, coords=Centroidi, add=T, col="red", lwd=1)
par(mfrow=c(1,1))

####RITARDO SPAZIALE####
#STANDARDIZZAZIONE PER RIGA
Lista_Ritardo_Spaziale=nb2listw(Lista_Contiguità_Q, style = "W", zero.policy = T)
View(Lista_Ritardo_Spaziale)

#RITARDO SPAZIALE
Ritardo_Spaziale_Incidenti_Strad=lag.listw(Lista_Ritardo_Spaziale, dataset$`Incidenti Stradali`, zero.policy = T)
Ritardo_Spaziale_Incidenti_Strad

dataset$Ritardo_Spaziale_Incidenti_Strad=Ritardo_Spaziale_Incidenti_Strad
View(dataset)

#INDICE DI MORAN I      #AUTOCORRELAZIONE BASSA
Lista_Ritardo_Spaziale=nb2listw(Lista_Contiguità_Q, style = "W", zero.policy = T)
moran(dataset$`Incidenti Stradali`, listw = Lista_Ritardo_Spaziale, length(dataset$`Incidenti Stradali`),Szero(Lista_Ritardo_Spaziale), zero.policy = T)
moran.test(dataset$`Incidenti Stradali`, Lista_Ritardo_Spaziale, zero.policy = T)

#PLOT INDICE DI MORAN I
Moran_Scatter_Plot=moran.plot(dataset$`Incidenti Stradali`,Lista_Ritardo_Spaziale,zero.policy = T, pch=20, col="Blue", labels = dataset$NUTS_ID, xlab = "Incidenti Stradali", ylab = "Incidenti Stradali Ritardati Spazialmente", main="Plot Moran's I")
summary(Moran_Scatter_Plot)
text(Moran_Scatter_Plot$x, Moran_Scatter_Plot$wx, labels=dataset$NUTS_ID, pos=4, cex=0.7, col="Darkred")

#PLOT VALORI STANDARDIZZATI
S_Incidenti=scale(dataset$`Incidenti Stradali`)
S_Incidenti
moran.plot(as.vector(S_Incidenti), Lista_Ritardo_Spaziale, zero.policy = T, pch=20, col="Blue", xlab = "Incidenti Standardizzati", ylab = "Incidenti Standardizzati Ritardati Spazialmente", main="Plot Moran's I con Valori Standardizzati")

#GEARY'S C              #AUTOCORRELAZIONE BASSA
geary.test(dataset$`Incidenti Stradali`,Lista_Ritardo_Spaziale,zero.policy = T)

####MODELLO OLS####
OLS_Chi=lm(`Incidenti Stradali`~`Degenza Ospedaliera`+`Ore Lavorative per Regione`+`Livello di Istruzione`+`Occupazione Regionale`+`Popolazione Regionale`+`Reddito`+`Veicoli Immatricolati`, data=dataset)
summary(OLS_Chi) #ADJUSTED R-SQUARED = 0.6535
AIC(OLS_Chi)     #AIC = 3840.874

Stepwise=step(OLS_Chi, directory="both")
# Backward=step(OLS_Chi, direction ="backward") DA COME RISULTATO LE STESSE VARIABILI DELLO STEPWISE

OLS_Step=lm(`Incidenti Stradali`~`Degenza Ospedaliera`+`Ore Lavorative per Regione`+`Occupazione Regionale`+`Popolazione Regionale`+Reddito+`Veicoli Immatricolati`, data = dataset)
summary(OLS_Step) #ADJUSTED R-SQUARED = 0.6521
AIC(OLS_Step)     #AIC = 3840.767

#VERIFICO LA MULTICOLLINEARITÀ NEL MODELLO OLS STEP
library(car)
vif(OLS_Step)

# DATO CHE IL VIF PER POPOLAZIONE REGIONALE RISULTA ELEVATO, MA RISULTA ANCHE ESSERE UNA DELLE VARIABILI PIÙ SIGNIFICATIVE DEL MODELLO, ALLORA ABBIAMO OPTATO DI VEDERE QUALE VARIABILE È MAGGIORMENTE CORRELATA A QUESTA (POPOLAZIONE REGIONALE), TRAMITE LA MATRICE DI CORRELAZIONE E VEDERE SE È POSSIBILE ELIMINARE QUELLA VARIABILE DAL MODELLO.

####MATRICE DI CORRELAZIONE####
# install.packages("corrplot")
library(corrplot)
# install.packages("ggcorrplot")
library(ggcorrplot)
attach(dataset)
Varibili_Correlate=Incidenti_Stradali[, c("Incidenti Stradali", "Degenza Ospedaliera", "Ore Lavorative per Regione", "Occupazione Regionale", "Popolazione Regionale", "Reddito", "Veicoli Immatricolati")] #HO RIPRESO IL FILE INCIDENTI_STRADALI PERCHÈ L'OGGETTO dataset ORAMAI È UN FILE SHP E QUINDI INCLUDERÀ SEMPRE LA COMPONENTE GEOGRAFICA, ANCHE NELLA CREAZIONE DELLA MATRICE DI CORRELAZIONE
Matrice_Correlazione=cor(Varibili_Correlate, method = "pearson")
print(Matrice_Correlazione)
ggcorrplot(Matrice_Correlazione, method = "square", type = "full", lab = TRUE, lab_size = 3.5, colors = c("#6D9EC1", "white", "#E46726"), outline.color = "black", title = "Matrice di Correlazione", ggtheme = ggplot2::theme_minimal())

#LA VARIABILE 'VEICOLI IMMATRICOLATI' PRESENTA UNA CORRELAZIONE ELEVATA CON LA VARIABILE 'POPOLAZIONE REGIONALE'. PUò RISULTARE UTILE, PER RIDURRE IL VIF, ELIMINARE QUESTA VARIABILE 'VEICOLI IMMATRICOLATI'.

OLS_New=lm(`Incidenti Stradali`~`Degenza Ospedaliera`+`Ore Lavorative per Regione`+`Occupazione Regionale`+`Popolazione Regionale`+Reddito, data = dataset)
summary(OLS_New) #ADJUSTED R-SQUARED = 0.6395
AIC(OLS_New)     #AIC = 3847.3847

# VERIFICO NUOVAMENTE LA MULTICOLLINEARITÀ SUL MODELLO NEW
library(car)
vif(OLS_New)

####ANALISI DEI RESIDUI SUL MODELLO OLS NEW####
#CALCOLO VALORI TEORICI E RESIDUI
Valori_Teorici_Multipli=fitted(OLS_New)
Valori_Teorici_Multipli
dataset$Valori_Teorici_Multipli=Valori_Teorici_Multipli

Residui_Multipli=residuals(OLS_New)
Residui_Multipli
dataset$Residui_Multipli=Residui_Multipli

#1. VERIFICO LA LINEARITÀ
#SCATTERPLOT RESIDUI OLS VS. VALORI TEORICI OLS
plot(Valori_Teorici_Multipli,Residui_Multipli,xlab="Valori Teorici", ylab="Residui", main = "Grafico Tra Valori Teorici e Residui", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI OLS VS. INCIDENTI STRADALI OLS (VARIABILE DIPENDENTE)
plot(`Incidenti Stradali`,Residui_Multipli,xlab="Incidenti Stradali", ylab="Residui", main = "Grafico Tra Residui e Incidenti Stradali", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI OLS VS. DEGENZA OSPEDALIERA OLS (VARIABILE INDIPENDENTE)
plot(`Degenza Ospedaliera`,Residui_Multipli,xlab="Degenza Ospedaliera", ylab="Residui", main = "Grafico Tra Residui e Degenza Ospedaliera", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI OLS VS. ORE LAVORATIVE PER REGIONE OLS (VARIABILE INDIPENDENTE)
plot(`Ore Lavorative per Regione`,Residui_Multipli,xlab="Ore Lavorative per Regione", ylab="Residui", main = "Grafico Tra Residui e Ore Lavorative per Regione", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI OLS VS. OCCUPAZIONE REGIONALE OLS (VARIABILE INDIPENDENTE)
plot(`Occupazione Regionale`,Residui_Multipli,xlab="Occupazione Regionale", ylab="Residui", main = "Grafico Tra Residui e Occupazione Regionale", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI OLS VS. POPOLAZIONE REGIONALE OLS (VARIABILE INDIPENDENTE)
plot(`Popolazione Regionale`,Residui_Multipli,xlab="Popolazione Regionale", ylab="Residui", main = "Grafico Tra Residui e Popolazione Regionale", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI OLS VS. REDDITO OLS (VARIABILE INDIPENDENTE)
plot(Reddito,Residui_Multipli,xlab="Reddito", ylab="Residui", main = "Grafico Tra Residui e Reddito", pch=19, cex=0.5)
abline(h=0, col="red")

#2. VERIFICO LA NORMALITÀ               (NON VERIFICATA PER TUTTI E 3 I TEST)
qqnorm(Residui_Multipli, main = "Normalità: QQplot Residui Modello OLS")
qqline(Residui_Multipli, col="red")
shapiro.test(Residui_Multipli)
ks.test(Residui_Multipli, "pnorm", mean=mean(Residui_Multipli), sd=sd(Residui_Multipli))
library(tseries)
jarque.bera.test(Residui_Multipli)

#3. VERIFICO L'OMOSCHEDASTICITÀ         (NON VERIFICATA)
#TEST DI BREUSCH-PAGAN
library(lmtest)
bptest(OLS_New)

#4. VERIFICO L'INDIPENDENZA             (NON VERIFICATA: AUTOCORRELAZIONE POSITIVA)
#TEST DI DURBIN-WATSON
dwtest(OLS_New)

#DATO CHE NON SONO RISPETTATE DELLE IPOTESI SULL'ANALISI DEI RESIDUI DEL MODELLO OLS, ALLORA OPTIAMO PER DELLE TRASFORMAZIONI

####BOX-COX####
library(MASS)
Box_Cox=boxcox(OLS_New,lambda = seq(-20,10, by=0.1))
lambda_opt=Box_Cox$x[which.max(Box_Cox$y)]
lambda_opt

#ARROTONDAMENTO DI LAMBDA_OTP AL PIÙ VICINO MULTIPLO DI 0.5 
lambda_opt=round(lambda_opt*2)/2
lambda_opt

if (lambda_opt==0) {
  dataset$Incidenti_Stradali_Log=log(dataset$`Incidenti Stradali`)
} else {
  dataset$Incidenti_Stradali_Lambda=(dataset$`Incidenti Stradali`^lambda_opt)
}
View(dataset)
attach(dataset)

#CONFRONTO ISTROGRAMMI DENSITÀ
par(mfrow=c(1,2))
hist(`Incidenti Stradali`, probability=T, main = "Istogramma Incidenti Stradali", ylab="Densità", xlab="Incidenti Stradali")
hist(Incidenti_Stradali_Log, probability=T, main = "Istogramma Incidenti Stradali Log", ylab="Densità", xlab="Incidenti Stradali Log")
par(mfrow=c(1,1))

#CONFRONTO ISTROGRAMMI FREQUENZE
par(mfrow=c(1,2))
plotNormalHistogram(`Incidenti Stradali`, main = "Istogramma Incidenti Stradali", ylab="Frequenze", xlab="Incidenti Stradali")
plotNormalHistogram(Incidenti_Stradali_Log, main = "Istogramma Incidenti Stradali Log", ylab="Frequenze", xlab="Incidenti Stradali Log")
par(mfrow=c(1,1))

#CONFRONTO BOXPLOT
par(mfrow=c(1,2))
boxplot(`Incidenti Stradali`, main = "BoxPlot Incidenti Stradali", ylab="Incidenti Stradali")
boxplot(Incidenti_Stradali_Log, main = "BoxPlot Incidenti Stradali Log", ylab="Incidenti Stradali Log")
par(mfrow=c(1,1))

#CONFRONTO ASIMMETRIA E CURTOSI
skewness(`Incidenti Stradali`)
skewness(Incidenti_Stradali_Log)

kurtosis(`Incidenti Stradali`)
kurtosis(Incidenti_Stradali_Log)

####TRASFORMAZIONI LOGRATIMITCHE####
#MODELLO LOG-LIN
OLS_Chi_Log_Lin=lm(Incidenti_Stradali_Log~`Degenza Ospedaliera`+`Popolazione Regionale` + Reddito,`Ore Lavorative per Regione`+`Occupazione Regionale`, data = dataset)
summary(OLS_Chi_Log_Lin) #ADJUSTED R-SQUARED = 0.4914
AIC(OLS_Chi_Log_Lin)     #AIC = 597.0714

#MODELLO LIN-LOG 
dataset$Degenza_Ospedaliera_Log=log(`Degenza Ospedaliera`)
dataset$Popolazione_Regionale_Log=log(`Popolazione Regionale`)
dataset$Reddito_Log=log(Reddito)
dataset$Ore_Lavorative_per_Regione_Log=log(`Ore Lavorative per Regione`)
dataset$Occupazione_Regionale_Log=log(`Occupazione Regionale`)
View(dataset)
attach(dataset)

OLS_Chi_Lin_Log=lm(`Incidenti Stradali`~Degenza_Ospedaliera_Log+Ore_Lavorative_per_Regione_Log+Occupazione_Regionale_Log+Popolazione_Regionale_Log+Reddito_Log, data = dataset)
summary(OLS_Chi_Lin_Log) #ADJUSTED R-SQUARED = 0.4081
AIC(OLS_Chi_Lin_Log)     #AIC = 3953.499

#MODELLO LOG-LOG
OLS_Chi_Log_Log=lm(Incidenti_Stradali_Log~Degenza_Ospedaliera_Log+Ore_Lavorative_per_Regione_Log+Occupazione_Regionale_Log+Popolazione_Regionale_Log+Reddito_Log, data = dataset)
summary(OLS_Chi_Log_Log) #ADJUSTED R-SQUARED = 0.6387  QUESTO MODELLO SI ADATTA MEGLIO AI DATI, PERCHÉ HA R-QUADRO AGGIUSTATO PIÙ ALTO RISPETTO AL OLS_Chi_Log_Lin E AL OLS_Chi_Lin_Log
AIC(OLS_Chi_Log_Log)     #AIC = 435.142                SCELGO QUESTO MODELLO, PERCHÉ HA UN VALORE PIÙ BASSO RISPETTO OLS_CHI_LOG_LIN

####LM TEST####
#TEST DI MORAN I DEL MODELLO OLS_Chi_Log_Log
Moran_LM_Test=lm.morantest(OLS_Chi_Log_Log,Lista_Ritardo_Spaziale,alternative = "two.sided")
Moran_LM_Test

LM_tests=lm.RStests(OLS_Chi_Log_Log,Lista_Ritardo_Spaziale,test="all", zero.policy = T)
LM_tests

#SEM
SEM_Chi=errorsarlm(Incidenti_Stradali_Log~Degenza_Ospedaliera_Log+Ore_Lavorative_per_Regione_Log+Occupazione_Regionale_Log+Popolazione_Regionale_Log+Reddito_Log, Lista_Ritardo_Spaziale, data = dataset, tol.solve = 6.2817e-17, zero.policy=TRUE)
summary(SEM_Chi, nagelkerke=T)
AIC(SEM_Chi) #AIC=340.0829 MODELLO MIGLIORE RISPETTO ALLO SDEM

#SDEM
SDEM_Chi=errorsarlm(Incidenti_Stradali_Log~Degenza_Ospedaliera_Log+Ore_Lavorative_per_Regione_Log+Occupazione_Regionale_Log+Popolazione_Regionale_Log+Reddito_Log, Lista_Ritardo_Spaziale, data = dataset, etype = "mixed", tol.solve=1.1e-20, zero.policy=T)
summary(SDEM_Chi, nagelkerke=T)
AIC(SDEM_Chi) #AIC=344.0403

#EFFETTI SDEM
Effetti_SDEM=impacts(SDEM_Chi,Lista_Ritardo_Spaziale)
Effetti_SDEM
summary(Effetti_SDEM)
SDEM_Chi$coefficients[-1]

#Confronto OLS, SEM E SDEM
library(stargazer)
stargazer(OLS_Chi_Log_Log,SEM_Chi,SDEM_Chi,type="text", title="OLS-SEM-SDEM")

####ANALISI DEI RESIDUI SUL MODELLO SEM####
#CALCOLO VALORI TEORICI E RESIDUI
Valori_Teorici_Multipli_SEM=fitted(SEM_Chi)
Valori_Teorici_Multipli_SEM
dataset$Valori_Teorici_Multipli_SEM=Valori_Teorici_Multipli_SEM

Residui_Multipli_SEM=residuals(SEM_Chi)
Residui_Multipli_SEM
dataset$Residui_Multipli_SEM=Residui_Multipli_SEM

#1. VERIFICO LA LINEARITÀ
#SCATTERPLOT RESIDUI SEM VS. VALORI TEORICI SEM
plot(Valori_Teorici_Multipli_SEM,Residui_Multipli_SEM,xlab="Valori Teorici SEM", ylab="Residui SEM", main = "Grafico Tra Valori Teorici (SEM) e Residui (SEM)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SEM VS. INCIDENTI STRADALI LOG (VARIABILE DIPENDENTE)
plot(Incidenti_Stradali_Log,Residui_Multipli_SEM,xlab="Incidenti Stradali Log", ylab="Residui SEM", main = "Grafico Tra Residui (SEM) e Incidenti Stradali (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SEM VS. DEGENZA OSPEDALIERA LOG (VARIABILE INDIPENDENTE)
plot(Degenza_Ospedaliera_Log,Residui_Multipli_SEM,xlab="Degenza Ospedaliera Log", ylab="Residui SEM", main = "Grafico Tra Residui (SEM) e Degenza Ospedaliera (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SEM VS. ORE LAVORATIVE PER REGIONE LOG (VARIABILE INDIPENDENTE)
plot(Ore_Lavorative_per_Regione_Log,Residui_Multipli_SEM,xlab="Ore Lavorative per Regione Log", ylab="Residui SEM", main = "Grafico Tra Residui (SEM) e Ore Lavorative per Regione (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SEM VS. OCCUPAZIONE REGIONALE LOG (VARIABILE INDIPENDENTE)
plot(Occupazione_Regionale_Log,Residui_Multipli_SEM,xlab="Occupazione Regionale Log", ylab="Residui SEM", main = "Grafico Tra Residui (SEM) e Occupazione Regionale (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SEM VS. POPOLAZIONE REGIONALE LOG (VARIABILE INDIPENDENTE)
plot(Popolazione_Regionale_Log,Residui_Multipli_SEM,xlab="Popolazione Regionale Log", ylab="Residui SEM", main = "Grafico Tra Residui (SEM) e Popolazione Regionale (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SEM VS. REDDITO LOG (VARIABILE INDIPENDENTE)
plot(Reddito_Log,Residui_Multipli_SEM,xlab="Reddito Log", ylab="Residui SEM", main = "Grafico Tra Residui (SEM) e Reddito (Log)", pch=19, cex=0.5)
abline(h=0, col="red")

#2. VERIFICO LA NORMALITÀ
qqnorm(Residui_Multipli_SEM, main = "QQplot Residui Modello SEM")
qqline(Residui_Multipli_SEM, col="red") 
shapiro.test(Residui_Multipli_SEM)
ks.test(Residui_Multipli_SEM, "pnorm", mean=mean(Residui_Multipli_SEM), sd=sd(Residui_Multipli_SEM))
library(tseries)
jarque.bera.test(Residui_Multipli_SEM)

#3. VERIFICO L'OMOSCHEDASTICITÀ VERIFICATA!!
##TEST DI BREUSCH-PAGAN
library(lmtest)
bptest.Sarlm(SEM_Chi)

#4. VERIFICO L'INDIPENDENZA VERIFICATA!!
#TEST DI DURBIN-WATSON.   UTILIZZO IL MORAN PER VERIFICARE L'INDIPENDENZA DEI RESIDUI
moran(dataset$Residui_Multipli_SEM, listw = Lista_Ritardo_Spaziale, length(dataset$Residui_Multipli_SEM),Szero(Lista_Ritardo_Spaziale), zero.policy = T)
moran.test(dataset$Residui_Multipli_SEM, Lista_Ritardo_Spaziale, zero.policy = T)

#5. VERIFICO LA MULTICOLLINEARITÀ
library(car)
vif(lm(Incidenti_Stradali_Log~Degenza_Ospedaliera_Log+Ore_Lavorative_per_Regione_Log+Occupazione_Regionale_Log+Popolazione_Regionale_Log+Reddito_Log, data = dataset))

####ANALISI DEI RESIDUI SUL MODELLO SDEM####
#CALCOLO VALORI TEORICI E RESIDUI
Valori_Teorici_Multipli_SDEM=fitted(SDEM_Chi)
Valori_Teorici_Multipli_SDEM
dataset$Valori_Teorici_Multipli_SDEM=Valori_Teorici_Multipli_SDEM

Residui_Multipli_SDEM=residuals(SDEM_Chi)
Residui_Multipli_SDEM
dataset$Residui_Multipli_SDEM=Residui_Multipli_SDEM

#1. VERIFICO LA LINEARITÀ
#SCATTERPLOT RESIDUI SDEM VS. VALORI TEORICI SDEM
plot(Valori_Teorici_Multipli_SDEM,Residui_Multipli_SDEM,xlab="Valori Teorici SDEM", ylab="Residui SDEM", main = "Grafico Tra Valori Teorici (SDEM) e Residui (SDEM)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SDEM VS. INCIDENTI STRADALI LOG (VARIABILE DIPENDENTE)
plot(Incidenti_Stradali_Log,Residui_Multipli_SDEM,xlab="Incidenti Stradali Log", ylab="Residui SDEM", main = "Grafico Tra Residui (SDEM) e Incidenti Stradali (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SDEM VS. DEGENZA OSPEDALIERA LOG (VARIABILE INDIPENDENTE)
plot(Degenza_Ospedaliera_Log,Residui_Multipli_SDEM,xlab="Degenza Ospedaliera Log", ylab="Residui SDEM", main = "Grafico Tra Residui (SDEM) e Degenza Ospedaliera (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SDEM VS. ORE LAVORATIVE PER REGIONE LOG (VARIABILE INDIPENDENTE)
plot(Ore_Lavorative_per_Regione_Log,Residui_Multipli_SDEM,xlab="Ore Lavorative per Regione Log", ylab="Residui SDEM", main = "Grafico Tra Residui (SDEM) e Ore Lavorative per Regione (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SDEM VS. OCCUPAZIONE REGIONALE LOG (VARIABILE INDIPENDENTE)
plot(Occupazione_Regionale_Log,Residui_Multipli_SDEM,xlab="Occupazione Regionale Log", ylab="Residui SDEM", main = "Grafico Tra Residui (SDEM) e Occupazione Regionale (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SDEM VS. POPOLAZIONE REGIONALE LOG (VARIABILE INDIPENDENTE)
plot(Popolazione_Regionale_Log,Residui_Multipli_SDEM,xlab="Popolazione Regionale Log", ylab="Residui SDEM", main = "Grafico Tra Residui (SDEM) e Popolazione Regionale (Log)", pch=19, cex=0.5)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SDEM VS. REDDITO LOG (VARIABILE INDIPENDENTE)
plot(Reddito_Log,Residui_Multipli_SDEM,xlab="Reddito Log", ylab="Residui SDEM", main = "Grafico Tra Residui (SDEM) e Reddito (Log)", pch=19, cex=0.5)
abline(h=0, col="red")

#2. VERIFICO LA NORMALITÀ
qqnorm(Residui_Multipli_SDEM, main = "QQplot Residui Modello SDEM")
qqline(Residui_Multipli_SDEM, col="red") 
shapiro.test(Residui_Multipli_SDEM)
ks.test(Residui_Multipli_SDEM, "pnorm", mean=mean(Residui_Multipli_SDEM), sd=sd(Residui_Multipli_SDEM))
library(tseries)
jarque.bera.test(Residui_Multipli_SDEM)

#3. VERIFICO L'OMOSCHEDASTICITÀ VERIFICATA!!
##TEST DI BREUSCH-PAGAN
library(lmtest)
bptest.Sarlm(SDEM_Chi)

#4. VERIFICO L'INDIPENDENZA VERIFICATA!!
#TEST DI DURBIN-WATSON.   UTILIZZO IL MORAN PER VERIFICARE L'INDIPENDENZA DEI RESIDUI
moran(dataset$Residui_Multipli_SDEM, listw = Lista_Ritardo_Spaziale, length(dataset$Residui_Multipli_SDEM),Szero(Lista_Ritardo_Spaziale), zero.policy = T) 
moran.test(dataset$Residui_Multipli_SDEM, Lista_Ritardo_Spaziale, zero.policy = T)

####CONFRONTO LINEARITÀ E NORMALITÀ DEI RESIDUI TRA IL MODELLO OLS, SEM E SDEM####
#LINEARITÀ
par(mfrow=c(1,3))
#SCATTERPLOT RESIDUI OLS VS. VALORI TEORICI OLS
plot(Valori_Teorici_Multipli,Residui_Multipli,xlab="Valori Teorici", ylab="Residui", main = "Grafico Tra Valori Teorici e Residui", pch=19, cex=0.75)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SEM VS. VALORI TEORICI SEM
plot(Valori_Teorici_Multipli_SEM,Residui_Multipli_SEM,xlab="Valori Teorici SEM", ylab="Residui SEM", main = "Grafico Tra Valori Teorici (SEM) e Residui (SEM)", pch=19, cex=0.75)
abline(h=0, col="red")
#SCATTERPLOT RESIDUI SDEM VS. VALORI TEORICI SDEM
plot(Valori_Teorici_Multipli_SDEM,Residui_Multipli_SDEM,xlab="Valori Teorici SDEM", ylab="Residui SDEM", main = "Grafico Tra Valori Teorici (SDEM) e Residui (SDEM)", pch=19, cex=0.75)
abline(h=0, col="red")
par(mfrow=c(1,1))

#NORMALITÀ
par(mfrow=c(1,3))
#MODELLO OLS
qqnorm(Residui_Multipli, main = "QQplot Residui Modello OLS")
qqline(Residui_Multipli, col="red")
#MODELLO SEM
qqnorm(Residui_Multipli_SEM, main = "QQplot Residui Modello SEM")
qqline(Residui_Multipli_SEM, col="red") 
#MODELLO SDEM
qqnorm(Residui_Multipli_SDEM, main = "QQplot Residui Modello SDEM")
qqline(Residui_Multipli_SDEM, col="red") 
par(mfrow=c(1,1))