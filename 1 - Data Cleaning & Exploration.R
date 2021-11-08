library("readxl")
library("sjmisc")
library("fastDummies")
library("GGally")
library("dplyr")

#!diagnostics off 

# import dataset
bomporto <- read_excel("bomporto.xlsx",
                       col_types = c("numeric","text", "text", "text", 
                                     "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))
str(bomporto)

bastiglia <- read_excel("bastiglia.xlsx", 
                        col_types = c("numeric","text", "text", "text", 
                                      "text", "text", "text", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
str(bastiglia)

# tolower
bastiglia$indirizzo <- tolower(bastiglia$indirizzo)
bastiglia$comune <- tolower(bastiglia$comune)
bastiglia$uso <- tolower(bastiglia$uso)
bastiglia$tipologia_strutturale <- tolower(bastiglia$tipologia_strutturale)
bomporto$indirizzo <- tolower(bomporto$indirizzo)
bomporto$comune <- tolower(bomporto$comune)
bomporto$uso <- tolower(bomporto$uso)
bomporto$tipologia_strutturale <- tolower(bomporto$tipologia_strutturale)

# add municipality where value = NA
bastiglia$comune[is.na(bastiglia$comune)] = "bastiglia"
bomporto$comune[is.na(bomporto$comune)] = "bomporto"

# delete spaces
bomporto$tipologia_strutturale <- gsub(" ", "", bomporto$tipologia_strutturale, 
                                       fixed = T)
bastiglia$tipologia_strutturale <- gsub(" ", "", bastiglia$tipologia_strutturale, 
                                        fixed = T)

# categorize structural typology
# Bastiglia
for (i in 1:nrow(bastiglia)){
  if(str_contains(bastiglia$tipologia_strutturale[i], c("cemento","muratura"), 
                  logic = "and") == TRUE){
    bastiglia$tipologia_strutturale[i] = "cm"
  } else if(str_contains(bastiglia$tipologia_strutturale[i], "cementoarmato") 
            == TRUE){
    bastiglia$tipologia_strutturale[i] = "c"
  } else if(str_contains(bastiglia$tipologia_strutturale[i], "facciavista") 
            == TRUE){
    bastiglia$tipologia_strutturale[i] = "altro"
  } else if(str_contains(bastiglia$tipologia_strutturale[i], "laterocemento")
            == TRUE){
    bastiglia$tipologia_strutturale[i] = "altro"
  } else if(str_contains(bastiglia$tipologia_strutturale[i], "muratura")
            == TRUE){
    bastiglia$tipologia_strutturale[i] = "m"
  }
}
# Bomporto
for (i in 1:nrow(bomporto)){
  if(str_contains(bomporto$tipologia_strutturale[i], c("cemento","muratura"), 
                  logic = "and") == TRUE){
    bomporto$tipologia_strutturale[i] = "cm"
  } else if(str_contains(bomporto$tipologia_strutturale[i], "cementoarmato")
            == TRUE){
    bomporto$tipologia_strutturale[i] = "c"
  } else if(str_contains(bomporto$tipologia_strutturale[i], "muratura") 
            == TRUE){
    bomporto$tipologia_strutturale[i] = "m"
  }
}

# add average property value
bastiglia$compr_max_mq <- bastiglia$compr_max_mq*bastiglia$superficie
bastiglia$compr_min_mq <- bastiglia$compr_min_mq*bastiglia$superficie
bastiglia$valore_immobile <- rowMeans(bastiglia[, c('compr_max_mq', 
                                                    'compr_min_mq')], na.rm = T)
bomporto$compr_max_mq <- bomporto$compr_max_mq*bomporto$superficie
bomporto$compr_min_mq <- bomporto$compr_min_mq*bomporto$superficie
bomporto$valore_immobile <- rowMeans(bomporto[, c('compr_max_mq', 
                                                  'compr_min_mq')], na.rm = T)

# add average construction cost
bastiglia$costo_max <- bastiglia$costo_max*bastiglia$superficie
bastiglia$costo_min <- bastiglia$costo_min*bastiglia$superficie
bastiglia$costo_medio <- rowMeans(bastiglia[, c('costo_max', 'costo_min')], na.rm = T)

bomporto$costo_max <- bomporto$costo_max*bomporto$superficie
bomporto$costo_min <- bomporto$costo_min*bomporto$superficie
bomporto$costo_medio <- rowMeans(bomporto[, c('costo_max', 'costo_min')], na.rm = T)

# add address 
indirizzo_bastiglia <- bastiglia[, c("id", "ID_univoco", "indirizzo", "civico", "comune")]
indirizzo_bomporto <- bomporto[, c("id", "ID_univoco", "indirizzo", "civico", "comune")]
indirizzo <- rbind(indirizzo_bastiglia, indirizzo_bomporto)

#keep only the vars useful to the analysis
bastiglia <- bastiglia[, c("id", "tipologia_strutturale", "superficie",
                           "altezza_acqua_cm", "distanza_secchia_mt",
                           "dislivello_rispetto_argini", "area", "edfc",
                           "valore_immobile", "costo_medio", "danno_beni_immobili")]
bomporto <- bomporto[, c("id", "tipologia_strutturale", "superficie",
                         "altezza_acqua_cm", "distanza_secchia_mt",
                         "dislivello_rispetto_argini", "area", "edfc",
                         "valore_immobile", "costo_medio", "danno_beni_immobili")]

# delete NA values
bastiglia_no_na <- na.omit(bastiglia)
bomporto_no_na <- na.omit(bomporto)

# trasformo la tipologia in matrice indicatrice 0-1 così da rendere corretta 
# la distanza negli algoritmi. se assegnassi valori 1-4, il 4 avrebbe più 
# importanza
bastiglia_numeric <- bastiglia_no_na[, c("id","superficie", "altezza_acqua_cm",
                                         "distanza_secchia_mt", "dislivello_rispetto_argini",
                                         "area", "edfc", "valore_immobile",
                                         "costo_medio", "danno_beni_immobili")] # dataset bastiglia - colonna tipo
nuovo_bastiglia <- as.data.frame(bastiglia_no_na[, "id"]) # dataset con solo col id
quantifica_bastiglia <- function(colonna){
  
  matrice = matrix(nrow=nrow(bastiglia_no_na), ncol=length(names(table(colonna))))
  elementi = list(bastiglia_no_na$id,names(table(colonna)))
  dimnames(matrice) = elementi
  
  for(i in 1:nrow(bastiglia_no_na)){
    for(k in 1:length(names(table(colonna)))){
      if(colonna[i] == elementi[[2]][k]){
        matrice[i,k] = 1
      } else{matrice[i,k] = 0}
    }
  }
  
  dati = as.data.frame(matrice)
  dati$id <- bastiglia_no_na$id
  #return(dati)
  return(merge(nuovo_bastiglia, dati, by = "id"))
}

nuovo_bastiglia <- quantifica_bastiglia(bastiglia_no_na$tipologia_strutturale) # tipo quantificato
bastiglia_touse <- merge(nuovo_bastiglia, bastiglia_numeric, by = "id") # dataset bastiglia ok
remove(nuovo_bastiglia)
remove(bastiglia_numeric)

bomporto_numeric <- bomporto_no_na[, c("id","superficie", "altezza_acqua_cm",
                                       "distanza_secchia_mt", "dislivello_rispetto_argini",
                                       "area", "edfc", "valore_immobile",
                                       "costo_medio", "danno_beni_immobili")]
nuovo_bomporto <- as.data.frame(bomporto_no_na[, "id"])
quantifica_bomporto <- function(colonna){
  
  matrice = matrix(nrow=nrow(bomporto_no_na), ncol=length(names(table(colonna))))
  elementi = list(bomporto_no_na$id,names(table(colonna)))
  dimnames(matrice) = elementi
  
  for(i in 1:nrow(bomporto_no_na)){
    for(k in 1:length(names(table(colonna)))){
      if(colonna[i] == elementi[[2]][k]){
        matrice[i,k] = 1
      } else{matrice[i,k] = 0}
    }
  }
  
  dati = as.data.frame(matrice)
  dati$id <- bomporto_no_na$id
  #return(dati)
  return(merge(nuovo_bomporto, dati, by = "id"))
}

nuovo_bomporto = quantifica_bomporto(bomporto_no_na$tipologia_strutturale)
bomporto_touse <- merge(nuovo_bomporto, bomporto_numeric, by = "id")
bomporto_touse$cm <- 0 # aggiungo cm al tipo in quanto non è presente in bomporto
# riposizione la colonna cm
bomporto_touse <- bomporto_touse[, c("id", "altro", "c", "cm", "m", "superficie", "altezza_acqua_cm",
                                   "distanza_secchia_mt", "dislivello_rispetto_argini",
                                   "area", "edfc", "valore_immobile",
                                   "costo_medio", "danno_beni_immobili")]
remove(nuovo_bomporto)
remove(bomporto_numeric)

# ricreo i dataset da usare
# dati_cat = tipologia categorizzata come c,m,cm,altro
# dati_bin = tipologia dummy
dati_cat <- rbind(bastiglia_no_na, bomporto_no_na) # da usare per le altre analisi
dati_cat$superficie <- as.numeric(dati_cat$superficie)
dati_bin <- rbind(bastiglia_touse, bomporto_touse) # dataset per trovare outlier
dati_bin$superficie <- as.numeric(dati_bin$superficie) 

# elimino df che non servono
remove(bastiglia_no_na)
remove(bomporto_no_na)
remove(bastiglia_touse)
remove(bomporto_touse)

######################################### OUTLIER ############################################
# essendo più variabili, procedo con analisi multivariata utilizzando mahalonobis
# studiato lof (con varianti cof e loci), (h)dbscan, 
# isolation forest, clustering gerarchico.
# ho scelto mahalanobis perchè gli altri sono sviluppati principalmente per clusterizzazione
# inoltre restituisce un risultato accettabile
# secondo i papers studiati, mahalonobis garantisce il risultato migliore, anche se ovviamente
# non è perfetto

df <- dati_bin[, c("altro", "c", "cm", "m", "superficie", "altezza_acqua_cm",
                   "distanza_secchia_mt", "dislivello_rispetto_argini",
                   "area", "edfc", "valore_immobile",
                   "costo_medio", "danno_beni_immobili")] # serve per "extra 1 - Outliers Detection"

center <- colMeans(df)
covar <- cov(df)

distances <- mahalanobis(x = df , center = center , cov = covar, tol = 1e-30)
# tol permette alla funzione di funzionare, altrimenti si avrebbe 
# l'errore "il sistema è numericamente singolare" https://bit.ly/3wTjnj8
cutoff <- qchisq(p = 0.99 , df = ncol(df))

dati_bin$mahalanobis <- distances
dati_bin$outlier <- ifelse(dati_bin$mahalanobis > cutoff, "1", "0")
# elimino la colonna mahalanobis
dati_bin <- dati_bin[, c("id", "altro", "c", "cm", "m", "superficie", "altezza_acqua_cm",
                         "distanza_secchia_mt", "dislivello_rispetto_argini",
                         "area", "edfc", "valore_immobile",
                         "costo_medio", "danno_beni_immobili", "outlier")]
table(dati_bin$outlier)

dati_cat$mahalanobis <- distances
dati_cat$outlier <- ifelse(dati_cat$mahalanobis > cutoff, "1", "0")
dati_cat <- merge(dati_cat[, c("id", "tipologia_strutturale")], dati_bin[, c("id", 
                                                                             "superficie", "altezza_acqua_cm",
                                                                             "distanza_secchia_mt", "dislivello_rispetto_argini",
                                                                             "area", "edfc", "valore_immobile",
                                                                             "costo_medio", "danno_beni_immobili", "outlier")], 
                  by = "id")

################### FINE OUTLIER ###############################

# aggiungo danno relativo
dati_bin$danno_relativo_valore <- dati_bin$danno_beni_immobili/dati_bin$valore_immobile
dati_bin$danno_relativo_costo <- dati_bin$danno_beni_immobili/dati_bin$costo_medio
dati_bin <- dati_bin[, c("id", "altro", "c", "cm", "m", "superficie", "altezza_acqua_cm",
                         "distanza_secchia_mt", "dislivello_rispetto_argini",
                         "area", "edfc", "valore_immobile",
                         "costo_medio", "danno_relativo_valore", "danno_relativo_costo", 
                         "danno_beni_immobili", "outlier")]

dati_cat$danno_relativo_valore <- dati_cat$danno_beni_immobili/dati_cat$valore_immobile
dati_cat$danno_relativo_costo <- dati_cat$danno_beni_immobili/dati_cat$costo_medio
dati_cat <- dati_cat[, c("id", "tipologia_strutturale", "superficie", "altezza_acqua_cm",
                         "distanza_secchia_mt", "dislivello_rispetto_argini",
                         "area", "edfc", "valore_immobile",
                         "costo_medio", "danno_relativo_valore", "danno_relativo_costo", 
                         "danno_beni_immobili", "outlier")]

plot(density(dati_bin$danno_relativo_valore), ylim = c(0, 10), xlab = "danno relativo", ylab = "",
     main = "", lwd = 2)
lines(density(dati_bin$danno_relativo_costo), col = "red", lwd = 2)
legend("topright", legend = c("danno/valore", "danno/costo"), col = c("black", "red"), lwd = 2)

##################### ANALISI NUMERICA ###########################
summary(dati_cat[, 3:12])
medians <- aggregate(dati_cat[, 3:12], 
                     list(dati_cat$tipologia_strutturale),
                     median, simplify = T)
names(medians)[1] <- "tipo"
medians # distribuzione mediane per tipologia

##################### ANALISI GRAFICA ############################
options(scipen = 100)
barplot(prop.table(table(dati_cat$tipologia_strutturale)),
        ylim = c(0,1), col = rainbow(4))
par(mfrow=c(2,4))
hist(dati_bin[dati_bin$outlier==0,]$superficie, n = 100, freq = F, main = "superficie", xlab = "mq")
hist(dati_bin[dati_bin$outlier==0,]$altezza_acqua_cm, n = 20, freq = F, main = "altezza acqua", xlab = "cm")
hist(dati_bin[dati_bin$outlier==0,]$distanza_secchia_mt, n = 30, freq = F, 
     xlab = "mt", xlim = c(0, 6000), main = "distanza fiume")
hist(dati_bin[dati_bin$outlier==0,]$valore_immobile, n = 30, freq = F, main = "valore", xlab = "\u20ac")
hist(dati_bin[dati_bin$outlier==0,]$dislivello_rispetto_argini, main = "dislivello", xlab = "mt", freq = F, n = 20)
hist(dati_bin[dati_bin$outlier==0,]$area, main = "area", xlab = "mq", freq = F, n = 20)
hist(dati_bin[dati_bin$outlier==0,]$edfc, main = "edifici", n = 20, xlab = "# edifici", freq = F)
hist(dati_bin[dati_bin$outlier==0,]$danno_relativo_valore, n = 20, freq = F, main = "danno relativo", xlab = "")
#hist(dati_bin$danno_beni_immobili, n = 50, freq = F)

par(mfrow=c(2,2))
plot(dati_bin[dati_bin$outlier==0,]$dislivello_rispetto_argini,
     dati_bin[dati_bin$outlier==0,]$altezza_acqua_cm,
     xlab = "dislivello (mt)", ylab = "altezza acqua (mt)", main = "dislivello vs acqua")
plot(dati_bin[dati_bin$outlier==0,]$superficie,
     dati_bin[dati_bin$outlier==0,]$danno_relativo_valore,
     xlab = "superficie (mq)", ylab = "danno (\u20ac)", main = "superficie vs danno",
     xlim = c(0,200), ylim = c(0, 1.5))
plot(dati_bin[dati_bin$outlier==0,]$altezza_acqua_cm,
     dati_bin[dati_bin$outlier==0,]$danno_relativo_valore,
     xlab = "altezza acqua (mt)", ylab = "danno (\u20ac)", main = "acqua vs danno",
     ylim = c(0, 1.5))

col <- c(rep(NA, length(dati_bin$dislivello_rispetto_argini)))
for(i in 1:1268){
  if(dati_bin$dislivello_rispetto_argini[i] <= 10){col[i] = "green"}
  if(dati_bin$dislivello_rispetto_argini[i] > 10 & 
     dati_bin$dislivello_rispetto_argini[i] <= 12){col[i] = "red"}
  if(dati_bin$dislivello_rispetto_argini[i] > 12 & 
     dati_bin$dislivello_rispetto_argini[i] <= 14){col[i] = "blue"}
  if(dati_bin$dislivello_rispetto_argini[i] > 14){col[i] = "orange"}
}

plot(dati_bin$distanza_secchia_mt, dati_bin$altezza_acqua_cm,
     col = col, main = "dislivello", xlab = "distanza (mt)", ylab = "altezza acqua (mt)")
legend("topright", legend = c("<10", "10-12", "12-14", ">14"), 
       col = c("green", "red", "blue", "orange"), pch = 1, title = "dislivello")

par(mfrow=c(2,4))
boxplot(dati_cat$superficie, main = "superficie", ylab = "mq")
boxplot(dati_cat$altezza_acqua_cm, main = "altezza acqua", ylab = "mt")
boxplot(dati_cat$distanza_secchia_mt, main = "distanza fiume", ylab = "mt")
boxplot(dati_cat$valore_immobile, main = "valore immobile", ylab = "\u20ac")
boxplot(dati_cat$dislivello_rispetto_argini, main = "dislivello", ylab = "mt")
boxplot(dati_cat$area, main = "area", ylab = "mq")
boxplot(dati_cat$edfc, main = "edifici")
boxplot(dati_cat$danno_relativo_valore, main = "danno relativo")

#################### CORRELAZIONE ############################
cor <- round(cor(dati_cat[, c(3:9, 11)]), 2)
#cor[upper.tri(cor)] <- ""
library("RColorBrewer")
library("corrplot")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", 
                          "#4477AA"))
colnames(cor) <- c("superficie", "altezza acqua", "distanza fiume", "dislivello", 
                   "area", "edifici", "valore", "danno relativo")
rownames(cor) <- c("superficie", "altezza acqua", "distanza fiume", "dislivello", 
                   "area", "edifici", "valore", "danno relativo")

par(mfrow=c(1,1))
corrplot(cor, type = "lower", method = "color", order = "alphabet",
         diag = F, tl.col = "black", tl.srt = 5, addCoef.col = T,
         col = col(200))

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)
}

# https://www.linkedin.com/posts/brohrer_matplotlib-activity-6776846775325126656-uohH
pairs(dati_cat[dati_cat$outlier==0, 3:8], lower.panel = panel.cor)

pairs(~superficie + altezza_acqua_cm + distanza_secchia_mt +
        dislivello_rispetto_argini + area + edfc + danno_relativo_valore, 
      data = dati_cat,
      labels = c("superficie", "altezza acqua", "distanza fiume",
                     "dislivello", "area", "edifici", "danno relativo"),
      upper.panel = NULL)

dati_cat[dati_cat$outlier==0, ] %>% 
  mutate(tipologia_strutturale = factor(tipologia_strutturale)) %>%
  ggpairs(columns = c("superficie", "danno_beni_immobili", "altezza_acqua_cm",
                      "tipologia_strutturale", "distanza_secchia_mt"),
          aes(color = tipologia_strutturale, alpha = 1),
          upper = list(continuous = wrap('cor', size = 5)),
          lower = list(combo = wrap("facethist", bins = 30)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)))
