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

### DATA CLEANING
# tolower
bastiglia$indirizzo <- tolower(bastiglia$indirizzo)
bastiglia$comune <- tolower(bastiglia$comune)
bastiglia$uso <- tolower(bastiglia$uso)
bastiglia$tipologia_strutturale <- tolower(bastiglia$tipologia_strutturale)
bomporto$indirizzo <- tolower(bomporto$indirizzo)
bomporto$comune <- tolower(bomporto$comune)
bomporto$uso <- tolower(bomporto$uso)
bomporto$tipologia_strutturale <- tolower(bomporto$tipologia_strutturale)

# replace NA with municipalities names var 'comune'
bastiglia$comune[is.na(bastiglia$comune)] = "bastiglia"
bomporto$comune[is.na(bomporto$comune)] = "bomporto"

# delete white spaces
bomporto$tipologia_strutturale <- gsub(" ", "", bomporto$tipologia_strutturale, 
                                       fixed = T)
bastiglia$tipologia_strutturale <- gsub(" ", "", bastiglia$tipologia_strutturale, 
                                        fixed = T)

# split building typology into 'concrete' (c), 'brick' (m), 'concrete+brick' (cm), other (altro)
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

# new var = property value
bastiglia$compr_max_mq <- bastiglia$compr_max_mq*bastiglia$superficie
bastiglia$compr_min_mq <- bastiglia$compr_min_mq*bastiglia$superficie
bastiglia$valore_immobile <- rowMeans(bastiglia[, c('compr_max_mq', 
                                                    'compr_min_mq')], na.rm = T)
bomporto$compr_max_mq <- bomporto$compr_max_mq*bomporto$superficie
bomporto$compr_min_mq <- bomporto$compr_min_mq*bomporto$superficie
bomporto$valore_immobile <- rowMeans(bomporto[, c('compr_max_mq', 
                                                  'compr_min_mq')], na.rm = T)

# new var = building construction cost
bastiglia$costo_max <- bastiglia$costo_max*bastiglia$superficie
bastiglia$costo_min <- bastiglia$costo_min*bastiglia$superficie
bastiglia$costo_medio <- rowMeans(bastiglia[, c('costo_max', 'costo_min')], na.rm = T)

bomporto$costo_max <- bomporto$costo_max*bomporto$superficie
bomporto$costo_min <- bomporto$costo_min*bomporto$superficie
bomporto$costo_medio <- rowMeans(bomporto[, c('costo_max', 'costo_min')], na.rm = T)

# 
indirizzo_bastiglia <- bastiglia[, c("id", "ID_univoco", "indirizzo", "civico", "comune")]
indirizzo_bomporto <- bomporto[, c("id", "ID_univoco", "indirizzo", "civico", "comune")]
indirizzo <- rbind(indirizzo_bastiglia, indirizzo_bomporto)

# select olny useful vars
bastiglia <- bastiglia[, c("id", "tipologia_strutturale", "superficie",
                           "altezza_acqua_cm", "distanza_secchia_mt",
                           "dislivello_rispetto_argini", "area", "edfc",
                           "valore_immobile", "costo_medio", "danno_beni_immobili")]
bomporto <- bomporto[, c("id", "tipologia_strutturale", "superficie",
                         "altezza_acqua_cm", "distanza_secchia_mt",
                         "dislivello_rispetto_argini", "area", "edfc",
                         "valore_immobile", "costo_medio", "danno_beni_immobili")]
                         
# delete NA value
bastiglia_no_na <- na.omit(bastiglia)
bomporto_no_na <- na.omit(bomporto)

# dummy building typology
bastiglia_numeric <- bastiglia_no_na[, c("id","superficie", "altezza_acqua_cm",
                                         "distanza_secchia_mt", "dislivello_rispetto_argini",
                                         "area", "edfc", "valore_immobile",
                                         "costo_medio", "danno_beni_immobili")] 
nuovo_bastiglia <- as.data.frame(bastiglia_no_na[, "id"]) 

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
  return(merge(nuovo_bastiglia, dati, by = "id"))
}

nuovo_bastiglia <- quantifica_bastiglia(bastiglia_no_na$tipologia_strutturale) 
bastiglia_touse <- merge(nuovo_bastiglia, bastiglia_numeric, by = "id") 

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
  return(merge(nuovo_bomporto, dati, by = "id"))
}

nuovo_bomporto = quantifica_bomporto(bomporto_no_na$tipologia_strutturale)
bomporto_touse <- merge(nuovo_bomporto, bomporto_numeric, by = "id")
bomporto_touse$cm <- 0 
bomporto_touse <- bomporto_touse[, c("id", "altro", "c", "cm", "m", "superficie", "altezza_acqua_cm",
                                   "distanza_secchia_mt", "dislivello_rispetto_argini",
                                   "area", "edfc", "valore_immobile",
                                   "costo_medio", "danno_beni_immobili")]
remove(nuovo_bomporto)
remove(bomporto_numeric)

# clean dataset
# dati_cat = building typo c, m, cm, other
# dati_bin = dummy building typo
dati_cat <- rbind(bastiglia_no_na, bomporto_no_na) 
dati_cat$superficie <- as.numeric(dati_cat$superficie)
dati_bin <- rbind(bastiglia_touse, bomporto_touse) 
dati_bin$superficie <- as.numeric(dati_bin$superficie) 

# delete useless datasets
remove(bastiglia_no_na)
remove(bomporto_no_na)
remove(bastiglia_touse)
remove(bomporto_touse)

### OUTLIER ANALYSIS 
df <- dati_bin[, c("altro", "c", "cm", "m", "superficie", "altezza_acqua_cm",
                   "distanza_secchia_mt", "dislivello_rispetto_argini",
                   "area", "edfc", "valore_immobile",
                   "costo_medio", "danno_beni_immobili")]
center <- colMeans(df)
covar <- cov(df)
distances <- mahalanobis(x = df , center = center , cov = covar, tol = 1e-30)
cutoff <- qchisq(p = 0.99 , df = ncol(df))

# add var 'outliers' in dati_bin
dati_bin$mahalanobis <- distances
dati_bin$outlier <- ifelse(dati_bin$mahalanobis > cutoff, "1", "0")
# delete var 
dati_bin <- dati_bin[, c("id", "altro", "c", "cm", "m", "superficie", "altezza_acqua_cm",
                         "distanza_secchia_mt", "dislivello_rispetto_argini",
                         "area", "edfc", "valore_immobile",
                         "costo_medio", "danno_beni_immobili", "outlier")]
table(dati_bin$outlier)
  
# add var 'outliers' in dati_cat
dati_cat$mahalanobis <- distances
dati_cat$outlier <- ifelse(dati_cat$mahalanobis > cutoff, "1", "0")
dati_cat <- merge(dati_cat[, c("id", "tipologia_strutturale")], dati_bin[, c("id", 
                                                                             "superficie", "altezza_acqua_cm",
                                                                             "distanza_secchia_mt", "dislivello_rispetto_argini",
                                                                             "area", "edfc", "valore_immobile",
                                                                             "costo_medio", "danno_beni_immobili", "outlier")], by = "id")
  
### DESCRIPTIVE ANALYSIS
# add var 'relative damage'
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

# median
summary(dati_cat[, 3:12])
medians <- aggregate(dati_cat[, 3:12], 
                     list(dati_cat$tipologia_strutturale),
                     median, simplify = T)
names(medians)[1] <- "tipo"
medians
  
# density and freq plots
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

par(mfrow=c(2,4))
boxplot(dati_cat$superficie, main = "superficie", ylab = "mq")
boxplot(dati_cat$altezza_acqua_cm, main = "altezza acqua", ylab = "mt")
boxplot(dati_cat$distanza_secchia_mt, main = "distanza fiume", ylab = "mt")
boxplot(dati_cat$valore_immobile, main = "valore immobile", ylab = "\u20ac")
boxplot(dati_cat$dislivello_rispetto_argini, main = "dislivello", ylab = "mt")
boxplot(dati_cat$area, main = "area", ylab = "mq")
boxplot(dati_cat$edfc, main = "edifici")
boxplot(dati_cat$danno_relativo_valore, main = "danno relativo")
  
# correlation plot
cor <- round(cor(dati_cat[, c(3:9, 11)]), 2)
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
