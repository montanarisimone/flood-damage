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
