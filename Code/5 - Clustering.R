library("tibble")
library("factoextra")
library("tidyverse")
library("fpc")
library("NbClust")
library("dendextend")
library("clusterSim")

data <- subset(dati_bin, select = -c(id, danno_beni_immobili,
                                     costo_medio,
                                     danno_relativo_costo))
                                     
# scaling
df <- scale(data[,-ncol(data)])

# matrix distances
distanze <- dist(df, method = "euclidean")

# clustering
clusters <- hclust(distanze, method = "ward.D2")

# dendrogram
plot(clusters)

# screeplot
scree_plot <- ggplot(clusters$height %>%
                       as_tibble() %>%
                       add_columns(groups = length(clusters$height):1) %>%
                       rename(height=value),
                     aes(x=groups, y=height)) + 
  geom_point() + 
  geom_line() +
  xlim(0,30)

scree_plot

# cut at 8 clusters
h8 <- clusters$height[length(clusters$height):1==8]
scree_plot + 
  geom_vline(xintercept = 8, colour = "orange") +
  geom_hline(yintercept = h8, colour = "orange", lty = 2)

k = 8
hc <- eclust(df, "hclust", k = k, hc_metric = "euclidean",
             hc_method = "ward.D2", graph = F)
             
# silhouette
fviz_silhouette(hc, palette = "jco")
# silhouette info
silinfo <- hc$silinfo
# silhouette widths of each observation
head(silinfo$widths,10)

# there are several samples with negative silhouette coefficient. Find their neighbor cluster and move to it
sil <- silinfo$widths 
# take raw index of samples with silhouette < 0
neg_sil_index <- which(sil[, 'sil_width'] < 0)
# new df with this samples
neigh_cl <- sil[neg_sil_index,]
# df with original clusters
newdata <- data.frame(hc$cluster)
newdata$n <- seq(1, 1366, by=1)
# df with original clusters, neighbor cluster and slihouette value
nuovo <- data.frame(sil)
nuovo$n <- as.numeric(row.names(nuovo))
nuovo <- nuovo[order(nuovo$n),]
# merge this 2 df
clust <- merge(newdata, nuovo, by="n")
# substitute original cluster with neighbor cluster if silhouette value < 0
for(i in 1:nrow(clust)){
  if(clust$sil_width[i] < 0){
    clust$cluster[i] = clust$neighbor[i]
  }
}

# clusters summary
data$outlier <- as.numeric(data$outlier)
data$hc <- as.numeric(data$hc)
sum_cluster <- round(data %>%
  group_by(hc) %>%
  summarise_all(mean),2)
sum_cluster

numerosita <- as.numeric(table(data$hc))

## VARS PSEUDO-F
dati <- as.data.frame(df)
dati$hc <- data$hc
n <- nrow(dati)

# centroids matrix
medie_clusters <- dati %>%
  group_by(hc) %>%
  summarise_all(mean)
medie_clusters <- as.matrix(medie_clusters)

# belonging matrix
clusterizzazione <- function(colonna){
  matrice = matrix(nrow=nrow(dati), ncol=length(names(table(dati$hc))))
  elementi = list(names(table(dati$hc)))
  
  for(i in 1:nrow(dati)){
    for(k in 1:length(names(table(dati$hc)))){
      if(colonna[i] == elementi[[1]][k]){
        matrice[i,k] = 1
      }else{matrice[i,k]=0}
    }
  }
  
  #  colnames(matrice) = elementi
  output = as.data.frame(matrice)
  colnames(output) = names(table(dati$hc))
  
  return(output)
  
}

appartenenza <- clusterizzazione(dati$hc)
appartenenza <- as.matrix(appartenenza)

# set df as matrix
dati <- as.matrix(dati)

# find within/between variance and pseudof
calcolo_f <- function(nome_var){
  col_dati <- grep(paste("^",nome_var,"$", sep=""), colnames(dati))
  col_medie <- grep(paste("^",nome_var,"$", sep=""), colnames(medie_clusters))
  
  w_variance <- sum(diag(t(dati[, col_dati] - appartenenza %*% medie_clusters[, col_medie]) %*%
                             (dati[, col_dati] - appartenenza %*% medie_clusters[, col_medie])))
  
  b_variance <- sum(diag(t(appartenenza %*% medie_clusters[, col_medie]) %*% (appartenenza %*% medie_clusters[, col_medie])))
  
  F <- (b_variance/(k-1))/(w_variance/(n-k))
  
  values <- list("WSS" = w_variance, "GSS" = b_variance, "PseudoF" = F)
  
  return(values)
  
}

# summarize
confronto_vars <- matrix(NA, nrow = 8, ncol = 3)

for(i in 1:8){
  nome <- colnames(dati)[i+4]
  confronto_vars[i, 1] = calcolo_f(nome)$WSS
  confronto_vars[i, 2] = calcolo_f(nome)$GSS
  confronto_vars[i, 3] = calcolo_f(nome)$PseudoF
}

row.names(confronto_vars) <- c("superficie", "altezza_acqua", "distanza_secchia",
                            "dislivello", "area", "edfc", "valore_immobile", 
                            "danno_relativo")
colnames(confronto_vars) <- c("WSS", "BSS", "PseudoF")

confronto_vars <- round(confronto_vars, 2)
