library(magrittr)
library(dplyr)
library(purrr)
library(ggplot2)

dat = iris %>% janitor::clean_names() %>% select(-species)

# cluster simple ----------------------------------------------------------

dat1 = dat %>% select(petal_width, petal_length)
km = kmeans(dat1, 3, nstart=50)
dat1 = dat1 %>% mutate(cluster=km$cluster)
(
  g_unclus = ggplot(dat1, aes(x=petal_width, y=petal_length)) +
    geom_jitter() +
    theme_minimal() +
    NULL
)
ggsave("slides_src/img/06/unclustered.png", plot=g_unclus, width=4, height=4)
(
  g_clus = ggplot(dat1, aes(x=petal_width, y=petal_length, color=factor(cluster))) +
    geom_point(position="jitter",show.legend=F) +
    theme_minimal() +
    NULL
)  
ggsave("slides_src/img/06/clustered.png", plot=g_clus, width=4, height=4)


# variando k --------------------------------------------------------------

km2 = kmeans(dat1, 2, nstart=50)
km4 = kmeans(dat1, 4, nstart=50)
dat2 = dat1 %>% mutate(cluster2=km2$cluster, cluster4=km4$cluster)
(
  g_clus2 = ggplot(dat2, aes(x=petal_width, y=petal_length, color=factor(cluster2))) +
    geom_point(position="jitter",show.legend=F) +
    theme_minimal() +
    NULL
)  
ggsave("slides_src/img/06/clustered_2.png", plot=g_clus2, width=4, height=4)
(
  g_clus4 = ggplot(dat2, aes(x=petal_width, y=petal_length, color=factor(cluster4))) +
    geom_point(position="jitter",show.legend=F) +
    theme_minimal() +
    NULL
)  
ggsave("slides_src/img/06/clustered_4.png", plot=g_clus4, width=4, height=4)


# jerarquico --------------------------------------------------------------

datsc = dat %>% scale() 
# matriz de distancias
distm = dist(datsc, method='euclidean')
metodos = c("complete", "average", "single")
cluster_j = metodos %>% map(function(x) hclust(distm, method=x)) %>% 
  setNames(metodos)
# dendograma
g_average = factoextra::fviz_dend(cluster_j$average) + labs(title="Average",y=NULL)
g_single = factoextra::fviz_dend(cluster_j$single) + labs(title="Single",y=NULL)
g_complete = factoextra::fviz_dend(cluster_j$complete) + labs(title="Complete",y=NULL)

ggsave("slides_src/img/06/dend_average.png", plot=g_average, width=4, height=6)
ggsave("slides_src/img/06/dend_single.png", plot=g_single, width=4, height=6)
ggsave("slides_src/img/06/dend_complete.png", plot=g_complete, width=4, height=6)

# correlacion cofenetica de cada metodo:
cluster_j %>% map_dfr(function(x) cor(distm, cophenetic(x))) %>% t %>% 
  set_colnames("Cophenetic Corr.") 


# k optimo ----------------------------------------------------------------

# WIC
g_elbow = factoextra::fviz_nbclust(datsc, kmeans, nstart=20, method="wss") + 
  labs(title=NULL)
ggsave("slides_src/img/06/k_elbow.png", plot=g_elbow, width=6, height=4)

# silhouette
km = kmeans(datsc, 2, nstart=50)
sil = cluster::silhouette(km$cluster, dist(datsc))
g_sils = fviz_silhouette(sil) + theme_minimal() + labs(title=NULL)
ggsave("slides_src/img/06/silhouette.png", plot=g_sils, width=6, height=4)
g_ksil = 
  factoextra::fviz_nbclust(datsc, kmeans, nstart=20, method="silhouette") +
  labs(title=NULL)
ggsave("slides_src/img/06/k_silhouette.png", plot=g_ksil, width=6, height=4)

# gap
g_kgap = factoextra::fviz_nbclust(datsc, kmeans, method="gap_stat") + 
  labs(title=NULL)
ggsave("slides_src/img/06/k_gap.png", plot=g_kgap, width=6, height=4)


# hopkins -----------------------------------------------------------------

# muestreo al azar
datsc_r = as.data.frame(datsc) %>% 
  map_dfc(function(x) runif(length(x), min(x), max(x)))

mapacalor = function(data) {
  dm = dist(data)
  clus = hclust(dm, method="average")
  dmat = as.matrix(dm)[order(clus$order), order(clus$order)]
  gdat = dmat %>% data.table::melt() %>% setNames(c("i","j","d"))
  ggplot(gdat, aes(x=i, y=j, z=d)) +
    geom_tile(aes(fill=d)) +
    scale_fill_viridis_c() +
    labs(x=NULL, y=NULL) +
    NULL
}

dendo = function(data) {
  dm = dist(data)
  clus = hclust(dm, method="average")
  factoextra::fviz_dend(clus, show_labels=F) +
    labs(y=NULL, title=NULL)
}

g_heat = mapacalor(datsc)
g_heat_rand = mapacalor(datsc_r)
ggsave("slides_src/img/06/heatmap.png", plot=g_heat, width=6, height=4)
ggsave("slides_src/img/06/heatmap_random.png", plot=g_heat_rand, width=6, height=4)
g_dend = dendo(datsc)
g_dend_rand = dendo(datsc_r)
ggsave("slides_src/img/06/dendo.png", plot=g_dend, width=5, height=4)
ggsave("slides_src/img/06/dendo_random.png", plot=g_dend_rand, width=5, height=4)

factoextra::get_clust_tendency(datsc, 20)
factoextra::get_clust_tendency(datsc_r, 20)

replicate(10, clustertend::hopkins(datsc, n=20)$H, simplify=T) %>% mean()
replicate(10, clustertend::hopkins(datsc_r, n=20)$H, simplify=T) %>% mean()


# PCA ---------------------------------------------------------------------

pca = prcomp(datsc)
g_pca =factoextra::fviz_pca_biplot(pca, label="var", col.ind=iris$Species) + 
  theme(legend.position="none") + 
  labs(title=NULL, legend=NULL)
ggsave("slides_src/img/06/pca.png", plot=g_pca, width=4, height=4)
