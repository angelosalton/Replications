library(igraph)
library(magrittr)
library(readr)
library(reshape2)
library(tidyverse)
library(stargazer)

set.seed(1992)

## Análise nacional -------------

# carrega a matriz de impacto intersetorial
mip <- read_delim("mip.csv", ";", escape_double = FALSE, 
                  trim_ws = TRUE)

# carrega a matriz de oferta e demanda
mip_od <- read_delim("mip_od.csv", "\t", 
                     escape_double = FALSE, trim_ws = TRUE)
names(mip_od) <- gsub("[0-9][0-9][0-9][0-9]\ ", replacement = "", names(mip_od))
oferta <- sort(unlist(mip_od[128,-c(1,2,69:78)]), decreasing = T)

# carrega a matriz de leontief
mip_leontief <- read_delim("mip_leontief.csv", 
                           "\t", escape_double = FALSE, locale = locale(decimal_mark = ","),
                           trim_ws = TRUE)
mip_leontief <- as.matrix(mip_leontief[,-1])
rownames(mip_leontief) <- colnames(mip_leontief)


# gera os links
mip <- melt(mip)
names(mip) <- c("from","to","coefficient")

# um threshold pra simplificar a rede
thr <- 0.01
mip$coefficient[mip$coefficient < thr] <- NA
mip <- mip %>% 
  na.omit() %>% 
  arrange(from,to)

# gera a rede
net <- graph_from_data_frame(mip)
net <- igraph::simplify(net, remove.multiple = T, remove.loops = T) 


# compara os 10 graus e setores com maior consumo intermediário
grau <- degree(net)

tabela <- cbind(1:10,names(sort(grau, decreasing = T)), names(sort(oferta, decreasing = T)))
tabela <- tabela[1:10,]; colnames(tabela) <- c("Ranking","Grau", "Consumo intermediário")
#stargazer(as.matrix(tabela), out = "ranking.tex", float = F)

## características do gráfico
# tamanho dos vértices de acordo com o grau
V(net)$size <- grau/3

E(net)$width <- edge.betweenness(net)/80

# mostrar apenas os rótulos dos vértices mais importantes (>20)
nomes <- names(grau)
#x <- (grau>20 | grau<10)
#nomes[which(x == FALSE)] <- NA

# layout
lay <- layout_with_kk(net)
#wid <- 1.2
#lay <- norm_coords(lay, ymin=-wid, ymax=wid, xmin=-wid, xmax=wid)

# clusters
group <- cluster_fast_greedy(as.undirected(net))

library(RColorBrewer)
V(net)$community <- group$membership
colrs <- adjustcolor(c("blue","red","green"), alpha=0.5)

# chama o gráfico
plot.igraph(net, rescale=T, edge.arrow.size=.0, edge_curved=.2, edge.color="gray20",
            vertex.label=nomes, vertex.label.family="Helvetica", vertex.label.cex = .6, vertex.label.color="gray20",
            vertex.color=colrs[V(net)$community], vertex.frame.color="gray20",
            vertex.label.dist=.5, vertex.label.degree=pi/2,
            layout=lay)

# calcular índice de Rasmussen-Hirschman
irh <- function(mat, para = "tras"){
  tam <- nrow(mat)
  med <- mean(c(mat))
  if(para == "tras"){
    x <- (colSums(mat)/tam)/med
  } else if(para == "frente"){
    x <- (colMeans(mat)/tam)/med
  }
  return(x[order(x, decreasing = T)])
}
 
irh(mip_leontief, para = "tras")[1:10]

# calcula os caminhos mais curtos por setores-chave (os 10 maiores) e pinta no gráfico
E(net)$weight <- edge.betweenness(net)

E(net)$color <- "white"
E(net)$width <- 0.01

paths <- list()
path_colors <- RColorBrewer::brewer.pal(10, "Set3")

for(i in 1:10){
  paths[[i]] <- shortest_paths(net, weights = 1/E(net)$weight, from = names(sort(grau, decreasing = T))[i], mode = "out")$vpath[[1]] %>% unlist() %>% unclass()
  E(net, path = paths[[i]])$color <- path_colors[i]
  E(net, path = paths[[i]])$width <- 3
}

# tirar do gráfico vértices que não interessam
x <- setdiff(1:66, unique(unlist(paths)))

plot.igraph(net-x, rescale=T, edge.arrow.size=.1, edge.arrow.color=E(net)$color,
            vertex.label.family="Helvetica", vertex.label.cex = .7, vertex.label.color="gray20",
            vertex.color="gray",
            vertex.frame.color="gray20",
            vertex.label.dist=1, vertex.label.degree=pi/2,
            layout=layout_with_kk(net-x))


## Análise regional ------------

rm(list=ls())

library(rgdal)
library(rgeos)
mapa <- readOGR("../Mapas/Estados_do_Brasil", "Brasil", encoding = "WINDOWS-1252")
laymap <- coordinates(mapa) # layout baseado nos centroides

load("~/Bases/testioreg.RData")
base_coef <- read_csv("~/Google Drive/Bases/coefs.csv") %>% arrange(Source,Target,Setor)

netr <- graph_from_data_frame(base_coef)
#netr <- igraph::simplify(netr)

E(netr)$weight <- edge.betweenness(netr)
E(netr)$width  <- edge.betweenness(netr)

# teste: agregando os setores
temp <- base_coef %>% group_by(Source, Target) %>% summarise(val = sum(Value))
netr2 <- graph_from_data_frame(temp)

E(netr2)$weight <- edge.betweenness(netr2)
E(netr2)$width  <- edge.betweenness(netr2)

plot(mapa)
plot.igraph(netr2, add=T, rescale=F, edge.arrow.size=.1, edge.color="gray50", edge.curved=.4,
              vertex.label.family="Helvetica", vertex.label.cex = .6, vertex.label.color="gray20",
              layout=laymap)

# modelo de Contreras (2014)
rm(list=ls())
load("~/Google Drive/Bases/haddad_mipr.RData")
mipr_va %<>% as.data.frame()
mipr_va$`Demanda Total` <- mipr_va$`Gross output` + mipr_va$`Var estoques`

# x = L.d
mipr_va$VBP <- mipr_l %*% mipr_va$`Demanda Total`

# seleciona as matrizes regionais
indice1 <- seq(1, 1836, by = 68)
indice2 <- seq(68, 1836, by = 68)

indices <- list()
miprs <- list()
for(i in 1:27){
  indices[[i]] <- indice1[i]:indice2[i]
  miprs[[i]] <- mipr_cti[indices[[i]],indices[[i]]]
  dimnames(miprs[[i]]) <- list(mipr_sec[,2], mipr_sec[,2])
}
rm(indice1,indice2)

# análise inter-regional
net_r <- graph_from_adjacency_matrix(mipr_cti, mode = "directed", weighted = T)

# model 1
# model1 <- function(c=.8, f=.1, Z){
#   
#   L <- solve(diag(ncol(Z))-Z)
#   
#   # inicializa
#   a  <- numeric(ncol(L))
#   e  <- numeric(ncol(L))
#   Dx <- matrix(0, ncol = ncol(L), nrow = nrow(L))
#   
#   # gera os choques e calcula o efeito em cada setor
#   for(i in 1:ncol(L)){
#     #browser()
#     e  <- rep(1, ncol(L))
#     e[i] <- (1-f)
#     Dx[,i] <- L[,i] %*% e
#     
#     a[i] <- sum(Dx[,i]<1)
#   }
#   return(a)
# }

# outras métricas pra regional ------------
netr <- list()
for(i in 1:27){
  netr[[i]] <- graph_from_adjacency_matrix(miprs[[i]], weighted = "directed")
}
names(netr) <- mipr_reg[,3]

# graus de entrada e saída
deg_in <- sapply(miprs, function (x) sort(colSums(x), decreasing = T)[1])#; names(deg_in) <- mipr_reg[,3]
deg_ou <- sapply(miprs, function (x) sort(rowSums(x), decreasing = T)[1])#; names(deg_ou) <- mipr_reg[,3]
deg <- data.frame(mipr_reg[,3], names(deg_in), names(deg_ou))
deg <- deg[order(deg[,1]),]

# centralidade de vértices (vertex betweeness)
vert.betw <- lapply(netr, function (x) {sort(betweenness(x), decreasing = T)[1]})
vert <- str_split(names(unlist(vert.betw)), pattern = fixed("."), simplify = T)
vert <- vert[order(vert[,1]),]
names(vert) <- c("Estado","Principal setor")
#stargazer(vert, out = "vert.tex", float = F)

# centralidade por autovalores
eigen <- lapply(netr, function (x) {eigen_centrality(x)$vector %>% sort(decreasing = T)})
for(i in 1:27){
  eigen[[i]] <- eigen[[i]][which(eigen[[i]]==1)] %>% names()
}

eigen <- unlist(eigen)
tabela_eigen <- data.frame(names(eigen), eigen, row.names = NULL)
tabela_eigen <- tabela_eigen[order(tabela_eigen[,1]),]
colnames(tabela_eigen) <- c("Estado","Setores centrais")
tabela_eigen <- as.matrix(tabela_eigen)
#stargazer(tabela_eigen, out = "eigen.tex", float = F)

# comparação
c_vert <- lapply(netr, betweenness); c_vert <- as.data.frame(c_vert)
c_eig  <- lapply(netr, function (x) {eigen_centrality(x)$vector}); c_eig <- as.data.frame(c_eig)
c_vert$setor <- rownames(c_vert); c_vert <- melt(c_vert); colnames(c_vert)[3] <- "vertex"
c_eig$setor <- rownames(c_eig); c_eig <- melt(c_eig); colnames(c_eig)[3] <- "eigen"

c_centr <- left_join(c_vert, c_eig); rm(c_vert, c_eig)
ggplot(c_centr, aes(x=eigen, y=vertex)) + geom_point(shape=1) + theme_classic() + labs(x = "Eigenvalue centrality", y = "Vertex betweeness")
ggsave("centr_scatter.pdf", width = 6, height = 4)

# herfindahl
indice_dif <- function(Z){
  # quantos setores?
  n <- ncol(Z)
  # calcula a leontief
  L <- solve(diag(n)-Z)
  h <- numeric(n)
  
  # calcula o indice
  for(i in 1:n){
    #browser()
    h[i] <- (L[i,i]/sum(L[,i]))^2
  }
  # normaliza
  h <- (h-1/n)/(1-1/n)
  # retorna o resultado
  return(1-h)
}

herf <- sapply(miprs, indice_dif)
herf <- data.frame(mipr_reg[,3], mipr_sec[,2][apply(herf, 2, which.max)], round(apply(herf, 2, max), 3))
herf <- herf[order(herf[,1]),]
colnames(herf) <- c("Estado","Maior índice de difusão","Índice")
#stargazer(as.matrix(herf), out = "herf.tex", float = F)

# identifica quais os maiores fluxos


