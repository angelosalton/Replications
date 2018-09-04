library(apt)
library(dplyr)
library(egcm)
library(lattice)
library(magrittr)
library(stargazer)
library(urca)

# SALTON, A., MATTOS, L.B. Transmissão assimétrica de preços no mercado brasileiro de etanol.
# Metodologia

# dados ---------------------

load("combustiveis.RData")
cidades <- unique(dados$MUNIC)

dados <- dados %>% filter(`DATA INICIAL` <= "2016-11-01")

dados <- dados[,c("DATA INICIAL","REGIAO","UF","MUNIC","ETANOL_P MED DISTRIB",
                  "ETANOL_P MED REVENDA","GASOLINA_P MED DISTRIB","GASOLINA_P MED REVENDA","Codmun")]
dados <- dados[complete.cases(dados),] # remove missing

# parâmetros dos testes
teste <- "pp"
pvalor <- 0.05

# descritivas

#stargazer(data.frame(dados), type = "latex")

# COINTEGRAÇÃO --------------------------

coint <- list()
for(i in cidades){
  y <- dados$`ETANOL_P MED REVENDA`[dados$MUNIC==i]
  x <- dados$`ETANOL_P MED DISTRIB`[dados$MUNIC==i]
  
  if(length(x) < 100 | length(y) < 100){
    next
  }
  
  if(any(is.na(x))==TRUE | any(is.na(y))==TRUE){
    next
  }
  
  coint[[i]] <- egcm(y, x, i1test = teste, urtest = teste, p.value = pvalor)
}

res <- unlist(lapply(coint, is.cointegrated))
cidades.etanol <- names(res[which(res==T)])

rm(i,res,x,y)
rm(coint)

# MODELOS VEC ASSIMETRICOS --------------------------
detach("package:dplyr", unload=TRUE) # conflitos entre pacotes

ecm <- list()
for(i in cidades.etanol){
  y <- as.ts(dados$`ETANOL_P MED REVENDA`[dados$MUNIC==i])
  x <- as.ts(dados$`ETANOL_P MED DISTRIB`[dados$MUNIC==i])
  
  ecm[[i]] <- print(ecmAsyTest(ecmAsyFit(y, x, lag = 1, model = "linear")))
}

# criando uma base para guardar os resultados

# descrição dos testes
# Hypothesis description|          Expression                                     
# [1,] "H1: Equ adjust path asymmetry|" "X.ECT.t_1.pos=X.ECT.t_1.neg"                  
# [2,] "H2: Granger causality test|"    "x (x) does not Granger cause..."              
# [3,] "H2: Granger causality test|"    "y (y) does not Granger cause..."              
# [4,] "H3: Distributed lag asymmetry|" "X.diff.x.t_1.pos = X.diff.x.t_1.neg"          
# [5,] "H3: Distributed lag asymmetry|" "X.diff.y.t_1.pos = X.diff.y.t_1.neg"          
# [6,] "H4: Cumulative asymmetry|"      "Cumulative positive x = Cumulative negative x"
# [7,] "H4: Cumulative asymmetry|"      "Cumulative positive y = Cumulative negative y"
# downstream: atacado para varejo
# upstream: varejo para atacado

# recolocando os resultados na base

library(dplyr)
resultados <- lapply(ecm, function(x) data.frame(x[,c(5,6)] < pvalor))
resultados <- do.call(rbind, lapply(resultados, data.frame, stringsAsFactors=FALSE))
resultados <- tbl_df(cbind(rep(names(ecm), each=7), 1:7, resultados))
rownames(resultados) <- NULL
colnames(resultados) <- c("MUNIC","teste","downstream","upstream")
resultados$MUNIC %<>% as.character()

dados <- dados %>% mutate(coint.comb = 0, as.ect = 0, as.ar = 0)

# relações de cointegração
dados$coint.comb[dados$MUNIC %in% cidades.etanol] <- 1
dados$coint.comb %<>% ordered(labels=c("sem relação","co-integração"))

# assimetria no ECT
cidades.as.ect.0 <- unique(resultados$MUNIC[resultados$teste==1 & resultados$downstream==F & resultados$upstream==F])
cidades.as.ect.d <- unique(resultados$MUNIC[resultados$teste==1 & resultados$downstream==T])
cidades.as.ect.u <- unique(resultados$MUNIC[resultados$teste==1 & resultados$upstream==T])

dados$as.ect[dados$MUNIC %in% cidades.as.ect.0] <- 1
dados$as.ect[dados$MUNIC %in% cidades.as.ect.d] <- 2
dados$as.ect[dados$MUNIC %in% cidades.as.ect.u] <- 3
dados$as.ect[dados$MUNIC %in% intersect(cidades.as.ect.d, cidades.as.ect.u)] <- 4

dados$as.ect %<>% ordered(labels=c("sem relação","simetria","atacado para varejo", "varejo para atacado", "ambos"))

# assimetria nos termos AR
cidades.as.ar.0 <- unique(resultados$MUNIC[resultados$teste==2 & resultados$downstream==F & resultados$upstream==F])
cidades.as.ar.d <- unique(resultados$MUNIC[resultados$teste==2 & resultados$downstream==T])
cidades.as.ar.u <- unique(resultados$MUNIC[resultados$teste==2 & resultados$upstream==T])

dados$as.ar[dados$MUNIC %in% cidades.as.ar.0] <- 1
dados$as.ar[dados$MUNIC %in% cidades.as.ar.d] <- 2
dados$as.ar[dados$MUNIC %in% cidades.as.ar.u] <- 3
dados$as.ar[dados$MUNIC %in% intersect(cidades.as.ar.d, cidades.as.ar.u)] <- 4

dados$as.ar %<>% ordered(labels=c("sem relação","simetria","atacado para varejo", "varejo para atacado", "ambos"))

# RESULTADOS -----------------------

# co-integração

x <- dados[dados$`DATA INICIAL`=="2004-05-09",]

x <- group_by(x, REGIAO, coint.comb)
tabela <- summarize(x, count = n())
tabela %<>% reshape2::melt() %>% reshape2::dcast(REGIAO ~ coint.comb + variable)
rownames(tabela) <- levels(tabela$REGIAO); tabela$REGIAO <- NULL
tabela$total <- rowSums(tabela)
tabela[6,] <- colSums(tabela)
tabela[7,] <- tabela[6,]/tabela[6,3]*100
dimnames(tabela) <- list(c("Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul", 
                                "total","porcentagem"), c("sem relação", "co-integração", "total"))
#stargazer(as.matrix(tabela))

# assimetria no ECT

x <- dados[dados$`DATA INICIAL`=="2004-05-09",]

x <- group_by(x, REGIAO, as.ect)
tabela <- summarize(x, count = n())
tabela %<>% reshape2::melt() %>% reshape2::dcast(REGIAO ~ as.ect + variable)
rownames(tabela) <- levels(tabela$REGIAO); tabela$REGIAO <- NULL
tabela[,1] <- NULL
tabela[is.na(tabela)] <- 0

tabela$total <- rowSums(tabela)
tabela[6,] <- colSums(tabela)
tabela[7,] <- tabela[6,]/tabela[6,5]*100
dimnames(tabela) <- list(c("Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul", 
                                "total","porcentagem"),
                         c("simetria","atacado para varejo", "varejo para atacado", "ambos", "total"))
#stargazer(as.matrix(tabela))

# assimetria nos termos AR

x <- dados[dados$`DATA INICIAL`=="2004-05-09",]

x <- group_by(x, REGIAO, as.ar)
tabela <- summarize(x, count = n())
tabela %<>% reshape2::melt() %>% reshape2::dcast(REGIAO ~ as.ar + variable)
rownames(tabela) <- levels(tabela$REGIAO); tabela$REGIAO <- NULL
tabela[,1] <- NULL
tabela[is.na(tabela)] <- 0

tabela$total <- rowSums(tabela)
tabela[6,] <- colSums(tabela)
tabela[7,] <- tabela[6,]/tabela[6,5]*100
dimnames(tabela) <- list(c("Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul", 
                                "total","porcentagem"),
                         c("simetria","atacado para varejo", "varejo para atacado", "ambos", "total"))
#stargazer(as.matrix(tabela))

# MAPAS -------------------
library(rgdal)
setwd("../Mapas")
mapa <- readOGR("Shapefile Mapa do Brasil Microrreg","MUNICIPIOS_poligonos")
mapa2 <- readOGR("Shapefile Estados","Brasil")
levels(mapa@data$COD_IBGE) %<>% stringr::str_sub(start = 1, end = 6)
colnames(mapa@data)[1] <- "Codmun"
mapa@data$Codmun %<>% as.character()
y <- dados[dados$`DATA INICIAL`=="2004-05-09", c(9:12)]
y$Codmun %<>% as.character()
mapa@data <- left_join(tbl_df(mapa@data), y)

mapa@data$coint.comb %<>% ordered(labels=c("sem relação","co-integração"))

# escala
#scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
#             offset = c(-70,-30), scale = 10, fill=c("transparent","black"))
#text1 = list("sp.text", c(-71, -32), "0")
#text2 = list("sp.text", c(-57, -32), "1000 km")

spplot(mapa, "coint.comb", col.regions = c("gray70", "green"), colorkey=TRUE, col = NA,
       par.settings = list(axis.line = list(col = 'transparent')),
       sp.layout = list(mapa2, first=FALSE, col = "gray"))

spplot(mapa, "as.ect", col.regions = c("white", "gray70", "red", "darkblue", "green"), colorkey=TRUE, col = NA,
       par.settings = list(axis.line = list(col = 'transparent')),
       sp.layout = list(mapa2, first=FALSE, col = "gray"))

spplot(mapa, "as.ar", col.regions = c("white", "gray70", "red", "darkblue", "green"), colorkey=TRUE, col = NA,
       par.settings = list(axis.line = list(col = 'transparent')),
       sp.layout = list(mapa2, first=FALSE, col = "gray"))
