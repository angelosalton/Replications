library(fastSOM)
library(ggplot2)
library(magrittr)
library(stargazer)
library(vars)
library(xts)

# estat. descritivas
load("industriais.RData")

ind_cni <- ind_cni %>%
  na.omit() %>%
  dplyr::select(-`19A_Coque_e_derivados_do_petróleo`)

# chamando por ordem de importância
ind_cni <- ind_cni %>%
  dplyr::select(data,`10_Produtos_alimentícios`,`19_Coque,_produtos_derivados_do_petróleo_e_biocombustíveis`,`29_Veículos_automotores,_reboques_e_carrocerias`,`20A_Químicos_(Exceto_Perfumaria,_Sabões,_Detergentes_e_Produtos_de_Limpeza_e_de_Higiene_Pessoal_)`,`24_Metalurgia`,`28_Máquinas_e_equipamentos`,`23_Produtos_de_minerais_não_metálicos`,`25_Produtos_de_metal_(exceto_máquinas_e_equipamentos)`,`22_Produtos_de_borracha_e_de_material_plástico`,`17_Celulose,_papel_e_produtos_de_papel`)

names(ind_cni) <- c("date","Foods","Chemicals","Oil and biofuels","Vehicles","Steelworks","Mach. and equip.","Mining products","Metal products","Rubber and plastic",
       "Paper and cellulose")

stargazer(ind_cni, out = "descritivas.tex", float = F, digits = 2)

# ajuste sazonal
b <- lapply(ind_cni[,-1], ts, start=c(2006,1), frequency=12)
b <- as.data.frame(b)

b <- lapply(b, function (x) {
  stl(x, s.window = 12)$time.series[,3]
})
b <- as.data.frame(b)

# modelo
x <- xts(b, order.by = ind_cni$date)

# primeira diferença
x <- diff.xts(x)
x <- na.omit(x)

# opcional: standardize
x <- scale(x)

#VARselect(ind_cni, type = "const")
model <- VAR(x, p = 1, type = "none") # roda o modelo VAR
arch.test(model)
normality.test(model)

phi <- Phi(model) # coefs MA
vc <- summary(model)$covres # a matriz varcov

# spillover table
sindex <- sot(Sigma = vc, A = phi)
sindex <- cbind(sindex,0)

colnames(sindex) <- gsub("\\."," ", colnames(sindex))
rownames(sindex) <- colnames(sindex)[-11]
colnames(sindex)[11] <- "External contrib."

# contribuições externas
for (i in 1:nrow(sindex)) { # linhas
  sindex[i,nrow(sindex)+1] <- 100-sindex[i,i]
    }

sindex <- round(sindex, digits = 1)
stargazer(sindex, out = "spillindex.tex", float = F, digits = 1)
