library(dplyr)
library(magrittr)
library(psych)
library(stargazer)
library(stringr)

load("GlobalEntrepreneurM.RData")

GEMdata <- GEM %>% select(-country, -year)

# Lista de variáveis
#stargazer(dic, out = "listavar.tex", float = F)

# Estatísticas descritivas
descritivas <- describe(GEMdata) %>% round(digits = 2) %>% as.matrix()
descritivas <- descritivas[, c("n","mean","median","sd","min","max")]
colnames(descritivas) <- c("Obs.","Média","Mediana","Desv. pad.","Mínimo","Máximo")
#stargazer(descritivas, out = "descr.tex", float = F, digits = 2, decimal.mark = ",")

# Análise fatorial exploratória -----------
nfat <- 8 # número de fatores identificados
(af1 <- fa(r = GEMdata, nfactors = nfat, rotate = "varimax", warnings = T, fm = "ols"))

# cargas fatoriais
cargas <- af1$loadings %>%
  unclass() %>%
  data.frame(., af1$uniquenesses) %>%
  round(digits = 2) %>%
  as.matrix()

colnames(cargas) <- c(paste0("f_", 1:af1$factors), "Unicidade")
#stargazer(cargas, out = "cargas.tex", float = F, digits = 2, decimal.mark = ",")

# proporções da variância
variancia <- af1$Vaccounted %>%
  round(digits=2) %>%
  t()

colnames(variancia) <- c("Autovalor", "Proporção da var.", "Var. acumulada", "Prop. explicada", "Prop. cumulativa")
#stargazer(variancia, out = "variancia.tex", float = F, digits = 2, decimal.mark = ",")

# diagrama
fa.diagram(af1, cut = .5, rsize = 2, e.size = .04, digits = 2, simple=F, main = "")
#dev.copy2pdf(out.type = "pdf")

# Diagnóstico e robustez -------------
# A matriz de correlações dos dados
r <- af1$r

(kmo <- KMO(r)) # critério de Kaiser
(bartl <- cortest.bartlett(r, n = nrow(GEMdata))) # teste de esfericidade de Bartlett
(alphacron <- psych::alpha(r, check.keys = T)) # alpha de Cronbach

# Montando uma tabela
diagn <- matrix(NA, nrow = 3, ncol = 1)
dimnames(diagn) <- list(c("Teste de Bartlett","Critério KMO","Alpha de Cronbach"), "Estimativa")
diagn[1:3,] <- c(bartl$p.value, kmo$MSA, alphacron$total$raw_alpha)
#stargazer(diagn, out = "diagn.tex", float = F, digits = 3, decimal.mark = ",")

load(file = "doingbusiness.RData")
doingb <- doingb %>% filter(Economy %in% c("Argentina","Brazil","Chile","Colombia","Mexico","Paraguay","Uruguay"))

library(ggplot2)
doingb %>% ggplot(aes(x = `DB Year`, y = `DTF - Starting a business`, group = Economy)) + geom_line(aes(colour = Economy, linetype = Economy)) + labs(x = "Ano", y = "Índice DTF") + theme_classic()
ggsave("doingbusiness.pdf", height = 4, width = 6)
