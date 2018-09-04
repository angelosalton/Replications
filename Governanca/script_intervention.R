library(xlsx)
library(TSA)
library(astsa)
library(urca)
library(forecast)
library(strucchange)

### CARREGAR DADOS COM AS DUMMIES
data <- read.xlsx("ativosretorno.xls", 1)

###C?digo para modelos de interven??o
retNM <- data[,] # Ativos do novo mercado
retN1 <- data[,'N1'] # Ativos N1
retN2 <- data[,'N2'] # Ativos N2

# Mostra os resultados do modelo AR(p) com p selecionado pelo criterio de Akaike
ar_NMa <- ar(retNM[,1], order.max = 10); ar_usa

# Com x=p e data[,'dummy'] sendo a dummy do ativo 1 do novo mercado - Efeito abrupto e permanente
reg_NMa <- arimax(retNM[,1], order=c(1,0,0), xtransf=data[,'gBBAS3'], transfer=list(c(0,0)), method="ML"); reg_NMa

# Com x=p e data[,'dummy'] sendo a dummy do ativo 1 do novo mercado - Efeito gradual e permanente
reg_NMb <- arimax(retNM[,1], order=c(1,0,0), xtransf=data[,'gBBAS3'], transfer=list(c(1,0)), method="ML"); reg_NMb