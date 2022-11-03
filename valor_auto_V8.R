#Fernanda Venturato Roquim
# JUN/2022
# Análise dos dados - Apenas severidade do sinistro de veículos

rm(list=ls())

# Set Working Space
setwd("~/Área de Trabalho/OneDrive/Tese/Rotinas R/Valor Auto")

# Chamando pacotes necessários
library(readr) # para ler CSV
library(gamlss)
library(lme4)
library(psych) #funcao describe()
library(epiDisplay) #funcao tab1()
library(tdr) #função tdStats()

# Lendo os dados
dados <- read_csv("data.csv", col_types = cols(...1 = col_skip(), 
                                               Car_2ndDriver_M = col_factor(levels = c("0", 
                                                                                       "1")), Claims2 = col_skip(), Insuredcapital_content_re = col_skip(), 
                                               Insuredcapital_continent_re = col_skip(), 
                                               NClaims2 = col_skip(), PolID = col_character(), 
                                               Policy_PaymentMethodA = col_factor(levels = c("0", 
                                                                                             "1")), Policy_PaymentMethodH = col_skip(), 
                                               Retention = col_skip(), Types = col_skip(), 
                                               appartment = col_skip(), gender = col_factor(levels = c("0", 
                                                                                                       "1")), metro_code = col_factor(levels = c("0", 
                                                                                                                                                 "1")), num_policiesC = col_skip(), 
                                               NClaims1 = col_integer()))




# Excluindo os carros outliers
dados <- subset(dados, age_of_car_M < 36)

# Balanceando apenas para as apólices que tem 5 observações
dados_b <- data.frame(subset(dados, year == 5)$PolID)
vetor <- dados_b$subset.dados..year....5..PolID
dados <- subset(dados, PolID %in% vetor)

# Corrigindo variáveis de tempo ao longo do período
vetor1s <- rep(1, length(dados$PolID))
dados$Age_client <- (dados$Age_client - vetor1s) + as.numeric(dados$year)
dados$age_of_car_M <- (dados$age_of_car_M - vetor1s) + as.numeric(dados$year)
dados$Client_Seniority <- (dados$Client_Seniority - vetor1s) + as.numeric(dados$year)

#Corrigindo as ocorrências gratuitas (Substitui em NClaims com base no critério de
#Claims)
dados$NClaims1[dados$Claims1 == 0] <- 0

dados_brutos <- dados

# Separando os anos
dados1 <- subset(dados, year == 1)
dados2 <- subset(dados, year == 2)
dados3 <- subset(dados, year == 3)
dados4 <- subset(dados, year == 4)
dados5 <- subset(dados, year == 5)

#Separando ocorrência de não ocorrência
dados_oc <- subset(dados, Claims1 > 0)
dados_ze <- subset(dados, Claims1 == 0)

# Excluindo o 5 ano dos dados principais
dados <- subset(dados, year < 5)

# Ocorrências ano a ano
dados_oc1 <- subset(dados1, Claims1 > 0)
dados_oc2 <- subset(dados2, Claims1 > 0)
dados_oc3 <- subset(dados3, Claims1 > 0)
dados_oc4 <- subset(dados4, Claims1 > 0)
dados_oc5 <- subset(dados5, Claims1 > 0)
#=============================================================================
#Analise Exploratória

describe(dados) # anos de 1 a 4
describe(dados_brutos) #dados brutos inclui o ano 5

#Ano a ano
describe(dados_oc1$Claims1)
describe(dados_oc2$Claims1)
describe(dados_oc3$Claims1)
describe(dados_oc4$Claims1)
describe(dados_oc5$Claims1)


#Variavel Resposta
hist(dados$Claims1) #a densidade de zeros atrapalha muito a visualizacao
boxplot(dados$Claims1)
plot(density(dados$Claims1))

#Fazendo alguns cortes para entender melhor as ocorrências
hist(dados_oc$Claims1, breaks = 300, xlim = c(0,5000))

plot(density(dados_oc$Claims1), xlim = c(0,5000), xlab = "Valor do sinistro (euros)", ylab = 
       "Densidade", main = "", ylim=c(0,0.010))

nrow(dados[dados$Claims1>5000, ]) #quantas obs foram omitidas

table(dados_oc$Claims1) #contagem da freq em 882,00
boxplot(dados_oc$Claims1, las =1)

# Covariáveis Continuas

#Idade do cliente
par(mfrow=c(3,1))
hist(dados$Age_client, xlim = range(20,100), breaks = 30)
hist(dados_oc$Age_client, xlim = range(20,100), breaks = 30, main = )
hist(dados_ze$Age_client, xlim = range(20,100), breaks = 30)

par(mfrow=c(1,3))
boxplot(dados$Age_client, main = "Idade do cliente total", ylim = c(20,100))
boxplot(dados_oc$Age_client, main = "Idade do cliente - obs com ocorrência", ylim = c(20,100))
boxplot(dados_ze$Age_client, main = "Idade do cliente - obs sem ocorrência", ylim = c(20,100))

#Idade do veículo
par(mfrow=c(3,1))
hist(dados$age_of_car_M, xlim = range(0,35), breaks = 30)
hist(dados_oc$age_of_car_M, xlim = range(0,35), breaks = 30)
hist(dados_ze$age_of_car_M, xlim = range(0,35), breaks = 30)

par(mfrow=c(1,3))
boxplot(dados$age_of_car_M, main = "Idade do veículo total", ylim = c(0,35))
boxplot(dados_oc$age_of_car_M, main = "Idade do veiculo - obs com ocorrência", ylim = c(0,35))
boxplot(dados_ze$age_of_car_M, main = "Idade do veículo - obs sem ocorrência", ylim = c(0,35))

#Potência do veículo
par(mfrow=c(3,1))
hist(dados$Car_power_M, xlim = range(5,560), breaks = 30)
hist(dados_oc$Car_power_M, xlim = range(5,560), breaks = 20)
hist(dados_ze$Car_power_M, xlim = range(5,560), breaks = 30)

par(mfrow=c(1,3))
boxplot(dados$Car_power_M, main = "Potencia do veículo total", ylim = c(5,560))
boxplot(dados_oc$Car_power_M, main = "Potencia do veiculo - obs com ocorrência", ylim = c(5,560))
boxplot(dados_ze$Car_power_M, main = "Poteĉnia do veículo - obs sem ocorrência", ylim = c(5,560))

#Anos do cliente dentro da mesma seguradora (Fidelidade)
par(mfrow=c(3,1))
hist(dados$Client_Seniority, xlim = range(5,50), breaks = 30)
hist(dados_oc$Client_Seniority, xlim = range(5,50), breaks = 20)
hist(dados_ze$Client_Seniority, xlim = range(5,50), breaks = 30)

par(mfrow=c(1,3))
boxplot(dados$Client_Seniority, main = "Fidelidade total", ylim = c(5,50))
boxplot(dados_oc$Client_Seniority, main = "Fidelidade - obs com ocorrência", ylim = c(5,50))
boxplot(dados_ze$Client_Seniority, main = "Fidelidade - obs sem ocorrência", ylim = c(5,50))

#Covariáveis discretas ou qualitativas

#Sexo: 0=homem, 1=mulher 
tab1(dados$gender)
tab1(dados_oc$gender)
tab1(dados_ze$gender)

# Ano
tab1(dados$year)
tab1(dados_oc$year)
tab1(dados_ze$year)

# Presença de segundo motorista
tab1(dados$Car_2ndDriver_M)
tab1(dados_oc$Car_2ndDriver_M)
tab1(dados_ze$Car_2ndDriver_M)

# Região: 0=rural, 1=urbano
tab1(dados$metro_code)
tab1(dados_oc$metro_code)
tab1(dados_ze$metro_code)

#Forma de Pagto: 0=Mensal, 1=Anual
tab1(dados$Policy_PaymentMethodA)
tab1(dados_oc$Policy_PaymentMethodA)
tab1(dados_ze$Policy_PaymentMethodA)

#RELACIONAMENTO ENTRE VARIAVEIS
par(mfrow=c(2,2))
plot(dados_oc$Age_client, dados_oc$Claims1, pch=20, ylab="Valor do sinistro (euros)", xlab = "Idade do Cliente (anos)")
plot(dados_oc$age_of_car_M, dados_oc$Claims1, pch=20, ylab="Valor do sinistro (euros)", xlab="Idade do veículo (anos)")
plot(dados_oc$Car_power_M, dados_oc$Claims1, pch=20, ylab="Valor do sinistro (euros)", xlab="Potência (hp)")
plot(dados_oc$Client_Seniority, dados_oc$Claims1, pch=20, ylab="Valor do sinistro (euros)", xlab="Fidelidade (anos)")

par(mfrow=c(1,4))
plot(dados_oc$gender, dados_oc$Claims1, pch = 20, ylab="Valor do sinistro (euros)", xlab = "Sexo", ylim=c(0,5000), xaxt = "n")
axis(1, at = c(1, 2), labels = c("Masculino", "Feminino"))
plot(dados_oc$Car_2ndDriver_M, dados_oc$Claims1, pch=20, ylab="", xlab="Presença de 2º motorista", ylim=c(0,5000), xaxt = "n")
axis(1, at = c(1, 2), labels = c("Não", "Sim"))
plot(dados_oc$metro_code, dados_oc$Claims1, pch=20, ylab="", xlab="Região", ylim=c(0,5000), xaxt = "n")
axis(1, at = c(1, 2), labels = c("Rural", "Urbano"))
plot(dados_oc$Policy_PaymentMethodA, dados_oc$Claims1, pch=20, ylab="", xlab="Forma de Pagto", ylim=c(0,5000), xaxt = "n")
axis(1, at = c(1, 2), labels = c("Mensal", "Anual"))

covariaveis <- data.frame(dados$Age_client, dados$age_of_car_M, dados$Car_power_M, dados$Client_Seniority)

cor(covariaveis, method = "pearson")

#=============================================================================
#Fazer aqui o blabla das distribuições, fitdist e gamlss.adj

marginal <- fitDist(dados_oc$Claims1, type = "realplus")
marginal$fits

#=============================================================================

#Modelos marginais
ZAGAfixo <- gamlss(Claims1~1, family=ZAGA, data = dados, method = RS(500))
ZAIGfixo <- gamlss(Claims1~1, family=ZAIG, data = dados, method = RS(500))

#GLM - ZAGA
GLM_ZAGAselec <- stepGAIC(ZAGAfixo, scope=list(lower=~1, upper= ~ gender + Age_client + 
                                                 + age_of_car_M + Car_power_M 
                                               + Car_2ndDriver_M 
                                               + metro_code + Policy_PaymentMethodA 
                                               + Client_Seniority),
                          trace = T, data=dados, what = "mu")
summary(GLM_ZAGAselec)
GLM_ZAGA <- gamlss(formula = Claims1 ~ age_of_car_M + Policy_PaymentMethodA +  
                   Car_2ndDriver_M, family = ZAGA, data = dados, trace = T)

#GLM - ZAIG
GLM_ZAIGselec <- stepGAIC(ZAIGfixo, scope=list(lower=~1, upper= ~ gender + Age_client + 
                                                 + age_of_car_M + Car_power_M 
                                               + Car_2ndDriver_M 
                                               + metro_code + Policy_PaymentMethodA 
                                               + Client_Seniority),
                          trace = T, data=dados, what = "mu")
summary(GLM_ZAIGselec)
GLM_ZAIG <- gamlss(formula = Claims1 ~ Car_power_M, family = ZAIG,  
                   data = dados, trace = T)

#GLMM - ZAGA
GLMM_ZAGA <- gamlss(formula = Claims1 ~ age_of_car_M + Policy_PaymentMethodA +  
                    Car_2ndDriver_M + re(random=~1|PolID), family = ZAGA, data = dados, trace = T)

#GLMM - ZAIG (Nao converge)
# GLMM_ZAIG <- gamlss(formula = Claims1 ~ Car_power_M + re(random=~1|PolID), family = ZAIG, 
#                          data = dados, trace = T, gd.tol=Inf, method = CG(50),
#                          n.cycs = 100)

#GAM - ZAGA
GAM_ZAGAselec <- stepGAIC(ZAGAfixo, scope=list(lower=~1, upper= ~ gender + pb(Age_client) + 
                                                 + pb(age_of_car_M) + pb(Car_power_M) 
                                               + Car_2ndDriver_M 
                                               + metro_code + Policy_PaymentMethodA 
                                               + pb(Client_Seniority)),
                          trace = T, data=dados, what = "mu")
summary(GAM_ZAGAselec)
term.plot(GAM_ZAGAselec)
edfAll(GAM_ZAGAselec) #Manter pb()
GAM_ZAGA <- gamlss(formula = Claims1 ~ pb(Age_client), family = ZAGA,  
                   data = dados, method = RS(500), trace = T)

# GAM - ZAIG
# GAM_ZAIGselec <- stepGAIC(ZAIGfixo, scope=list(lower=~1, upper= ~ gender + pb(Age_client) + 
#                                                  + pb(age_of_car_M) + pb(Car_power_M) 
#                                                + Car_2ndDriver_M 
#                                                + metro_code + Policy_PaymentMethodA 
#                                                + pb(Client_Seniority)),
#                           trace = T, data=dados, what = "mu", method = RS(500))
# 
# summary(GAM_ZAIGselec) #Não é GAM

# GAMM - ZAGA (Não converge)
# GAMM_ZAGA <- gamlss(formula = Claims1 ~ pb(Age_client) + re(random=~1|PolID),
#                     family = ZAGA,  data = dados, trace = T, method = RS(100), mu.start=1000)

# GAMM - ZAIG (Não é GAM)

#GAMLSS - ZAGA
GAMLSS_ZAGAselec <- stepGAICAll.A(ZAGAfixo, scope=list(lower=~1, upper= ~ gender + pb(Age_client) + 
                                                  + pb(age_of_car_M) + pb(Car_power_M) 
                                                + Car_2ndDriver_M 
                                                + metro_code + Policy_PaymentMethodA 
                                                + pb(Client_Seniority)),
                           trace = T, data=dados)
summary(GAMLSS_ZAGAselec)
term.plot(GAMLSS_ZAGAselec, what = "mu")
term.plot(GAMLSS_ZAGAselec, what = "nu")
edfAll(GAMLSS_ZAGAselec) #próximo de 2 pb() desnecessario
GAMLSS_ZAGA <- gamlss(formula = Claims1 ~ pb(Age_client), sigma.formula = ~1,  
                nu.formula = ~pb(Age_client) + pb(Client_Seniority) +  
                  Car_2ndDriver_M + age_of_car_M + Policy_PaymentMethodA,  
                family = ZAGA, data = dados, trace = T, method = RS(100))


#GAMLSS - ZAIG
GAMLSS_ZAIGselec <- stepGAICAll.A(ZAIGfixo, scope=list(lower=~1, upper= ~ gender + pb(Age_client) + 
                                                  + pb(age_of_car_M) + pb(Car_power_M) 
                                                + Car_2ndDriver_M 
                                                + metro_code + Policy_PaymentMethodA 
                                                + pb(Client_Seniority)),
                           trace = 2, data=dados, method = RS(500)) 
summary(GAMLSS_ZAIGselec) 
term.plot(GAMLSS_ZAIGselec, what = "sigma")
term.plot(GAMLSS_ZAIGselec, what = "nu")
edfAll(GAMLSS_ZAIGselec)
GAMLSS_ZAIG <- gamlss(formula = Claims1 ~ Car_2ndDriver_M + Policy_PaymentMethodA,  
                      sigma.formula = ~ pb(Client_Seniority) + pb(Age_client) +  
                        Car_2ndDriver_M + Policy_PaymentMethodA + pb(age_of_car_M) +  
                        pb(Car_power_M), nu.formula = ~pb(Age_client) +  
                        pb(Client_Seniority) + Car_2ndDriver_M + age_of_car_M +  
                        Policy_PaymentMethodA, family = ZAIG, data = dados, trace = T)

#GAMMLSS - ZAGA
GAMMLSS_ZAGA <- gamlss(formula = Claims1 ~ Age_client + re(random=~1|PolID), sigma.formula = ~1,  
                       nu.formula = ~pb(Age_client) + pb(Client_Seniority) +  
                         Car_2ndDriver_M + age_of_car_M + Policy_PaymentMethodA + re(random=~1|PolID),      
                       family = ZAGA, data = dados, trace = T, method = RS(500))

#GAMMLSS - ZAIG
GAMMLSS_ZAIG <- gamlss(formula = Claims1 ~ Car_2ndDriver_M + Policy_PaymentMethodA,  
                       sigma.formula = ~ pb(Client_Seniority) + pb(Age_client) +  
                         Car_2ndDriver_M + Policy_PaymentMethodA + pb(age_of_car_M) +  
                         pb(Car_power_M), nu.formula = ~pb(Age_client) +  
                         pb(Client_Seniority) + Car_2ndDriver_M + age_of_car_M +  
                         Policy_PaymentMethodA + re(random=~1|PolID)
                       , family = ZAIG, data = dados, trace = T,
                       method = RS(100), gd.tol=Inf, mu.start = 1000)

# Total de 8 modelos de 12 idealizados
#--------------------------------------------------------------------
#Resumos 

summary(GLM_ZAGA)
summary(GLM_ZAIG)
summary(GLMM_ZAGA)
summary(GAM_ZAGA)
summary(GAMLSS_ZAGA)
summary(GAMLSS_ZAIG)
summary(GAMMLSS_ZAGA)
summary(GAMMLSS_ZAIG)

#-------------------------------------------------------------------
#Adequacao
par(mfrow=c(4,2))
rqres.plot(GLM_ZAGA, howmany =4, cex=.5, pch=20, ylim.all=0.7, xlim.all = 5, 
           plot.type ="all"); title(main = "GLM_ZAGA")
rqres.plot(GLM_ZAIG, howmany =4, cex=.5, pch=20, ylim.all=0.7, xlim.all = 5, 
           plot.type ="all"); title(main = "GLM_ZAIG")
rqres.plot(GLMM_ZAGA, howmany =4, cex=.5, pch=20, ylim.all=0.7, xlim.all = 5, 
           plot.type ="all"); title(main = "GLMM_ZAGA")
rqres.plot(GAM_ZAGA, howmany =4, cex=.5, pch=20, ylim.all=0.7, xlim.all = 5, 
           plot.type ="all"); title(main = "GAM_ZAGA")
rqres.plot(GAMLSS_ZAGA, howmany =4, cex=.5, pch=20, ylim.all=0.7, xlim.all = 5, 
           plot.type ="all"); title(main = "GAMLSS_ZAGA")
rqres.plot(GAMLSS_ZAIG, howmany =4, cex=.5, pch=20, ylim.all=0.7, xlim.all = 5, 
           plot.type ="all"); title(main = "GAMLSS_ZAIG")
rqres.plot(GAMMLSS_ZAGA, howmany =4, cex=.5, pch=20, ylim.all=0.7, xlim.all = 5, 
           plot.type ="all"); title(main = "GAMMLSS_ZAGA")
rqres.plot(GAMMLSS_ZAIG, howmany =4, cex=.5, pch=20, ylim.all=0.7, xlim.all = 5, 
           plot.type ="all"); title(main = "GAMMLSS_ZAIG")

#-------------------------------------------------------------------
# Capacidade preditiva

# Rearranjando o quinto ano para leitura em Predict
dados_pred <- dados5
observados <- dados_pred$Claims1
dados_pred$Claims1 <- NULL
sum(observados) #perda agregada real

# Obtendo as predicoes
predGLM_ZAGA <- data.frame(predictAll(GLM_ZAGA, newdata = dados_pred, type = "response"))
predGLM_ZAIG <- data.frame(predictAll(GLM_ZAIG, newdata = dados_pred, type = "response"))
predGLMM_ZAGA <- data.frame(predictAll(GLMM_ZAGA, newdata = dados_pred, type = "response"))
predGAM_ZAGA <- data.frame(predictAll(GAM_ZAGA, newdata = dados_pred, type = "response"))
predGAMLSS_ZAGA <- data.frame(predictAll(GAMLSS_ZAGA, newdata = dados_pred, type = "response"))
predGAMLSS_ZAIG <- data.frame(predictAll(GAMLSS_ZAIG, newdata = dados_pred, type = "response"))
predGAMMLSS_ZAGA <- data.frame(predictAll(GAMMLSS_ZAGA, newdata = dados_pred, type = "response"))
predGAMMLSS_ZAIG <- data.frame(predictAll(GAMMLSS_ZAIG, newdata = dados_pred, type = "response"))

# Obtendo o valor esperado (formula das zero ajustadas)
preditoGLM_ZAGA <- (1 - predGLM_ZAGA$nu) * predGLM_ZAGA$mu
preditoGLM_ZAIG <- (1 - predGLM_ZAIG$nu) * predGLM_ZAIG$mu
preditoGLMM_ZAGA <- (1 - predGLMM_ZAGA$nu) * predGLMM_ZAGA$mu
preditoGAM_ZAGA <- (1 - predGAM_ZAGA$nu) * predGAM_ZAGA$mu
preditoGAMLSS_ZAGA <- (1 - predGAMLSS_ZAGA$nu) * predGAMLSS_ZAGA$mu
preditoGAMLSS_ZAIG <- (1 - predGAMLSS_ZAIG$nu) * predGAMLSS_ZAIG$mu
preditoGAMMLSS_ZAGA <- (1 - predGAMMLSS_ZAGA$nu) * predGAMMLSS_ZAGA$mu
preditoGAMMLSS_ZAIG <- (1 - predGAMMLSS_ZAIG$nu) * predGAMMLSS_ZAIG$mu

# Perda agregada predita
sum(preditoGLM_ZAGA)
sum(preditoGLM_ZAIG)
sum(preditoGLMM_ZAGA)
sum(preditoGAM_ZAGA)
sum(preditoGAMLSS_ZAGA)
sum(preditoGAMLSS_ZAIG)
sum(preditoGAMMLSS_ZAGA)
sum(preditoGAMMLSS_ZAIG)

#Acurácia (mbe = viés médio do erro, mae = média do erro absoluto)
#Precisão (rmse = raíz quadrada media do erro)

tdStats(preditoGLM_ZAGA, observados, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGLM_ZAIG, observados, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGLMM_ZAGA, observados, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAM_ZAGA, observados, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMLSS_ZAGA, observados, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMLSS_ZAIG, observados, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMMLSS_ZAGA, observados, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMMLSS_ZAIG, observados, functions = c("mbe", "mae", "rmse"))

#-----------------------------------------------------------------------
# Mais detalhes dos modelos GAMLSS ZAIG

plot(GAMMLSS_ZAIG) #leva em consideração apenas mu

#Avaliação de pb
par(mfrow=c(3,2))

term.plot(GAMMLSS_ZAIG, what = "sigma", ylim = "free", terms = 1, scheme = "lines", se=F,
          xlab = "Fidelidade (anos)", ylab = expression(paste("pb(Fidelidade) em ", sigma,)))
grid(nx = NULL, ny = NA, lty = 3, col = "gray", lwd = 1)   
title(main = "I")

term.plot(GAMMLSS_ZAIG, what = "sigma", ylim = "free", terms = 2, scheme = "lines", se=F,
          xlab = "Idade do cliente (anos)", ylab = expression(paste("pb(Idade do cliente) em ", sigma,)))
grid(nx = NULL, ny = NA, lty = 3, col = "gray", lwd = 1)  
title(main = "II")

term.plot(GAMMLSS_ZAIG, what = "sigma", ylim = "free", terms = 5, scheme = "lines", se=F,
          xlab = "Idade do veículo (anos)", ylab = expression(paste("pb(Idade do veículo) em ", sigma,)))
grid(nx = NULL, ny = NA, lty = 3, col = "gray", lwd = 1)  
title(main = "III")

term.plot(GAMMLSS_ZAIG, what = "sigma", ylim = "free", terms = 6, scheme = "lines", se=F,
          xlab = "Potência (hp)", ylab = expression(paste("pb(Potência) em ", sigma,)))
grid(nx = NULL, ny = NA, lty = 3, col = "gray", lwd = 1)  
title(main = "IV")

term.plot(GAMMLSS_ZAIG, what = "nu", ylim = "free", terms = 1, scheme = "lines", se=F,
          xlab = "Idade do cliente (anos)", ylab = expression(paste("pb(Idade do cliente) em ", nu,)))
grid(nx = NULL, ny = NA, lty = 3, col = "gray", lwd = 1)  
title(main = "V")

term.plot(GAMMLSS_ZAIG, what = "nu", ylim = "free", terms = 2, scheme = "lines", se=F,
          xlab = "Fidelidade (anos)", ylab = expression(paste("pb(Fidelidade) em ", nu,)))
grid(nx = NULL, ny = NA, lty = 3, col = "gray", lwd = 1)  
title(main = "VI")

#Avaliando os efeitos aleatórios
summary(getSmo(GAMMLSS_ZAIG, what = "nu", which = 3))
plot(getSmo(GAMMLSS_ZAIG, what = "nu", which = 3))
ranef <- ranef(getSmo(GAMMLSS_ZAIG, what = "nu", which = 3))$`(Intercept)`
plot(ranef, xlab = "ID da apólice", ylab = "Estimativa do efeito aleatório", main = "")
plot(density(ranef), ylab = "Densidade", xlab = "Estimativa do efeito aleatório", main="")
ks.test(ranef, 'pnorm') #é normal
summary(ranef)
sd(ranef)

#Densidade das predições
par(mfrow=c(1,3))
plot(density(observados, bw=1), main = "Observado no ano 5", ylab = "Densidade", xlab = "Valor do sinistro (euros)")
rug(jitter(observados))
plot(density(preditoGAMLSS_ZAIG, bw=1),  main = "Predito por GAMLSS_ZAIG", ylab = " ", xlab = "Valor do sinistro (euros)")
rug(jitter(preditoGAMLSS_ZAIG))
plot(density(preditoGAMMLSS_ZAIG, bw=1), main = "Predito por GAMMLSS_ZAIG", ylab = " ", xlab = "Valor do sinistro (euros)")
rug(jitter(preditoGAMMLSS_ZAIG))

#Comportamento do modelo onde houve e onde não houve sinistro no ano 5
dados_sim <- dados_oc5
obs_sim <- dados_sim$Claims1
dados_sim$Claims1 <- NULL

predGAMLSS_ZAIG_sim <- data.frame(predictAll(GAMLSS_ZAIG, newdata = dados_sim, type = "response", output = "matrix"))
preditoGAMLSS_ZAIG_sim <- (1 - predGAMLSS_ZAIG_sim$nu) * predGAMLSS_ZAIG_sim$mu
predGAMMLSS_ZAIG_sim <- data.frame(predictAll(GAMMLSS_ZAIG, newdata = dados_sim, type = "response", output = "matrix"))
preditoGAMMLSS_ZAIG_sim <- (1 - predGAMMLSS_ZAIG_sim$nu) * predGAMMLSS_ZAIG_sim$mu

dados_nao <- subset(dados5, Claims1 == 0)
obs_nao <- dados_nao$Claims1
dados_nao$Claims1 <- NULL

predGAMLSS_ZAIG_nao <- data.frame(predictAll(GAMLSS_ZAIG, newdata = dados_nao, type = "response", output = "matrix"))
preditoGAMLSS_ZAIG_nao <- (1 - predGAMLSS_ZAIG_nao$nu) * predGAMLSS_ZAIG_nao$mu
predGAMMLSS_ZAIG_nao <- data.frame(predictAll(GAMMLSS_ZAIG, newdata = dados_nao, type = "response", output = "matrix"))
preditoGAMMLSS_ZAIG_nao <- (1 - predGAMMLSS_ZAIG_nao$nu) * predGAMMLSS_ZAIG_nao$mu

tdStats(preditoGAMLSS_ZAIG_sim, obs_sim, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMMLSS_ZAIG_sim, obs_sim, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMLSS_ZAIG_nao, obs_nao, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMMLSS_ZAIG_nao, obs_nao, functions = c("mbe", "mae", "rmse"))

#-----------------------------------------------------------------------
# Estudando alguns casos particulares

#Gráfico ano 5
par(mfrow=c(1,1))
percentil90 <- quantile(dados_oc5$Claims1, 0.90)
percentil80 <- quantile(dados_oc5$Claims1, 0.80)
plot(obs_sim, xlab="Índice da apólice", ylab="Valor do sinistro (euros)", xaxt="n", pch=20, 
     col = ifelse(obs_sim < percentil80, "black", ifelse(obs_sim > percentil90, "blue", "red")))
abline(percentil90, 0, col="gray", lty=3)
abline(percentil80, 0, col="gray", lty=3)
legend("topright", legend=c("Superior ao percentil 90", "Entre os percentis 80 e 90"),
       col=c("blue", "red"), pch=20, cex=0.8)

#-------------------------------------------------------------------------
#Análise de outliers (acima do percentil 90) no ano 5.
corte90 <- subset(dados5, Claims1 > percentil90) #13 Observações
dados_corte90 <- subset(dados_brutos, PolID %in% corte90$PolID)
plot(dados_corte90$Claims1)
dados90 <- dados_corte90
dados90$Claims1 <- NULL  

mean(corte90$Age_client)
mean(corte90$age_of_car_M)
mean(corte90$Client_Seniority)
tab1(corte90$Car_2ndDriver_M)
tab1(corte90$Policy_PaymentMethodA)

predGAMLSS_ZAIG_90 <- data.frame(predictAll(GAMLSS_ZAIG, newdata = dados90, type = "response", output = "matrix"))
preditoGAMLSS_ZAIG_90 <- (1 - predGAMLSS_ZAIG_90$nu) * predGAMLSS_ZAIG_90$mu
predGAMMLSS_ZAIG_90 <- data.frame(predictAll(GAMMLSS_ZAIG, newdata = dados90, type = "response", output = "matrix"))
preditoGAMMLSS_ZAIG_90 <- (1 - predGAMMLSS_ZAIG_90$nu) * predGAMMLSS_ZAIG_90$mu

dados_corte90$GAMLSS <- preditoGAMLSS_ZAIG_90
dados_corte90$GAMMLSS <- preditoGAMMLSS_ZAIG_90

par(mfrow=c(1,3)) #spaghetti plot
interaction.plot(dados_corte90$year, dados_corte90$PolID, main = "Observado", las=1, col=c(1,2,3,4,6),
                 lty = 1:3, dados_corte90$Claims1, xlab="Ano", ylab="Valor do sinistro (euros)", legend=F)
interaction.plot(dados_corte90$year, dados_corte90$PolID, main = "GAMLSS", las=1, col=c(1,2,3,4,6),
                 lty = 1:3, dados_corte90$GAMLSS, xlab="Ano", ylab="Valor do sinistro (euros)", legend=F)
interaction.plot(dados_corte90$year, dados_corte90$PolID, main = "GAMMLSS", las=1, col=c(1,2,3,4,6),
                 lty = 1:3, dados_corte90$GAMMLSS, xlab="Ano", ylab="Valor do sinistro (euros)", legend=F)

tdStats(preditoGAMLSS_ZAIG_90, corte90$Claims1, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMMLSS_ZAIG_90, corte90$Claims1, functions = c("mbe", "mae", "rmse")) #melhor*

#-------------------------------------------------------------------------
#Análise de entre percentil 80-90 no ano 5.
corte8090 <- subset(dados5, Claims1 >= percentil80 & Claims1 < percentil90) #12 Observações
dados_corte8090 <- subset(dados_brutos, PolID %in% corte8090$PolID)
plot(dados_corte8090$Claims1)
dados8090 <- dados_corte8090
dados8090$Claims1 <- NULL  

mean(corte8090$Age_client)
mean(corte8090$age_of_car_M)
mean(corte8090$Client_Seniority)
tab1(corte8090$Car_2ndDriver_M)
tab1(corte8090$Policy_PaymentMethodA)


predGAMLSS_ZAIG_8090 <- data.frame(predictAll(GAMLSS_ZAIG, newdata = dados8090, type = "response", output = "matrix"))
preditoGAMLSS_ZAIG_8090 <- (1 - predGAMLSS_ZAIG_8090$nu) * predGAMLSS_ZAIG_8090$mu
predGAMMLSS_ZAIG_8090 <- data.frame(predictAll(GAMMLSS_ZAIG, newdata = dados8090, type = "response", output = "matrix"))
preditoGAMMLSS_ZAIG_8090 <- (1 - predGAMMLSS_ZAIG_8090$nu) * predGAMMLSS_ZAIG_8090$mu

dados_corte8090$GAMLSS <- preditoGAMLSS_ZAIG_8090
dados_corte8090$GAMMLSS <- preditoGAMMLSS_ZAIG_8090

par(mfrow=c(1,3)) #spaghetti plot
interaction.plot(dados_corte8090$year, dados_corte8090$PolID, main = "Observado", las=1, col=c(1,2,3,4,6),
                 lty = 1:3, dados_corte8090$Claims1, xlab="Ano", ylab="Valor do sinistro (euros)", legend=F)
interaction.plot(dados_corte8090$year, dados_corte8090$PolID, main = "GAMLSS", las=1, col=c(1,2,3,4,6),
                 lty = 1:3, dados_corte8090$GAMLSS, xlab="Ano", ylab="Valor do sinistro (euros)", legend=F)
interaction.plot(dados_corte8090$year, dados_corte8090$PolID, main = "GAMMLSS", las=1, col=c(1,2,3,4,6),
                 lty = 1:3, dados_corte8090$GAMMLSS, xlab="Ano", ylab="Valor do sinistro (euros)", legend=F)

tdStats(preditoGAMLSS_ZAIG_8090, dados_corte8090$Claims1, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMMLSS_ZAIG_8090, dados_corte8090$Claims1, functions = c("mbe", "mae", "rmse")) #melhor*


#-------------------------------------------------------------------------
#Amostrando 10 apólices que nunca acionaram seguros

dados_zeb <- data.frame(subset(dados_ze, year == 5)$PolID)
vetor_ze <- dados_zeb$subset.dados_ze..year....5..PolID
set.seed(50)
amostra <- sample(vetor_ze, 10, replace = FALSE)
dados0 <- subset(dados_ze, PolID %in% amostra)

mean(dados0$Age_client)
mean(dados0$age_of_car_M)
mean(dados0$Client_Seniority)
tab1(dados0$Car_2ndDriver_M)
tab1(dados0$Policy_PaymentMethodA)

predGAMLSS_ZAIG_0 <- data.frame(predictAll(GAMLSS_ZAIG, newdata = dados0, type = "response"))
preditoGAMLSS_ZAIG_0 <- (1 - predGAMLSS_ZAIG_0$nu) * predGAMLSS_ZAIG_0$mu
predGAMMLSS_ZAIG_0 <- data.frame(predictAll(GAMMLSS_ZAIG, newdata = dados0, type = "response"))
preditoGAMMLSS_ZAIG_0 <- (1 - predGAMMLSS_ZAIG_0$nu) * predGAMMLSS_ZAIG_0$mu

dados0$GAMLSS <- preditoGAMLSS_ZAIG_0
dados0$GAMMLSS <- preditoGAMMLSS_ZAIG_0

par(mfrow=c(1,2)) #spaghetti plot
interaction.plot(dados0$year, dados0$PolID, main = "GAMLSS", las=1, col=c(1,2,3,4,6),
                 lty = 1:3, dados0$GAMLSS, xlab="Ano", ylab="Valor do sinistro (euros)", legend=F)
interaction.plot(dados0$year, dados0$PolID, main = "GAMMLSS", las=1, col=c(1,2,3,4,6),
                 lty = 1:3, dados0$GAMMLSS, xlab="Ano", ylab="Valor do sinistro (euros)", legend=F)

tdStats(preditoGAMLSS_ZAIG_0, dados0$Claims1, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMMLSS_ZAIG_0, dados0$Claims1, functions = c("mbe", "mae", "rmse")) #melhor*

#=====================================================================
#Juntando os dois modelos por credibilidade de buhlmann

getSigmab <- function(obj) # new function
{
  vc <- VarCorr(obj)
  suppressWarnings(storage.mode(vc) <- "numeric")
  vc[1:2,"StdDev"][1]
}

sigma_b <- getSigmab(getSmo(GAMMLSS_ZAIG, what = "nu", which = 3))
sigma <- getSmo(GAMMLSS_ZAIG, what = "nu", which = 3)$sigma


razao <- sigma/sigma_b
zeta <- 5 /(5+razao)

premioconjunto <- (zeta * preditoGAMMLSS_ZAIG) + ((1-zeta) * preditoGAMLSS_ZAIG)
sum(premioconjunto)
plot(premioconjunto)

tdStats(preditoGAMLSS_ZAIG, observados, functions = c("mbe", "mae", "rmse"))
tdStats(preditoGAMMLSS_ZAIG, observados, functions = c("mbe", "mae", "rmse"))
tdStats(premioconjunto, observados, functions = c("mbe", "mae", "rmse"))
