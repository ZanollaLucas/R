# Learning - Lucas Zanolla

#####################
# Projeto/pergunta
#####################
# Qual a probalidade de encontrar um foco de dengue?
# Ou
# O que afeta a presença de focos de dengue?


# Dados

library(DAAG) # pacote
data(dengue) # dados
names(dengue)
summary(dengue) # resumo dos dados


# Regressão binomial e gráfico

# Explicar o conceito de probabilidade utilizando um modelo estat�?stico binomial
mb<-glm(NoYes~humid, data=dengue, family=binomial)
summary(mb) # a humidade foi significativa

# Gráfico utilizando o ggplot2
library(ggplot2) # pacote
ggplot(dengue, aes(x=humid, y=NoYes)) +
  geom_point() +
  geom_smooth(method="glm", method.args = list(family = "binomial")) # gráfico


# Predição de novos focos

# Com base no modelo criado, é poss�?vel predizer a chance de ser 0/1 com base na "humid"
hn<-data.frame(humid=23) # data frame com o novo valor de humid
predict(mb, hn, type="response") # predição do eixo y com base no valor de x

#####################
# Aumentando o modelo
#####################

mb2<-glm(NoYes~humid+temp, data=dengue, family=binomial)
summary(mb2) # ambas variáveis foram significativas

hn2<-data.frame(humid=23, temp=20)

predict(mb2, hn2, type="response")

#####################
# Decision boundaries
#####################

library(lattice)
xyplot(temp ~ humid , data = dengue, groups = NoYes,
       panel=function(...){
         panel.xyplot(...)
         # panel.abline(intercept, slope)
         panel.grid(...)
       })

#####################
# Preparando os dados (treino e teste)
#####################
set.seed(123) # "para contralar a aleatoriedade"
library(caret) # pacote para o ML

# removendo NAs
dengue2 <- na.omit(dengue)

# dividindo o banco de dados para o modelo
dataindex <- createDataPartition(dengue2$NoYes, p= .7, list=FALSE) # 70%

# separando em treino e teste
denguetreino <- dengue2[dataindex,]
dengueteste <- dengue2[-dataindex,]

#####################
# Classificador Dummy
#####################

maiscomum <- sum(denguetreino$NoYes==1) / (dim(denguetreino)[1])
maiscomum

# descobre qual é a classe mais comum
if (maiscomum >= 0.5) {
  print("classeMaisComum <- 1")
  classeMaisComum <- 1
} else {
  print("classeMaisComum <- 0")
  classeMaisComum <- 0
}

taxadeacerto <- sum(dengueteste$NoYes==classeMaisComum) /
  (dim(dengueteste)[1])
taxadeacerto

#####################
# Arvore de decisão
#####################
library(rattle)
library(rpart)
modelohumid<-rpart(as.factor(NoYes)~humid, data=denguetreino)
fancyRpartPlot(modelohumid)

modelotree<-rpart(as.factor(NoYes)~trees, data=denguetreino)
fancyRpartPlot(modelotree)

modelofull<-rpart(as.factor(NoYes)~temp+trees, data=denguetreino)
fancyRpartPlot(modelofull)

#####################
# Modelos de classificação
#####################
# Montando modelos com diferentes algor�tmos

# Link para todos os algor�tmos poss�veis: https://topepo.github.io/caret/available-models.html
modeloML1<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, method="glm")

modeloML2<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, method="ranger")

modeloML3<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, method="avNNet")

#####################
# Matriz de confusão
#####################

# Avaliando o modelo com o banco de teste 
#matriz de confus�o para a avalia��o

pGLM<-predict(modeloML1, dengueteste)
pRANGER<-predict(modeloML2, dengueteste)
pNNET<-predict(modeloML3, dengueteste)

confusionMatrix(factor(pGLM), factor(dengueteste$NoYes)) #0.8605
confusionMatrix(factor(pRANGER), factor(dengueteste$NoYes)) #0.8655
confusionMatrix(factor(pNNET), factor(dengueteste$NoYes)) #0.8723 (melhor accuracy)


#####################
# Curva ROC
#####################
# ROC curve é o mlehor jeito de achar esse "treshold probability"
library(caTools)
library(pROC) # carregando o pacote para as curvas
plot.roc(dengueteste$NoYes, as.numeric(pRANGER), print.auc=TRUE)
plot.roc(dengueteste$NoYes, as.numeric(pGLM), print.auc=TRUE)
plot.roc(dengueteste$NoYes, as.numeric(pNNET), print.auc=TRUE)


#####################
# Importância das variáveis
#####################

modeloML3<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, method="avNNet",
                 importance="impurity") # adicionando o argumento "importance"
plot(varImp(modeloML3, scale = FALSE))
varImp(modeloML3, scale = FALSE)


# O que afeta a presença de focos de dengue? A variável humidade foi a mais importante no nosso modelo.


