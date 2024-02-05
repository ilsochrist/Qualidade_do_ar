#Instalação de pacotes e carregamento de libraries.
library(dplyr)
if(!require(Amelia)) install.packages("Amelia") 
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(plotly)) install.packages("plotly")
library(plotly)
library(rstatix)
library(lubridate)


airquality$Date <- make_date(1973, airquality$Month, airquality$Day)

head(airquality)

# Renomeando colunas
airquality_Rename <- rename(airquality, Ozônio = Ozone, Radiação = Solar.R, Vento = Wind, Temperatura = Temp, Mês = Month, Dia = Day, Data = Date)
head(airquality_Rename)

##Extraindo o mês por extenso
airquality_Rename$Mês <- format(airquality_Rename$Data, "%B")
head(airquality_Rename)

# Convertendo vento para quilômetros por hora (kph). Sabendo que 1 mph é igual a 1,609 quilômetros por hora (kph).
airquality_Rename$Vento <- airquality_Rename$Vento * 1.609
head(airquality_Rename)

# Convertendo temperatura em F para T
airquality_Rename$Temperatura <- (airquality_Rename$Temperatura -32)/1.8
head(airquality_Rename)

# Formatando arrendondamento de temperatura e Vento
airquality_Rename$Temperatura <- format(round(airquality_Rename$Temperatura, 1), nsmall = 1)
airquality_Rename$Vento <- format(round(airquality_Rename$Vento, 1), nsmall = 1)
head(airquality_Rename)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valores indefinidos)
sapply(airquality_Rename, function(x) sum(is.na(x)))
sapply(airquality_Rename, function(x) sum(is.nan(x)))

# Observando valores NA com missimap
Amelia::missmap(airquality_Rename)

# Verificando a média das variáveis com valores NA
summary(airquality_Rename)

#Substituindo valores NA em Ozônio e Radiação pela Média.
airquality_Rename$Ozônio[is.na(airquality_Rename$Ozônio)] = mean(airquality_Rename$Ozônio,na.rm=TRUE)
airquality_Rename$Radiação[is.na(airquality_Rename$Radiação)] = mean(airquality_Rename$Radiação,na.rm=TRUE)

#Verificando Valores nulos
sapply(airquality_Rename, function(x) sum(is.na(x)))

# Formatando arrendondamento de Ozônio e Radiação
airquality_Rename$Ozônio <- format(round(airquality_Rename$Ozônio, 1), nsmall = 1)
airquality_Rename$Radiação <- format(round(airquality_Rename$Radiação, 1), nsmall = 1)
head(airquality_Rename)

#Verificação da tipagem das variáveis
str(airquality_Rename)

#Alterando o tipo das variáveis vento e temperatura para NUM e data para DATE.
airquality_Rename[1:4] <- lapply(airquality_Rename[1:4], as.numeric)
airquality_Rename$Data <- as.Date(airquality_Rename$Data, "%d-%m-%Y")
str(airquality_Rename)

# Formatando a data no dataset.
airquality_Rename$Data <- format(airquality_Rename$Data, "%d-%m-%Y")
tail(airquality_Rename)

#Verificando os outliers das variáveis com ggplot.

airquality_Rename %>% filter(!is.na(Ozônio)) %>% 
  ggplot(aes(x = " ", y = Ozônio)) + 
  geom_boxplot(fill = "dodgerblue", width = .3, outlier.colour = "purple")

airquality_Rename %>% filter(!is.na(Radiação)) %>% 
  ggplot(aes(x = " ", y = Radiação)) + 
  geom_boxplot(fill = "red", width = .3, outlier.colour = "purple")

airquality_Rename %>% filter(!is.na(Vento)) %>% 
  ggplot(aes(x = " ", y = Vento)) + 
  geom_boxplot(fill = "yellow", width = .3, outlier.colour = "purple")

airquality_Rename %>% filter(!is.na(Temperatura)) %>% 
  ggplot(aes(x = " ", y = Temperatura)) + 
  geom_boxplot(fill = "orange", width = .3, outlier.colour = "purple")

#Explorando o Ploty e sua interatividade.

plot_ly(airquality_Rename, y = airquality_Rename$Ozônio, 
        type = "box") %>%
  layout(title = "BOXPLOT OZÔNIO",
         yaxis = list(title = "Ozônio"))

plot_ly(airquality_Rename, y = airquality_Rename$Radiação, 
        type = "box") %>%
  layout(title = "BOXPLOT RADIAÇÃO",
         yaxis = list(title = "Radiação"))

#Tratando Outliers Vento
airquality_Rename %>% identify_outliers(Vento)
outliers = c(boxplot.stats(airquality_Rename$Vento)$out)
airquality_Rename_sem_outliers <- airquality_Rename[-c(which(airquality_Rename$Vento %in% outliers)),  ]

#Verificando Outliers Vento
airquality_Rename_sem_outliers %>% filter(!is.na(Vento)) %>% 
  ggplot(aes(x = " ", y = Vento)) + 
  geom_boxplot(fill = "yellow", width = .3, outlier.colour = "purple")

#Correlação Linear (Método Spearmen) obtensão do R
cor(airquality_Rename$Temperatura,airquality_Rename$Ozônio, method = "spearman")

#Correlação de Pearson
jmv::reliability(
  data = airquality_Rename,
  vars = vars(Temperatura, Ozônio, Radiação, Vento),
  omegaScale = TRUE,
  corPlot = TRUE,
  alphaItems = TRUE,
  omegaItems = TRUE,
  itemRestCor = TRUE)

#Gráfico de dispersão para Ozônio versus Temperatura.
p <- ggplot(airquality_Rename, aes(Temperatura, Ozônio)) +
  geom_point()
p + geom_smooth(method = "lm")

#Gráfico de dispersão para Ozônio versus Temperatura, regressão linear para cada mês.
ggplot(airquality_Rename, aes(x=Temperatura, y=Ozônio, col=Mês)) +
  geom_point() +
  stat_smooth(method='lm', se = F)

tail(airquality_Rename)

#Histograma Temperatura x Meses
p <- airquality_Rename %>% 
  dplyr::select(Temperatura, Mês) %>% 
  drop_na() %>% 
  ggplot(mapping = aes(x = Temperatura,
                       fill = Mês)) +
  facet_wrap(~Mês)

p + geom_histogram(binwidth = 0.8)

#Análises de Variâncias

#Análise de Variância Temperatura(One Way Anova)
library(jmv)
anovaOneW(
  formula = Temperatura ~ Mês,
  data = airquality_Rename,
  fishers = TRUE,
  desc = TRUE,
  descPlot = TRUE,
  qq = TRUE,
  eqv = TRUE,
  phMethod = "tukey",
  phTest = TRUE)


#Análise de Variância Radiação(One Way Anova)
anovaOneW(
  formula = Radiação ~ Mês,
  data = airquality_Rename,
  fishers = TRUE,
  desc = TRUE,
  descPlot = TRUE,
  qq = TRUE,
  eqv = TRUE,
  phMethod = "tukey",
  phTest = TRUE)

#Análise de Variância Vento(One Way Anova)
library(jmv)
anovaOneW(
  formula = Vento ~ Mês,
  data = airquality_Rename,
  fishers = TRUE,
  desc = TRUE,
  descPlot = TRUE,
  qq = TRUE,
  eqv = TRUE,
  phMethod = "tukey",
  phTest = TRUE)

#Análise de Variância Ozônio(One Way Anova)
anovaOneW(
  formula = Ozônio ~ Mês,
  data = airquality_Rename,
  fishers = TRUE,
  desc = TRUE,
  descPlot = TRUE,
  qq = TRUE,
  eqv = TRUE,
  phMethod = "tukey",
  phTest = TRUE)

