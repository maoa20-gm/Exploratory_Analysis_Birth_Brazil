#--------------------------------------------------------------------#
# Dependencies                                                       #
#--------------------------------------------------------------------#

library(renv)
library(tidyverse)
library(janitor)
library(psych)
library(corrplot)


#--------------------------------------------------------------------#
# Reading                                                            #
#--------------------------------------------------------------------#

birth <- read.csv("Datasets/PE_TODOS2.csv")


#--------------------------------------------------------------------#
#                 Data_Wrangling                                     #
#--------------------------------------------------------------------#

# Jose temos que mudar algumas variaveis que sao numericas mas sao categoricas
# e numericas isso é mudar para o tipo categorico por exemplo a variavel ano.

birth %>%
  clean_names() -> birth # Eu coloquei os nomes em minuscula, nao gosto as maisculas

cols.fact <- c("ano_nasc", "estcivmae" , "escmae" , "racacor")  

birth[cols.fact] <- sapply(birth[cols.fact], as.character)

birth %>% 
  mutate_if(is.character, as.factor) -> birth


#--------------------------------------------------------------------#
#                 Analise por quantidade de anos                     #
#--------------------------------------------------------------------#
  
cantidad <- birth %>% 
  group_by(data_nasc,ano_nasc) %>% 
  count(data_nasc,sort = F) 
  

ggplot(cantidad, aes(x = ano_nasc, y = n, color = ano_nasc)) +
  geom_boxplot() +
  labs( x = "Anos",
       y = "Numero de nascidos vivos")+
  geom_jitter() +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(cantidad, aes(color = ano_nasc)) +
  geom_boxplot(x = escmae, ) +
  labs( x = "Anos",
        y = "Numero de nascidos vivos")+
  geom_jitter() +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  theme(legend.position = "none")


#--------------------------------------------------------------------#
#                 Analise por escolaridade                           #
#--------------------------------------------------------------------#

escolaridade <- birth %>% 
  group_by(data_nasc,ano_nasc, escmae) %>% 
  count(data_nasc,sort = F)

ggplot(cantidad, aes(x = escmae, y = n, color = escmae)) +
  geom_boxplot() +
  labs( x = "Escolaridade da mae",
        y = "Numero de nascidos vivos")+
  scale_color_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  theme(legend.position = "none")


#--------------------------------------------------------------------#
#                 Analise por estado civil                           #
#--------------------------------------------------------------------#

estado <- birth %>% 
  group_by(data_nasc,ano_nasc, estcivmae) %>% 
  count(data_nasc,sort = F)

ggplot(cantidad, aes(x = estcivmae, y = n, color = estcivmae)) +
  geom_boxplot() +
  labs( x = "Estado civil da mae",
        y = "Numero de nascidos vivos")+
  scale_color_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  theme(legend.position = "none")


#--------------------------------------------------------------------#
#                 Analise por raca da mae                            #
#--------------------------------------------------------------------#

raca <- birth %>% 
  group_by(data_nasc,ano_nasc, racacor) %>% 
  count(data_nasc,sort = F)

ggplot(cantidad, aes(x = racacor, y = n, color = racacor)) +
  geom_boxplot() +
  labs( x = "Estado civil da mae",
        y = "Numero de nascidos vivos")+
  scale_color_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  theme(legend.position = "none")

#--------------------------------------------------------------------#
#                 Correlacion                                        #
#--------------------------------------------------------------------#

numeric_variables <- birth[ , purrr::map_lgl(birth, is.numeric)]
numeric_variables <- numeric_variables[1:9]

corr <- abs(cor(numeric_variables))

colors <- dmat.color(corr)
order <- order.single(corr)

birth %>% 
  mutate(classe = as.character(classe)) %>% 
  mutate(classe = as.factor(classe)) -> birth
  

corrplot(cor(numeric_variables),
         method = "circle",       
         order = "hclust",         
         hclust.method = "ward.D",         
         rect.col = 3,            
         rect.lwd = 3,
         addCoef.col = "white")  

install.packages('scales')
library(scales)
### Pruebas

corrplot(cor(numeric_variables),
         method = "circle",       
         order = "hclust",         
         hclust.method = "ward.D",
         addrect = 2,
         rect.col = 2,            
         rect.lwd = 2,
         addCoef.col = "white")
