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
         addCoef.col = "black")  



### Pruebas

numerica <- birth %>%
  select(qtdfilvivo:qtdpartces)

pairs.panels(x = numerica,
             ellipses = FALSE,
             lm =T , method = "spearman")
  
ggplot(birth, aes(x = idademae))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(fill = "#FF6666", alpha = 0.2)+
  scale_x_continuous(limits = c(10,46),breaks = seq(0,50, 2))+
  labs(x = "Idade da mae")+
  theme_classic()

birth %>% 
  gather(qtdfilvivo:qtdpartces, key = "Diagnostico", value = "value") %>% 
  mutate(Diagnostico = as.factor(Diagnostico))-> resp

ggplot(resp, aes(x = reorder(Diagnostico, value), y = value, fill = Diagnostico)) +
  geom_boxplot(show.legend = FALSE)+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  labs(y = "valores")


ggplot(unbalanced, mapping = aes(x = classe, y= n, fill= classe))+
  geom_col(show.legend = F)+ 
  scale_y_continuous(breaks = seq(0,600000, 100000),labels = scales::comma)+
  scale_x_discrete("classe", labels= c("Não prematuro", "Prematuro"))+
  geom_text(aes(label = n), vjust = -0.5)+
  labs(y = "contagem")+
  theme_classic()+
  theme(axis.title.x = element_blank())
