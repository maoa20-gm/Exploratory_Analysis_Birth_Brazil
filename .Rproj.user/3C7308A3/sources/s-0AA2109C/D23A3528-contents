#--------------------------------------------------------------------#
# Dependencies                                                       #
#--------------------------------------------------------------------#

library(tidyverse)
library(janitor)

#--------------------------------------------------------------------#
# Reading                                                            #
#--------------------------------------------------------------------#

birth <- read.csv("Datasets/PE_TODOS.csv")


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
  mutate(key = c(1:492569)) %>% 
  group_by(key, ano) 


