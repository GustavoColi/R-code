# (I) abrir questionario LAPOP 2014 e escolher as variaveis com as quais vai trabalhar
      
      #EXEMPLO COM 2 VAR QUAL: sexo (sex) e felicidade (ls3)
      #EXEMPLO COM 2 VAR QUAN (como nao temos, forcamos ordinais com mais de 6 categorias - as.numeric)

# (II) formular uma hipotese de pesquisa e enunciar a hipotese nula
   
 ## DUAS VAR QUAL
    # H1 = sexo influencia o nivel de felicidade de uma pessoa, sendo que os homens tendem a ser mais felizes do que as mulheres.
    # H0 = sexo e nivel de felicidade não estão associados.

 ## DUAS VAR QUAL
    # H1 = escolaridade ajuda a explicar o nivel de rendimento: quanto maior a escolaridade de uma pessoa, maior sera sua renda.
    # H0 = escolaridade e nivel de rendimento não estão correlacionados


### (III) Abrir a base de dados LAPOP_2014.sav

#baixar e carregar biblioteca foreign
install.packages("foreign")
library(foreign)

# (IV) criar objeto que recebera banco de dados (substituir XXX pelo endereco do diretorio de trabalho)

getwd() #verifica o diretorio de trabalho
lapop2014<-read.spss("XXX/LAPOP_2014.sav", 
                     use.value.labels=TRUE, to.data.frame=TRUE)

options(scipen=999) #desativa notacao cientifica

# (V) teste de hipotese (vars QUAL)
install.packages("vcd")
library(vcd)

#verificar nivel de mensuracao var
class(lapop2014$sex)
class(lapop2014$ls3)

#realizar teste
teste<-xtabs(~sex+ls3, data=lapop2014)
prop.table(teste,1)*100
assocstats(teste)

#interprete o resultado: descreva a tabela de contingencia, interprete o nivel de significancia do teste
#para decidir se rejeita ou nao a H0, e interprete a forca da associacao a partir do coeficiente de contingencia.

# (V) teste de hipotese (vars QUAN)

#verificar nivel de mensuracao var e caso necessario alterar

#escoalridade ed2
class(lapop2014$ed2)
table(lapop2014$ed2)
lapop2014$ed2 = as.numeric(lapop2014$ed2)
summary(lapop2014$ed2)

#nivel de rendimento q10g
class(lapop2014$q10g)
table(lapop2014$q10g)
lapop2014$q10g = as.numeric(lapop2014$q10g)
summary(lapop2014$q10g)

cor.test(lapop2014$ed2, lapop2014$q10g)

#interprete o resultado: descreva o nivel de significancia do teste para decidir se rejeita ou nao a H0, 
#e interprete a forca da correlacao a partir do coeficiente. 


