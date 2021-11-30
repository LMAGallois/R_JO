library(tidyverse)
library(ggplot2)
JO<- read.table(file = 'dataset/summer.csv', sep=',', header=TRUE)
dict <- read.table(file = 'dataset/dictionary.csv', sep=',', header=TRUE)

#proportion des athlètes femmes et hommes par année
JO %>% 
    group_by(Year,Gender) %>%
    count(Gender) %>%
    ggplot( aes(x=Year, y=n)) + geom_col(aes(fill= Gender)) 

#Est-ce que l'on a gagné ?

names=unlist(strsplit(JO$Athlete, ", "))
surnames=names[seq(2,length(names), 2)]
'Alfred' %in% surnames
'Auriane' %in% surnames
'Leonie' %in% surnames
#gilles et pas gillespi
#le dollar marque la fin du mot
'Gilles$' %in% surnames

#le prénom qui a le plus gagné de médailles
names=unlist(strsplit(JO$Athlete, ", "))
surnames=names[seq(2,length(names), 2)]

JO %>%
  group_by(Athlete, Medal)%>%
  summarize(Surname= unlist(strsplit(Athlete , ", "))[2])%>%
  count(Surname) %>%
  arrange() 
  