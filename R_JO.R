install.packages("plyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")

library(dplyr)
library(plyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

#------------------------------------------------------------------------------#
# Lecture du dataset et stockage dans 2 variables globales
JO<- read.table(file = 'dataset/summer.csv', sep=',', header=TRUE)
dict <- read.table(file = 'dataset/dictionary.csv', sep=',', header=TRUE)

#------------------------------------------------------------------------------#
# Proportion des athlètes femmes et hommes par année
JO %>% 
    group_by(Year,Gender) %>%
    count(Gender) %>%
    ggplot( aes(x=Year, y=n)) + geom_col(aes(fill= Gender)) 

#------------------------------------------------------------------------------#
# Est-ce-que des personnes portant le même prénom que nous ont gagné ?
Athletes=unlist(strsplit(JO$Athlete, ", ")) # Names and surnames into a list
names=Athletes[seq(2,length(Athletes), 2)] # Only names
'Alfred' %in% names # => TRUE
'Auriane' %in% names # => FALSE
'Leonie' %in% names # => FALSE
'Gilles$' %in% names # => FALSE
#Attention une personne s'appelle 'gillespi',le dollar marque la fin du mot

#------------------------------------------------------------------------------#
# Le prénom qui a le plus gagné de médailles
JO %>%
  group_by(Athlete)%>%
  summarize(Names= unlist(strsplit(Athlete , ", "))[2]) %>%
  na.omit(Names) %>%
  count(Names) %>%
  arrange(-n)

#------------------------------------------------------------------------------#
# Remplacer URSS par la Russie afin de pouvoir comparer aux fils des années
JO[JO == "URS"] <- "RUS"

# Nombre de médailles gagnés par pays  (toutes années confonfues)
JO %>%
  group_by(Country) %>%
  count(Country)%>%
  arrange(-n) %>%
  head(10)%>%
  ggplot( aes(x=Country, y=n)) + geom_col(aes(fill= n)) 

#------------------------------------------------------------------------------#
# Nombre d'épreuves par type de sports en 2012 et 1896 pour comparer
JO %>%
  filter(Year == 2012)%>%
  summarise(Sport,Event)%>%
  unique()%>% 
  count(Sport) -> Sport_2012
JO %>%
  filter(Year == 1896)%>%
  summarise(Sport,Event)%>%
  unique()%>% 
  count(Sport) -> Sport_1896
Sport <- merge(Sport_1896,Sport_2012,by="Sport",all=TRUE)
Sport <- rename(Sport,c('1896'='n.x','2012'='n.y'))
Sport[is.na(Sport)] <- 0 
Sport %>%
  pivot_longer(c('1896','2012'),names_to="Year",values_to = "N") %>%
  ggplot( aes(x=Sport, y=N)) + geom_col(aes(fill= Year)) +
  coord_flip()+
  labs(title="Number of events by Sport",caption="Data of 1896 and 2012",x="Sport",y="Number of Events") 

#------------------------------------------------------------------------------#  
# Evolution du nombre de pays participant aux JO
JO %>%
  summarize(Year,Country) %>%
  unique() %>% 
  count(Year)%>%
  ggplot(aes(x = Year, y = n, group = 1)) + geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")

#------------------------------------------------------------------------------#
# Les pays riches ont-ils le plus de champions ?

dict %>% mutate(rankGDP=dense_rank(desc(-GDP.per.Capita)))

JO %>% 
  group_by(Country) %>%
  filter(Year == 2012)%>%
  count(Country)%>%
  rename(Code=Country, Medals=n)%>%
  merge(dict, by=c('Code'), all.x = TRUE, all.y=TRUE)%>%
  select(-c(Country, Population))%>%
  na.omit()%>%
  mutate(rankGDP=dense_rank(-GDP.per.Capita),rankMedals=dense_rank(-Medals), rank = dense_rank(rankGDP + rankMedals )) %>%
  arrange(rank)%>% 
  head(20)%>%
  ggplot( aes(x=Code, y=rank))  +
  geom_col()+coord_flip()


#------------------------------------------------------------------------------#
# Evolution du nombre d'épreuves par type de sports en 2012 et 1896 pour comparer
# JO %>%
#   group_by(Sport, Discipline) %>%
#   filter(Year == 1896) %>%
#   count(Sport) #%>%
#   #ggplot(aes(x = "", y=n, fill = Sport)) +
#   #geom_bar(width = 1, stat = "identity", color = "white") #+ 
#   #coord_polar("y", start = 0) +
#   #geom_text(aes(y = n, label = n), color = "white")+
#   #theme_void()

