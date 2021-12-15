#------------------------------------------------------------------------------#
#                  Les Jeux Olympiques depuis 1896                             #
#                  Léonis GALLOIS & Auriane LARTIGUE                           #
#                              5 SDBD A2                                       #
#------------------------------------------------------------------------------#

# Installation des packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")

library(dplyr)
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
    ggplot( aes(x=Year, y=n)) + geom_col(aes(fill= Gender)) +
    labs(y='Number of athletes')

#------------------------------------------------------------------------------#
# Est-ce-que des personnes portant le même prénom que nous ont déjà gagné ?
Athletes=unlist(strsplit(JO$Athlete, ", ")) # Noms et Prénoms dans une liste
names=Athletes[seq(2,length(Athletes), 2)] # On conserve les prénoms
'Alfred' %in% names # => TRUE
'Auriane' %in% names # => FALSE
'Leonie' %in% names # => FALSE
'Gilles$' %in% names # => FALSE
#Attention une personne s'appelle 'gillespi',le dollar marque la fin du mot

#------------------------------------------------------------------------------#
# Le prénom qui a le plus gagné de médailles
# Hommes
JO %>%
  group_by(Athlete)%>%
  filter(Gender == "Men")%>%
  summarize(Names= unlist(strsplit(Athlete , ", "))[2]) %>%
  na.omit(Names) %>%
  count(Names) %>%
  arrange(-n) %>% 
  head(5) -> Men_names
# Femmes
JO %>%
  group_by(Athlete)%>%
  filter(Gender == "Women")%>%
  summarize(Names= unlist(strsplit(Athlete , ", "))[2]) %>%
  na.omit(Names) %>%
  count(Names) %>%
  arrange(-n) %>% 
  head(5) -> Women_names

#------------------------------------------------------------------------------#
# Remplacer URSS par la Russie afin de pouvoir comparer aux fils des années
JO[JO == "URS"] <- "RUS"

# Nombre de médailles gagnées par pays  (toutes années confondues)
JO %>%
  group_by(Country) %>%
  count(Country)%>%
  arrange(-n) %>%
  head(10)%>%
  ggplot( aes(x=Country, y=n)) + geom_col(aes(fill= n))+
  labs(y="Number of medals") 

# La Chine n'est arrivé qu'en 1984
# Evolution Chine vs France 
JO%>%
  group_by(Country)%>%
  filter(Year>= 1984 & (Country=='FRA'|Country=='CHN'))%>%
  summarize(total=sum(Country=='FRA')+sum(Country=='CHN'))
# FR: 482
# CHN: 807

#------------------------------------------------------------------------------#
# Nombre d'épreuves par type de sports en 2012 et 1896 
# Attention à ne pas compter les sports mixtes
# 2012
JO %>%
  filter(Year == 2012)%>%
  mutate(Gender = ifelse(grepl("Mixed", Event , fixed = TRUE), "Mixed", Gender))%>%
  summarise(Sport,Event,Gender)%>%
  unique()%>% 
  count(Sport) -> Sport_2012
# 1896
JO %>%
  filter(Year == 1896) %>%
  summarise(Sport,Event,Gender)%>%
  unique()%>% 
  count(Sport) -> Sport_1896
# Merge des deux dataframes dans Sport
Sport <- merge(Sport_1896,Sport_2012,by="Sport",all=TRUE)
Sport <- rename(Sport,c('1896'='n.x','2012'='n.y'))
Sport[is.na(Sport)] <- 0 
Sport %>%
  pivot_longer(c('1896','2012'),names_to="Year",values_to = "N") %>%
  ggplot( aes(x=Sport, y=N)) + geom_col(aes(fill= Year)) +
  coord_flip()+
  labs(title="Number of events by Sport",x="Sport",y="Number of Events") 

#------------------------------------------------------------------------------#  
# Evolution du nombre de pays participant aux JO
JO %>%
  summarize(Year,Country) %>%
  unique() %>% 
  count(Year)%>%
  ggplot(aes(x = Year, y = n, group = 1)) + geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")+labs(y="Number of countries")

#------------------------------------------------------------------------------#
# Les pays riches ont-ils le plus de champions ?
JO %>% 
  group_by(Country) %>%
  filter(Year == 2012)%>%
  count(Country)%>%
  rename(Code=Country, Medals=n)%>%
  merge(dict, by=c('Code'), all.x = TRUE, all.y=TRUE)%>%
  select(-c(Country, Population))%>%
  na.omit()%>%
  ggplot( aes(x=GDP.per.Capita, y=Medals))  +
  geom_point() +
  geom_smooth(method=lm,se=FALSE)+
  labs(x="GDP per capita in 2012", y="Number of medals")

#------------------------------------------------------------------------------#
# En tant que français, dans quelle discipline a t-on le plus de chance d'exceller ?
JO%>%
  group_by(Country, Event)%>%
#  filter(Year=='2012')%>%
  filter(Country=='FRA')%>%
  filter(Gender=='Men')%>%
  count(Event)%>%
  arrange(-n)%>%
  head(10)%>%
  ggplot(aes(x="", y=n, fill=Event))+ geom_bar(width=1, stat = 'identity',color = 'white')+coord_polar('y', start=0)+
  geom_text(aes( label=n), position=position_stack(vjust = 0.5))+theme_void()+labs(title="Number of French men athletes by sport event") 

JO%>%
  group_by(Country, Event)%>%
  #  filter(Year=='2012')%>%
  filter(Country=='FRA')%>%
  filter(Gender=='Women')%>%
  count(Event)%>%
  arrange(-n)%>%
  head(10)%>%
  ggplot(aes(x="", y=n, fill=Event))+ geom_bar(width=1, stat = 'identity',color = 'white')+coord_polar('y', start=0)+
  geom_text(aes( label=n), position=position_stack(vjust = 0.5))+theme_void() +labs(title="Number of French women athletes by sport event") 
#------------------------------------------------------------------------------#