# ------- PROJETO FANTASMA -------

## 1. Carregando o banco de dados e os pacotes ----

continentes <-readxl::read_xlsx("C:\\Users\\Ana Luisa\\Downloads\\RStudio\\DocumentosR\\Continentes.xlsx")
worldcups <-readxl::read_xlsx("C:\\Users\\Ana Luisa\\Downloads\\RStudio\\DocumentosR\\World_Cups.xlsx")
worldcups_audiencia <-readxl::read_xlsx("C:\\Users\\Ana Luisa\\Downloads\\RStudio\\DocumentosR\\World_Cups_Audiencia.xlsx")
library(tidyverse)
library(naniar)
library(goeveg)
        
## 2. Setando o tema da ESTAT ----

cores_estat <- c('#A11D21','#663333','#FF6600','#CC9900','#CC9966','
#999966','#006606','#008091','#003366','#041835','#666666')
theme_estat <- function(...) {
  theme <- ggplot2:: theme_bw() +
    ggplot2:: theme(
      axis.title.y = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.title.x = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.text = ggplot2:: element_text(colour = "black", size
                                         = 9.5),
      panel.border = ggplot2:: element_blank (),
      axis.line = ggplot2:: element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme ,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

## 3. Análise 1: Total de público presente por ano de copa ----
### 3.1 Organizando o banco de dados ----

# Selecionei as variáveis data e audiência.
ano_audiencia <- worldcups %>% 
  select(Datetime, Attendance) 

# Removi os horários, dias e meses da variável respectivamente.
ano_audiencia$Datetime = ano_audiencia$Datetime %>%
  str_replace_all(" - 11:00| - 11:15| - 11:30| - 11:45| - 12:00| - 12:15| - 12:30| - 12:45| - 12:50| - 13:00
                  | - 13:15| - 13:30| - 13:45| - 14:00| - 14:15| - 14:30| - 14:45| - 14:50| - 15:00| - 15:10
                  | - 15:15| - 15:30| - 15:40| - 15:45| - 16:00| - 16:15| - 16:30| - 16:45| - 16:50| - 17:00
                  | - 17:10| - 17:15| - 17:30| - 17:45| - 17:50| - 18:00| - 18:10| - 18:15| - 18:30| - 18:45
                  | - 19:00| - 19:15| - 19:30| - 19:45| - 20:00| - 20:15| - 20:30| - 20:45| - 21:00| - 21:15
                  | - 21:30| - 21:45| - 22:00| - 22:15| - 22:30| - 22:45| - 23:00", "") %>%
  str_replace_all("01 |02 |03 |04 |05 |06 |07 |08 |09 |10 |11 |12 |13 |14 |15 |
                  16 |17 |18 |19 |20 |21 |22 |23 |24 |25 |26 |27 |28 |29 |30 |31 ", "") %>%
  str_replace_all("May |Jun |Jul |June |July ", "") %>%
  as.character()

# Substitui os NA's em zeros.
ano_audiencia[is.na(ano_audiencia)] <- 0

# Fiz um backup do data frame ano_audiencia
ano_audiencia2 <- data.frame(ano_audiencia)

# Audiência total por ano
ano_audiencia <- ano_audiencia %>% 
      group_by(Datetime) %>% 
      summarise(Audiencia = sum(Attendance))

### 3.2 Gráfico ----

ggplot(ano_audiencia) +
  aes(x=Datetime, y=Audiencia, group=1) +
  geom_line(size=1, colour="#A11D21") + 
  geom_point(colour="#A11D21", size=2) +
  labs(x="Ano", y="Audiencia") +
  scale_y_continuous(breaks=seq(0,4200000, by= 600000)) +
  theme_estat() +
  theme(axis.text.x = element_text(size = 7))
ggsave("ano_audiencia.pdf", width =158, height =93, units ="mm"
)

### 3.3 Medidas descritivas ---

summary(ano_audiencia$Audiencia)
var(ano_audiencia$Audiencia)
sd(ano_audiencia$Audiencia)

## 4. Análise 2: Análise da dispersão do número de gols marcados pelo time da casa por partida para cada continente ----
### 4.1 Organizando os bancos de dados ----

# Separando as variáveis desejadas de ambos os bancos de dados:
## continentes - FIFA e Continent;

FIFA_continentes <- continentes %>%
  select(FIFA, Continent)

### Mudando o nome da coluna "FIFA" por "Home Team Initials".

colnames(FIFA_continentes)[1] <- 'Home Team Initials'

## worldcups - Home Team Initials, Home Team Goals.
FIFA_gols <- worldcups %>%
  select(`Home Team Initials`, `Home Team Goals`)

### Agrupando a quantidade de gols por país.

FIFA_gols_totais <- FIFA_gols %>% 
  group_by(`Home Team Initials`) %>% 
  summarise('Total_Gols' = sum(`Home Team Goals`))

### Nova coluna com a frequência de cada país, ou seja, quantas partidas cada um disputou.

FIFA_partidas_totais <- FIFA_gols%>%
  group_by(`Home Team Initials`) %>%
  count()

FIFA_totais <- FIFA_gols_totais %>%
  left_join(FIFA_partidas_totais, by= c("Home Team Initials"))

#### Mudando o nome da coluna 3 de "n" para "Total de Partidas".

colnames(FIFA_totais)[3] <- 'Total_Partidas'

# Juntando os bancos de dados criados anteriormente.

FIFA_totais_continente <- FIFA_totais %>%
  inner_join(FIFA_continentes, by= "Home Team Initials")

## Removendo um elemento (ENG) que foi repetido na linha 23.

FIFA_totais_continente <- FIFA_totais_continente[-23, ]

## Renomeando os continentes para uma melhor visualização.

FIFA_totais_continente$Continent = FIFA_totais_continente$Continent %>%
  str_replace_all("AF", "África") %>%
  as.character() %>%
  str_replace_all("AS", "Ásia") %>%
  str_replace_all("EU", "Europa") %>%
  str_replace_all("NA", "América do Norte") %>%
  str_replace_all("OC", "Oceania") %>%
  str_replace_all("SA", "América do Sul")

### 4.2 Gráfico ----

ggplot(FIFA_totais_continente) +
  aes(x = Total_Partidas, y = Total_Gols) +
  geom_point(colour ="#A11D21", size =3) +
  labs(x ="Partidas disputadas",
       y ="Gols marcados") +
  facet_wrap(~Continent) +
  theme_estat()
ggsave("Totais_continente.pdf", width =158, height =93, units ="mm"
)

### 4.3 Medidas descritivas ----

# Medidas descritivas gerais:

FIFA_totais_continente %>%
  group_by(Continent) %>%
  summary(Total_Gols)

# Medidas descritivas do total de gols por continentes

filter(FIFA_totais_continente, Continent == "América do Sul") %>%
  summary()
tapply(FIFA_totais_continente$Total_Gols, 
       FIFA_totais_continente$Continent, summary)
tapply(FIFA_totais_continente$Total_Gols, 
       FIFA_totais_continente$Continent, sd)

# Frequência de cada continente.

FIFA_totais_continente%>%
  group_by(`Continent`) %>%
  count()

# Quantidade de gols por continente.

FIFA_totais_continente%>%
  group_by(`Continent`) %>%
  summarise(sum(Total_Gols))

## 5. Análise 3:  Relação entre número de partidas por etapa (fase de grupos x eliminatórias) e turno da partida (diurno ou noturno) ----

### 5.1 Organizando o banco de dados ----

# Selecionando as variáveis de interesse.
etapa_turno <- worldcups %>%
  dplyr::select(Datetime, Stage)

# Verificando quantos tipos há na variável Stage.

distinct(etapa_turno, Stage)

# Organizando a variável Datetime - Removi os anos, os dias e os meses respectivamente.

etapa_turno$Datetime = etapa_turno$Datetime %>%
  str_replace_all("1930 - |1934 - |1938 - |1950 - |1954 - |1958 - |1962 - |1966 - |1970 - |1974 - |1978 - |1982 - |1986 - |1990 - |1994 - |1998 - |2002 - |2006 - |2010 - |2014 - ", "") %>%
  str_replace_all("01 |02 |03 |04 |05 |06 |07 |08 |09 |10 |11 |12 |13 |14 |15 |16 |17 |18 |19 |20 |21 |22 |23 |24 |25 |26 |27 |28 |29 |30 |31 ", "") %>%
  str_replace_all("May |Jun |Jul |June |July ", "") %>%
  as.character()

# Rescrevendo os horários pelos turnos que ocorreram, diurno ou noturno, considerando que a noite começa às 18h.

etapa_turno$Datetime = etapa_turno$Datetime %>%
  str_replace_all("11:00|11:15|11:30|11:45|12:00|12:15|12:30|12:45|12:50|13:00|
                  |13:15|13:30|13:45|14:00|14:15|14:30|14:45|14:50|15:00|15:10|
                  |15:15|15:30|15:40|15:45|16:00|16:15|16:30|16:45|16:50|17:00|
                  |17:10|17:15|17:30|17:45|17:50", "Diurno") %>%
  str_replace_all("18:00|18:10|18:15|18:30|18:45|19:00|19:15|19:30|19:45|20:00|
                  |20:15|20:30|20:45|21:00|21:15|21:30|21:45|22:00|22:15|22:30|
                  |22:45|23:00", "Noturno")

# Rescrevendo as etapas pelas fases de grupo e fases eliminatórias.

etapa_turno$Stage = etapa_turno$Stage %>%
  str_replace_all("Group 1|Group 2|Group 3|Group 4|Group 5|Group 6|
                  |Group A|Group B|Group C|Group D|Group E|Group F|Group G|Group H", "Fase de grupos")%>%
  str_replace_all("Semi-finals|Final|Preliminary round|Quarter-finals|Match for third place|
                  |First round|Round of 16|Third place|Play-off for third place", "Fase eliminatória")

### 5.2 Gráfico ----
# Preparação para o gráfico de colunas bivariado.

etapa_turno_freq <-  etapa_turno %>%
  mutate(Stage = case_when(Stage %>% str_detect("Fase de grupos") ~ "Fase de grupos", 
                           Stage %>% str_detect("Fase eliminatória") ~ "Fase eliminatória")) %>%
  group_by(Stage, Datetime) %>%
  summarise(freq = n()) 

etapa_turno_freq$freq_relativa <- c("0.6062","0.3938","0.6683","0.3317")
etapa_turno_freq$freq_relativa <- as.numeric(etapa_turno_freq$freq_relativa)

etapa_turno_freq$freq_relativa <- etapa_turno_freq$freq_relativa * 100

porcentagens <-  str_c(etapa_turno_freq$freq_relativa,"%") %>% 
  str_replace("\\.",",")

legendas <-  str_squish(str_c(etapa_turno_freq$freq," (", porcentagens ,")"))

# Gráfico de Colunas em si.

ggplot(etapa_turno_freq) +
  aes(x = fct_reorder(Stage, freq, .desc = T), y = freq,fill = Datetime, label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding =0)) +
  geom_text(position = position_dodge(width = .9),vjust = -0.5, hjust =0.5,size =3) +
  labs(x ="Etapas", y ="Frequência") +
  theme_estat() +
  ylim(0, 400) +
  scale_fill_manual(values = c("#A11D21", "#003366"), name = "Turno")
ggsave("etapa_turno.pdf", width =158, height =93, units ="mm"
)

## 6. Análise 4: Relação entre público total da copa e número de gols marcados ----

### 6.1 Organizando o banco de dados ----

audiencia_gols <- worldcups_audiencia %>%
  dplyr::select(Year, GoalsScored, Attendance)

### 6.2 Gráfico ----

options(scipen = 999)

ggplot(audiencia_gols) +
  aes(x = Attendance, y = GoalsScored) +
  geom_point(colour ="#A11D21", size =3) +
  labs(x ="Público",
       y ="Gols marcados") +
  ylim(0,200) +
  xlim(0, 4000000) +
  theme_estat()
ggsave("Publico_gols.pdf", width =158, height =93, units ="mm"
)

cor(audiencia_gols$Attendance, audiencia_gols$GoalsScored, method = "pearson")

## 7. Análise 5: Top 10 países com maior soma de público de todas as partidas de todas as copas ----

### 7.1 Organizando o banco de dados ----

# Verificando se ambas as colunas 'Home_Team_Name' e 'Away_Team_Name' têm a mesma quantidade de países.

worldcups2 <- data.frame(worldcups)
colnames(worldcups2)[5] <- 'Home_Team_Name'
colnames(worldcups2)[8] <- 'Away_Team_Name'

teste <- worldcups2 %>%
  dplyr::select(Home_Team_Name)
distinct(teste)
teste1 <- worldcups2 %>%
  dplyr::select(Away_Team_Name)
distinct(teste)

colnames(teste)[1] <- 'Team_name'
colnames(teste1)[1] <- 'Team_name'

teste <- teste %>%
  group_by(Team_name) %>%
  count()

teste <- teste[,-2]

teste1 <- teste1 %>%
  group_by(Team_name) %>%
  count()
teste1 <- teste1[,-2]

teste2 = merge(x = teste, y = teste1, by = "Team_name",
               all = TRUE)

# Selecionando os bancos de dados de fato.

home_team <- worldcups2 %>%
  dplyr::select(Home_Team_Name, Attendance)

away_team <- worldcups2 %>%
  dplyr::select(Away_Team_Name, Attendance)

home_team <- home_team %>%
  group_by(Home_Team_Name) %>%
  summarise(Publico = sum(Attendance))

away_team <- away_team %>%
  group_by(Away_Team_Name) %>%
  summarise(n = sum(Attendance, na.rm = TRUE))

colnames(home_team)[1] <- 'Team_name'
colnames(away_team)[1] <- 'Team_name'

team_publico = merge(x = home_team, y =away_team, by = "Team_name",
                     all = TRUE)

team_publico <- team_publico %>%
  group_by(Team_name) %>%
  summarise(total = Publico + n) %>%
  arrange(desc(total))

### 7.2 Tabela ----

team_publico <- head(team_publico, 10)

## 8. Análise extra: top 10 geral dos resultados finais dos países ----

### 8.1 Organizando o banco de dados ----

# Verificando quantos estágios há.

Etapas <- worldcups %>%
  distinct(Stage)

# Selecionando as colunas.

resultados <- data.frame(worldcups[, c(2, 5, 6, 7, 8, 9)])

# Filtrando os estágios.

resultados <- resultados %>%
  filter(Stage %in% c('Match for third place', 
                      'Third place', 'Play-off for third place',
                      'Final'))

# Atribuindo à diferentes variáveis os diferentes estágios.

## Partida para o terceiro lugar.

resultados_n1 <- resultados %>%
  filter(Stage %in% c('Match for third place', 
                      'Third place', 'Play-off for third place'))

## Partidas da final.

resultados_n2 <- resultados %>%
  filter(Stage == 'Final')

# Novas colunas com as pontuações obtidas.

resultados_n1 <- resultados_n1 %>%
  mutate(home.points = 
           ifelse(Home.Team.Goals > Away.Team.Goals, 2,
                  ifelse(Home.Team.Goals < Away.Team.Goals, 1, 0))) %>%
  mutate(away.points = 
           ifelse(Away.Team.Goals > Home.Team.Goals, 2,
                  ifelse(Away.Team.Goals < Home.Team.Goals, 1, 0)))

resultados_n2 <- resultados_n2 %>%
  mutate(home.points = 
           ifelse(Home.Team.Goals > Away.Team.Goals, 4,
                  ifelse(Home.Team.Goals < Away.Team.Goals, 3, 0))) %>%
  mutate(away.points = 
           ifelse(Away.Team.Goals > Home.Team.Goals, 4,
                  ifelse(Away.Team.Goals < Home.Team.Goals, 3, 0)))

# Empataram duas finais. Portanto, conforme o "win conditions", fiz a seguinte substituição:

resultados_n2[14, 7] <- 4
resultados_n2[14, 8] <- 3
resultados_n2[17, 7] <- 4
resultados_n2[17, 8] <- 3

resultados_n1 <- resultados_n1[ , c(2, 5, 7, 8)]
resultados_n2 <- resultados_n2[ , c(2, 5, 7, 8)]

# Separando os times da casa dos que não são:

resultados_n1_home <- resultados_n1[ , c(1, 3)]
resultados_n1_home <- resultados_n1_home %>%
  group_by(Home.Team.Name) %>%
  summarise(pontos = sum(home.points))

resultados_n1_away <- resultados_n1[ , c(2, 4)]
resultados_n1_away <- resultados_n1_away %>%
  group_by(Away.Team.Name) %>%
  summarise(pontos = sum(away.points))

resultados_n2_home <- resultados_n2[ , c(1, 3)]
resultados_n2_home <- resultados_n2_home %>%
  group_by(Home.Team.Name) %>%
  summarise(pontos = sum(home.points))

resultados_n2_away <- resultados_n2[ , c(2, 4)]
resultados_n2_away <- resultados_n2_away %>%
  group_by(Away.Team.Name) %>%
  summarise(pontos = sum(away.points))

resultados_home = merge(x = resultados_n1_home, y = resultados_n2_home, by = 'Home.Team.Name',
                        all = TRUE)

resultados_home <- resultados_home %>% 
  replace(is.na(.), 0)
resultados_home <- resultados_home %>%
  mutate(pontos_totais = pontos.x + pontos.y)

resultados_away = merge(x = resultados_n1_away, y = resultados_n2_away, by = 'Away.Team.Name',
                        all = TRUE)

resultados_away <- resultados_away %>% 
  replace(is.na(.), 0)
resultados_away <- resultados_away %>%
  mutate(pontos_totais = pontos.x + pontos.y)

colnames(resultados_home)[1] <- 'Team_Name'
colnames(resultados_home)[2] <- 'pontos'
colnames(resultados_away)[1] <- 'Team_Name'

resultados_home <- resultados_home[ , c(1, 4)]
resultados_away <- resultados_away[ , c(1, 4)]

resultados_gerais = merge(x = resultados_away, y = resultados_home, 
                          by = 'Team_Name',
                          all = TRUE)

resultados_gerais <- resultados_gerais %>% 
  replace(is.na(.), 0)
resultados_gerais <- resultados_gerais %>%
  mutate(Total_pontos = pontos_totais.x + pontos_totais.y) %>%
  arrange(desc(Total_pontos))

resultados_gerais <- head(resultados_gerais, 10)

-------------
  
worldcups_audiencia[c(5, 10, 14), 3] <- 'Germany'
worldcups_audiencia[c(8, 12,13), 4] <- 'Germany'
worldcups_audiencia[9, 5] <- 'Germany'
worldcups_audiencia[6, 6] <- 'Germany'
  
  primeiro_lugar <- worldcups_audiencia %>%
  dplyr::select(Winner) %>%
  mutate(Points = 4) %>%
  group_by(Winner) %>%
  summarise(Points = sum(Points))

colnames(worldcups_audiencia)[4] <- 'Runners_Up'
segundo_lugar <- worldcups_audiencia %>%
  dplyr::select(Runners_Up) %>%
  mutate(Points = 3) %>%
  group_by(Runners_Up) %>%
  summarise(Points = sum(Points))

terceiro_lugar <- worldcups_audiencia %>%
  dplyr::select(Third) %>%
  mutate(Points = 2) %>%
  group_by(Third) %>%
  summarise(Points = sum(Points))

quarto_lugar <- worldcups_audiencia %>%
  dplyr::select(Fourth) %>%
  mutate(Points = 1) %>%
  group_by(Fourth) %>%
  summarise(Points = sum(Points))

colnames(primeiro_lugar)[1] <- 'Team_Name'
colnames(segundo_lugar)[1] <- 'Team_Name'
colnames(terceiro_lugar)[1] <- 'Team_Name'
colnames(quarto_lugar)[1] <- 'Team_Name'

primeiro_segundo_lugar = merge(x = primeiro_lugar, y = segundo_lugar, 
                               by = 'Team_Name',
                               all = TRUE)

terceiro_quarto_lugar = merge(x = terceiro_lugar, y = quarto_lugar, 
                              by = 'Team_Name',
                              all = TRUE)

todos_paises = merge(x = primeiro_segundo_lugar, y = terceiro_quarto_lugar, 
                     by = 'Team_Name',
                     all = TRUE)

todos_paises <- todos_paises %>% 
  replace(is.na(.), 0) %>%
  mutate(Total_pontos = Points.x.x + Points.y.x + Points.x.y + Points.y.y) %>%
  arrange(desc(Total_pontos))
