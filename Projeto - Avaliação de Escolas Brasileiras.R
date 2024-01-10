# Carregando os pacotes ----
library(tidyverse)
library(readxl)
library(xtable)

# Organizando o banco de dados para que fique mais legível ----
Dados <- read_excel("Dados/Dados_amostra.xls")

Dados$REG <- Dados$REG  %>%
  str_replace('CO', 'Centro-Oeste') %>%
  str_replace('SE', 'Sudeste') %>%
  str_replace('NE', 'Nordeste') %>%
  str_replace('[N]', 'Norte') %>%
  str_replace('[S]', 'Sul') %>%
  str_replace('Norteordeste', 'Nordeste') %>%
  str_replace('Suludeste', 'Sudeste')

Dados$LOCAL <- Dados$LOCAL %>%
  str_replace('1', 'Urbana') %>%
  str_replace('2', 'Rural')

Dados$TAM_MUN <- Dados$TAM_MUN %>%
  str_replace('1', '<20.000') %>%
  str_replace('^2', '20.000 a 49.999') %>%
  str_replace('^5', '1.000.000 ou mais') %>%
  str_replace('^3', '50.000 a 99.999') %>%
  str_replace('^4', '100.000 a 999.999')

Dados$ADM <- Dados$ADM %>%
  str_replace('2', 'Estadual') %>%
  str_replace('3', 'Municipal') 

Dados$TAM_ESCOLA <- Dados$TAM_ESCOLA %>%
  str_replace('1', '<25') %>%
  str_replace('^2', '25 a 49') %>%
  str_replace('^3', '50 a 99') %>%
  str_replace('^4', '100 ou mais')

# Análise 1: Descrever as características das escolas e o desempenho de seus estudantes na Prova de Brasil em 2011 ----
## Escola e Região ----

ESC_REG <- Dados %>%
  group_by(REG) %>%
  summarise(Frequência = n()) 

colnames(ESC_REG)[1] <- c('Região')
  
xtable(tibble(ESC_REG))

ESC_REG <- ESC_REG%>%
  mutate(f = (Frequência/200)*100)

legendas1 <- str_squish(str_c(ESC_REG$f, '%')) %>%
  str_replace_all("[.]", ",")
  
ggplot(ESC_REG) +
  aes(x = fct_reorder(Região, Frequência, .desc=T), y = Frequência, label = legendas1 ) +
  geom_bar(stat = "identity", fill = "#611C35", width = 0.7) +
  geom_text(
    position = position_dodge (width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Regiões do Brasil", y = "Número de escolas") +
  tema_met()
ggsave ("Analises/Ana Luisa/esc_reg.pdf", width = 158, height = 93, units = "mm"
)

## Escola e Local ----

ESC_LOC <- Dados %>%
  group_by(LOCAL) %>%
  summarise(Frequência = n()) 

colnames(ESC_LOC)[1] <- c('Localidade')

xtable(tibble(ESC_LOC))

ESC_LOC <- ESC_LOC%>%
  mutate(f = (Frequência/200)*100)

legendas1 <- str_squish(str_c(ESC_LOC$f, '%')) %>%
  str_replace_all("[.]", ",")

ggplot(ESC_LOC) +
  aes(x = fct_reorder(Localidade, Frequência, .desc=T), y = Frequência, label = legendas1 ) +
  geom_bar(stat = "identity", fill = "#611C35", width = 0.7) +
  geom_text(
    position = position_dodge (width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Localidades", y = "Número de escolas") +
  tema_met()
ggsave ("Analises/Ana Luisa/esc_loc.pdf", width = 158, height = 93, units = "mm"
)

## Escola e Tamanho do Município ----  

ESC_TAM_MUN <- Dados %>%
  group_by(TAM_MUN) %>%
  summarise(Frequência = n()) 

colnames(ESC_TAM_MUN)[1] <- c('Tamanho_do_município')

xtable(tibble(ESC_TAM_MUN))

ESC_TAM_MUN <- ESC_TAM_MUN%>%
  mutate(f = (Frequência/200)*100)

legendas1 <- str_squish(str_c(ESC_TAM_MUN$f, '%')) %>%
  str_replace_all("[.]", ",")

ggplot(ESC_TAM_MUN) +
  aes(x = fct_reorder(Tamanho_do_município, Frequência, .desc=T), y = Frequência, label = legendas1 ) +
  geom_bar(stat = "identity", fill = "#611C35", width = 0.7) +
  geom_text(
    position = position_dodge (width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Tamanho do município (em número de habitantes)", y = "Número de escolas") +
  tema_met() +
  scale_x_discrete(labels = scales::label_wrap(10))
ggsave ("Analises/Ana Luisa/esc_tam_mun.pdf", width = 158, height = 93, units = "mm"
)

## Escola e categoria administrativa ----

ESC_ADM <- Dados %>%
  group_by(ADM) %>%
  summarise(Frequência = n()) 

colnames(ESC_ADM)[1] <- c('Categoria_administrativa')

xtable(tibble(ESC_ADM))

ESC_ADM <- ESC_ADM %>%
  mutate(f = (Frequência/200)*100)

legendas1 <- str_squish(str_c(ESC_ADM$f, '%')) %>%
  str_replace_all("[.]", ",")

ggplot(ESC_ADM) +
  aes(x = fct_reorder(Categoria_administrativa, Frequência, .desc=T), y = Frequência, label = legendas1 ) +
  geom_bar(stat = "identity", fill = "#611C35", width = 0.7) +
  geom_text(
    position = position_dodge (width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Categoria administrativa", y = "Número de escolas") +
  tema_met()
ggsave ("Analises/Ana Luisa/esc_adm.pdf", width = 158, height = 93, units = "mm"
)

## Escola e tamanho da escola ----

ESC_TAM_ESC <- Dados %>%
  group_by(TAM_ESCOLA) %>%
  summarise(Frequência = n()) 

colnames(ESC_TAM_ESC)[1] <- c('Tamanho_da_escola')

xtable(tibble(ESC_TAM_ESC))

ESC_TAM_ESC <- ESC_TAM_ESC %>%
  mutate(f = (Frequência/200)*100)

legendas1 <- str_squish(str_c(ESC_TAM_ESC$f, '%')) %>%
  str_replace_all("[.]", ",")

ggplot(ESC_TAM_ESC) +
  aes(x = fct_reorder(Tamanho_da_escola, Frequência, .desc=T), y = Frequência, label = legendas1 ) +
  geom_bar(stat = "identity", fill = "#611C35", width = 0.7) +
  geom_text(
    position = position_dodge (width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "Tamanho da escola (em número de alunos)", y = "Número de escolas") +
  tema_met()
ggsave ("Analises/Ana Luisa/esc_tam_esc.pdf", width = 158, height = 93, units = "mm"
)

## Escola e número de matriculados ----

ggplot(Dados) +
  aes(x = MATRICULADOS) +
  geom_histogram (
    aes(y = 100 * (..count..) / sum (..count..)),
    colour = "white ",
    fill = "#611C35",
    binwidth = 20
  ) +
  labs(x = "Quantidade de alunos matriculados", y = "Frequência relativa ") +
  tema_met ()
ggsave ("Analises/Ana Luisa/matriculados.pdf", width = 158, height = 93, units = "mm")

MED_MATR <- data.frame(Estatística = c('Média', 'Desvio Padrão',
                                       'Mínimo',
                                       '1º Quartil', 'Mediana',
                                       '3º Quartil', 'Máximo'),
                       Valor = c(mean(Dados$MATRICULADOS), sd(Dados$MATRICULADOS),
                                 min(Dados$MATRICULADOS), quantile(Dados$MATRICULADOS, 0.25),
                                 median(Dados$MATRICULADOS), quantile(Dados$MATRICULADOS, 0.75),
                                 max(Dados$MATRICULADOS)
                                 ))

xtable(tibble(MED_MATR))

## Escola e participação ----

y <- str_wrap("Participação das escolas na Prova Brasil em 2011", width = 25)

ggplot(Dados) +
  aes(x = factor(""), y = PARTICIPACAO) +
  geom_boxplot(fill=c("#2E5077"), width = 0.5) +
  guides(fill= none ) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = y) +
  tema_met() 
ggsave("Analises/Ana Luisa/participacao.pdf", width = 158, height = 93, units = "mm")

MED_PART <- data.frame(Estatística = c('Média', 'Desvio Padrão',
                                       'Mínimo',
                                       '1º Quartil', 'Mediana',
                                       '3º Quartil', 'Máximo'),
                       Valor = c(mean(Dados$PARTICIPACAO), sd(Dados$PARTICIPACAO),
                                 min(Dados$PARTICIPACAO), quantile(Dados$PARTICIPACAO, 0.25),
                                 median(Dados$PARTICIPACAO), quantile(Dados$PARTICIPACAO, 0.75),
                                 max(Dados$PARTICIPACAO)
                       ))

xtable(tibble(MED_PART))

## Escola e nota em LP ----

ggplot(Dados) +
  aes(x = factor(""), y = NOTA_LP) +
  geom_boxplot(fill=c("#2E5077"), width = 0.5) +
  guides(fill= none ) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = 'Notas médias em Língua Portuguesa') +
  tema_met() 
ggsave("Analises/Ana Luisa/nota_lp.pdf", width = 158, height = 93, units = "mm")

MED_NOTA_LP <- data.frame(Estatística = c('Média', 'Desvio Padrão',
                                       'Mínimo',
                                       '1º Quartil', 'Mediana',
                                       '3º Quartil', 'Máximo'),
                       Valor = c(mean(Dados$NOTA_LP), sd(Dados$NOTA_LP),
                                 min(Dados$NOTA_LP), quantile(Dados$NOTA_LP, 0.25),
                                 median(Dados$NOTA_LP), quantile(Dados$NOTA_LP, 0.75),
                                 max(Dados$NOTA_LP)
                       ))

xtable(tibble(MED_NOTA_LP))

## Escola e nota em MT ----
  
ggplot(Dados) +
  aes(x = factor(""), y = NOTA_MT) +
  geom_boxplot(fill=c("#2E5077"), width = 0.5) +
  guides(fill= none ) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = 'Notas médias em Matemática') +
  tema_met() 
ggsave("Analises/Ana Luisa/nota_mt.pdf", width = 158, height = 93, units = "mm")

MED_NOTA_MT <- data.frame(Estatística = c('Média', 'Desvio Padrão',
                                          'Mínimo',
                                          '1º Quartil', 'Mediana',
                                          '3º Quartil', 'Máximo'),
                          Valor = c(mean(Dados$NOTA_MT), sd(Dados$NOTA_MT),
                                    min(Dados$NOTA_MT), quantile(Dados$NOTA_MT, 0.25),
                                    median(Dados$NOTA_MT), quantile(Dados$NOTA_MT, 0.75),
                                    max(Dados$NOTA_MT)
                          ))

xtable(tibble(MED_NOTA_MT))



# Análise 9: Verificar se: ----
## a. Região e categoria administrativa estão associadas; ----

REG_ADM <- Dados %>%
  select(ADM, REG) %>%
  group_by(ADM, REG) %>%
  summarise(n = n())

REG_ADM <- table(REG_ADM)

xtable(REG_ADM)

as.data.frame(REG_ADM)
# Agora vamos ter acesso a cada variável individualmente
regiao <- factor(Dados$REG)
adm <- factor(Dados$ADM)


# *** Tabelas de Contingência  ***
tabela0 <- table(Dados$REG, Dados$ADM)
tabela0 

# ou... com os rótulos das categorias
tabela1 <- table(regiao, adm)
tabela1

# ** Colocando rótulos na tabela - alternativa
rownames(tabela1)<-c("Centro-Oeste","Nordeste", "Norte", "Sudeste", "Sul" )
colnames(tabela1)<-c("Estadual", "Municipal")
tabela1

# ** Trocando a posição das variáveis na tabela 
tabela2 <- table(adm,regiao)
tabela2


#   Totais marginais (junto com a tabela) 
addmargins(tabela1)


# Frequência Relativa 
prop.table(tabela1)     # em relação ao total geral (n)

prop.table(tabela1,1)   # em relação ao total da linha 

prop.table(tabela1,2)   # em relação ao total da coluna


# Teste Qui-quadrado 
chisq.test(regiao, adm)


## b. Tamanho da escola e tamanho do município estão associados. ----

ESC_MUN <- Dados %>%
  select(TAM_ESCOLA, TAM_MUN) %>%
  group_by(TAM_ESCOLA, TAM_MUN) %>%
  summarise(n = n())

ESC_MUN<- table(ESC_MUN)

xtable(ESC_MUN)


# Agora vamos ter acesso a cada variável individualmente
tam_esc <- factor(Dados$TAM_ESCOLA)
tam_mun <- factor(Dados$TAM_MUN)


# *** Tabelas de Contingência  ***
tabela0 <- table(Dados$TAM_ESCOLA, Dados$TAM_MUN)
tabela0 

# ou... com os rótulos das categorias
tabela1 <- table(tam_esc, tam_mun)
tabela1

xtable(tabela1)

#   Totais marginais (junto com a tabela) 
addmargins(tabela1)


# Frequência Relativa 
prop.table(tabela1)     # em relação ao total geral (n)

prop.table(tabela1,1)   # em relação ao total da linha 

prop.table(tabela1,2)   # em relação ao total da coluna


# Teste Qui-quadrado 
chisq.test(tam_esc, tam_mun)
