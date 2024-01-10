# 1. Lendo os pacotes e o banco de dados ----
source("packages.R")
library(xtable)
Dados <- read_excel("dados/Questionário Tese.xlsx")

3# 2. Colunas 35-70, 107-121 e 127-138 ----

cols1 <- str_extract(names(Dados)[35:70], "(?<=\\.\\.\\.).*")
cols2 <- str_extract(names(Dados)[107:121], "(?<=\\.\\.\\.).*")
cols3 <- str_extract(names(Dados)[127:138], "(?<=\\.\\.\\.).*")

Nomes <- data.frame(Pergunta = c(rep(5, 12), rep(6, 24), rep(8, 15), rep(14, 12)),
                    Motivo = str_c("Motivo ", c(1:12,1:24,1:15,1:12)),
                    Exp = c(cols1, cols2, cols3))

dados <- Dados[, c(35:70,107:121, 127:138)]

# 3. Pergunta 5 ----
## 3.1 Tabelas de frequência e de dicionário ----
nomes <- paste0("Motivo", c(seq(1:12)))

qui <- dados[1:12] %>% as.data.frame()
colnames(qui) <- nomes

qui_tab <- qui %>% map(table)
qui_tab <- qui_tab[-5]

lista <- qui_tab %>% as.data.frame()
lista <- lista[c(1, seq(2,22,2))] 

lComp <- as.data.frame(t(lista)) 
nomes <- c(" ", paste0("Motivo ", c(1:4, 6:12)))
rownames(lComp) <- nomes

xtable(lComp)
xtable(Nomes[1:12, 2:3])

## 3.2 Gráfico ----
grafico_peg5 <- data.frame(valor = c(qui$Motivo1, qui$Motivo2, qui$Motivo3, qui$Motivo4,
                                     qui$Motivo5, qui$Motivo6, qui$Motivo7, qui$Motivo8,
                                     qui$Motivo9, qui$Motivo10, qui$Motivo11, qui$Motivo12),
                           motivo = str_c("Motivo ",rep(1:12, each=111)))

grafico_peg5_2 <- grafico_peg5 %>%
  group_by(valor, motivo) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

ordem <- str_c("Motivo ", seq(1:12))

ggplot(grafico_peg5_2) +
  aes(x = factor(motivo,levels =ordem), y = freq, fill = valor) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  theme_bw() +
  theme_estat() +
  theme(axis.text.x = element_text(size = 7.75)) +
  scale_fill_manual(values = cores_estat, name="")
ggsave("graficos/Ana Luisa/colunas_perg5.pdf", width = 208, height = 93, units = "mm")

# 4. Pergunta 6 ----
## 4.1 Tabelas de frequência e de dicionário ----
nomes <- paste0("Fator", c(seq(1:24)))

sex <- dados[13:36] %>% as.data.frame()
colnames(sex) <- nomes

sex_tab <- sex %>% map(table)
sex_tab <- sex_tab[c(-1, -10, -11)]

lista <- sex_tab %>% as.data.frame()
lista <- lista[c(1, seq(2,42,2))] 

lComp <- as.data.frame(t(lista)) 
nomes <- c(" ", paste0("Fator ", c(2:9, 12:24)))
rownames(lComp) <- nomes

xtable(lComp)
xtable(Nomes[13:36, 2:3])

## 4.2 Gráfico ----
grafico_peg6 <- data.frame(valor = c(sex$Fator1, sex$Fator2, sex$Fator3, sex$Fator4,
                                     sex$Fator5, sex$Fator6, sex$Fator7, sex$Fator8,
                                     sex$Fator9, sex$Fator10, sex$Fator11, sex$Fator12,
                                     sex$Fator13, sex$Fator14, sex$Fator15, sex$Fator16,
                                     sex$Fator17, sex$Fator18, sex$Fator19, sex$Fator20,
                                     sex$Fator21, sex$Fator22, sex$Fator23, sex$Fator24),
                           fator = str_c("Fator ",rep(1:24, each=111)))

grafico_peg6_2 <- grafico_peg6 %>%
  group_by(valor, fator) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

ordem <- str_c("Fator ", seq(1:24))

ggplot(grafico_peg6_2) +
  aes(x = factor(fator,levels =ordem), y = freq, fill = valor) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  theme_bw() +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=90),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values = cores_estat, name="") +
  scale_colour_manual(values = cores_estat) +
  theme(axis.text.x = element_text(size = 9.3))
ggsave("graficos/Ana Luisa/colunas_perg6.pdf", width = 208, height = 93, units = "mm")

# 5. Pergunta 8 ----
## 5.1 Tabelas de frequência e de dicionário ----
nomes <- paste0("Conceito", c(seq(1:15)))

oit <- dados[37:51] %>% as.data.frame()
colnames(oit) <- nomes

oit_tab <- oit %>% map(table)

lista <- oit_tab %>% as.data.frame()
lista <- lista[c(1, seq(2,30,2))] 

lComp <- as.data.frame(t(lista)) 
nomes <- c(" ", paste0("Conceito ", c(1:15)))
rownames(lComp) <- nomes[]

xtable(lComp)
xtable(Nomes[37:51, -1])

## 5.2 Gráfico ----
grafico_peg8 <- data.frame(valor = c(oit$Conceito1, oit$Conceito2, oit$Conceito3, oit$Conceito4,
                                     oit$Conceito5, oit$Conceito6, oit$Conceito7, oit$Conceito8,
                                     oit$Conceito9, oit$Conceito10, oit$Conceito11, oit$Conceito12,
                                     oit$Conceito13, oit$Conceito14, oit$Conceito15),
                           conceito = str_c("Conceito ",rep(1:15, each=111)))

grafico_peg8_2 <- grafico_peg8 %>%
  group_by(valor, conceito) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

ordem <- str_c("Conceito ", seq(1:15))

ggplot(grafico_peg8_2) +
  aes(x = factor(conceito,levels =ordem), y = freq, fill = valor) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  theme_bw() +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=90),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values = cores_estat, name="") +
  scale_colour_manual(values = cores_estat) +
  theme(axis.text.x = element_text(size = 13))
ggsave("graficos/Ana Luisa/colunas_perg8.pdf", width = 208, height = 93, units = "mm")


# 4. Pergunta 14 ----
## 14.1 Tabelas de frequência e de dicionário ----
nomes <- paste0("Maneira", c(seq(1:12)))

decim_quar <- dados[52:63] %>% as.data.frame()
colnames(decim_quar) <- nomes

decim_quar_tab <- decim_quar %>% map(table)
decim_quar_tab <- decim_quar_tab[c(-1, -2, -3, -6, -9)]

lista <- decim_quar_tab %>% as.data.frame()
lista <- lista[c(1, seq(2,14,2))] 

lComp <- as.data.frame(t(lista)) 
nomes <- c(" ", paste0("Maneira ", c(4, 5, 7, 8, 10, 11, 12)))
rownames(lComp) <- nomes[]

xtable(lComp)
xtable(Nomes[52:63, -1])

## 14.2 Gráfico ----
grafico_peg14 <- data.frame(valor = c(decim_quar$Maneira1, decim_quar$Maneira2, decim_quar$Maneira3, decim_quar$Maneira4,
                                     decim_quar$Maneira5, decim_quar$Maneira6, decim_quar$Maneira7,decim_quar$Maneira8,
                                     decim_quar$Maneira9, decim_quar$Maneira10, decim_quar$Maneira11, decim_quar$Maneira12),
                           maneira = str_c("Maneira ",rep(1:12, each=111)))

grafico_peg14_2 <- grafico_peg14 %>%
  group_by(valor, maneira) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

ordem <- str_c("Maneira ", seq(1:12))

ggplot(grafico_peg14_2) +
  aes(x = factor(maneira,levels=ordem), y = freq, fill = valor) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  theme_estat()+
  theme(
    axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=90)
  ) +
  theme(axis.text.x = element_text(size = 9.5)) +
  scale_fill_manual(values = cores_estat, name="")
ggsave("graficos/Ana Luisa/colunas_perg14.pdf", width = 208, height = 93, units = "mm")

# 5. Cruzamentos (Categórica X Categórica) ----
## 3.2 X Região - Gráfico de barras ----
Regiao_3.2 <- Dados[ ,c(13, 139)]

colnames(Regiao_3.2) <- c('Motivo', 'Região')

Regiao_3.2 %>%
  distinct(Região)

Regiao_3.2$Região <- Regiao_3.2$Região %>%
  trimws(which = c("both")) %>%
  str_replace_all('Tocantins|Pará|Parq|Manaus|AMAZONAS|Roraima', 'Norte') %>%
  str_replace_all('Sergipe|Pernambuco|Bahia|Paraíba|Maranhão|Fortaleza/CE|Ceará|Piauí|pe', 'Nordeste') %>%
  str_replace_all('Rio Grande do Sul|RS|Santa Catarina|Pr|rio grande do sul|Paraná', 'Sul') %>%
  str_replace_all('Rio de Janeiro|Espírito Santo|São Paulo|Rio de janeiro|Minas Gerais|sp|ES|MG|Minas gerais|rio de janeiro', 'Sudeste') %>%
  str_replace_all('Distrito Federal|Mato Grosso|Goias', 'Centro-Oeste') %>%
  str_replace_all('ESudesteirito Santo', 'Sudeste')

Regiao_3.2 <- Regiao_3.2 %>%
  group_by(Motivo, Região) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

niveis = c('00. Não sabe/não responde', '1. Nada importante',
           '2. Pouco importante', '3. Mais ou menos importante',
           '4. Importante', '5. Extremamente importante')

ggplot(Regiao_3.2) +
  aes(x = factor(Motivo,levels= niveis), y = freq, fill = Região, label = freq) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 3) +
  theme_bw() +
  theme_estat()+
  theme(axis.text.x = element_text(size = 11)) +
  scale_x_discrete(labels = scales::label_wrap(10))+
  scale_fill_manual(values = cores_estat, name="") +
  ylim(0, 25) 
ggsave("graficos/Ana Luisa/colunas_regiao_3.2.pdf", width = 208, height = 93, units = "mm")

## 5.9 X Região - Gráfico de barras ----
Regiao_5.9 <- Dados[ ,c(43, 139)]

colnames(Regiao_5.9) <- c('Motivo', 'Região')

Regiao_5.9 %>%
  distinct(Região)

Regiao_5.9$Região <- Regiao_5.9$Região %>%
  trimws(which = c("both")) %>%
  str_replace_all('Tocantins|Pará|Parq|Manaus|AMAZONAS|Roraima', 'Norte') %>%
  str_replace_all('Sergipe|Pernambuco|Bahia|Paraíba|Maranhão|Fortaleza/CE|Ceará|Piauí|pe', 'Nordeste') %>%
  str_replace_all('Rio Grande do Sul|RS|Santa Catarina|Pr|rio grande do sul|Paraná', 'Sul') %>%
  str_replace_all('Rio de Janeiro|Espírito Santo|São Paulo|Rio de janeiro|Minas Gerais|sp|ES|MG|Minas gerais|rio de janeiro', 'Sudeste') %>%
  str_replace_all('Distrito Federal|Mato Grosso|Goias', 'Centro-Oeste') %>%
  str_replace_all('ESudesteirito Santo', 'Sudeste')

Regiao_5.9 <- Regiao_5.9 %>%
  group_by(Motivo, Região) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

ggplot(Regiao_5.9) +
  aes(x = factor(Motivo,levels= niveis ), y = freq, fill = Região, label = freq) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 3) +
  theme_bw() +
  theme_estat()+
  theme(axis.text.x = element_text(size = 11)) +
  scale_x_discrete(labels = scales::label_wrap(10))+
  scale_fill_manual(values = cores_estat, name="") +
  ylim(0,15)
ggsave("graficos/Ana Luisa/colunas_regiao_5.9.pdf", width = 208, height = 93, units = "mm")

## 1.6 X 6.6 - Tabela ----

cruzamento_1.6 <- Dados[, c(2, 52)]
colnames(cruzamento_1.6) <- c('Resposta1.6', 'Resposta6.6')

cruzamento_1.6 <- cruzamento_1.6 %>%
  mutate(Resposta = case_when(
    Resposta1.6 %>% str_detect("1.6 Programa/Curso e Universidade tiveram a mesma influência") ~ "1. Sim",
    Resposta1.6 %>% str_detect("[^1.6 Programa/Curso e Universidade tiveram a mesma influência]") ~ "2. Não")) %>%
    select(Resposta, Resposta6.6)

table(cruzamento_1.6$Resposta, cruzamento_1.6$Resposta6.6)  %>%
  addmargins() %>%
  xtable()

# 6. Cruzamentos (Categórica X Dicotômica{Sim/Não}) ----
## 2.8 e 8.5 - Gráfico de barras e tabela ----
### Gráfico de barras ----

cruzamento_2.8 <- Dados[, c(10 , 111 )]
# 2.8 - Sonho de morar fora
# 8.5 - Instituição global e internacionalizada

colnames(cruzamento_2.8) <- c('R2.8', 'R8.5')

cruzamento_2.8_2 <- cruzamento_2.8 %>%
  arrange(R2.8) %>%
  group_by(R2.8, R8.5) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

cruzamento_2.8_2$R2.8 <- cruzamento_2.8_2$R2.8 %>%
str_replace_all('5. Extremamente importante,',
                '5. Extremamente importante')

ggplot(cruzamento_2.8_2) +
  aes(x = factor(R2.8 ,levels= niveis), y = freq, fill = R8.5) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  theme_bw() +
  theme_estat()+
  theme(axis.text.x = element_text(size = 10)) +
  scale_x_discrete(labels = scales::label_wrap(10))+
  scale_fill_manual(values = cores_estat, name="")
ggsave("graficos/Ana Luisa/colunas_cruzamento_2.8.pdf", width = 178, height = 93, units = "mm")

### Tabela ----

table(cruzamento_2.8$R2.8, cruzamento_2.8$R8.5)  %>%
  addmargins() %>%
  xtable()

## 3.5 e 8.5 - Gráfico de barras e tabela ----
### Gráfico de Barras ----

cruzamento_3.5 <- Dados[, c(16 , 111 )]
# 3.5 - Vivenciar uma cultura diferente
# 8.5 - Instituição global e internacionalizada
colnames(cruzamento_3.5) <- c('R3.5', 'R8.5')

cruzamento_3.5_2 <- cruzamento_3.5 %>%
  group_by(R3.5, R8.5) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

ggplot(cruzamento_3.5_2) +
  aes(x = factor(R3.5 ,levels= niveis), y = freq, fill = R8.5) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  theme_bw() +
  theme_estat()+
  theme(axis.text.x = element_text(size = 10)) +
  scale_x_discrete(labels = scales::label_wrap(10))+
  scale_fill_manual(values = cores_estat, name="")
ggsave("graficos/Ana Luisa/colunas_cruzamento_3.5.pdf", width = 178, height = 93, units = "mm")

### Tabela ----

table(cruzamento_3.5$R3.5, cruzamento_3.5$R8.5)  %>%
  addmargins() %>%
  xtable()

## 4.2 e 6.2 - Gráfico de barras e tabela ----
### Gráfico de barras ----

cruzamento_4.2 <- Dados[, c(24, 48 )]
# 4.2 - Conselhos ou indicação de professores do Brasil
# 6.2 - Indicação de professores da universidade de origem
colnames(cruzamento_4.2) <- c('R4.2', 'R6.2')

cruzamento_4.2_2 <- cruzamento_4.2 %>%
  group_by(R4.2, R6.2) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

ggplot(cruzamento_4.2_2) +
  aes(x = factor(R4.2 ,levels= c('00. Não sabe/não responde', '1. Sim', '2. Não')), y = freq, fill = R6.2) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  theme_bw() +
  theme_estat()+
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_manual(values = cores_estat, name="")
ggsave("graficos/Ana Luisa/colunas_cruzamento_4.2.pdf", width = 178, height = 93, units = "mm")

### Tabela ----
table(cruzamento_4.2$R4.2, cruzamento_4.2$R6.2)  %>%
  addmargins() %>%
  xtable()

## 4.4 e 6.5 - Gráfico de barras e tabela ----
### Gráfico de barras ----

cruzamento_4.4 <- Dados[, c(26 , 51)]
# 4.4 - Conselhos ou indicações de ex-alunos
# 6.5 - Tradição da Universidade
colnames(cruzamento_4.4) <- c('R4.4', 'R6.5')

cruzamento_4.4_2 <- cruzamento_4.4 %>%
  group_by(R4.4, R6.5) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

ggplot(cruzamento_4.4_2) +
  aes(x = factor(R4.4 ,levels= c('00. Não sabe/não responde', '1. Sim', '2. Não')), y = freq, fill = R6.5) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  theme_bw() +
  theme_estat()+
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_manual(values = cores_estat, name="")
ggsave("graficos/Ana Luisa/colunas_cruzamento_4.4.pdf", width = 178, height = 93, units = "mm")

### Tabela ----

table(cruzamento_4.4$R4.4, cruzamento_4.4$R6.5)  %>%
  addmargins() %>%
  xtable()

# Item 5.6 cruzar/correlacionar com 8.13 (tabela) ----
## Tabela ----
cruzamento_5.6 <- Dados[, c(40, 119)]
colnames(cruzamento_5.6) <- c('Resposta5.6', 'Resposta8.13')

table(cruzamento_5.6$Resposta5.6, cruzamento_5.6$Resposta8.13)  %>%
  addmargins() %>%
  xtable()

# Item 4.8 cruzar/correlacionar com questão 7(Gráfico/Tabela) 
cruzamento_4.8_7 <- Dados[, c(30, 71:106)]

colnames(cruzamento_4.8_7) <- c("Fonte_acesso_redes_sociais", paste0("Canal ", c(seq(1:36))))

## Respondeu "Sim" ao item 4.8.----

cruzamentopt1 <- cruzamento_4.8_7 %>%
  filter(Fonte_acesso_redes_sociais == '1. Sim')

cruzamentopt1 <- cruzamentopt1[ , c(-1, -37)]

cruzamentopt1 <- cruzamentopt1 %>%
  pivot_longer(seq(1:35), names_to = "Canal", values_to = "Respostas")

cruzamentopt1 <- cruzamentopt1 %>%
  group_by(Canal, Respostas) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

### Gráfico ----

ordem <- str_c("Canal ",seq(1:35))
ordem2 <- c("1.Sim","2. Não","00. Não sabe/não responde")

ggplot(cruzamentopt1) +
  aes(x = factor(Canal, levels =ordem), y = freq, fill = factor(Respostas,levels=ordem2), label = freq) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2) +
  theme_bw() +
  theme(
    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
    axis.title.x = ggplot2::element_text(colour = "black", size = 12),
    axis.text = ggplot2::element_text(colour = "black", size = 9.5),
    axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=90),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "top") +
  scale_fill_manual(values = cores_estat, name="") +
  scale_colour_manual(values = cores_estat)
ggsave("graficos/Ana Luisa/colunas_4.8_perg7.pdf",width = 208, height = 93, units = "mm")

## Respondeu "Não" ao item 4.8.----

cruzamentopt2 <- cruzamento_4.8_7 %>%
  filter(Fonte_acesso_redes_sociais == '2. Não')

cruzamentopt2 <- cruzamentopt2[ , c(-1, -37)]

cruzamentopt2 <- cruzamentopt2 %>%
  pivot_longer(seq(1:35), names_to = "Canal", values_to = "Respostas")

cruzamentopt2 <- cruzamentopt2 %>%
  group_by(Canal, Respostas) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

### Gráfico ----

ggplot(cruzamentopt2) +
  aes(x = factor(Canal, levels =ordem), y = freq, fill = factor(Respostas,levels=ordem2), label = freq) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = " ", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2) +
  theme_bw() +
  theme(
    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
    axis.title.x = ggplot2::element_text(colour = "black", size = 12),
    axis.text = ggplot2::element_text(colour = "black", size = 9.5),
    axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=90),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "top") +
  scale_fill_manual(values = cores_estat, name="") +
  scale_colour_manual(values = cores_estat)
ggsave("graficos/Ana Luisa/colunas_4.8_perg7_pt2.pdf",width = 208, height = 93, units = "mm")


# Item 4.8 cruzar/correlacionar com 9.10 (Tabela) ----
cruzamento_4.8_9.10 <- Dados[, c(30, 122)]
colnames(cruzamento_4.8_9.10) <- c('Resposta4.8', 'Resposta9.10')
cruzamento_4.8_9.10<- cruzamento_4.8_9.10%>%
  mutate(Resposta = case_when(
    Resposta9.10 %>% str_detect("9.10 Esclarecimentos/diálogos/interação via redes sociais não oficiais.") ~ "1. Sim",
    Resposta9.10 %>% str_detect("[^9.10 Esclarecimentos/diálogos/interação via redes sociais não oficiais.]") ~ "2. Não")) %>%
  select(Resposta, Resposta4.8)

table(cruzamento_4.8_9.10$Resposta4.8, cruzamento_4.8_9.10$Resposta)  %>%
  addmargins() %>%
  xtable()

## limpando o workspace
rm(cruzamento_1.6, cruzamento_2.8, cruzamento_2.8_2, cruzamento_3.5, cruzamento_3.5_2,
   cruzamento_4.2, cruzamento_4.2_2, cruzamento_4.4, cruzamento_4.4_2,
   cruzamento_4.8_7, cruzamento_4.8_9.10, cruzamento_5.6)

rm(grafico_peg14, grafico_peg14_2, grafico_peg5, grafico_peg5_2, grafico_peg6,
   grafico_peg8, grafico_peg8_2. grafico_peg6_2)

# Item 4.4 cruzar/correlacionar com 6.15 (tabela) ----
cruzamento_4.4_6.15 <- Dados[, c(26, 61)]
colnames(cruzamento_4.4_6.15) <- c('Resposta4.4', 'Resposta6.15')
table(cruzamento_4.4_6.15$Resposta4.4, cruzamento_4.4_6.15$Resposta6.15)  %>%
  addmargins() %>%
  xtable()

# Item 4.11 cruzar/correlacionar com 7.2 ----
cruzamento_4.11_7.2 <- Dados[, c(33, 72)]
colnames(cruzamento_4.11_7.2) <- c('Resposta4.11', 'Resposta7.2')
table(cruzamento_4.11_7.2$Resposta4.11, cruzamento_4.11_7.2$Resposta7.2)  %>%
  addmargins() %>%
  xtable()

# Item 4.11 com 14.7 (Tabela) ----

cruzamento_4.11_14.7 <- Dados[, c(33, 133)]
colnames(cruzamento_4.11_14.7) <- c('Resposta4.11', 'Resposta14.7')
table(cruzamento_4.11_14.7$Resposta4.11, cruzamento_4.11_14.7$Resposta14.7)  %>%
  addmargins() %>%
  xtable()

# Item 7.15 cruzar/correlacionar com 9.1 (tabela) ----

cruzamento_7.15_9.1 <- Dados[, c(85, 122)]
colnames(cruzamento_7.15_9.1) <- c('Resposta7.15', 'Resposta9.1')
cruzamento_7.15_9.1<- cruzamento_7.15_9.1 %>%
  mutate(Resposta = case_when(
    Resposta9.1 %>% str_detect("9.1 Vídeos depoimentos.") ~ "1. Sim",
    Resposta9.1 %>% str_detect("[9.1 Vídeos depoimentos.]") ~ "2. Não")) %>%
  select(Resposta, Resposta7.15)

table(cruzamento_7.15_9.1$Resposta7.15, cruzamento_7.15_9.1$Resposta)  %>%
  addmargins() %>%
  xtable()

# Item 6.1 cruzar/correlacionar com 8.3 (tabela) ----

cruzamento_6.1_8.3 <- Dados[, c(47, 109)]
colnames(cruzamento_6.1_8.3) <- c('Resposta6.1', 'Resposta8.3')
table(cruzamento_6.1_8.3$Resposta6.1, cruzamento_6.1_8.3$Resposta8.3)  %>%
  addmargins() %>%
  xtable()
