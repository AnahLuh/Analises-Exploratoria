source("rdocs/source/packages.R")

QUESTIONARIO_TCC <- read_excel("banco/QUESTIONARIO TCC.xlsx")

# Ano de ingresso no CBMTO        ----

a1 <- QUESTIONARIO_TCC[ , 5]

a1 <- a1 %>%
  group_by(`3. Em que ano o senhor(a) ingressou no CBMTO?`) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(a1$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(a1$freq, " (", porcentagens, ")"))

ordem <- c("1993", "1994",
           "2001", "2004",
           "2006", "2007",
           "2009", "2014",
           "2016", "2022")

a1$`3. Em que ano o senhor(a) ingressou no CBMTO?` <- factor(a1$`3. Em que ano o senhor(a) ingressou no CBMTO?`, levels = ordem) 

ggplot(a1) +
  aes(
    x = `3. Em que ano o senhor(a) ingressou no CBMTO?`,
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.75
  ) +
  labs(x = "Em que ano o senhor(a) ingressou no CBMTO?", y = "Frequência") +
  scale_x_discrete(labels = wrap_format(15)) +
  theme_estat() +
  scale_y_continuous(breaks = seq(0, 40, by = 10), limits = c(0, 40)) 
ggsave("resultados/analu/ano-ingresso.pdf", width = 158, height = 93, units = "mm")

# Relação entre se já atuou em um curso de instrução e unidade do CBMTO; ----

a2 <- QUESTIONARIO_TCC[ , c(10, 6)]

colnames(a2) <- c("instrutor", "unidade")

a2$unidade <- a2$unidade %>%
  str_to_upper()

a2$unidade <- a2$unidade %>%
  str_replace("1ª CIA/1ºBBM|2ª CIA/1ºBBM|3ª CIA/1ºBBM|5ª CIA/1ºBBM|CIBS|DISTEC|SIOP|DPE|ASCOM|SIOP|CASA MILITAR|CIOPAER|GEINFRA|2 CIA 1BBM|SIOP|COLÉGIO MILITAR/CMTO|DEP", "1º BBM")

a2$unidade <- a2$unidade %>%
  str_replace("1ª CIA/2ºBBM|2ª CIA/2ºBBM|3ª CIA/2ºBBM|5° CIBM – ARAGUATINS|SESTEC|5ª CIBM|CMTO JARDENIR JORGE FREDERICO|2° BBM|2º BBM/2°BBM|5° CIBM - ARAGUATINS", "2º BBM")

a2$unidade <- a2$unidade %>%
  str_replace("/2°BBM", "")

a2$unidade <- a2$unidade %>%
  str_replace("1ª CIA/3ºBBM|2ª CIA/3ºBBM", "3º BBM")

a2$unidade <- a2$unidade %>%
  str_replace("COORDENADORIA ESTADUAL DE DEFESA CIVIL ESTADUAL", str_to_title("COORDENADORIA ESTADUAL DE DEFESA CIVIL ESTADUAL"))

a2 <- a2 %>%
  group_by(unidade, instrutor) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(a2$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(a2$freq, " (", porcentagens, ")"))

ggplot(a2) +
  aes(
    x = unidade,
    y = freq,
    fill = instrutor,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 2.8
  ) +
  labs(x = "Em qual unidade do CBMTO o senhor(a) está lotado(a)?", y = "Frequência") +
  guides(fill=guide_legend(title="O senhor(a) já atuou como instrutor(a)\nde algum curso de formação de\nBrigada de Incêndio Florestal pelo CBMTO?")) +
  theme_estat() +
  scale_x_discrete(labels = wrap_format(20)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 40))
ggsave("resultados/analu/cargo-instrutor-unidade.pdf", width = 158, height = 93, units = "mm")

# Quais foram as maiores dificuldades enfrentadas durante o curso; * ----

a3 <- QUESTIONARIO_TCC[ , 12]

colnames(a3) <- "Dificuldades"

a3 %>%
  filter(is.na(Dificuldades)) %>%
  count() # 77 não foram instrutores

a3 <- a3 %>%
  na.omit() %>%
  mutate(dificuldades = strsplit(as.character(Dificuldades), ", [A-Z]")) %>%
unnest(dificuldades)

a3$dificuldades <- a3$dificuldades %>%
  str_replace("^ão", "Não") %>%
  str_replace("NNão", "Não") %>%
  str_replace("alta", "Falta") %>%
  str_replace("FFalta", "Falta") %>%
  str_replace("nsegurança", "Insegurança")  %>%
  str_replace("IInsegurança", "Insegurança")

a3 <- a3 %>%
  group_by(dificuldades) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(a3$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(a3$freq, " (", porcentagens, ")"))

ggplot(a3) +
  aes(
    x = fct_reorder(dificuldades, freq, .desc = T),
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.3, hjust = -0.1,
    size = 2.65
  ) +
  labs(x = "Informe qual(ais) foram as dificuldades enfrentadas\ndurante o(s) Curso de formação de brigada de\nincêndio florestal pelo CBMTO na qual o\n senhor(a) tenha sido instrutor(a)", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(breaks = seq(0, 40, by = 5), limits = c(0, 40)) +
  scale_x_discrete(labels = wrap_format(40)) +
  coord_flip()
ggsave("resultados/analu/dificuldades.pdf", width = 158, height = 93, units = "mm")

# Relação entre patente e se já foi instrutor ou não. ----

a4 <- QUESTIONARIO_TCC[ , c(4, 10)]
colnames(a4) <- c("Qual seu posto ou graduação?", "O senhor(a) já atuou como instrutor(a)\nde algum curso de formação de\nBrigada de Incêndio Florestal pelo CBMTO?")

a4 <- a4 %>%
  group_by(`Qual seu posto ou graduação?`, `O senhor(a) já atuou como instrutor(a)\nde algum curso de formação de\nBrigada de Incêndio Florestal pelo CBMTO?`) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(a4$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(a4$freq, " (", porcentagens, ")"))

ordem <- c("Soldado", "Cabo", "2º Sargento", "1º Sargento", "Subtenente", "2º Tenente", "Capitão", "Tenente Coronel")

a4$`Qual seu posto ou graduação?` <- factor(a4$`Qual seu posto ou graduação?`, levels = ordem)

ggplot(a4) +
  aes(
    x = `Qual seu posto ou graduação?`,
    y = freq,
    fill = `O senhor(a) já atuou como instrutor(a)\nde algum curso de formação de\nBrigada de Incêndio Florestal pelo CBMTO?`,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.3, hjust = -0.1,
    size = 2.65
  ) +
  labs(x = "Qual seu posto ou graduação?", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(breaks = seq(0, 35, by = 5), limits = c(0, 35)) +
  coord_flip()
ggsave("resultados/analu/cargo-instrutor.pdf", width = 158, height = 93, units = "mm")

########
