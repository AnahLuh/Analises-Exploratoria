source("rdocs/source/packages.R")

banco <- read_excel("banco/banco_final_UCB_2.xlsx")
banco <- banco[ , -1]

# 1 ----

an1 <- banco[, c(1,19)]
colnames(an1) <- c("email", "fontes")
an1 <- an1[!duplicated(an1$email), ]
an1 <- an1 %>% 
  separate_rows(fontes, sep = ", ")

an1 <- an1 %>%
  group_by(fontes) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an1$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an1$freq, " (", porcentagens, ")"))

ggplot(an1) +
  aes(
    x = fct_reorder(fontes, freq, .desc = T),
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Maiores fontes de informação sobre\n PLANEJAMENTO FAMILIAR", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(breaks = seq(from = 0, to = 140, by = 20), limits=c(0, 140)) +
  scale_x_discrete(labels = wrap_format(15)) +
  theme(axis.text.x = ggplot2::element_text(colour = "black", size = 7.5, angle=0))
ggsave("resultado/AnaLu/AnaLu_1.pdf", width = 158, height = 93, units = "mm")

# 2 ----

an2 <- banco[, 24]
colnames(an2) <- "ter_filhos"

an2 <- an2 %>%
  group_by(ter_filhos) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an2$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an2$freq, " (", porcentagens, ")"))

ggplot(an2) +
  aes(
    x = fct_reorder(ter_filhos, freq, .desc = T),
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Planeja ter filhos?", y = "Frequência") +
  theme_estat()
ggsave("resultado/AnaLu/AnaLu_2.pdf", width = 158, height = 93, units = "mm")

# 3 ----

an3 <- banco[, c(1,29)]
colnames(an3) <- c("email", "fatores")
an3 <- an3[!duplicated(an3$email), ]
an3 <- an3 %>% 
  separate_rows(fatores, sep = ", ")

an3 <- an3 %>%
  group_by(fatores) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an3$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an3$freq, " (", porcentagens, ")"))

ggplot(an3) +
  aes(
    x = fct_reorder(fatores, freq),
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.5,  hjust = -.1,
    size = 2.75
  ) +
  labs(x = "FATORES que influenciam na decisão de ter filhos\n ou na quantidade de filhos", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(breaks = seq(from = 0, to = 230, by = 20), limits=c(0, 230)) +
  scale_x_discrete(labels = wrap_format(15)) +
  theme(axis.text.y = ggplot2::element_text(colour = "black", size = 6, angle=0)) +
  coord_flip()
ggsave("resultado/AnaLu/AnaLu_3.pdf", width = 158, height = 93, units = "mm")


# 4 ----

an4 <- banco[, c(1,47)]
colnames(an4) <- c("email", "tecnicas_reproducao")
an4 <- an4[!duplicated(an4$email), ]
an4 <- an4 %>% 
  separate_rows(tecnicas_reproducao, sep = ", ")

an4 <- an4 %>%
  group_by(tecnicas_reproducao) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an4$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an4$freq, " (", porcentagens, ")"))

ggplot(an4) +
  aes(
    x = fct_reorder(tecnicas_reproducao, freq, .desc = T),
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "O que faria você não considerar as\n técnicas de reprodução assistida?", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(breaks = seq(from = 0, to = 140, by = 20), limits=c(0, 140)) +
  scale_x_discrete(labels = wrap_format(15)) +
  theme(axis.text.x = ggplot2::element_text(colour = "black", size = 7.5, angle=0))
ggsave("resultado/AnaLu/AnaLu_4.pdf", width = 158, height = 93, units = "mm")


# 5 ----

an5 <- banco[, c(41, 56)]
colnames(an5) <- c("reserva_ovariana", "ano_faculdade")
an5 <- an5 %>%
  group_by(reserva_ovariana, ano_faculdade) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an5$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an5$freq, " (", porcentagens, ")"))
legendas <- str_wrap(legendas, width = 1, whitespace_only = TRUE)
ordem <- c("Não sei",
           "Sim, mas nunca fiz testes de reserva ovariana",
           "Sim, inclusive já testei a minha reserva ovariana")

ggplot(an5) +
  aes(x = factor(reserva_ovariana, levels = ordem), y = freq, fill = ano_faculdade, label = legendas) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Você sabe o que é RESERVA OVARIANA?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2.1) +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, name="Ano da\nfaculdade")+
  scale_x_discrete(labels = wrap_format(26)) +
  scale_y_continuous(breaks = seq(from = 0, to = 65, by = 10), limits=c(0, 65))
ggsave("resultado/AnaLu/AnaLu_5.pdf", width = 158, height = 93, units = "mm")

# 6 ----

an6 <- banco[, c(42, 56)]
colnames(an6) <- c("criopreservacao", "ano_faculdade")
an6 <- an6 %>%
  group_by(criopreservacao, ano_faculdade) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an6$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an6$freq, " (", porcentagens, ")"))

ggplot(an6) +
  aes(x = criopreservacao, y = freq, fill = ano_faculdade, label = legendas) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Você sabe o que é\n CRIOPRESERVAÇÃO de óvulos?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2.1) +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, name="Ano da\nfaculdade")+
  scale_y_continuous(breaks = seq(from = 0, to = 60, by = 10), limits=c(0, 60))
ggsave("resultado/AnaLu/AnaLu_6.pdf", width = 158, height = 93, units = "mm")

# 7 ----

an7 <- banco[, c(43, 45)]
colnames(an7) <- c("cogita_criopreservacao", "idade_criopreservacao")
an7 <- an7 %>%
  group_by(cogita_criopreservacao, idade_criopreservacao) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an7$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an7$freq, " (", porcentagens, ")"))
legendas <- str_wrap(legendas, width = 1, whitespace_only = TRUE)

ordem <- c("20-25 anos", "25-30 anos", "30-35 anos", "35-40 anos", "> 40 anos")

ggplot(an7) +
  aes(x = cogita_criopreservacao, y = freq, fill = factor(idade_criopreservacao, levels = ordem), label = legendas) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Você cogita fazer\n CRIOPRESERVAÇÃO DE ÓVULOS?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 1.85) +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, name="Faixa\n etária")+
  scale_y_continuous(breaks = seq(from = 0, to = 70, by = 10), limits=c(0, 70))
ggsave("resultado/AnaLu/AnaLu_7.pdf", width = 158, height = 93, units = "mm")

# 8 ----

an8 <- banco[, c(1, 43, 46)]
colnames(an8) <- c("email", "cogita_criopreservacao", "Razões")
an8 <- an8[!duplicated(an8$email), ]
an8 <- an8 %>% 
  separate_rows(Razões, sep = ", ")

an8 <- an8 %>%
  group_by(cogita_criopreservacao, Razões) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an8$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an8$freq, " (", porcentagens, ")"))
legendas <- str_wrap(legendas, width = 1, whitespace_only = TRUE)

ordem <- c("Segurança reprodutiva para investir na carreira", "Condição médica (cirurgia ovariana/quimioterapia/similar)",
           "Ovodoação para familiar", "Outros", "Não se aplica")
ggplot(an8) +
  aes(x = cogita_criopreservacao, y = freq, fill = factor(Razões, levels = ordem), label = legendas) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Você cogita fazer\n CRIOPRESERVAÇÃO DE ÓVULOS?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 1.8) +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, name="Razões")+
  scale_fill_manual(labels = ~ stringr::str_wrap(.x, width = 13), values = cores_estat, name="Razões") +
  scale_y_continuous(breaks = seq(from = 0, to = 120, by = 20), limits=c(0, 120)) +
  theme(axis.title.x = ggplot2::element_text(colour = "black", size = 10),
        axis.text.x = ggplot2::element_text(colour = "black", size = 9, angle=0),
        legend.text = element_text(size = 6.5))
ggsave("resultado/AnaLu/AnaLu_8.pdf", width = 158, height = 93, units = "mm")

# 9 ----

an9 <- banco[, c(43, 51)]
colnames(an9) <- c("cogita_criopreservacao", "empregador")

an9 <- an9 %>%
  group_by(cogita_criopreservacao, empregador) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an9$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an9$freq, " (", porcentagens, ")"))
legendas <- str_wrap(legendas, width = 1, whitespace_only = TRUE)

ggplot(an9) +
  aes(x = cogita_criopreservacao, y = freq, fill = empregador, label = legendas) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Você cogita fazer\n CRIOPRESERVAÇÃO DE ÓVULOS?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2.8) +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, name="Se o empregador ou o plano de saúde\npagassem, mesmo que parcialmente,\nseria mais provável congelar os óvulos?")+
  scale_y_continuous(breaks = seq(from = 0, to = 120, by = 20), limits=c(0, 120)) +
  theme(axis.title.x = ggplot2::element_text(colour = "black", size = 10),
        axis.text.x = ggplot2::element_text(colour = "black", size = 9, angle=0),
        legend.text = element_text(size = 10))
ggsave("resultado/AnaLu/AnaLu_9.pdf", width = 158, height = 93, units = "mm")

# 10 ----

an10 <- banco[, c(1, 18, 56)]
colnames(an10) <- c("email","planejamento_familiar", "ano_faculdade")
an10 <- an10[!duplicated(an10$email), ]
an10 <- an10 %>% 
  separate_rows(planejamento_familiar, sep = ", ")

an10 <- an10 %>%
  group_by(planejamento_familiar, ano_faculdade) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an10$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an10$freq, " (", porcentagens, ")"))

ggplot(an10) +
  aes(x = planejamento_familiar, y = freq, fill = ano_faculdade, label = legendas) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Você sabe o que é\n CRIOPRESERVAÇÃO de óvulos?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2.1) +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, name="Ano da\nfaculdade")+
  scale_y_continuous(breaks = seq(from = 0, to = 60, by = 10), limits=c(0, 60))
ggsave("resultado/AnaLu/AnaLu_10.pdf", width = 158, height = 93, units = "mm")


ggplot(an10) +
  aes(x = planejamento_familiar, y = freq, fill = ano_faculdade, label = legendas) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "O que você entende por\nPLANEJAMENTO FAMILIAR?", y = "Frequência relativa") +
  geom_text(position = position_fill(vjust = 0.5), size = 2, colour = "white") +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect(colour =" black", fill=" white")) +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 12, angle=0),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values =  cores_estat, name="Ano da\nfaculdade") +
  scale_colour_manual(values =  cores_estat) +
  theme(axis.text.x = element_text(size = 6.4)) +
  scale_x_discrete(labels = wrap_format(18))
ggsave("resultado/AnaLu/AnaLu_10.pdf", width = 158, height = 93, units = "mm")
