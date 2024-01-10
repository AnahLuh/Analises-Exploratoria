source("rdocs/source/packages.R")

## Atualizar o banco
questionario <- read_excel("banco/banco completo.xlsx")
cores <- c("#171F32", "#FF9400", "#FBC335", "#AEA923", "#6C8B22", "#346924", "#173220", "#363636", "#CFCFCF")
ordem_1D <- c("Péssimo", "Regular", "Bom", "Ótimo")

# (an1) Pergunta 1 por federação ----

an1 <- questionario[, c(3,21)]
colnames(an1) <- c('federacao', 'pergunta1')
an1 <- an1 %>%
  group_by(pergunta1, federacao) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an1$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an1$freq, " (", porcentagens, ")"))

ggplot(an1) +
  aes(x = factor(pergunta1, levels = ordem_1D), y = freq, fill = federacao, label = legendas) +
  geom_bar(stat = "identity") +
  labs(x = "Federação", y = "Frequência") +
  geom_text(position = position_stack(), vjust = 1, hjust = 0.5, size = 2.5, colour = "white") +
  theme_estat() +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=0),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values =  cores, name="") +
  scale_colour_manual(values =  cores) +
  theme(axis.text.x = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(from = 0, to = 90, by = 10), limits=c(0, 90))
ggsave("resultados/Ana Lu/federacao_pergt1.pdf", width = 198, height = 133, units = "mm")

# (an2) Pergunta 1 com pergunta 1 do bloco A ----

an2 <- questionario[, c(4,21)]
colnames(an2) <- c('pergunta1A', 'pergunta1D')
an2 <- an2 %>%
  group_by(pergunta1D, pergunta1A) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an2$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an2$freq, " (", porcentagens, ")"))
legendas <- str_wrap(legendas, width = 1, whitespace_only = TRUE)

ggplot(an2) +
  aes(x = factor(pergunta1D, levels = ordem_1D), y = freq, fill = pergunta1A, label = legendas) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Resposta à pergunta 1", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2.8) +
  theme_estat() +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=0),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values =  cores, name="") +
  scale_colour_manual(values =  cores) +
  theme(axis.text.x = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(from = 0, to = 55, by = 10), limits=c(0, 55))
ggsave("resultados/Ana Lu/pergt1D_pergt1A.pdf", width = 198, height = 133, units = "mm")

# (an3) Pergunta 1 com pergunta 2 do bloco A ----

an3 <- questionario[, c(5,21)]
colnames(an3) <- c('pergunta2A', 'pergunta1D')
an3 <- an3 %>%
  group_by(pergunta1D, pergunta2A) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

an3$pergunta2A <- an3$pergunta2A %>%
  str_replace("Mídia impressa \\(jornais ou revistas\\)", "Mídia impressa") %>%
  str_replace("Portais de notícias \\(UOL, G1, Folha Online, jornais, revistas digitais\\)",
              "Portais de notícias") %>%
  str_replace("Redes sociais \\(Facebook, Instagram, Twitter, LinkedIn\\)",
              "Redes sociais") %>%
  str_replace("Portais de Governo \\(ANAC, SAC e Ministério do Transporte, etc\\)",
              "Portais de Governo")

porcentagens <- str_c(an3$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an3$freq, " (", porcentagens, ")"))
# legendas <- str_wrap(legendas, width = 1, whitespace_only = TRUE)

ggplot(an3) +
  aes(x = factor(pergunta1D, levels = ordem_1D) , y = freq, fill = pergunta2A, label = legendas) +
  geom_bar(stat = "identity") +
  labs(x = "Resposta à pergunta 1", y = "Frequência") +
  geom_text(position = position_stack(), vjust = 1, hjust = 0.5, size = 2.5, colour = "white") +
  theme_estat() +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=0),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values =  cores, name="") +
  scale_colour_manual(values =  cores) +
  theme(axis.text.x = element_text(size = 7.6)) +
  scale_y_continuous(breaks = seq(from = 0, to = 90, by = 10), limits=c(0, 90))
ggsave("resultados/Ana Lu/pergt1D_pergt2A.pdf", width = 198, height = 133, units = "mm")

# (an4) Pergunta 1 com pergunta 3 do bloco A ----

an4 <- questionario[, c(6,21)]
colnames(an4) <- c('pergunta3A', 'pergunta1D')
an4 <- an4 %>%
  group_by(pergunta1D, pergunta3A) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an4$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an4$freq, " (", porcentagens, ")"))
legendas <- str_wrap(legendas, width = 1, whitespace_only = TRUE)

ordem_1D <- c("Péssimo", "Regular", "Bom", "Ótimo")
ggplot(an4) +
  aes(x = factor(pergunta1D, levels = ordem_1D), y = freq, fill = pergunta3A, label = legendas) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Resposta à pergunta 1", y = "Frequência") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 2.75) +
  theme_estat() +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=0),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values =  cores, name="") +
  scale_colour_manual(values =  cores) +
  scale_y_continuous(breaks = seq(from = 0, to = 55, by = 10), limits=c(0, 55)) +
  theme(axis.text.x = element_text(size = 12))
ggsave("resultados/Ana Lu/pergt1D_pergt3A.pdf", width = 198, height = 133, units = "mm")

# (an5) Pergunta 1 com pergunta 1 do bloco A por federação ----

an5 <- questionario[, c(3,4,21)]
colnames(an5) <- c('federacao', 'pergunta1A', 'pergunta1D')
an5 <- an5 %>%
  group_by(pergunta1D, federacao, pergunta1A) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an5$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an5$freq, " (", porcentagens, ")"))

ggplot(an5) +
  aes(x = factor(pergunta1D, levels = ordem_1D), y = freq, fill = pergunta1A, label = legendas) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(federacao ~ .) +
  labs(x = "Resposta à pergunta 1", y = "Frequência") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.23, colour = "white") +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white")) +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=0),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values =  cores, name="") +
  scale_colour_manual(values =  cores) +
  theme(axis.text.x = element_text(size = 7.4))
ggsave("resultados/Ana Lu/pergt1D_pergt1A_federacao.pdf", width = 198, height = 133, units = "mm")

# (an6) Pergunta 1 com pergunta 2 do bloco A por federação ----

an6 <- questionario[, c(3,5,21)]
colnames(an6) <- c('federacao','pergunta2A', 'pergunta1D')
an6 <- an6 %>%
  group_by(pergunta1D, federacao, pergunta2A) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

an6$pergunta2A <- an6$pergunta2A %>%
  str_replace("Mídia impressa \\(jornais ou revistas\\)", "Mídia impressa") %>%
  str_replace("Portais de notícias \\(UOL, G1, Folha Online, jornais, revistas digitais\\)",
              "Portais de notícias") %>%
  str_replace("Redes sociais \\(Facebook, Instagram, Twitter, LinkedIn\\)",
              "Redes sociais") %>%
  str_replace("Portais de Governo \\(ANAC, SAC e Ministério do Transporte, etc\\)",
              "Portais de Governo")

porcentagens <- str_c(an6$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an6$freq, " (", porcentagens, ")"))

ggplot(an6) +
  aes(x = factor(pergunta1D, levels = ordem_1D), y = freq, fill = pergunta2A, label = legendas) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(federacao ~ .) +
  labs(x = "Resposta à pergunta 1", y = "Frequência") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.1, colour = "white") +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white")) +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=0),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values =  cores, name="") +
  scale_colour_manual(values =  cores) +
  theme(axis.text.x = element_text(size = 7.4))
ggsave("resultados/Ana Lu/pergt1D_pergt2A_federacao.pdf", width = 198, height = 133, units = "mm")

# (an7) Pergunta 1 com pergunta 3 do bloco A por federação ----

an7 <- questionario[, c(3, 6,21)]
colnames(an7) <- c('federacao','pergunta3A', 'pergunta1D')
an7 <- an7 %>%
  group_by(pergunta1D, federacao, pergunta3A) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(an7$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(an7$freq, " (", porcentagens, ")"))

ordem_1D <- c("Péssimo", "Regular", "Bom", "Ótimo")
ggplot(an7) +
  aes(x = factor(pergunta1D, levels = ordem_1D), y = freq, fill = pergunta3A, label = legendas) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(federacao ~ .) +
  labs(x = "Resposta à pergunta 1", y = "Frequência") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.24, colour = "white") +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect(colour =" black", fill=" white")) +
  theme(    axis.title.y = ggplot2::element_text(colour = "black", size = 12),
            axis.title.x = ggplot2::element_text(colour = "black", size = 12),
            axis.text = ggplot2::element_text(colour = "black", size = 9.5),
            axis.text.x = ggplot2::element_text(colour = "black", size = 9.5, angle=0),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            legend.position = "top") +
  scale_fill_manual(values =  cores, name="") +
  scale_colour_manual(values =  cores) +
  theme(axis.text.x = element_text(size = 7.4))
ggsave("resultados/Ana Lu/pergt1D_pergt3A_federacao.pdf", width = 198, height = 133, units = "mm")

