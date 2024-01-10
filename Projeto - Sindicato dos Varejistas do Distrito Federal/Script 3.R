# CARREGANDO OS PACOTES E BANCOS ----
library(readxl)
library(tidyverse)


comerciantes <- read_excel("banco/comerciantes_limpo.xlsx")
consumidores <- read_excel("banco/consumidores.xlsx")

# LIMPEZA DOS DADOS - COMERCIANTES------------------------------------------------------
colnames(comerciantes) <- c("ID", "RA", "nome_loja", "localizacao_loja", "segmento_loja",
                            "produtos_potencial_venda", "previsao_faturamento", "pretende_fazer_promocao",
                            "quando_promocao", "produtos_para_colocar_promocao", "disposicao_gasto_consumidor")

colnames(consumidores) <- c("ID", "RA", "genero", "idade", "pretende_comprar_presente",
                            "porque_nao_compra_presente", "comprar_apenas_1_presente",
                            "quantos_presentes_comprara", "onde_compra_presentes", "considera_importante_promocao",
                            "categoria_presente", "quanto_pretende_gastar_presente", "quando_pretende_comprar", 
                            "pretende_presentear_mais_de_uma_pessoa", "quem_presentearia")

## COMERCIANTES ----
### nome_loja ----
comerciantes$nome_loja <- str_to_title(comerciantes$nome_loja)

### segmento_loja ----
x <- comerciantes %>%
  select(segmento_loja) %>%
  group_by(segmento_loja) %>%
  summarise(n = n())

comerciantes$segmento_loja[comerciantes$segmento_loja == "Pet"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Armarinho (lã, toalha)"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Animais"] <- "Pet Shop"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Armarinho (Lã, Toalha)"] <- "Armarinho"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Móveis"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "AGROPECUARIA"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Animais"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "SERVIÇO DE TELECOMUNICAÇOES"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Agropet"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Agropecuária"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pet Shop"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Utilidades"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Óculos"] <- "Relógios, jóias ou acessórios"
comerciantes$segmento_loja[comerciantes$segmento_loja == "áudio profissional e instrumento musical"] <- "Instrumentos Musicais"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Papelaria"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Ótica"] <- "Relógios, jóias ou acessórios"
comerciantes$segmento_loja[comerciantes$segmento_loja == "móveis planejados"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Vestuário e viagem"] <- "Vestuário"
comerciantes$segmento_loja[comerciantes$segmento_loja == "armarinho"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Vestuário e artigos de viagem"] <- "Vestuário"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Alimentício"] <- "Mercados e Conveniências"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Bebida"] <- "Mercados e Conveniências"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Brinquedos infantis"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Artigos de festa"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Artigos para festas"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Banca"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Caça e pesca"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Clínica Veterinária"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Clínica de podologia"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Comércio varejista (mercado)"] <- "Mercados e Conveniências"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Confecções"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Construção"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Decorações"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Diversos"] <- "Variedade"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Drogaria"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Embalagens"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Enxovais."] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Estacionamento"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Floricultura"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Floricultura e presentes"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Frios"] <- "Mercados e Conveniências"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Hospital veterinário"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Hospitalar"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Loja de presente"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Loja de tecidos"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Materiais de Construção"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Mercado"] <- "Mercados e Conveniências"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Mobiliário Executivo"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Móveis Planejados"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Móveis."] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Papelaria, Brinquedos e utilidades"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Infantil"] <- "Infantil"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pet shop"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pets"] <- "Petshops e Agropecuária"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Presentes (1,99)"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Presentes, brinquedos."] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Produtos de sinalização"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Salão e Barbearia"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Tapeçaria e decorações"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Tecido"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Varejista"] <- "Mercados e Conveniências"
comerciantes$segmento_loja[comerciantes$segmento_loja == "cosméticos e medicamentos (farmácia)"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "equipamentos industriais"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "skate"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Variedade"] <- "Variedades"

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}
comerciantes$segmento_loja <- str_to_lower(comerciantes$segmento_loja)
comerciantes$segmento_loja <- capFirst(comerciantes$segmento_loja)

### produtos_potencial_venda ----
# Como se trata de muitos itens em uma mesma célula, resolvi criar um dataframe à parte.

potencial_venda <- comerciantes %>%
  select(RA, produtos_potencial_venda)

potencial_venda <- potencial_venda %>% 
  separate_longer_delim(produtos_potencial_venda, delim = ",")
potencial_venda <- potencial_venda %>% 
  separate_longer_delim(produtos_potencial_venda, delim = " e ")

potencial_venda$produtos_potencial_venda <- trimws(potencial_venda$produtos_potencial_venda)
potencial_venda$produtos_potencial_venda <- str_to_lower(potencial_venda$produtos_potencial_venda)
potencial_venda$produtos_potencial_venda <- capFirst(potencial_venda$produtos_potencial_venda)

### previsão_faturamento ----

comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "\"Não quis informar\""] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não sabe informar"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não quis informar ou não foi autorizado"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "."] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "não sabe informar"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não sei dizer"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não quis informar"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NA"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Nao sei dizer"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não sei informar"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nao sabe"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "não quis informar"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "500 600 reais"] <- "550"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "N sabe"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Nao quis informar ou nao foi autorizado"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Nao sabe informar"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Nao sabem"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não  sabe informar"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não foi autorizado"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não foi autorizado."] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não informou"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não posso dizer"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não sabe informa"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não sabe informal"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não sabe/ não quer informar"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não sei dizer, todo ano muda"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não soube informar"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não tem certeza"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não tem, verificar com o shopping"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Previsão de aumento do faturamento (o responsável não autorizou o compartilhamento do valor exato de aumento do faturamento)."] <- "Previsão de aumento"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Zero"] <- "0"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "aumento de 30% 50 mil"] <- "50000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nao sabe informar"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "não foi autorizado"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "não podem responder"] <- "Não informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "não soube responder…"] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "100 mil"] <- "100000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "100 mol"] <- "100000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "2 mil reais no dia"] <- "2000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "250 mil"] <- "250000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "55 mil"] <- "55000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "80 mil"] <- "80000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Previsão de faturamento pequena."] <- "Previsão de faturamento pequeno"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Previsão de Aumento."] <- "Previsão de aumento"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Não sabe informar."] <- "Não sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Entre 30.000 e 50.000 reais"] <- "40000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Entre 40mil e 50mil"] <- "45000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Expectativa de aumento do faturamento."] <- "Previsão de aumento"


### produtos_para_colocar_promocao ----

produtos_promocao <- comerciantes %>%
  select(RA, produtos_para_colocar_promocao)

produtos_promocao <- produtos_promocao %>% separate_longer_delim(produtos_para_colocar_promocao, delim = ",")
produtos_promocao <- produtos_promocao %>% separate_longer_delim(produtos_para_colocar_promocao, delim = " e ")

produtos_promocao$produtos_para_colocar_promocao <- trimws(produtos_promocao$produtos_para_colocar_promocao)
produtos_promocao$produtos_para_colocar_promocao <- str_to_lower(produtos_promocao$produtos_para_colocar_promocao)
produtos_promocao$produtos_para_colocar_promocao <- capFirst(produtos_promocao$produtos_para_colocar_promocao)



#ANÁLISES - COMERCIANTES-------------------

###ra-----
RA_comercio <- comerciantes %>%
  select(RA) %>%
  group_by(RA) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

ggplot(RA_comercio) +
  aes(x = fct_reorder(RA, freq, .desc=T), y = freq, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7)+
  labs(x = "Comércios entrevistados por região administrativa", y = "Frequência") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.2,
            size = 3) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 10))
ggsave("resultados/RAs_comercio.pdf", width = 178, height = 100, units = "mm")

###-segmento----------------

segmento <- comerciantes %>%
  select(segmento_loja) %>%
  group_by(segmento_loja) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

###segmento por RA
segmento_tabela <- comerciantes %>%
  group_by(RA, segmento_loja) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  mutate(freq_relativa = freq / sum(freq) * 100,
         Freq = sprintf("%.2f%%", freq_relativa),
         label = str_c(freq, "(", Freq, ")")
         ) %>%
  select(-freq_relativa, -freq, -Freq) %>%
  pivot_wider(names_from = RA, values_from = label, values_fill = "0 (0%)")

xtable(segmento_tabela)

###-localização-------------------------

comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Centro comercial"] <- "Centro comercial"
comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Comercial"] <- "Centro comercial"
comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Entre quadras"] <- "Centro comercial"
comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Feira dos importados"] <- "Feira"
comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Feira"] <- "Feira"
comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Galeria"] <- "Galeria"
comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Mercado Assaí"] <- "Outros"
comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Ruas comerciais movimentadas"] <- "Ruas comerciais movimentadas"
comerciantes$localizacao_loja[comerciantes$localizacao_loja == "Shopping"] <- "Shopping"

loc <- comerciantes %>%
  select(localizacao_loja) %>%
  group_by(localizacao_loja) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

cores_estat <- c('#A11D21',
                 '#663333',
                 '#FF6600','#CC9900'
                 ,'#CC9966','#999966',
                 '#006606','#008091',
                 '#003366','#041835',
                 '#666666')

ggplot(loc) +
  aes(x = fct_reorder(localizacao_loja, freq, .desc=T), y = freq, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Localização dos coméricos entrevistados", y = "Frequência") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.2,
            size = 3) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 10))+
  scale_x_discrete(labels=c("Centros\n Comerciais","Ruas comerciais\n movimentadas","Feira", "Galeria",
                            "Outros", "Shopping"
                            ))
ggsave("resultados/loc_comercio.pdf", width = 178, height = 100, units = "mm")

###-Produtos com maior potencial de venda  (**)-----

produtos_pot<- potencial_venda %>%
  select(produtos_potencial_venda) %>%
  add_count(produtos_potencial_venda) %>%
  distinct()

coluna_produto <- potencial_venda$produtos_potencial_venda
lista_produtos <- as.list(coluna_produto)



# Dicionário de agrupamento
agrupamento <- list(
  "Camisa polo" = c("Camiseta polo", "Polo","Camisa polo",
                    'Camisas polo',"Camisas golo polo",
                    "Camisetas polo"),
  
  "Camisas, camisetas e blusas" = c("Camisas de esporte",'Camisa',
                                    "Camiseta","Camisas","Camisas","Blusas masculinas",
                                    "Uma blusa personalizada para o dia dos pais","Camiseta de time (vestuário)","Camisa polo"),
  
  "Bermudas e calças"=c("Bermuda","Bermudas","Calça jeans","Calça","Calças"),

  
  "Perfumes" = c("Perfumes", "Perfumes masculinos","Malbec","Perfumaria masculina"),
  
"Sapatos masculinos" = c("Slip","Mocacins","Tenis",'Tênis',
                         "Sapato","Sapatos","Sapatênis","Calçados esportivos",
                         "Os calçados masculinos","Chinelo","Sandália"),

"Acessórios para celular e/ou computadores" = c("Acessórios para celulares",
                              "Acessórios para celulares.",
                              'Capinha de celular',"Fone de ouvido","Acessórios de computador"),

"Eletrônicos" = c('Celulares',"Celular","Computadores.","Caixa de som",
                  "Eletrodomesticos",'Tv',"Notebook","Caixa de música"),

"Produtos para barbar"=c("Barba",'Pós barba','Pomada para barba',"Kit de barba","Aparador de barba"),

"Produtos de cama e banho" = c("Travesseiros", "Travisseiros","Travesseiro",
"Enxovais","Edredons…","Cobertores toalhas","Toalha","Atoalhados","Roupão"),

"Cosméticos" = c("Cosméticos","kit completo masculino com creme","Shampoo masculino",
                 'Shampoo',"Shampoo condicionador","Shampoo",	
                 "Hidratande facial/ base","Pomada","Condicionador"),

"Chocolates e/ou doces"=c("Doces.","Chocolate"),

"Acessórios"=c("Boné","Acessórios","Acessório","Relógios","Relógio","Bijuteria",
               "Pulseiras","Pulceiras","Gravata","Bolsas",
               "Óculos de sol","Óculos de sol","Bolsa viagem","Bone","Bolsa",
               "Bolsa viagem","Cordões","Correntes","Correntes de prata",""),

"Roupas masculinas no geral"= c("Terno","Roupa","Roupas","Roupas masculinas","Cueca","Meias","Pijamas"),
'Itens de casa e decoração'=c("Sofá","Quadros","Tapetes","Colchões","Colchão","Poltronas","Posters",
                              'Ferramentas para decoração',
                              "Decorativa","Poltrona","Cortinas","Enfeites","Acessórios para casa",
                              "Cadeira operacional"),

"Outros"=c('Corte','Fraldas','Cachorro',
"Maquina de cortar cabelo","Balões","Copos","Furadeira",
"Ferramentas","Cestas",
"Pijama","Os uniformes","Lembrança",
"Presentes","Tecido de camiseta","Decoração","Aquário",
"Bomba de aquario","Mochila","Ferramentas para construção","Arma","Munição","Purificador de água","	
Porta-retrato","Copo","Bônus de internet","Caixa de presente","Maquina de gelo","Serviços de massagem ou serviços de podologia hidratação",
"Coleção pai",'Filho','Cortador de cabelo',"Vários utensilhos","Barbeador","Bikes","Cabos","Brindes","Caneca",'Canecas',"Caixas de joia"),
"Produtos relacionados a alimentação e bebidas alcoólicas" = c("Creatina","Castanhas", "Cerveja",
                                                               "Carnes para churrasco","Por exemplo a coleção de vinho",
                                                               "Produtos de taça e vinho",
                                                               "Carne","Cervejeira",
                                                               "Vinhos","Utensílios de vinho",
                                                               "Artigos para bebidas",
                                                               "Balde de cerveja","Cachaça",),
"Não há produtos em potencial" =c('Nao possui','Não há produtos em potencial',
'Nao','Nenhum',"Nehum","-","Não te muito","Não enxerga nenhum","A loja não se aplica","Não se aplica."))

# Função para substituir os itens na lista de produtos
agrupar_produtos <- function(lista, dicionario) {
  for (chave in names(dicionario)) {
    sinonimos <- dicionario[[chave]]
    lista[lista %in% sinonimos] <- chave
  }
  return(lista)
}

lista_agrupada <- agrupar_produtos(lista_produtos, agrupamento)
print(lista_agrupada)

#mesma coisa com as RAs
coluna_ra <- potencial_venda$RA
lista_ra <- as.list(coluna_ra)

#novo dataframe
vetor_produtos <- unlist(lista_agrupada)
vetor_RAs <- unlist(lista_ra)
produtos_pot_novo <- data.frame(vetor_RAs,vetor_produtos)


#contagem geral
prod_cont <- produtos_pot_novo %>%
  select(vetor_produtos) %>%
  group_by(vetor_produtos) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )


ggplot(
  prod_cont) %>%
  aes(x = fct_reorder(vetor_produtos, freq, .desc=T), y = freq, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) + 
  labs(x = "Pretende comprar presente para o dia dos pais?", y = "Frequ?ncia") +
  scale_y_continuous(breaks = seq(0, 350, by = 50), limits = c(0, 350)) +
  theme_estat(axis.text.x = element_text(size = 10))
ggsave("pf23023 - Sindivarejista/resultados/pretende_comprar_presente.pdf", width = 158, height = 93, units = "mm")

#-Previsão de faturamento (aqui, fazer um quadro de medidas geral + box plot pro DF todo - não faremos distinção das RAs)
#-Pretende realizar campanha ou promoção?  (*)
#-Quando? (**)
#-Qual ou quais produtos pretende colocar em promoção ou em campanha? (**)
#-Quanto o comerciante acredita que o consumidor estará disposto a gastar em média na sua loja no período do dia dos pais?  (*)