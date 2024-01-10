# CARREGANDO OS PACOTES E BANCOS ----
library(readxl)
library(tidyverse)
library(xtable)


comerciantes <- read_excel("pf23023 - Sindivarejista/banco/comerciantes.xlsx")
consumidores <- read_excel("pf23023 - Sindivarejista/banco/consumidores.xlsx")


# LIMPEZA DOS DADOS ----
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

comerciantes$segmento_loja[comerciantes$segmento_loja == "Pet"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Armarinho (lã, toalha)"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Animais"] <- "Pet Shop"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Armarinho (Lã, Toalha)"] <- "Armarinho"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Móveis"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "AGROPECUARIA"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Animais"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "SERVIÇO DE TELECOMUNICAÇOES"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Agropet"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Agropecuária"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pet Shop"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Utilidades"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Óculos"] <- "Relógios, jóias ou acessórios"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Áudio profissional e instrumento musical"] <- "Instrumentos Musicais"
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
comerciantes$segmento_loja[comerciantes$segmento_loja == "Clínica Veterinária"] <- "Outros"
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
comerciantes$segmento_loja[comerciantes$segmento_loja == "Hospital veterinÃ¡rio"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Hospitalar"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Loja de presente"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Loja de tecidos"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Materiais de ConstruÃ§Ã£o"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Mercado"] <- "Mercados e ConveniÃªncias"
comerciantes$segmento_loja[comerciantes$segmento_loja == "MobiliÃ¡rio Executivo"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "MÃ³veis Planejados"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "MÃ³veis."] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Papelaria, Brinquedos e utilidades"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Infantil"] <- "VestuÃ¡rio"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pet shop"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pets"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Presentes (1,99)"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Presentes, brinquedos."] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Produtos de sinalizaÃ§Ã£o"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "SalÃ£o e Barbearia"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Sex shop"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "TapeÃ§aria e decoraÃ§Ãµes"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Tecido"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Varejista"] <- "Mercados e ConveniÃªncias"
comerciantes$segmento_loja[comerciantes$segmento_loja == "cosmÃ©ticos e medicamentos (farmÃ¡cia)"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "equipamentos industriais"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "skate"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Variedade"] <- "Variedades"

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}
comerciantes$segmento_loja <- str_to_lower(comerciantes$segmento_loja)
comerciantes$segmento_loja <- capFirst(comerciantes$segmento_loja)

### produtos_potencial_venda ----
# Como se trata de muitos itens em uma mesma cÃ©lula, resolvi criar um dataframe Ã  parte.

potencial_venda <- comerciantes %>%
  select(RA, produtos_potencial_venda)

potencial_venda <- potencial_venda %>% separate_longer_delim(produtos_potencial_venda, delim = ",")
potencial_venda <- potencial_venda %>% separate_longer_delim(produtos_potencial_venda, delim = " e ")

potencial_venda$produtos_potencial_venda <- trimws(potencial_venda$produtos_potencial_venda)
potencial_venda$produtos_potencial_venda <- str_to_lower(potencial_venda$produtos_potencial_venda)
potencial_venda$produtos_potencial_venda <- capFirst(potencial_venda$produtos_potencial_venda)

### previsÃ£o_faturamento ----

comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "\"NÃ£o quis informar\""] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o sabe informar"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o quis informar ou nÃ£o foi autorizado"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "."] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nÃ£o sabe informar"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o sei dizer"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o quis informar"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NA"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Nao sei dizer"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o sei informar"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nao sabe"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nÃ£o quis informar"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "500 600 reais"] <- "550"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "N sabe"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Nao quis informar ou nao foi autorizado"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Nao sabe informar"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Nao sabem"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o  sabe informar"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o foi autorizado"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o foi autorizado."] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o informou"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o posso dizer"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o sabe informa"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o sabe informal"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o sabe/ nÃ£o quer informar"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o sei dizer, todo ano muda"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o soube informar"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o tem certeza"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o tem, verificar com o shopping"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "PrevisÃ£o de aumento do faturamento (o responsÃ¡vel nÃ£o autorizou o compartilhamento do valor exato de aumento do faturamento)."] <- "PrevisÃ£o de aumento"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Zero"] <- "0"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "aumento de 30% 50 mil"] <- "50000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nao sabe informar"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nÃ£o foi autorizado"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nÃ£o podem responder"] <- "NÃ£o informado"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "nÃ£o soube responderâ€¦"] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "100 mil"] <- "100000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "100 mol"] <- "100000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "2 mil reais no dia"] <- "2000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "250 mil"] <- "250000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "55 mil"] <- "55000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "80 mil"] <- "80000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "PrevisÃ£o de faturamento pequena."] <- "PrevisÃ£o de faturamento pequeno"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "PrevisÃ£o de Aumento."] <- "PrevisÃ£o de aumento"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "NÃ£o sabe informar."] <- "NÃ£o sabe"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Entre 30.000 e 50.000 reais"] <- "40000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Entre 40mil e 50mil"] <- "45000"
comerciantes$previsao_faturamento[comerciantes$previsao_faturamento == "Expectativa de aumento do faturamento."] <- "PrevisÃ£o de aumento"

### produtos_para_colocar_promocao ----

produtos_promocao <- comerciantes %>%
  select(RA, produtos_para_colocar_promocao)

produtos_promocao <- produtos_promocao %>% separate_longer_delim(produtos_para_colocar_promocao, delim = ",")
produtos_promocao <- produtos_promocao %>% separate_longer_delim(produtos_para_colocar_promocao, delim = " e ")

produtos_promocao$produtos_para_colocar_promocao <- trimws(produtos_promocao$produtos_para_colocar_promocao)
produtos_promocao$produtos_para_colocar_promocao <- str_to_lower(produtos_promocao$produtos_para_colocar_promocao)
produtos_promocao$produtos_para_colocar_promocao <- capFirst(produtos_promocao$produtos_para_colocar_promocao)

## CONSUMIDORES ----
### idade ----

consumidores$idade <- consumidores$idade %>%
  str_replace("anos", "")

consumidores$idade[consumidores$idade == "55  ou mais"] <- "55+"
consumidores$idade[consumidores$idade == "Menos de 18 "] <- "< 18"

consumidores$idade <- trimws(consumidores$idade)
### porque_nao_compra_presente ----

x <- consumidores %>% select(porque_nao_compra_presente) %>% group_by(porque_nao_compra_presente) %>% summarise(n = n())

consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Tem outra prioridade de gasto no momento"] <- "Outra prioridade de gasto"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "não tem pai"] <- "Não tem pai"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Apenas não tem interesse na compra"] <- "Não tem interesse na compra"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "o pai está longe"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "o pai já faleceu"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai mora longe"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Falecidos."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O pai mora longe."] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não tem mais pai vivo."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O pai já faleceu."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Mora longe"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Mora longe."] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não tem pai, faleceu"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Parente mora distante"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "pai falecido"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "pai mora em outro estado"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai não mora perto"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não possui pai"] <- "Não tem pai"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "País falecidos"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Familiar mora longe"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não ter mais pai e seu filho não ser pai."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não tem pai."] <- "Não tem pai"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai mora longe e não tem contato."] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Distância/Pai mora em outro estado ou cidade"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai faleceu"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "falecido"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não ter pai."] <- "Não tem pai"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Distancia e morte"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "falecidos"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Ausência"] <- "Ausência paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Distância"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não ter presença de pais."] <- "Ausência paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não ter relação com pai."] <- "Ausência paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não vai ver o pai"] <- "Ausência paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai falecido."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "distancia"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "distante"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "A mãe vai comprar por ele"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Já costuma presentear durante o ano, e no dia dos pais não compra presentes."] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não conhece"] <- "Ausência paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não gosta de presente"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não quis comunicar"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Não tem boa relação com o pai"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O pai não faz questão."] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O pai não gosta de presente"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O presente é a própria presença."] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Ser divorciada."] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "não tem o costume de comemorar"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Falecimento"] <- "Pai falecido"

# ANALISES - CONSUMIDORES ----
## GENERO ----

genero_consumidores <- consumidores %>%
  select(genero) %>%
  add_count(genero) %>%
  distinct()

genero_consumidores[3,1] <- "TransgÃªnero"

genero_consumidores <- genero_consumidores %>%
  mutate(
    freq = round((n/ sum(genero_consumidores$n)*100), 2)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

colnames(genero_consumidores)[2] <- "Freq"

ggplot(genero_consumidores) +
  aes(x = reorder(genero, -Freq), y = Freq, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) + 
  labs(x = "GÃªnero", y = "FrequÃªncia") +
  theme_estat(axis.text.x = element_text(size = 10))
ggsave("resultados/GÃªnero.pdf", width = 158, height = 93, units = "mm")

# ---

genero_consumidores_RA <- consumidores %>%
  select(genero, RA) %>%
  add_count(genero, RA) %>%
  distinct()

genero_consumidores_RA <- consumidores %>%
  select(genero, RA) %>%
  group_by(RA, genero) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

genero_consumidores_RA[13,2] <- "TransgÃªnero"

ggplot(genero_consumidores_RA) +
  aes(x = fct_reorder(genero, desc(freq)), y = freq, label = label) +
  geom_bar(stat = "identity", fill="#A11D21" , position = "dodge") +
  facet_wrap(RA ~ .) +
  labs(x = "GÃªnero", y = "FrequÃªncia") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.35) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 8))
ggsave("resultados/GÃªnero_RA.pdf", width = 178, height = 100, units = "mm")

## FAIXA ETÃRIA ----

idade_consumidores <- consumidores %>%
  select(idade) %>%
  add_count(idade) %>%
  distinct()

idade_consumidores <- idade_consumidores %>%
  mutate(
    freq = round((n/ sum(idade_consumidores$n)*100), 2)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ordem_idade = c("< 18", "18-24", "25-34", "35-44", "45-54", "55+")
idade_consumidores$idade <- factor(idade_consumidores$idade, levels = ordem_idade)

ggplot(idade_consumidores) +
  aes(x = idade, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) + 
  labs(x = "Faixa etÃ¡ria", y = "FrequÃªncia") +
  scale_y_continuous(breaks = seq(0, 150, by = 25), limits = c(0, 150)) +
  theme_estat(axis.text.x = element_text(size = 10))
ggsave("resultados/idade.pdf", width = 158, height = 93, units = "mm")

# ---

idade_consumidores_RA <- consumidores %>%
  select(idade, RA) %>%
  add_count(idade, RA) %>%
  distinct()

idade_consumidores_RA <- consumidores %>%
  select(idade, RA) %>%
  group_by(RA, idade) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

idade_consumidores_RA$idade <- factor(idade_consumidores_RA$idade, levels = ordem_idade)

legenda <- idade_consumidores_RA$label %>%
  str_wrap(width = 1, whitespace_only = TRUE)

ggplot(idade_consumidores_RA) +
  aes(x = idade, y = freq, label = legenda) +
  geom_bar(stat = "identity", fill="#A11D21" , position = "dodge") +
  facet_wrap(RA ~ .) +
  labs(x = "Faixa etÃ¡ria", y = "FrequÃªncia") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.2,
            size = 2.2) +
  scale_y_continuous(breaks = seq(0, 40, by = 10), limits = c(0, 40)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 7))
ggsave("resultados/idade_RA.pdf", width = 178, height = 100, units = "mm")

## PRETENDE OU NÃO COMPRAR PRESENTE ----

comprar_presente <- consumidores %>%
  select(pretende_comprar_presente) %>%
  add_count(pretende_comprar_presente) %>%
  distinct()

comprar_presente <- comprar_presente %>%
  mutate(
    freq = round((n/ sum(comprar_presente$n)*100), 2)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(comprar_presente) +
  aes(x = reorder(pretende_comprar_presente, -n), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) + 
  labs(x = "Pretende comprar presente para o dia dos pais?", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 350, by = 50), limits = c(0, 350)) +
  theme_estat(axis.text.x = element_text(size = 10))
ggsave("pf23023 - Sindivarejista/resultados/pretende_comprar_presente.pdf", width = 158, height = 93, units = "mm")

# ---

pretende_comprar_RA <- consumidores %>%
  select(pretende_comprar_presente, RA) %>%
  add_count(pretende_comprar_presente, RA) %>%
  distinct()

pretende_comprar_RA <- consumidores %>%
  select(pretende_comprar_presente, RA) %>%
  group_by(RA, pretende_comprar_presente) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

ggplot(pretende_comprar_RA) +
  aes(x = pretende_comprar_presente, y = freq, label = label) +
  geom_bar(stat = "identity", fill="#A11D21" , position = "dodge") +
  facet_wrap(RA ~ .) +
  labs(x = "Pretende comprar presente para o dia dos pais?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.2,
            size = 3) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 10))
ggsave("pf23023 - Sindivarejista/resultados/pretende_comprar_presente_RA.pdf", width = 178, height = 100, units = "mm")

## PORQUE NAO COMPRA PRESENTE ----

n_comprar_presente <- consumidores %>%
  select(porque_nao_compra_presente) %>%
  na.omit() %>%
  add_count(porque_nao_compra_presente) %>%
  distinct()

n_comprar_presente <- n_comprar_presente %>%
  mutate(
    freq = round((n/ sum(n_comprar_presente$n)*100), 2)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
) %>%
  arrange(desc(n))

xtable(n_comprar_presente)

## COMPRARÁ MAIS DE UM PRESENTE? ----

consumidores$comprar_apenas_1_presente[is.na(consumidores$comprar_apenas_1_presente)] <- "Não informado"

mais_1_presente <- consumidores %>%
  select(comprar_apenas_1_presente) %>%
  add_count(comprar_apenas_1_presente) %>%
  distinct()

mais_1_presente <- mais_1_presente %>%
  mutate(
    freq = round((n/ sum(mais_1_presente$n)*100), 2)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(mais_1_presente) +
  aes(x = reorder(comprar_apenas_1_presente, -n), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) + 
  labs(x = "Pretende comprar apenas 1 presente?", y = "Frequência") +
  theme_estat(axis.text.x = element_text(size = 10)) +
  scale_y_continuous(breaks = seq(0, 300, by = 50), limits = c(0, 300))
ggsave("pf23023 - Sindivarejista/resultados/comprara_mais_de_1_presente.pdf", width = 158, height = 93, units = "mm")

# ---

mais_1_presente_RA <- consumidores %>%
  select(comprar_apenas_1_presente, RA) %>%
  add_count(comprar_apenas_1_presente, RA) %>%
  distinct()

mais_1_presente_RA <- consumidores %>%
  select(comprar_apenas_1_presente, RA) %>%
  group_by(RA, comprar_apenas_1_presente) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

ggplot(mais_1_presente_RA) +
  aes(x = fct_reorder(comprar_apenas_1_presente, desc(freq)), y = freq, label = label) +
  geom_bar(stat = "identity", fill="#A11D21" , position = "dodge") +
  facet_wrap(RA ~ .) +
  labs(x = "Pretende comprar apenas 1 presente?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.35) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 8))
ggsave("pf23023 - Sindivarejista/resultados/comprara_mais_de_1_presente_RA.pdf", width = 178, height = 100, units = "mm")

## Quantos presentes comprará? ----

presentes<- data.frame(consumidores$quantos_presentes_comprara %>% na.omit())

colnames(presentes) <- "qnt_presentes"

presentes$qnt_presentes[presentes$qnt_presentes == "4 ou mais"] <- "4+"
presentes$qnt_presentes[presentes$qnt_presentes == "2.0"] <- "2"
presentes$qnt_presentes[presentes$qnt_presentes == "3.0"] <- "3"

ordem_presentes <- c("2", "3", "4+")

presentes <- presentes %>%
  add_count(qnt_presentes) %>%
  distinct()

presentes <- presentes %>%
  mutate(
    freq = round((n/ sum(presentes$n)*100), 2)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(presentes) +
  aes(x = fct_reorder(qnt_presentes, ordem_presentes), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3.2
  ) + 
  labs(x = "Caso não pretenda comprar apenas um presente,\n quantos comprará? ", y = "Frequência") +
  theme_estat(axis.text.x = element_text(size = 10)) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80))
ggsave("pf23023 - Sindivarejista/resultados/qnts_presentes_comprara.pdf", width = 158, height = 93, units = "mm")

# ---

consumidores$quantos_presentes_comprara[consumidores$quantos_presentes_comprara == "4 ou mais"] <- "4+"
consumidores$quantos_presentes_comprara[consumidores$quantos_presentes_comprara == "2.0"] <- "2"
consumidores$quantos_presentes_comprara[consumidores$quantos_presentes_comprara == "3.0"] <- "3"

presentes_RA <- consumidores %>%
  select(quantos_presentes_comprara, RA) %>%
  na.omit() %>%
  group_by(RA, quantos_presentes_comprara) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

presentes_RA$quantos_presentes_comprara <- factor(presentes_RA$quantos_presentes_comprara, levels = ordem_presentes)

ggplot(presentes_RA) +
  aes(x = quantos_presentes_comprara, y = freq, label = label) +
  geom_bar(stat = "identity", fill="#A11D21" , position = "dodge") +
  facet_wrap(RA ~ .) +
  labs(x = "Caso não pretenda comprar apenas um presente,\n quantos comprará?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.35) +
  scale_y_continuous(breaks = seq(0, 25, by = 5), limits = c(0, 25)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 10))
ggsave("pf23023 - Sindivarejista/resultados/qnts_presentes_comprara_RA.pdf", width = 178, height = 100, units = "mm")


## IRÁ PRESENTEAR MAIS DE UMA PESSOA? ----

consumidores$pretende_presentear_mais_de_uma_pessoa[is.na(consumidores$pretende_presentear_mais_de_uma_pessoa)] <- "Não informado"
mais_pessoa <- consumidores[ ,14]

mais_pessoa <- mais_pessoa %>%
  add_count(pretende_presentear_mais_de_uma_pessoa) %>%
  distinct()

mais_pessoa <- mais_pessoa %>%
  mutate(
    freq = round((n/ sum(mais_pessoa$n)*100), 2)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(mais_pessoa) +
  aes(x = reorder(pretende_presentear_mais_de_uma_pessoa, -n), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) + 
  labs(x = "Irá presentear mais de uma pessoa?", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 300, by = 50), limits = c(0, 300)) +
  theme_estat(axis.text.x = element_text(size = 10))
ggsave("pf23023 - Sindivarejista/resultados/presentear_mais_uma_pessoa.pdf", width = 158, height = 93, units = "mm")

# ---

mais_pessoa_RA <- consumidores %>%
  select(pretende_presentear_mais_de_uma_pessoa, RA) %>%
  na.omit() %>%
  group_by(RA, pretende_presentear_mais_de_uma_pessoa) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(
    Freq = gsub("\\.", ",", freq_relativa) %>% paste("%", sep = ""),
    label = str_c(freq, " (", Freq, ")") %>% str_squish()
  )

ggplot(mais_pessoa_RA) +
  aes(x = pretende_presentear_mais_de_uma_pessoa, y = freq, label = label) +
  geom_bar(stat = "identity", fill="#A11D21" , position = "dodge") +
  facet_wrap(RA ~ .) +
  labs(x = "Caso não pretenda comprar apenas um presente,\n quantos comprará?", y = "Frequência") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.35) +
  scale_y_continuous(breaks = seq(0, 75, by = 15), limits = c(0, 75)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 10))
ggsave("pf23023 - Sindivarejista/resultados/presentear_mais_uma_pessoa_RA.pdf", width = 178, height = 100, units = "mm")

## Quem presentearão ----

## Onde geralmente os presentes são comprados ---

## Importância das lojas em oferecerem ou não promoções ----

## Categorias que o presente em questão se encaixa ----

## Quanto os consumidores pretendem gastam ----

## Data pretendida para compra ----

consumidores$quando_pretende_comprar[is.na(consumidores$quando_pretende_comprar)] <- "Não informado"

quando_comprar <- consumidores %>%
  select(quando_pretende_comprar) %>%
  add_count(quando_pretende_comprar) %>%
  distinct()

quando_comprar <- quando_comprar %>%
  mutate(
    freq = round((n/ sum(quando_comprar$n)*100), 2)
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
  )

xtable(quando_comprar)


