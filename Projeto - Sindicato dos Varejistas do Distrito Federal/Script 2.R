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
comerciantes$segmento_loja[comerciantes$segmento_loja == "Armarinho (l�, toalha)"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Animais"] <- "Pet Shop"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Armarinho (L�, Toalha)"] <- "Armarinho"
comerciantes$segmento_loja[comerciantes$segmento_loja == "M�veis"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "AGROPECUARIA"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Animais"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "SERVI�O DE TELECOMUNICA�OES"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Agropet"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Agropecu�ria"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pet Shop"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Utilidades"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "�culos"] <- "Rel�gios, j�ias ou acess�rios"
comerciantes$segmento_loja[comerciantes$segmento_loja == "�udio profissional e instrumento musical"] <- "Instrumentos Musicais"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Papelaria"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "�tica"] <- "Rel�gios, j�ias ou acess�rios"
comerciantes$segmento_loja[comerciantes$segmento_loja == "m�veis planejados"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Vestu�rio e viagem"] <- "Vestu�rio"
comerciantes$segmento_loja[comerciantes$segmento_loja == "armarinho"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Vestu�rio e artigos de viagem"] <- "Vestu�rio"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Aliment�cio"] <- "Mercados e Conveni�ncias"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Bebida"] <- "Mercados e Conveni�ncias"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Brinquedos infantis"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Artigos de festa"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Artigos para festas"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Banca"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Ca�a e pesca"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Cl�nica Veterin�ria"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Cl�nica de podologia"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Com�rcio varejista (mercado)"] <- "Mercados e Conveni�ncias"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Confec��es"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Constru��o"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Decora��es"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Diversos"] <- "Variedade"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Drogaria"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Embalagens"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Enxovais."] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Estacionamento"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Floricultura"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Floricultura e presentes"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Frios"] <- "Mercados e Conveni�ncias"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Hospital veterinário"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Hospitalar"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Loja de presente"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Loja de tecidos"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Materiais de Construção"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Mercado"] <- "Mercados e Conveniências"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Mobiliário Executivo"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Móveis Planejados"] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Móveis."] <- "Objetos do lar"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Papelaria, Brinquedos e utilidades"] <- "Variedades"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Infantil"] <- "Vestuário"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pet shop"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Pets"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Presentes (1,99)"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Presentes, brinquedos."] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Produtos de sinalização"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Salão e Barbearia"] <- "Outros"
comerciantes$segmento_loja[comerciantes$segmento_loja == "Sex shop"] <- "Outros"
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

potencial_venda <- potencial_venda %>% separate_longer_delim(produtos_potencial_venda, delim = ",")
potencial_venda <- potencial_venda %>% separate_longer_delim(produtos_potencial_venda, delim = " e ")

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
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "n�o tem pai"] <- "N�o tem pai"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Apenas n�o tem interesse na compra"] <- "N�o tem interesse na compra"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "o pai est� longe"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "o pai j� faleceu"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai mora longe"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Falecidos."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O pai mora longe."] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o tem mais pai vivo."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O pai j� faleceu."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Mora longe"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Mora longe."] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o tem pai, faleceu"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Parente mora distante"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "pai falecido"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "pai mora em outro estado"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai n�o mora perto"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o possui pai"] <- "N�o tem pai"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pa�s falecidos"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Familiar mora longe"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o ter mais pai e seu filho n�o ser pai."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o tem pai."] <- "N�o tem pai"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai mora longe e n�o tem contato."] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Dist�ncia/Pai mora em outro estado ou cidade"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai faleceu"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "falecido"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o ter pai."] <- "N�o tem pai"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Distancia e morte"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "falecidos"] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Aus�ncia"] <- "Aus�ncia paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Dist�ncia"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o ter presen�a de pais."] <- "Aus�ncia paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o ter rela��o com pai."] <- "Aus�ncia paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o vai ver o pai"] <- "Aus�ncia paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Pai falecido."] <- "Pai falecido"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "distancia"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "distante"] <- "Pai mora longe"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "A m�e vai comprar por ele"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "J� costuma presentear durante o ano, e no dia dos pais n�o compra presentes."] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o conhece"] <- "Aus�ncia paterna"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o gosta de presente"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o quis comunicar"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "N�o tem boa rela��o com o pai"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O pai n�o faz quest�o."] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O pai n�o gosta de presente"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "O presente � a pr�pria presen�a."] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Ser divorciada."] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "n�o tem o costume de comemorar"] <- "Outros"
consumidores$porque_nao_compra_presente[consumidores$porque_nao_compra_presente == "Falecimento"] <- "Pai falecido"

# ANALISES - CONSUMIDORES ----
## GENERO ----

genero_consumidores <- consumidores %>%
  select(genero) %>%
  add_count(genero) %>%
  distinct()

genero_consumidores[3,1] <- "Transgênero"

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
  labs(x = "Gênero", y = "Frequência") +
  theme_estat(axis.text.x = element_text(size = 10))
ggsave("resultados/Gênero.pdf", width = 158, height = 93, units = "mm")

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

genero_consumidores_RA[13,2] <- "Transgênero"

ggplot(genero_consumidores_RA) +
  aes(x = fct_reorder(genero, desc(freq)), y = freq, label = label) +
  geom_bar(stat = "identity", fill="#A11D21" , position = "dodge") +
  facet_wrap(RA ~ .) +
  labs(x = "Gênero", y = "Frequência") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.35) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 8))
ggsave("resultados/Gênero_RA.pdf", width = 178, height = 100, units = "mm")

## FAIXA ETÁRIA ----

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
  labs(x = "Faixa etária", y = "Frequência") +
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
  labs(x = "Faixa etária", y = "Frequência") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.2,
            size = 2.2) +
  scale_y_continuous(breaks = seq(0, 40, by = 10), limits = c(0, 40)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 7))
ggsave("resultados/idade_RA.pdf", width = 178, height = 100, units = "mm")

## PRETENDE OU N�O COMPRAR PRESENTE ----

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
  labs(x = "Pretende comprar presente para o dia dos pais?", y = "Frequ�ncia") +
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
  labs(x = "Pretende comprar presente para o dia dos pais?", y = "Frequ�ncia") +
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

## COMPRAR� MAIS DE UM PRESENTE? ----

consumidores$comprar_apenas_1_presente[is.na(consumidores$comprar_apenas_1_presente)] <- "N�o informado"

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
  labs(x = "Pretende comprar apenas 1 presente?", y = "Frequ�ncia") +
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
  labs(x = "Pretende comprar apenas 1 presente?", y = "Frequ�ncia") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.35) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 8))
ggsave("pf23023 - Sindivarejista/resultados/comprara_mais_de_1_presente_RA.pdf", width = 178, height = 100, units = "mm")

## Quantos presentes comprar�? ----

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
  labs(x = "Caso n�o pretenda comprar apenas um presente,\n quantos comprar�? ", y = "Frequ�ncia") +
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
  labs(x = "Caso n�o pretenda comprar apenas um presente,\n quantos comprar�?", y = "Frequ�ncia") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.35) +
  scale_y_continuous(breaks = seq(0, 25, by = 5), limits = c(0, 25)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 10))
ggsave("pf23023 - Sindivarejista/resultados/qnts_presentes_comprara_RA.pdf", width = 178, height = 100, units = "mm")


## IR� PRESENTEAR MAIS DE UMA PESSOA? ----

consumidores$pretende_presentear_mais_de_uma_pessoa[is.na(consumidores$pretende_presentear_mais_de_uma_pessoa)] <- "N�o informado"
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
  labs(x = "Ir� presentear mais de uma pessoa?", y = "Frequ�ncia") +
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
  labs(x = "Caso n�o pretenda comprar apenas um presente,\n quantos comprar�?", y = "Frequ�ncia") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.35) +
  scale_y_continuous(breaks = seq(0, 75, by = 15), limits = c(0, 75)) +
  theme_estat(strip.text = element_text(size=12),
              strip.background = element_rect( colour =" black", fill=" white"),
              axis.text.x = element_text(size = 10))
ggsave("pf23023 - Sindivarejista/resultados/presentear_mais_uma_pessoa_RA.pdf", width = 178, height = 100, units = "mm")

## Quem presentear�o ----

## Onde geralmente os presentes s�o comprados ---

## Import�ncia das lojas em oferecerem ou n�o promo��es ----

## Categorias que o presente em quest�o se encaixa ----

## Quanto os consumidores pretendem gastam ----

## Data pretendida para compra ----

consumidores$quando_pretende_comprar[is.na(consumidores$quando_pretende_comprar)] <- "N�o informado"

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


