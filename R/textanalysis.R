###############################################################################
#                          Vizualização textual                               #
###############################################################################


# Aplicar remoções e substituições ----------------------------------------

# Aplicando remoções de palavras selecionadas

lista_de_remocao <- 
  c(
    "\n",
    '\"',
    "\\(",
    "\\)",
    "- programa \\d+",
    "Menos.*?Mostrar\\sTudo",
    "Mostrar Tudo", 
    "objetivo deste trabalho", 
    "pode ser", 
    "podem ser",
    "deve ser",
    "além disso",
    "é", 
    "presente trabalho", 
    "neste trabalho",
    "estudo de caso",
    "resultados obtidos",
    "objetivo deste estudo",
    "objetivo deste trabalho",
    "objetivo desse estudo",
    "objetivo desse trabalho",
    "presente estudo",
    "presente artigo",
    "referencial teórico",
    "considerações finais"
  )

registros_completos$conteudo <- 
  registros_completos$conteudo |> 
  aplicar_remocoes(lista_de_remocao)

# Padronização de entidades

lista_de_substituicoes <- 
  c("Assistência Técnica e Extensão Rural ATER" = "ATER",
    "Assistência Técnica e Extensão Rural" = "ATER",
    "Empresa Brasileira Pesquisa Agropecuária Embrapa" = "EMBRAPA",
    "EMBRAPA Empresa Brasileira Pesquisa Agropecuária" = "EMBRAPA",
    "Empresa Brasileira Pesquisa Agropecuária" = "EMBRAPA",
    "Companhia Nacional de Abastecimento CONAB" = "CONAB",
    "CONAB Companhia Nacional de Abastecimento" = "CONAB",
    "Companhia Nacional de Abastecimento" = "CONAB",
    "Ministério do Desenvolvinto Agrário MDA" = "MDA",
    "MDA Ministério do Desenvolvinto Agrário" = "MDA",
    "Ministério do Desenvolvinto Agrário" = "MDA",
    "Ministério do Desenvolvimento Social MDS" = "MDS",
    "MDS Ministério do Desenvolvimento Social" = "MDS",
    "Ministério do Desenvolvimento Social" = "MDS",
    "Ministério do Meio Ambiente MMA" = "MMA",
    "MMA Ministério do Meio Ambiente" = "MMA",
    "Ministério do Meio Ambiente" = "MMA",
    "Ministério da Agricultura Pecuária e Abastecimento MAPA" = "MAPA",
    "MAPA Ministério da Agricultura Pecuária e Abastecimento" = "MAPA",
    "Ministério da Agricultura Pecuária e Abastecimento"  = "MAPA",
    "Ministério da Agricultura" = "MAPA",
    "Organização das Nações Unidas ONU" = "ONU",
    "ONU Organização das Nações Unidas" = "ONU",
    "Organização das Nações Unidas" = "ONU",
    "Plano Nacional de Fortalecimento da Agricultura Familiar PLANAF" = "PLANAF",
    "PLANAF Plano Nacional de Fortalecimento da Agricultura Familiar" = "PLANAF",
    "Plano Nacional de Fortalecimento da Agricultura Familiar" = "PLANAF",
    "Programa Nacional de Fortalecimento da Agricultura Familiar PRONAF" = "PRONAF",
    "PRONAF Programa Nacional de Fortalecimento da Agricultura Familiar" = "PRONAF",
    "Programa Nacional de Fortalecimento da Agricultura Familiar" = "PRONAF",
    "Programa de Fortalecimento da Agricultura Familiar PRONAF" = "PRONAF",
    "PRONAF Programa de Fortalecimento da Agricultura Familiar" = "PRONAF",
    "Programa de Fortalecimento da Agricultura Familiar" = "PRONAF",
    "Programa Nacional de Produção e Uso de Biodiesel PNPB"  = "PNPB",
    "Programa Nacional de Produção e Uso de Biodiesel" = "PNPB",
    "Programa Nacional de Produção e Uso do Biodiesel PNPB"  = "PNPB",
    "Programa Nacional de Produção e Uso do Biodiesel"  = "PNPB",
    "Programa Nacional de Alimentação Escolar Pnae"  = "PNAE",
    "Pnae Programa Nacional de Alimentação Escolar"  = "PNAE",
    "Programa Nacional de Alimentação Escolar" = "PNAE",
    "PAA Programa de Aquisição De Alimentos" = "PAA",
    "Programa de Aquisição De Alimentos PAA" = "PAA",
    "Programa de Aquisição De Alimentos" = "PAA",
    "producao" = "produção",
    "agraria" = "agrária",
    "regiao" = "região",
    "agricolas" = "agrícola",
    "credito rural" = "crédito rural",
    "Sistemas de produção" = "Sistema de produção",
    "Inovações" = "Inovação",
    "Mão de obra" = "Mão-de-obra",
    "Sistemas Agroflorestais SAFS" = "SAFS",
    "Sistemas Agroflorestais" = "SAFS",
    "PRONAF PRONAF" = "PRONAF"
  )

registros_completos$conteudo <- 
  registros_completos$conteudo |> 
  aplicar_substituicoes(lista_de_substituicoes)

# Criando o corpus de conteúdos -------------------------------------------

corpus_conteudo <- 
  corpus(registros_completos$conteudo)

# Adicionado variáveis

docvars(corpus_conteudo, "Material") <- 
  registros_completos$formato

docvars(corpus_conteudo, "Ano") <- 
  registros_completos$ano_de_publicacao

docvars(corpus_conteudo, "Idioma") <- 
  registros_completos$idioma

docvars(corpus_conteudo, "Conteudo_digital") <- 
  if_else(is.na(registros_completos$url), "Indisponível", "Disponível")

discos <- c("DVD", "CD-ROM", "Videodiscos")

corpus_conteudo$Material[corpus_conteudo$Material %in% discos] <- "CD-ROM/DVD/Videodiscos"

fitas <- c("Fitas magnéticas", "Fitas de videocassete", "Microfilmes", "--")

corpus_conteudo$Material[corpus_conteudo$Material %in% fitas] <- "Fitas magnéticas/videocassete/microfilmes e Outros"

corpus_conteudo$Material[is.na(corpus_conteudo$Material)] <- "Fitas magnéticas/videocassete/microfilmes e Outros"

docnames(corpus_conteudo) <- 
  registros_completos$titulo |> 
  str_remove_all("\\.$")

# Eliminando registros vazios

corpus_conteudo <- 
  corpus_conteudo[corpus_conteudo != ""]

toks <- 
  corpus_conteudo |> 
  tokens(remove_numbers = TRUE, 
         remove_punct = TRUE)

# Análise de colocações ---------------------------------------------------

col <- 
  toks |> 
  tokens_remove(stopwords('en')) |> 
  tokens_remove(stopwords('pt')) |> 
  tokens_remove(stopwords('es')) |> 
  tokens_select(pattern = "[A-Z]", 
                valuetype = "regex", 
                case_insensitive = TRUE, 
                padding = TRUE) |> 
  textstat_collocations(min_count = 50, 
                        tolower = TRUE) |> 
  arrange(desc(count)) 

# Criando tokens com palavras compostas

nomes_ufs <- 
  c('Acre','Alagoas','Amapá','Amazonas','Bahia','Ceará','Distrito Federal',
    'Espírito Santo','Goiás','Maranhão','Mato Grosso','Mato Grosso do Sul',
    'Minas Gerais','Pará','Paraíba','Paraná','Pernambuco','Piauí',
    'Rio de Janeiro','Rio Grande do Norte','Rio Grande do Sul','Rondônia',
    'Roraima','Santa Catarina','São Paulo','Sergipe','Tocantins')

prefixo_ufs <- 
  map(c("a","e","o"), ~ paste0("Estado d", .x)) |> 
  map(~ paste(.x, nomes_ufs))

prefixo_ufs <- 
  purrr::flatten(prefixo_ufs)

multiword <- 
  c("Agricultura Familiar", 
    "Agricultores Familiares",
    "Assistência técnica",
    "cadeia produtiva",
    "Extensão Rural",
    "desenvolvimento rural sustentável",
    "Desenvolvimento Rural",
    "desenvolvimento sustentável",
    "desenvolvimento local",
    "Governo Federal",
    "Meio ambiente",
    "Pesquisa Agropecuária",
    "Política agrícola",
    "Políticas Públicas",
    "Reforma Agrária",
    "Sistema de produção",
    "Segurança Alimentar"
  ) |> 
  append(nomes_ufs[str_detect(nomes_ufs," ")]) |> 
  append(prefixo_ufs) 

multiword <- 
  as.character(multiword)

# Criando tokens compostos -----------------------------------------------

comp_toks <- 
  toks |> 
  tokens_compound(pattern = phrase(multiword),
                  valuetype = "fixed",
                  case_insensitive = TRUE) |> 
  tokens_remove(stopwords('en')) |> 
  tokens_remove(stopwords('pt')) |> 
  tokens_remove(stopwords('es'))

# tabelas de frequência relativa ------------------------------------------

dfm_online <- 
  comp_toks |> 
  dfm() |> 
  dfm_weight(scheme = "count")

freq_online <- 
  textstat_frequency(
    dfm_online, 
    n = 20, 
    groups = dfm_online$Material
  )

# Hapax por tipo de acesso ------------------------------------------------

tb_hapax_online <- 
  map_dfr(
    as.character(unique(comp_toks$Conteudo_digital)), 
    calcular_hapax, 
    "Conteudo_digital"
    )

# Hapax por tipo de material ----------------------------------------------

tb_hapax_material <- 
  map_dfr(
    as.character(unique(comp_toks$Material)), 
    calcular_hapax, 
    "Material"
    )

# thesagro ----------------------------------------------------------------

corpus_thesagro <- 
  corpus(documentos_por_categoria$thesagro)

docvars(corpus_thesagro, "Categoria") <- 
  documentos_por_categoria$categoria_do_assunto

toks_thesagro <- 
  corpus_thesagro |>
  quanteda::tokens(include_docvars = TRUE, 
                   remove_punct = TRUE) |>
  quanteda::tokens_remove(stopwords("en")) |> 
  quanteda::tokens_remove(stopwords("pt")) |> 
  quanteda::tokens_remove(stopwords("es")) |> 
  quanteda::tokens_compound(pattern = phrase(multiword), 
                            case_insensitive = TRUE)


