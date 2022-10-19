###############################################################################
#                            Tabelas de Dados                                 #
###############################################################################


# TABELAS -----------------------------------------------------------------

registros <- readRDS("data/registros.RDS")
formatos <- readRDS("data/formatos.RDS")
descricoes <- readRDS("data/descricoes.RDS")

registros <- registros |> filter(ano_de_publicacao < 2022)

documentos_por_tipo <- 
  registros |> 
  filter(!is.na(tipo_da_producao_cientifica)) |> 
  count(tipo_da_producao_cientifica) |> 
  arrange(desc(n))

documentos_por_ano_formato <- 
  registros |> 
  left_join(formatos) |> 
  left_join(descricoes) |> 
  separate(titulo_descricao, sep = " - ", into = c("tipo", "formato")) |> 
  filter(ano_de_publicacao >= 1990  & !is.na(formato) & formato != '--')|> 
  mutate(ano_de_publicacao = as.Date(paste0(ano_de_publicacao,"-01-01"))) |> 
  count(formato, ano_de_publicacao)

documentos_por_ano_tipo <- 
  registros |> 
  left_join(formatos) |> 
  left_join(descricoes) |> 
  separate(titulo_descricao, sep = " - ", into = c("tipo", "formato")) |> 
  filter(ano_de_publicacao >= 1990 & !is.na(tipo) & tipo != '--') |> 
  mutate(ano_de_publicacao = as.Date(paste0(ano_de_publicacao,"-01-01"))) |> 
  count(tipo, ano_de_publicacao)

documentos_por_categoria <- 
  registros |> 
  mutate(categoria_do_assunto = map(registros$categoria_do_assunto, 
                                    separar_categorias)) |> 
  separate_rows(categoria_do_assunto, sep = " - ") 

form_transf <- future_map_dfr(unique(formatos$id), unificar_formatos)

registros_completos <- 
  registros |> 
  left_join(form_transf) |> 
  left_join(descricoes) |> 
  separate(titulo_descricao, 
           sep = " - ", 
           into = c("tipo", 
                    "formato")) |>
  arrange(desc(id))
