##############################################################################

#  Conjunto de funções para acesso às páginas, download e leitura dos dados  #

##############################################################################


# requisições de busca e cópia das paginas --------------------------------

termo_de_busca <- "agricultura familiar"

url_base <- "https://www.bdpa.cnptia.embrapa.br/consulta/busca"

# formar body
body_busca_inicial <- 
  list(
    "b" = "null", 
    "clickBuscaSimples" = "f", 
    "clickBuscaAvancada" = "t", 
    "field1" = "",
    "value1" = str_replace(termo_de_busca, " ", "+"), 
    "sort" = "", 
    "registrosPagina" = 100,
    "join1" = "AND", 
    "field2" = "", 
    "value2" = "",
    "join2" = "AND", 
    "field3" = "", 
    "value3" = "",
    "anoInicialPublicacao" = "*",
    "anoFinalPublicacao" = "*",
    "anoInicialProducaoCientifica" = "*",
    "anoFinalProducaoCientifica" = "*"
  )

# efetuar a busca

pagina <- POST(
  url_base,
  body = body_busca_inicial,
  encode = "form",
  write_disk("busca.html", overwrite = TRUE)
)  

if (pagina$status_code != 200) {
  stop(paste0("Busca não realizada. Código de Status = ", pagina$status_code))
}

x_resultados <- "./body/div[2]/table[2]/tr[2]/td[2]/div/div[1]/table/tr/td[2]/table[2]/tr/td[1]/span"

n_resultados <- 
  pagina |> 
  content() |> 
  xml_find_first(x_resultados) |> 
  xml_text() |> 
  str_extract("(?<=Registros recuperados : )[\\d.]+") |> 
  str_replace("\\.", "") |> 
  as.numeric()

n_paginas <- ceiling(n_resultados / 100)

walk(1:n_paginas, baixar_paginas)

# leitura dos dados dos registros ------------------------------------------
# As etapas de leitura das páginas e construção das tabelas
# são realizadas com paralelização

plan(multisession)

arquivos <- fs::dir_ls("data-raw/html")

progressr::with_progress({
  p <- progressr::progressor(length(arquivos))
  registros <- future_map_dfr(arquivos, ler_registro)
})

saveRDS(registros, "data/registros.RDS")

progressr::with_progress({
  p <- progressr::progressor(length(arquivos))
  formatos <- future_map_dfr(arquivos, ler_formatos, prog = p)
})

saveRDS(formatos, "data/formatos.RDS")

progressr::with_progress({
  p <- progressr::progressor(length(arquivos))
  descricoes <- future_map_dfr(arquivos, ler_descricoes, prog = p) |> unique()
})

descricoes <- descricoes |> 
  rename(tipo_formato = codigo, 
         titulo_descricao = titulo)

saveRDS(descricoes, "data/descricoes.RDS")
