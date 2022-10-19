###############################################################################
#                                   Funções                                   #
###############################################################################


# função buscar próxima(s) página(s) --------------------------------------------

buscar_proxima_pagina <- function(n_pagina, termo_de_busca) {
  url_base <- "https://www.bdpa.cnptia.embrapa.br/consulta/busca"
  busca <-  str_replace(termo_de_busca, " ", "+")
  qFacets <-  busca
  query <- glue("b=ad&busca=(({busca}))&qFacets=(({qFacets}))&biblioteca=vazio&sort=&paginacao=t&paginaAtual={n_pagina}")
  url_pagina <- paste0(url_base,"?", query)
  # efetuar a busca
  busca <- GET(
    url_pagina,
    write_disk(glue("busca{n_pagina}.html"), overwrite = TRUE)
  )  
  if (busca$status_code != 200) {
    stop(paste0("Busca não realizada. Código de Status = ", busca$status_code))
  }
  return(busca)
}

# função para pegrar links ------------------------------------------------

pegar_links <- function(pagina){
  search <- "./body/div[2]/table[2]/tr[2]/td[2]/div/div[1]/table/tr/td[2]"
  colunaRegistro <- xml_find_first(pagina, xpath = search)
  if(length(colunaRegistro) != 0) {
    detalhes <- xml_find_all(colunaRegistro, ".//*[@title='Visualizar detalhes do registro']")
    links <- xml_attr(detalhes, "href") |> 
      str_replace_all("busca\\?", "")
  }
  return(links)
}


# função para baixar paginas de registros completos -----------------------

baixar_registro <- function(link){
  id_registro <- str_extract(link, "(?<=id=)\\d+")
  caminho_registro <- paste0(url_base, "?", str_replace_all(link, " ", "+"))
  arquivo <- paste0("data-raw/html/", id_registro,".html")
  if (!file.exists(arquivo)) 
  {    
    res <- GET(caminho_registro, write_disk(arquivo))
    if (res$status_code != 200) {
      stop(paste0("Deu ruim no registro ", id_registro))
    }
  }
}


# função para baixar todas as páginas de resultados -----------------------

baixar_paginas <- function(n_pagina){
  if(n_pagina > 1) { pagina <- buscar_proxima_pagina(n_pagina, termo_de_busca) }
  # pegar links para arquivos
  links <- pegar_links(pagina |> content())
  # baixar registros da primeira página
  walk(links, baixar_registro)
}

# parseamento dos registros -----------------------------------------------

ler_registro <- function(arquivo, prog = NULL){
  read_html(arquivo) |> 
    xml_find_first(".//*[@class='colunaRegistro']/table[not(@class)]") |> 
    html_table() |> 
    select(c(1:2)) |> 
    slice(-c(1:2)) |> 
    mutate(X1 = str_remove(X1, ":")) |> 
    rename("name" = X1, "value" = X2) |> 
    pivot_wider() |> 
    janitor::clean_names() |>
    mutate(id = str_remove(basename(arquivo), ".html"), .before = 1) 
  if (!is.null(prog)) prog()
}

ler_formatos <- function(arquivo, prog = NULL){
  res <- read_html(arquivo)
  titulos <- 
    res |> 
    xml_find_all(".//*[@class='colunaRegistro']/table/tr[@class='registroCompletoExemplar']/th") |> 
    xml_text() |> 
    str_squish()
  if(length(titulos)==0) {
    mensagem <- res |> 
      xml_find_all(".//td[contains(text(),'Nenhum exemplar cadastrado')]") |> 
      xml_text() |> 
      str_squish()
    if(!str_starts(mensagem, "Nenhum exemplar cadastrado")) warning(paste0(basename(arquivo), " com problema"))
    return(NULL)
  } else {
    formatos <- 
      res |> 
      xml_find_first(".//*[@class='colunaRegistro']/div/table") |> 
      html_table() |> 
      mutate(id = str_remove(basename(arquivo), ".html"), .before = 1) |> 
      rename_at(c(2:11), ~titulos) |> 
      janitor::clean_names() |>
      mutate(classificacao = as.character(classificacao),
             registro = as.character(registro),
             volume = as.character(volume),
             cutter = as.character(cutter))
    if (!is.null(prog)) prog()
    return(formatos)
  }
}

ler_descricoes <- function(arquivo, prog = NULL){
  res <- read_html(arquivo)
  mensagem <- res |> 
    xml_find_all(".//td[contains(text(),'Nenhum exemplar cadastrado')]") |> 
    xml_text() |> 
    str_squish()
  if(length(mensagem) == 0) {
    titulos <- 
      tibble(
        titulo = res |> 
          xml_find_all(".//*[@class='registroCompletoExemplarRegistroImpar']/td") |> 
          xml_attr('title'),
        codigo = res |> 
          xml_find_all(".//*[@class='registroCompletoExemplarRegistroImpar']/td") |> 
          xml_text()
      ) |> filter(!is.na(titulo))
    return(titulos)
  } else {
    return(NULL)
  }
  if (!is.null(prog)) prog()
}

# funções para tratamento de textos ---------------------------------------

aplicar_substituicoes <- 
  function(campo, lista_de_substituicoes) {
    for (i in 1:length(lista_de_substituicoes)) {
      campo <- 
        str_replace_all(campo, 
                        names(lista_de_substituicoes[i]), 
                        lista_de_substituicoes[i]) 
    }
    return(campo)
  }

aplicar_remocoes <- 
  function(campo, lista_de_remocao) {
    for (i in 1:length(lista_de_remocao)) {
      campo <-
        str_remove_all(campo,
                       lista_de_remocao[i])
    }
    return(campo)
  }

unificar_formatos <- function(iD){
  f <- 
    formatos |> 
    filter(id == iD) |> 
    separate(
      tipo_formato, 
      into = c("sg_tipo", "sg_formato"), 
      " - "
    )
  todos_os_formatos <- 
    f$sg_formato[f$sg_formato != "--"] |> 
    as.character() |> 
    unique()
  if(length(todos_os_formatos) == 0){
    todos_os_formatos <- 
      f$sg_formato[f$sg_formato == "--"] |> 
      as.character() |> 
      unique()
  }
  if(length(todos_os_formatos) == 0) todos_os_formatos <- NA
  todos_os_tipos <- 
    f$sg_tipo[f$sg_tipo != "--"] |> 
    as.character() |> 
    unique()
  if(length(todos_os_tipos) == 0) {
    todos_os_tipos <- 
      f$sg_tipo[f$sg_tipo == "--"] |> 
      as.character() |> 
      unique()
  }
  if(length(todos_os_tipos) == 0) todos_os_tipos <- NA
  f <-  
    tibble("id" = iD, 
           "sg_tipo" = todos_os_tipos, 
           "sg_formato" = todos_os_formatos) |> 
    mutate("tipo_formato" = paste(sg_tipo, "-", sg_formato)
    )
  return(f)
}

separar_categorias <- function(categoria){
  sl <- str_locate_all(categoria, "\\w[A-Z]")
  tx <- str_sub(categoria, sl[[1]])
  if(length(tx) > 0) {
    tx_ <- paste(str_sub(tx, 1,1), "-", str_sub(tx, 2,2))
    for (i in 1:length(tx)) {
      categoria <- str_replace_all(categoria, tx[i], tx_[i])
    }
  }
  if(str_starts(categoria, "--") & categoria != "--") categoria <- categoria |> str_remove("--")
  return(categoria)
}

calcular_hapax <- function(valor, campo) {
  if(is.na(valor)) {
    tk <- tokens_subset(comp_toks,is.na(docvars(comp_toks, campo)))
  } else {
    tk <- tokens_subset(comp_toks, docvars(comp_toks, campo) == valor)
  }
  dfm_hapax <- 
    tk |> dfm()
  hapax <- 
    mean(rowSums(dfm_hapax == 1) / ntoken(dfm_hapax))
  hapax <- 
    tibble({{campo}} := valor, "Hapax" = hapax) # Primeira vez que o {{}} funciona!!!
}

pull_layout <- function(tbl) {
  umap <- umap::umap(as.matrix(tbl))
  layout <- umap$layout
  rownames(layout) <- rownames(tbl)
  return(as.data.frame(layout))
}

# dfm wordcloud -----------------------------------------------------------

plotar_word_cloud <- 
  function(min_termfreq = 0, 
           min_size = 0.5, 
           max_size = 7,
           max_words = NULL) {
    dfm_conteudo <- 
      comp_toks |> 
      dfm() |> 
      dfm_group(groups = Material) |> 
      dfm_trim(termfreq_type = "prop",
               min_termfreq = min_termfreq) |> 
      textplot_wordcloud(
        comparison = TRUE,
        random_order = FALSE,
        min_size = min_size,
        max_size = max_size,
        max_words = max_words,
        labeloffset = 0.2,
        color = rev(RColorBrewer::brewer.pal(4, "RdBu"))
      )
  } 

# textnetwork -------------------------------------------------------------

plotar_net_thesagro <- function(min_termfreq) {
  toks_thesagro |> 
    quanteda::dfm() |>
    quanteda::dfm_trim(min_termfreq = min_termfreq) |>
    quanteda::fcm() |>
    quanteda.textplots::textplot_network()
}

# LDA (Alocação de Dirichlet Latente) -------------------------------------

calcular_lda <- function(k) {
  dtm <- 
    toks_thesagro |> 
    quanteda::dfm() |>
    quanteda::dfm_tfidf()
  
  features <- 
    toks_thesagro |> 
    quanteda::dfm() |>
    quanteda::ntoken()
  
  m <- 
    dtm |>
    as("dgCMatrix") |>
    textmineR::FitLdaModel(k = k, iterations = 200, burnin = 175)
  
  table_lda <- 
    m$phi |>
    textmineR::GetTopTerms(15L)
  
  suppressWarnings({
    LDAvis::createJSON(
      phi = m$phi,
      theta = m$theta,
      doc.length = features,
      vocab = stringi::stri_enc_toutf8(dtm@Dimnames$features),
      term.frequency = quanteda::colSums(dtm)
    )  |> 
      LDAvis::serVis(open.browser = FALSE, 
                     out.dir = file.path(getwd(), "ldavis"))
  })
}

# Similaridade ------------------------------------------------------------

calcular_similaridade <- function(n) {
  toks_thesagro_vec <-  
    toks_thesagro |> 
    quanteda::tokens_compound(pattern = phrase(multiword)) |> 
    quanteda::tokens_tolower() |> 
    as.list() |>
    text2vec::itoken()
  vocab <- 
    toks_thesagro_vec |>
    text2vec::create_vocabulary() |>
    text2vec::prune_vocabulary(term_count_min = 10L)
  vectorize <- 
    text2vec::vocab_vectorizer(vocab)
  tcm <- 
    text2vec::create_tcm(
      it = toks_thesagro_vec,
      vectorizer = vectorize,
      skip_grams_window = 5L
    )
  glove <- 
    text2vec::GlobalVectors$new(
      rank = 50,
      x_max = 15L
    )
  wv <- 
    glove$fit_transform(
      x = tcm,
      n_iter = 10L
    ) |>
    as.data.frame(stringsAsFactors = FALSE) |>
    tibble::as_tibble(.name_repair = "minimal", 
                      rownames = NA)
  vec <- 
    vocab |>
    dplyr::anti_join(
      y = tibble::tibble(words = stopwords('pt')),
      by = c("term" = "words")
    ) |>
    dplyr::arrange(desc(term_count)) |>
    dplyr::slice_head(n = 100L) |>
    dplyr::left_join(tibble::rownames_to_column(wv), 
                     by = c("term" = "rowname")) |>
    tibble::column_to_rownames("term") |>
    dplyr::select(starts_with("V"))
  dist <- 
    proxyC::simil(as(as.matrix(vec), "dgCMatrix"), method = "cosine")
  clust <- 
    kmeans(x = dist, centers = n)
  vec_layout <- 
    pull_layout(vec) |>
    tibble::rownames_to_column() |>
    dplyr::mutate(cluster = as.factor(clust$cluster))
  vec_layout |>
    ggplot2::ggplot(aes(x = V1, y = V2, colour = cluster)) +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(aes(label = rowname)) +
    ggplot2::theme_light()
}

# funções para análise de redes -------------------------------------------

obter_time_line_twitter <- function(id) {
  time_line <- NULL
  tryCatch(
    expr = {
      arquivo <- glue("data-raw/time_lines/time_line{id}.RDS")
      if (!file.exists(arquivo)) {
        time_line <- get_timeline(tl$id, n = 1000)
        time_line$perfil_id <- id
        saveRDS(time_line, arquivo)
      }
    }, error = function(e){
      print(paste("Erro ao ler o perfil", id))
      tl <- perfis_embrapa |> filter(id == {{id}})
      if(nrow(tl)>0) {
        time_line <- get_timeline(tl$screen_name, n = 1000)
        time_line$perfil_id <- id
        saveRDS(time_line, arquivo)
      }
    }, warning = function(w){
      print(w)
    }, finally = function(f){

          })
  Sys.sleep(1)
  return(time_line)
}
