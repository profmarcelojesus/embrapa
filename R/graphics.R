###############################################################################
#                                 Gráficos                                    #
###############################################################################


# figura 4.2 ----------------------------------------------------------------

plot_documentos_por_ano <- 
  registros |> 
  filter(ano_de_publicacao != 0)|> 
  ggplot(aes(ano_de_publicacao)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Ano de Publicação", 
       y = "Número de documentos")

# figura 4.3 ----------------------------------------------------------------

plot_documentos_por_tipo <- 
  documentos_por_tipo|> 
  ggplot(aes(x = n, y = reorder(tipo_da_producao_cientifica, n))) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=n), hjust=-0.1) +
  scale_x_continuous(expand = expansion(add = c(10,80))) +
  labs(x = "Número de documentos", 
       y = "Tipo de produção científica") 

# figura 4.4 ----------------------------------------------------------------

plot_documentos_por_ano_formato <- 
  documentos_por_ano_formato |> 
  ggplot() +
  scale_y_log10()+
  geom_line(aes(x = ano_de_publicacao, y = n, color = formato)) +
  scale_x_date(name = "Ano de publicação", date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Número de documentos",
       colour = "Formato")

# figura 4.5 ----------------------------------------------------------------

plot_documentos_por_ano_tipo <- 
  documentos_por_ano_tipo |> 
  ggplot() +
  scale_y_log10()+
  geom_point(aes(x = ano_de_publicacao, y = n, color = tipo), size = 3) +
  scale_x_date(name = "Ano de publicação", date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Volume de publicações",
       colour = "Tipo de Publicação")

# figura 4.6 ----------------------------------------------------------------

plot_documentos_por_categoria <- 
  documentos_por_categoria |> 
  filter(ano_de_publicacao >= 1990) |> 
  mutate(ano_de_publicacao = as.Date(paste0(ano_de_publicacao,"-01-01"))) |> 
  count(categoria_do_assunto, ano_de_publicacao) |> 
  ggplot() +
  scale_y_log10()+
#  geom_line(aes(x = ano_de_publicacao, y = n, color = categoria_do_assunto)) +
  geom_point(aes(x = ano_de_publicacao, y = n, color = categoria_do_assunto), size = 3) +
  scale_x_date(name = "Ano de publicação", date_breaks = "2 years", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Volume de publicações",
       colour = "Categoria")



