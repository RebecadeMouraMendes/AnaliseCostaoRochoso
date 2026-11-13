# Analise Ecologica do costão rochoso

#pacotes
pacotes <- c("readxl", "dplyr", "tidyr", "ggplot2", "vegan", "stringr")

#instala o que estiver faltando
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  install.packages(faltantes)
}
invisible(lapply(pacotes, library, character.only = TRUE))

#acha o arquivo e cria a pasta dos resultados
arquivo <- "endereco da planilha"
resultado <- file.path(dirname(arquivo), "resultado_costao")
dir.create(resultado, recursive = TRUE, showWarnings = FALSE)

aba <- "Ficha"
subdivisoes_por_quadrado <- 64


#importacao da ficha bruta
brutos <- readxl::read_excel(
  arquivo, sheet = aba, col_names = FALSE
)

if (nrow(brutos) < 4 || ncol(brutos) < 2) {
  stop("A aba Ficha nao possui a estrutura esperada.")
}

# Vai usar a linha 2 como identificacao
transecto_col <- trimws(as.character(unlist(brutos[2, -1], use.names = FALSE)))

if (anyNA(transecto_col) || any(!nzchar(transecto_col))) {
  stop("Ha colunas sem identificacao de transecto na linha 2 da aba Ficha.")
}

# Aqui vamos separar o grupo do identificador (então A T1 fica 'A').
grupo_col <- stringr::str_match(transecto_col, "^\\s*([[:alnum:]]+)")[, 2]
if (anyNA(grupo_col)) {
  stop("Nao foi possivel identificar o grupo em algum nome de transecto.")
}

# numera os quadrados pela ordem em que aparecem dentro de cada transecto.
# assim, cada transecto pode ter uma quantidade diferente de quadrados.
quadrado_col <- ave(
  seq_along(transecto_col),
  transecto_col,
  FUN = seq_along
)
quadrado_col <- as.integer(quadrado_col)

# numera os quadrados pela ordem em que aparecem dentro de cada transecto
meta <- data.frame(
  coluna_excel = seq_along(transecto_col) + 1L,
  grupo = grupo_col,
  transecto = transecto_col,
  quadrado = quadrado_col,
  amostra = sprintf("Q_%03d", seq_along(transecto_col)),
  stringsAsFactors = FALSE
)

# preserva a ordem dos grupos conforme aparece na planilha
ordem_grupos <- unique(meta$grupo)
meta$grupo <- factor(meta$grupo, levels = ordem_grupos)

# a primeira coluna contem os nomes; as colunas seguintes sao fotoquadrados
dados <- as.data.frame(brutos[-(1:2), , drop = FALSE], check.names = FALSE)

taxa <- trimws(as.character(dados[[1]]))
valores <- dados[-1]

# remove linhas sem nome e a linha vazia
manter <- !is.na(taxa) & nzchar(taxa) & tolower(taxa) != "vazio"
taxa <- taxa[manter]
valores <- valores[manter, , drop = FALSE]

if (length(taxa) == 0) stop("Nenhum taxon foi encontrado na aba Ficha.")
if (anyDuplicated(taxa)) {
  warning("Ha nomes de taxa repetidos")
}

# converte todas os valores para numero
valores[] <- lapply(valores, function(x) {
  suppressWarnings(as.numeric(as.character(x)))
})

if (anyNA(as.matrix(valores))) {
  stop("Existem celulas vazias ou nao numericas entre os dados dos taxa")
}

if (any(as.matrix(valores) < 0 | as.matrix(valores) > subdivisoes_por_quadrado)) {
  stop("Ha valores fora do intervalo 0 a 64")
}

colnames(valores) <- meta$amostra
valores$taxon <- taxa

# cobertura percentual
longos <- valores %>%
  tidyr::pivot_longer(
    cols = -taxon,
    names_to = "amostra",
    values_to = "subdivisoes"
  ) %>%
  dplyr::left_join(meta, by = "amostra") %>%
  dplyr::mutate(
    cobertura_pct = 100 * subdivisoes / subdivisoes_por_quadrado,
    quadrado = as.integer(quadrado)
  )

# a soma pode ultrapassar 64 quando ha sobreposicao entre organismos.
verificacao <- longos %>%
  group_by(grupo, transecto, amostra, quadrado) %>%
  summarise(total_subdivisoes_taxa = sum(subdivisoes), .groups = "drop")

write.csv(longos, file.path(resultado, "dados_longos.csv"), row.names = FALSE)
write.csv(meta, file.path(resultado, "metadados_fotoquadrados.csv"), row.names = FALSE)
write.csv(verificacao, file.path(resultado, "verificacao_subdivisoes.csv"), row.names = FALSE)

# cobertura media e frequencia por taxon
cobertura_taxon <- longos %>%
  group_by(taxon) %>%
  summarise(cobertura_media_pct = mean(cobertura_pct), .groups = "drop")

fig_cobertura <- ggplot(cobertura_taxon,
                        aes(reorder(taxon, cobertura_media_pct), cobertura_media_pct)) +
  geom_col() + coord_flip() + theme_minimal() +
  labs(title = "Cobertura media por taxon", x = "Taxon", y = "Cobertura media (%)")
ggsave(file.path(resultado, "01_cobertura_media_taxon.png"),
       fig_cobertura, width = 9, height = 6, dpi = 300)

frequencia <- longos %>%
  group_by(taxon) %>%
  summarise(frequencia_pct = 100 * mean(subdivisoes > 0), .groups = "drop")

fig_frequencia <- ggplot(frequencia,
                         aes(reorder(taxon, frequencia_pct), frequencia_pct)) +
  geom_col() + coord_flip() + ylim(0, 100) + theme_minimal() +
  labs(title = "Frequencia de ocorrencia", x = "Taxon",
       y = "Fotoquadrados com ocorrencia (%)")
ggsave(file.path(resultado, "02_frequencia_taxon.png"),
       fig_frequencia, width = 9, height = 6, dpi = 300)

# diversidade alfa por fotoquadrado
mat_quadrados <- longos %>%
  select(amostra, taxon, cobertura_pct) %>%
  tidyr::pivot_wider(names_from = taxon, values_from = cobertura_pct) %>%
  as.data.frame()

rownames(mat_quadrados) <- mat_quadrados$amostra
mat_quadrados$amostra <- NULL
mat_quadrados[] <- lapply(mat_quadrados, as.numeric)
mat_quadrados <- as.matrix(mat_quadrados[meta$amostra, , drop = FALSE])

alpha <- meta %>%
  mutate(
    riqueza = vegan::specnumber(mat_quadrados),
    shannon = vegan::diversity(mat_quadrados, index = "shannon"),
    simpson_1_menos_D = vegan::diversity(mat_quadrados, index = "simpson"),
    equitabilidade = ifelse(riqueza > 1, shannon / log(riqueza), NA_real_)
  )

write.csv(alpha, file.path(resultado, "indices_alfa_por_fotoquadrado.csv"),
          row.names = FALSE)

# media dos Qs de cada transecto. Transectos podem ter n de Q diferente.
alpha_transecto <- alpha %>%
  group_by(grupo, transecto) %>%
  summarise(
    riqueza_media = mean(riqueza),
    shannon_medio = mean(shannon),
    simpson_medio = mean(simpson_1_menos_D),
    n_fotoquadrados = n(),
    .groups = "drop"
  )

write.csv(alpha_transecto, file.path(resultado, "indices_alfa_por_transecto.csv"),
          row.names = FALSE)

fig_shannon <- ggplot(alpha_transecto, aes(grupo, shannon_medio)) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.08) +
  theme_minimal() +
  labs(title = "Shannon medio por grupo", x = "Grupo (praia para mar)",
       y = "Indice de Shannon")
ggsave(file.path(resultado, "03_shannon_por_grupo.png"),
       fig_shannon, width = 8, height = 5, dpi = 300)

# composicao media por transecto e PCoA --------------------
media_transecto <- longos %>%
  group_by(grupo, transecto, taxon) %>%
  summarise(cobertura_media = mean(cobertura_pct), .groups = "drop")

mat_transectos <- media_transecto %>%
  select(transecto, taxon, cobertura_media) %>%
  pivot_wider(names_from = taxon, values_from = cobertura_media) %>%
  as.data.frame()

rownames(mat_transectos) <- mat_transectos$transecto
mat_transectos$transecto <- NULL
mat_transectos[] <- lapply(mat_transectos, as.numeric)
mat_transectos <- as.matrix(mat_transectos)

setor_transecto <- meta %>%
  distinct(transecto, grupo) %>%
  slice(match(rownames(mat_transectos), transecto))

if (nrow(mat_transectos) >= 3 && all(rowSums(mat_transectos) > 0)) {
  dist_bray <- vegan::vegdist(mat_transectos, method = "bray")
  ord <- vegan::wcmdscale(dist_bray, k = 2, eig = TRUE, add = "lingoes")

  pontos <- data.frame(
    transecto = rownames(mat_transectos),
    PCoA1 = ord$points[, 1],
    PCoA2 = ord$points[, 2]
  ) %>% left_join(setor_transecto, by = "transecto")

  variacao <- 100 * ord$eig[1:2] / sum(ord$eig[ord$eig > 0])

  fig_pcoa <- ggplot(pontos, aes(PCoA1, PCoA2, color = grupo, label = transecto)) +
    geom_point(size = 3) + geom_text(vjust = -0.7, show.legend = FALSE) +
    theme_minimal() +
    labs(title = "PCoA - Bray-Curtis",
         x = sprintf("PCoA1 (%.1f%%)", variacao[1]),
         y = sprintf("PCoA2 (%.1f%%)", variacao[2]), color = "Grupo")

  ggsave(file.path(resultado, "04_PCoA_Bray_Curtis.png"),
         fig_pcoa, width = 8, height = 6, dpi = 300)
}

# distribuicao vertical: todos os quadrados 
# a comparacao e feita dentro de cada transecto e depois resumida por grupo.
vertical <- longos %>%
  group_by(grupo, transecto, quadrado, taxon) %>%
  summarise(cobertura_pct = mean(cobertura_pct), .groups = "drop")

vertical_media <- vertical %>%
  group_by(grupo, quadrado, taxon) %>%
  summarise(cobertura_media = mean(cobertura_pct), n_transectos = n_distinct(transecto),
            .groups = "drop")

fig_vertical <- ggplot(
  vertical_media,
  aes(x = quadrado, y = cobertura_media, color = taxon, group = taxon)
) +
  geom_line() + geom_point() +
  facet_wrap(~ grupo) +
  scale_x_continuous(breaks = sort(unique(vertical_media$quadrado))) +
  theme_minimal() +
  labs(title = "Distribuicao ao longo dos quadrados",
       subtitle = "Q1 e a posicao superior",
       x = "Posicao no transecto (Q)", y = "Cobertura media (%)", color = "Taxon")

ggsave(file.path(resultado, "05_distribuicao_vertical_todos_Q.png"),
       fig_vertical, width = 12, height = 7, dpi = 300)

# calcula diferencas apenas para pares consecutivos existentes em cada transecto.
diferencas <- vertical %>%
  arrange(grupo, transecto, taxon, quadrado) %>%
  group_by(grupo, transecto, taxon) %>%
  mutate(
    Q_anterior = quadrado,
    Q_seguinte = lead(quadrado),
    cobertura_seguinte = lead(cobertura_pct),
    diferenca = cobertura_pct - cobertura_seguinte
  ) %>%
  filter(!is.na(Q_seguinte), Q_seguinte == Q_anterior + 1) %>%
  ungroup()

write.csv(diferencas, file.path(resultado, "diferencas_entre_Qs_consecutivos.csv"),
          row.names = FALSE)

# distribuicao horizontal: praia para mar
horizontal <- longos %>%
  group_by(grupo, taxon) %>%
  summarise(cobertura_media = mean(cobertura_pct), .groups = "drop")

horizontal$grupo <- factor(horizontal$grupo, levels = ordem_grupos)

fig_horizontal <- ggplot(
  horizontal,
  aes(x = grupo, y = cobertura_media, color = taxon, group = taxon)
) +
  geom_line() + geom_point() + theme_minimal() +
  labs(title = "Cobertura media dos taxa ao longo do costao",
       subtitle = "Grupos na ordem praia para mar",
       x = "Grupo", y = "Cobertura media (%)", color = "Taxon")

ggsave(file.path(resultado, "06_cobertura_praia_mar.png"),
       fig_horizontal, width = 10, height = 6, dpi = 300)

# riqueza por grupo
presenca_grupo <- longos %>%
  group_by(grupo, taxon) %>%
  summarise(presente = any(subdivisoes > 0), .groups = "drop")

riqueza_grupo <- presenca_grupo %>%
  group_by(grupo) %>%
  summarise(riqueza_observada = sum(presente), .groups = "drop")

write.csv(riqueza_grupo, file.path(resultado, "riqueza_por_grupo.csv"),
          row.names = FALSE)



# dendrograma de similaridade entre transectos
# calcula a distancia de Bray-Curtis independentemente da PCoA.
if (nrow(mat_transectos) >= 2 && all(rowSums(mat_transectos) > 0)) {
  # calcular dissimilaridade de Bray-Curtis
  dist_bray_dend <- vegan::vegdist(mat_transectos, method = "bray")
  # agrupamento hierarquico UPGMA
  agrupamento <- hclust(as.dist(dist_bray_dend), method = "average")
  # converter a altura do dendrograma em similaridade (%)
  altura_similaridade <- (1 - agrupamento$height) * 100
  png(
    file.path(resultado, "07_dendrograma_similaridade_bray_curtis.png"),
    width = 2400, height = 1600, res = 300
  )
  plot(
    agrupamento,
    main = "Dendrograma de similaridade entre transectos",
    xlab = "Transectos",
    ylab = "Similaridade (%)",
    sub = "Bray-Curtis com Agrupamento UPGMA",
    yaxt = "n"
  )
  # substituir a escala vertical por similaridade
  eixo_sim <- pretty(c(0, 100))
  eixo_sim <- eixo_sim[eixo_sim >= 0 & eixo_sim <= 100]
  
  axis(
    side = 2,
    at = 1 - eixo_sim / 100,
    labels = eixo_sim,
    las = 1
  )
  
  dev.off()
  
  # salvar matriz de dissimilaridade
  write.csv(
    as.matrix(dist_bray_dend),
    file.path(resultado, "matriz_dissimilaridade_bray_curtis.csv")
  )
  
  # salvar matriz de similaridade (%)
  write.csv(
    (1 - as.matrix(dist_bray_dend)) * 100,
    file.path(resultado, "matriz_similaridade_bray_curtis.csv")
  )
  
} else {
  message(
    "Dendrograma nao calculado: sao necessarios pelo menos 2 transectos ",
    "com cobertura total maior que zero."
  )
}

cat("\nTodas as analises foram feitas!")