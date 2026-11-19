# Analise Ecologica do costão rochoso

#pacotes
pacotes <- c("readxl", "dplyr", "tidyr", "ggplot2", "vegan", "stringr", "ggh4x", "colorspace", "ggrepel", "patchwork")

#instala o que estiver faltando
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  install.packages(faltantes)
}
invisible(lapply(pacotes, library, character.only = TRUE))

#acha o arquivo e cria a pasta dos resultados
arquivo <- "/home/becca/Downloads/planilha_costao.xlsx"
resultado <- file.path(dirname(arquivo), "resultado_costao")
dir.create(resultado, recursive = TRUE, showWarnings = FALSE)

aba <- "Ficha"
subdivisoes_por_quadrado <- 64


#importacao da ficha bruta
# read_excel() do pacote readxl importa a planilha Excel para o R
# Aqui, col_names = FALSE impede que o R use a primeira linha como cabeçalho
brutos <- readxl::read_excel(
  arquivo, sheet = aba, col_names = FALSE
)

if (nrow(brutos) < 4 || ncol(brutos) < 2) {
  stop("A aba Ficha nao possui a estrutura esperada.")
}

# Vai usar a linha 2 como identificacao e nao a primeira
transecto_col <- trimws(as.character(unlist(brutos[2, -1], use.names = FALSE)))

if (anyNA(transecto_col) || any(!nzchar(transecto_col))) {
  stop("Ha colunas sem identificacao de transecto na linha 2 da aba Ficha.")
}

# Aqui vamos separar o grupo do identificador (então A T1 fica 'A').
grupo_col <- stringr::str_match(transecto_col, "^\\s*([[:alnum:]]+)")[, 2]
if (anyNA(grupo_col)) {
  stop("Nao foi possivel identificar o grupo em algum nome de transecto.")
}

# numera os quadrados pela ordem em que aparecem dentro de cada transecto
# assim, cada transecto pode ter uma quantidade diferente de quadrados
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
  warning("Ha nomes de taxon repetidos")
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
# group_by() organiza os dados em grupos para que os cálculos seguintes sejam feitos dentro deles
# pivot_longer() transforma as colunas dos fotoquadrados em linhas, deixando uma linha por taxon e amostra
longos <- valores %>%
  tidyr::pivot_longer(
    cols = -taxon,
    names_to = "amostra",
    values_to = "subdivisoes"
  ) %>%
  # left_join() acrescenta os metadados de cada amostra, relacionando as tabelas pela coluna "amostra"
  dplyr::left_join(meta, by = "amostra") %>%
  # mutate() cria a coluna de cobertura percentual e converte quadrado para número
  dplyr::mutate(
    cobertura_pct = 100 * subdivisoes / subdivisoes_por_quadrado,
    quadrado = as.integer(quadrado)
  )

# a soma pode ultrapassar 64 quando ha sobreposicao entre organismos
verificacao <- longos %>%
  # group_by() separa os registros de cada fotoquadrado dentro de seu grupo e transecto
  group_by(grupo, transecto, amostra, quadrado) %>%
  # summarise() resume cada grupo somando as subdivisões registradas para os taxons
  summarise(total_subdivisoes_taxa = sum(subdivisoes), .groups = "drop")

write.csv(longos, file.path(resultado, "dados_longos.csv"), row.names = FALSE)
write.csv(meta, file.path(resultado, "metadados_fotoquadrados.csv"), row.names = FALSE)
write.csv(verificacao, file.path(resultado, "verificacao_subdivisoes.csv"), row.names = FALSE)

# cobertura media
cobertura_taxon <- longos %>%
  # group_by() reúne todas as observações do mesmo taxon
  group_by(taxon) %>%
  # summarise() calcula uma única cobertura média para cada taxon
  summarise(cobertura_media_pct = mean(cobertura_pct), .groups = "drop")

fig_cobertura <- ggplot(cobertura_taxon,
                        aes(reorder(taxon, cobertura_media_pct), cobertura_media_pct)) +
  geom_col(fill = "paleturquoise3") + coord_flip() + theme_minimal() +
  labs(title = "Cobertura média por táxon", x = "Táxon", y = "Cobertura media (%)") + 
  theme(
    axis.text.y = element_text(
      color = "grey15",
      size = 10,
      face = "plain"
    )
  )
ggsave(file.path(resultado, "01_cobertura_media_taxon.png"),
       fig_cobertura, width = 9, height = 6, dpi = 300)

#frequencia por taxon
frequencia <- longos %>%
  # group_by() reúne os registros de cada taxon
  group_by(taxon) %>%
  # summarise() calcula em quantos fotoquadrados o taxon ocorreu e transforma isso em porcentagem
  summarise(frequencia_pct = 100 * mean(subdivisoes > 0), .groups = "drop")

fig_frequencia <- ggplot(frequencia,
                         aes(reorder(taxon, frequencia_pct), frequencia_pct)) +
  geom_col(fill = "#EE799F") + coord_flip() + ylim(0, 100) + theme_minimal() +
  labs(title = "Frequência de ocorrência", x = "Táxon",
       y = "Frequência nos fotoquadrados (%)") + 
  theme(
    axis.text.y = element_text(
      color = "grey15",
      size = 10,
      face = "plain"
    )
  )
ggsave(file.path(resultado, "02_frequencia_taxon.png"),
       fig_frequencia, width = 9, height = 6, dpi = 300)

# diversidade alfa por fotoquadrado
mat_quadrados <- longos %>%
  # select() mantém apenas as colunas necessárias para montar a matriz com amostra, taxon e cobertura
  select(amostra, taxon, cobertura_pct) %>%
  # pivot_wider() transforma os taxa em colunas e deixa cada fotoquadrado em uma linha
  tidyr::pivot_wider(names_from = taxon, values_from = cobertura_pct) %>%
  as.data.frame()

rownames(mat_quadrados) <- mat_quadrados$amostra
mat_quadrados$amostra <- NULL
mat_quadrados[] <- lapply(mat_quadrados, as.numeric)
mat_quadrados <- as.matrix(mat_quadrados[meta$amostra, , drop = FALSE])

alpha <- meta %>%
  # mutate() adiciona os índices ecológicos calculados para cada fotoquadrado
  mutate(
    # specnumber() conta quantos taxa têm ocorrência em cada fotoquadrado (riqueza)
    riqueza = vegan::specnumber(mat_quadrados),
    # diversity(index = "shannon") calcula o índice de diversidade de Shannon para cada fotoquadrado
    shannon = vegan::diversity(mat_quadrados, index = "shannon"),
    # diversity(index = "simpson") calcula o índice de Simpson (1 - D) para cada fotoquadrado
    simpson_1_menos_D = vegan::diversity(mat_quadrados, index = "simpson"),
    equitabilidade = ifelse(riqueza > 1, shannon / log(riqueza), NA_real_)
  )

write.csv(alpha, file.path(resultado, "indices_alfa_por_fotoquadrado.csv"),
          row.names = FALSE)

# media dos quadrados de cada transecto
alpha_transecto <- alpha %>%
  # group_by() reúne os fotoquadrados pertencentes ao mesmo grupo e transecto
  group_by(grupo, transecto) %>%
  # summarise() calcula a média dos índices entre os fotoquadrados de cada transecto
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
  labs(title = "Shannon médio por grupo", subtitle = "Valores médios dos fotoquadrados em cada transecto", x = "Grupo (praia para mar)",
       y = "Indice de Shannon")
ggsave(file.path(resultado, "03_shannon_por_grupo.png"),
       fig_shannon, width = 8, height = 6, dpi = 300)



# nMDS utilizando Bray-Curtis (cobertura dos taxons)

# utiliza a matriz de cobertura dos fotoquadrados
mat_bray <- mat_quadrados

# remove fotoquadrados sem nenhum taxon registrado
manter_bray <- rowSums(mat_bray) > 0
mat_bray <- mat_bray[manter_bray, , drop = FALSE]

if (nrow(mat_bray) >= 3) {
  
  # calcula a dissimilaridade de Bray-Curtis entre os fotoquadrados
  dist_bray <- vegan::vegdist(
    mat_bray,
    method = "bray"
  )
  
  # realiza o nMDS utilizando as distâncias de Bray-Curtis
  # set.seed() garante a reprodutibilidade da ordenação
  # trymax número máximo de tentativas para encontrar uma solução estável
  set.seed(123)
  
  ord_bray <- vegan::metaMDS(
    dist_bray,
    k = 3,
    trymax = 500,
    trace = FALSE
  )
  
  # recupera os metadados dos fotoquadrados na mesma ordem da matriz
  meta_bray <- meta[
    match(rownames(mat_bray), meta$amostra),
  ]
  
  # organiza as coordenadas dos fotoquadrados e seus metadados
  pontos_bray <- data.frame(
    amostra = rownames(ord_bray$points),
    NMDS1 = ord_bray$points[, 1],
    NMDS2 = ord_bray$points[, 2],
    NMDS3 = ord_bray$points[, 3]
  ) %>%
    left_join(meta_bray, by = "amostra")
  
  # constrói os gráficos de ordenação dos fotoquadrados
  fig_nmds_bray <- (
    ggplot(
      pontos_bray,
      aes(
        NMDS1,
        NMDS2,
        color = factor(quadrado),
        shape = grupo
      )
    ) +
      geom_point(size = 3.2, alpha = 1) +
      
      # afasta os nomes dos pontos para evitar sobreposição
      ggrepel::geom_text_repel(
        aes(label = paste0(transecto, " Q", quadrado)),
        show.legend = FALSE,
        size = 2.8,
        max.overlaps = Inf,
        box.padding = 0.5,
        point.padding = 0.4,
        segment.color = "grey50",
        segment.size = 0.3,
        seed = 123
      ) +
      
      # representa as posições verticais por diferentes cores
      scale_color_manual(
        values = c(
          "1" = "#174A8B",
          "2" = "#CD69C9",
          "3" = "#C51B5A",
          "4" = "#D45A00",
          "5" = "#FFA500"
        ),
        name = "Posição vertical",
        labels = function(x) paste0("Q", x)
      ) +
      
      # representa os grupos por diferentes formatos
      scale_shape_manual(
        values = c(
          "A" = 16,
          "B" = 17,
          "C" = 15,
          "D" = 18,
          "E" = 3,
          "F" = 7,
          "G" = 8
        ),
        name = "Grupo"
      ) +
      
      theme_minimal() +
      
      labs(
        title = "nMDS - Bray-Curtis",
        subtitle = sprintf(
          "Composição dos táxons considerando a cobertura | Stress = %.3f",
          ord_bray$stress
        ),
        x = "nMDS1",
        y = "nMDS2"
      )
  ) +
    (
      ggplot(
        pontos_bray,
        aes(
          NMDS1,
          NMDS3,
          color = factor(quadrado),
          shape = grupo
        )
      ) +
        geom_point(size = 3.2, alpha = 1) +
        
        # afasta os nomes dos pontos para evitar sobreposição
        ggrepel::geom_text_repel(
          aes(label = paste0(transecto, " Q", quadrado)),
          show.legend = FALSE,
          size = 2.8,
          max.overlaps = Inf,
          box.padding = 0.5,
          point.padding = 0.4,
          segment.color = "grey50",
          segment.size = 0.3,
          seed = 123
        ) +
        
        # representa as posições verticais por diferentes cores
        scale_color_manual(
          values = c(
            "1" = "#174A8B",
            "2" = "#CD69C9",
            "3" = "#C51B5A",
            "4" = "#D45A00",
            "5" = "#FFA500"
          ),
          name = "Posição vertical",
          labels = function(x) paste0("Q", x)
        ) +
        
        # representa os grupos por diferentes formatos
        scale_shape_manual(
          values = c(
            "A" = 16,
            "B" = 17,
            "C" = 15,
            "D" = 18,
            "E" = 3,
            "F" = 7,
            "G" = 8
          ),
          name = "Grupo"
        ) +
        
        theme_minimal() +
        
        labs(
          title = "nMDS - Bray-Curtis",
          subtitle = sprintf(
            "Composição dos táxons considerando a cobertura | Stress = %.3f",
            ord_bray$stress
          ),
          x = "nMDS1",
          y = "nMDS3"
        )
    ) +
    (
      ggplot(
        pontos_bray,
        aes(
          NMDS2,
          NMDS3,
          color = factor(quadrado),
          shape = grupo
        )
      ) +
        geom_point(size = 3.2, alpha = 1) +
        
        # afasta os nomes dos pontos para evitar sobreposição
        ggrepel::geom_text_repel(
          aes(label = paste0(transecto, " Q", quadrado)),
          show.legend = FALSE,
          size = 2.8,
          max.overlaps = Inf,
          box.padding = 0.5,
          point.padding = 0.4,
          segment.color = "grey50",
          segment.size = 0.3,
          seed = 123
        ) +
        
        # representa as posições verticais por diferentes cores
        scale_color_manual(
          values = c(
            "1" = "#174A8B",
            "2" = "#CD69C9",
            "3" = "#C51B5A",
            "4" = "#D45A00",
            "5" = "#FFA500"
          ),
          name = "Posição vertical",
          labels = function(x) paste0("Q", x)
        ) +
        
        # representa os grupos por diferentes formatos
        scale_shape_manual(
          values = c(
            "A" = 16,
            "B" = 17,
            "C" = 15,
            "D" = 18,
            "E" = 3,
            "F" = 7,
            "G" = 8
          ),
          name = "Grupo"
        ) +
        
        theme_minimal() +
        
        labs(
          title = "nMDS - Bray-Curtis",
          subtitle = sprintf(
            "Composição dos táxons considerando a cobertura | Stress = %.3f",
            ord_bray$stress
          ),
          x = "nMDS2",
          y = "nMDS3"
        )
    )
  
 
  
  # salva o gráfico
  ggsave(
    file.path(resultado, "04a_nMDS_Bray_NMDS1_NMDS2.png"),
    fig_nmds_bray[[1]],
    width = 10,
    height = 8,
    dpi = 300
  )
  
  ggsave(
    file.path(resultado, "04b_nMDS_Bray_NMDS1_NMDS3.png"),
    fig_nmds_bray[[2]],
    width = 10,
    height = 8,
    dpi = 300
  )
  
  ggsave(
    file.path(resultado, "04c_nMDS_Bray_NMDS2_NMDS3.png"),
    fig_nmds_bray[[3]],
    width = 10,
    height = 8,
    dpi = 300
  )
}
# distribuicao vertical: todos os quadrados
# a comparacao e feita dentro de cada transecto e depois resumida por grupo
vertical <- longos %>%
  # group_by() separa a cobertura por grupo, transecto, posição do quadrado e taxon.
  group_by(grupo, transecto, quadrado, taxon) %>%
  # summarise() resume os registros de cada combinação, calculando a cobertura média.
  summarise(cobertura_pct = mean(cobertura_pct), .groups = "drop")

vertical_media <- vertical %>%
  # group_by() reúne a mesma posição de quadrado e taxon dentro de cada grupo
  group_by(grupo, quadrado, taxon) %>%
  # summarise() calcula a cobertura média entre transectos e conta quantos transectos contribuíram
  summarise(
    cobertura_media = mean(cobertura_pct),
    n_transectos = n_distinct(transecto),
    .groups = "drop"
  )

# prepara os dados
vertical_mapa <- vertical_media %>%
  # mutate() transforma o quadrado em categoria e organiza os taxons
  mutate(
    quadrado = factor(quadrado, levels = sort(unique(quadrado))),
    taxon = factor(taxon, levels = sort(unique(as.character(taxon))))
  ) %>%
  # group_by() mantém separadas as combinações de grupo e quadrado existentes
  group_by(grupo, quadrado) %>%
  # complete() inclui os taxa sem registro em cada quadrado observado
  tidyr::complete(
    taxon,
    fill = list(cobertura_media = 0)
  ) %>%
  ungroup()

# constrói e salva um mapa de calor para cada grupo

# identifica os grupos existentes nos dados
grupos <- sort(unique(as.character(vertical_mapa$grupo)))

# define os mesmos limites de cobertura para todos os gráficos
limite_cobertura <- range(
  vertical_mapa$cobertura_media,
  na.rm = TRUE
)

# identifica todas as posições de quadrados existentes
niveis_quadrados <- levels(vertical_mapa$quadrado)

# identifica todos os taxons e mantém a ordem original
niveis_taxons <- levels(vertical_mapa$taxon)

# percorre os grupos e cria uma figura individual para cada um
for (g in grupos) {
  
  # filtra os dados para manter somente o grupo atual
  dados_grupo <- vertical_mapa %>%
    filter(as.character(grupo) == g)
  
  # constrói o mapa de calor do grupo
  fig_vertical <- ggplot(
    dados_grupo,
    aes(x = quadrado, y = taxon, fill = cobertura_media)
  ) +
    # geom_tile() cria uma célula para cada combinação de taxon e quadrado
    geom_tile(color = "white", linewidth = 0.3) +
    
    # mantém a mesma escala de cores em todos os grupos
    scale_fill_viridis_c(
      option = "H",
      limits = limite_cobertura,
      name = "Cobertura\nmédia (%)"
    ) +
    
    # mantém todas as posições de quadrados na mesma ordem
    scale_x_discrete(
      limits = niveis_quadrados,
      drop = FALSE,
      labels = function(x) paste0("Q", x)
    ) +
    
    # mantém todos os taxons na mesma ordem
    scale_y_discrete(
      limits = niveis_taxons,
      drop = FALSE
    ) +
    
    # define o título e os nomes dos eixos
    labs(
      title = paste("Distribuição vertical - Grupo", g),
      subtitle = "Cobertura média em cada posição dos transectos",
      x = "Quadrados",
      y = "Táxon"
    ) +
    
    theme_minimal() +
    
    # mantém os mesmos ajustes visuais do gráfico original
    theme(
      axis.text.y = element_text(
        size = 7,
        color = "grey20"
      ),
      axis.text.x = element_text(
        size = 9,
        color = "grey20"
      ),
      plot.title = element_text(
        face = "bold",
        size = 15
      )
    )
  
  # salva uma imagem individual para cada grupo
  ggsave(
    file.path(
      resultado,
      paste0("05_distribuicao_vertical_grupo_", g, ".png")
    ),
    fig_vertical,
    width = 9,
    height = 11,
    dpi = 300
  )
}

# calcula diferencas apenas para pares consecutivos existentes em cada transecto
diferencas <- vertical %>%
  # arrange() ordena os registros para que os quadrados fiquem na sequência correta dentro de cada transecto
  arrange(grupo, transecto, taxon, quadrado) %>%
  # group_by() garante que lead() compare quadrados do mesmo grupo, transecto e taxon
  group_by(grupo, transecto, taxon) %>%
  # mutate() cria colunas com o quadrado seguinte, sua cobertura e a diferença entre as coberturas
  mutate(
    Q_anterior = quadrado,
    # lead() pega o valor da linha seguinte; aqui identifica o próximo quadrado do transecto
    Q_seguinte = lead(quadrado),
    # lead() também busca a cobertura do próximo quadrado para permitir a comparação
    cobertura_seguinte = lead(cobertura_pct),
    diferenca = cobertura_pct - cobertura_seguinte
  ) %>%
  # filter() seleciona pares de quadrados consecutivos 
  filter(!is.na(Q_seguinte), Q_seguinte == Q_anterior + 1) %>%
  ungroup()

write.csv(diferencas, file.path(resultado, "diferencas_entre_Qs_consecutivos.csv"),
          row.names = FALSE)


# distribuicao horizontal: praia para mar

# calcula a cobertura média de cada taxon em cada grupo
horizontal <- longos %>%
  group_by(grupo, taxon) %>%
  summarise(
    cobertura_media = mean(cobertura_pct),
    .groups = "drop"
  )

# preserva a ordem dos grupos conforme aparecem na planilha
horizontal$grupo <- factor(
  horizontal$grupo,
  levels = ordem_grupos
)

# organiza os taxons pela maior cobertura observada
ordem_taxons <- horizontal %>%
  group_by(taxon) %>%
  summarise(
    maior_cobertura = max(cobertura_media),
    .groups = "drop"
  ) %>%
  arrange(maior_cobertura)

horizontal$taxon <- factor(
  horizontal$taxon,
  levels = ordem_taxons$taxon
)

# constrói o mapa de calor da cobertura ao longo do costão
fig_horizontal <- ggplot(
  horizontal,
  aes(
    x = grupo,
    y = taxon,
    fill = cobertura_media
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.4
  ) +
  
  # escala de cores representa a cobertura média
  scale_fill_viridis_c(
    option = "H",
    name = "Cobertura\nmédia (%)"
  ) +
  
  # define os títulos e os eixos
  labs(
    title = "Distribuição dos táxons ao longo do costão",
    subtitle = "Cobertura média dos organismos no gradiente praia-mar",
    x = "Grupos (praia para mar)",
    y = "Táxon"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.y = element_text(
      size = 8,
      color = "grey15"
    ),
    axis.text.x = element_text(
      size = 10,
      color = "grey15"
    ),
    panel.grid = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 15
    )
  )

# salva a figura em alta resolução
ggsave(
  file.path(resultado, "06_cobertura_praia_mar.png"),
  fig_horizontal,
  width = 11,
  height = 9,
  dpi = 300
)


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
# calcula a distancia de Bray-Curtis independentemente da PCoA
if (nrow(mat_transectos) >= 2 && all(rowSums(mat_transectos) > 0)) {
  # calcular dissimilaridade de Bray-Curtis
  # vegdist() calcula novamente Bray-Curtis, agora para construir o dendrograma dos transectos
  dist_bray_dend <- vegan::vegdist(mat_transectos, method = "bray")
  # agrupamento hierarquico UPGMA
  # as.dist() garante que o resultado seja tratado como objeto de distâncias
  # hclust() faz o agrupamento hierárquico; method = "average" corresponde ao UPGMA
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

# dendrograma de similaridade entre todos os fotoquadrados
# verifica se existem pelo menos 2 fotoquadrados com cobertura total maior que zero
if (nrow(mat_quadrados) >= 2 && all(rowSums(mat_quadrados) > 0)) {
  
  # calcula a dissimilaridade de Bray-Curtis entre todos os fotoquadrados
  # vegdist() calcula a dissimilaridade de Bray-Curtis entre todos os fotoquadrados.
  dist_bray_quadrados <- vegan::vegdist(mat_quadrados, method = "bray")
  
  # realiza o agrupamento hierárquico utilizando o método UPGMA
  # as.dist() fornece a matriz de distâncias no formato esperado pelo agrupamento.
  # hclust() agrupa os fotoquadrados por similaridade; average é o método UPGMA.
  agrupamento_quadrados <- hclust(
    as.dist(dist_bray_quadrados),
    method = "average"
  )
  
  # identifica o transecto e o quadrado de cada amostra para utilizar como identificação no gráfico
  rotulos_quadrados <- paste(
    meta$transecto,
    paste0("Q", meta$quadrado),
    sep = " - "
  )
  
  # substitui os nomes das amostras pelos transectos e quadrados correspondentes
  agrupamento_quadrados$labels <- rotulos_quadrados
  
  # cria o arquivo PNG para salvar o dendrograma
  png(
    file.path(resultado, "08_dendrograma_similaridade_fotoquadrados.png"),
    width = 4000, height = 2500, res = 300
  )
  
  
  # constrói o dendrograma de similaridade entre os fotoquadrados
  plot(
    agrupamento_quadrados,
    main = "Dendrograma de similaridade entre fotoquadrados",
    xlab = "Fotoquadrados",
    ylab = "Similaridade (%)",
    sub = "Bray-Curtis com agrupamento UPGMA",
    yaxt = "n",
    ylim = c(0, 1),
    cex = 0.65,
    hang = -1
  )
  
  # cria uma escala de 0 a 100 para representar a similaridade em porcentagem
  eixo_sim_quadrados <- pretty(c(0, 100))
  eixo_sim_quadrados <- eixo_sim_quadrados[
    eixo_sim_quadrados >= 0 & eixo_sim_quadrados <= 100
  ]
  
  # substitui o eixo de dissimilaridade pela escala de similaridade
  axis(
    side = 2,
    at = 1 - eixo_sim_quadrados / 100,
    labels = eixo_sim_quadrados,
    las = 1
  )
  
  # encerra a criação do arquivo PNG
  dev.off()
  
  # salva a matriz de dissimilaridade entre os fotoquadrados
  write.csv(
    as.matrix(dist_bray_quadrados),
    file.path(resultado, "matriz_dissimilaridade_fotoquadrados.csv")
  )
  
  # salva a matriz de similaridade entre os fotoquadrados em porcentagem
  write.csv(
    (1 - as.matrix(dist_bray_quadrados)) * 100,
    file.path(resultado, "matriz_similaridade_fotoquadrados.csv")
  )
  
} else {
  
  # informa que não existem amostras suficientes para realizar a análise
  message(
    "Dendrograma nao calculado: sao necessarios pelo menos 2 fotoquadrados ",
    "com cobertura total maior que zero."
  )
}

# nMDS utilizando Jaccard (presenca e ausencia)

# transforma a cobertura em presença (1) ou ausência (0) de cada taxon.
mat_presenca <- (mat_quadrados > 0) * 1

# remove fotoquadrados sem nenhum taxon registrado
manter <- rowSums(mat_presenca) > 0
mat_presenca <- mat_presenca[manter, , drop = FALSE]

# calcula a dissimilaridade de Jaccard entre os fotoquadrados
dist_jaccard <- vegan::vegdist(
  mat_presenca,
  method = "jaccard",
  binary = TRUE
)

if (nrow(mat_presenca) >= 3) {
  
  # realiza o nMDS utilizando as distâncias de Jaccard
  # set.seed() garante a reprodutibilidade da ordenação
  # k = 2 define a representação em duas dimensões
  # trymax número máximo de tentativas para encontrar uma solução estável
  set.seed(123)
  
  ord_jaccard <- vegan::metaMDS(
    dist_jaccard,
    k = 3,
    trymax = 500,
    trace = FALSE
  )
  
  # recupera os metadados dos fotoquadrados na mesma ordem da matriz
  meta_jaccard <- meta[
    match(rownames(mat_presenca), meta$amostra),
  ]
  
  # organiza as coordenadas dos fotoquadrados e seus metadados
  pontos_jaccard <- data.frame(
    amostra = rownames(ord_jaccard$points),
    NMDS1 = ord_jaccard$points[, 1],
    NMDS2 = ord_jaccard$points[, 2],
    NMDS3 = ord_jaccard$points[, 3]
  ) %>%
    left_join(meta_jaccard, by = "amostra")
  
  # constrói os gráficos de ordenação dos fotoquadrados
  fig_nmds_jaccard <- (
    ggplot(pontos_jaccard, aes(
      NMDS1, NMDS2,
      color = factor(quadrado),
      shape = grupo
    )) +
      geom_point(size = 3.2, alpha = 1) +
      ggrepel::geom_text_repel(
        aes(label = paste0(transecto, " Q", quadrado)),
        show.legend = FALSE,
        size = 2.8,
        max.overlaps = Inf,
        box.padding = 0.5,
        point.padding = 0.4,
        segment.color = "grey50",
        segment.size = 0.3,
        seed = 123
      ) +
      scale_color_manual(
        values = c(
          "1" = "#174A8B",
          "2" = "#CD69C9",
          "3" = "#C51B5A",
          "4" = "#D45A00",
          "5" = "#FFA500"
        ),
        name = "Posição vertical",
        labels = function(x) paste0("Q", x)
      ) +
      scale_shape_manual(
        values = c(
          "A" = 16,
          "B" = 17,
          "C" = 15,
          "D" = 18,
          "E" = 3,
          "F" = 7,
          "G" = 8
        ),
        name = "Grupo"
      ) +
      theme_minimal() +
      labs(
        title = "nMDS - Jaccard",
        subtitle = sprintf(
          "Composição dos táxons por presença e ausência | Stress = %.3f",
          ord_jaccard$stress
        ),
        x = "nMDS1",
        y = "nMDS2"
      )
  ) +
    (
      ggplot(pontos_jaccard, aes(
        NMDS1, NMDS3,
        color = factor(quadrado),
        shape = grupo
      )) +
        geom_point(size = 3.2, alpha = 1) +
        ggrepel::geom_text_repel(
          aes(label = paste0(transecto, " Q", quadrado)),
          show.legend = FALSE,
          size = 2.8,
          max.overlaps = Inf,
          box.padding = 0.5,
          point.padding = 0.4,
          segment.color = "grey50",
          segment.size = 0.3,
          seed = 123
        ) +
        scale_color_manual(
          values = c(
            "1" = "#174A8B",
            "2" = "#CD69C9",
            "3" = "#C51B5A",
            "4" = "#D45A00",
            "5" = "#FFA500"
          ),
          name = "Posição vertical",
          labels = function(x) paste0("Q", x)
        ) +
        scale_shape_manual(
          values = c(
            "A" = 16,
            "B" = 17,
            "C" = 15,
            "D" = 18,
            "E" = 3,
            "F" = 7,
            "G" = 8
          ),
          name = "Grupo"
        ) +
        theme_minimal() +
        labs(
          title = "nMDS - Jaccard",
          subtitle = sprintf(
            "Composição dos táxons por presença e ausência | Stress = %.3f",
            ord_jaccard$stress
          ),
          x = "nMDS1",
          y = "nMDS3"
        )
    ) +
    (
      ggplot(pontos_jaccard, aes(
        NMDS2, NMDS3,
        color = factor(quadrado),
        shape = grupo
      )) +
        geom_point(size = 3.2, alpha = 1) +
        ggrepel::geom_text_repel(
          aes(label = paste0(transecto, " Q", quadrado)),
          show.legend = FALSE,
          size = 2.8,
          max.overlaps = Inf,
          box.padding = 0.5,
          point.padding = 0.4,
          segment.color = "grey50",
          segment.size = 0.3,
          seed = 123
        ) +
        scale_color_manual(
          values = c(
            "1" = "#174A8B",
            "2" = "#CD69C9",
            "3" = "#C51B5A",
            "4" = "#D45A00",
            "5" = "#FFA500"
          ),
          name = "Posição vertical",
          labels = function(x) paste0("Q", x)
        ) +
        scale_shape_manual(
          values = c(
            "A" = 16,
            "B" = 17,
            "C" = 15,
            "D" = 18,
            "E" = 3,
            "F" = 7,
            "G" = 8
          ),
          name = "Grupo"
        ) +
        theme_minimal() +
        labs(
          title = "nMDS - Jaccard",
          subtitle = sprintf(
            "Composição dos táxons por presença e ausência | Stress = %.3f",
            ord_jaccard$stress
          ),
          x = "nMDS2",
          y = "nMDS3"
        )
    )
  
  # salva o gráfico.
  ggsave(
    file.path(resultado, "09a_nMDS_Jaccard_NMDS1_NMDS2.png"),
    fig_nmds_jaccard[[1]],
    width = 10,
    height = 8,
    dpi = 300
  )
  
  ggsave(
    file.path(resultado, "09b_nMDS_Jaccard_NMDS1_NMDS3.png"),
    fig_nmds_jaccard[[2]],
    width = 10,
    height = 8,
    dpi = 300
  )
  
  ggsave(
    file.path(resultado, "09c_nMDS_Jaccard_NMDS2_NMDS3.png"),
    fig_nmds_jaccard[[3]],
    width = 10,
    height = 8,
    dpi = 300
  )
}
  


cat("\nTodas as analises foram feitas!")