# AnaliseCostaoRochoso

Primeiro passo - Instale as bibliotecas

```
install.packages( 
  c("readxl", "dplyr", "tidyr", "ggplot2", "vegan"), 
  dependencies = TRUE, 
  repos = "https://cloud.r-project.org" 
)
```
Segundo passo - Coloque o endereço da planilha nessa linha

```arquivo <- "endereco da planilha"```

Terceiro passo - Ctrl + Enter roda todo o script

** Funções importantes e seus pacotes **

# ANALISES ECOLOGICAS — vegan
# vegan::specnumber() — calcula a riqueza de táxons.
# vegan::diversity() — calcula os índices de diversidade de Shannon e Simpson.
# vegan::vegdist() — calcula a dissimilaridade de Bray-Curtis.
# vegan::wcmdscale() — realiza a Análise de Coordenadas Principais (PCoA).

# AGRUPAMENTO — stats
# stats::hclust() — realiza o agrupamento hierárquico (UPGMA).
# stats::as.dist() — converte uma matriz em um objeto de distâncias.

# MANIPULACAO DE DADOS — dplyr
# dplyr::group_by() — agrupa os dados por uma ou mais variáveis.
# dplyr::summarise() — resume os dados agrupados, calculando médias, somas etc.
# dplyr::mutate() — cria ou modifica colunas.
# dplyr::select() — seleciona colunas.
# dplyr::filter() — filtra linhas de acordo com condições.
# dplyr::left_join() — combina tabelas utilizando uma coluna em comum.
# dplyr::arrange() — organiza os dados em determinada ordem.
# dplyr::distinct() — remove registros duplicados.
# dplyr::slice() — seleciona linhas por posição.
# dplyr::lead() — obtém o próximo valor de uma sequência.

# ORGANIZACAO DOS DADOS — tidyr
# tidyr::pivot_longer() — transforma dados do formato largo para longo.
# tidyr::pivot_wider() — transforma dados do formato longo para largo.

# GRAFICOS — ggplot2
# ggplot2::ggplot() — inicia a construção de um gráfico.
# ggplot2::aes() — define as variáveis visuais do gráfico.
# ggplot2::geom_col() — constrói gráficos de barras com valores definidos.
# ggplot2::geom_point() — adiciona pontos ao gráfico.
# ggplot2::geom_line() — adiciona linhas ao gráfico.
# ggplot2::geom_boxplot() — constrói gráficos de caixas.
# ggplot2::geom_jitter() — adiciona pontos com pequeno deslocamento.
# ggplot2::facet_wrap() — separa o gráfico em painéis.
# ggplot2::coord_flip() — inverte os eixos do gráfico.
# ggplot2::theme_minimal() — aplica um tema visual minimalista.
# ggplot2::labs() — define títulos e nomes dos eixos.
# ggplot2::ggsave() — salva os gráficos em arquivos.

# IMPORTACAO DE DADOS — readxl
# readxl::read_excel() — importa planilhas do Excel.

# MANIPULACAO DE TEXTO — stringr
# stringr::str_match() — identifica e extrai padrões de texto.
