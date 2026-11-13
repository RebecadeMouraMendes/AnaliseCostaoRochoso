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

## Funções importantes e seus pacotes 

### ANALISES ECOLOGICAS — vegan

vegan::specnumber() — calcula a riqueza de táxons.<br>
vegan::diversity() — calcula os índices de diversidade de Shannon e Simpson. <br>
vegan::vegdist() — calcula a dissimilaridade de Bray-Curtis. <br>
vegan::wcmdscale() — realiza a Análise de Coordenadas Principais (PCoA).

### AGRUPAMENTO — stats
stats::hclust() — realiza o agrupamento hierárquico (UPGMA).<br>
stats::as.dist() — converte uma matriz em um objeto de distâncias.

### MANIPULACAO DE DADOS — dplyr
dplyr::group_by() — agrupa os dados por uma ou mais variáveis.<br>
dplyr::summarise() — resume os dados agrupados, calculando médias, somas etc. <br>
dplyr::mutate() — cria ou modifica colunas.<br>
dplyr::select() — seleciona colunas.<br>
dplyr::filter() — filtra linhas de acordo com condições.<br>
dplyr::left_join() — combina tabelas utilizando uma coluna em comum.<br>
dplyr::arrange() — organiza os dados em determinada ordem.<br>
dplyr::distinct() — remove registros duplicados.<br>
dplyr::slice() — seleciona linhas por posição.<br>
dplyr::lead() — obtém o próximo valor de uma sequência.
