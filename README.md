# AnaliseCostaoRochoso

- Instale o R Studio: https://posit.co/downloads
  
- Instale as bibliotecas no console

<br>
<img width="938" height="189" alt="Captura de tela de 2026-11-14 19-18-59" src="https://github.com/user-attachments/assets/4b32da43-3898-460b-b27c-fe112cd1d728" />
<br>
```
install.packages( 
  c("readxl", "dplyr", "tidyr", "ggplot2", "vegan", "stringr", "ggh4x","colorspace", "ggrepel"),
  dependencies = TRUE, 
  repos = "https://cloud.r-project.org" 
)
```
- Coloque o endereço da planilha na linha 14:

```arquivo <- "endereco da planilha"```

- Ctrl + A e depois Ctrl + Enter roda todo o script

- Vai ser criado a pasta resultado_costao no downloads, suas figuras estarão lá

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
