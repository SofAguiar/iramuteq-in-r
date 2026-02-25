# IRAMUTEQ in R

[![R-v4.0+](https://img.shields.io/badge/R-v4.0+-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Este repositório oferece a **reprodução programática** das análises realizadas pelo software IRAMUTEQ (versão 0.8 Alpha 7) diretamente em ambiente R. O objetivo é remover a interface gráfica (GUI), permitindo maior transparência metodológica, customização de parâmetros estatísticos e automação de fluxos de trabalho para análise de dados textuais.

## Motivação e Diferenciais

O IRAMUTEQ é uma interface poderosa, mas a execução via script R oferece:
* **Transparência:** Inspeção direta dos algoritmos de classificação e redução de dimensionalidade.
* **Reprodutibilidade:** Facilidade em replicar análises em diferentes conjuntos de dados sem cliques manuais.
* **Customização:** Liberdade para ajustar temas de gráficos, lematizações e limiares de frequência que a GUI limita.
* **Integração:** Conecte os resultados diretamente com outros pacotes modernos de NLP e visualização (ggplot2, tidytext).

## Análises Implementadas
O projeto busca cobrir as principais funcionalidades do IRAMUTEQ:
- [ ] **Estatísticas Textuais:** Frequência, formas ativas e suplementares.
- [ ] **Nuvem de Palavras:** Customização estética avançada.
- [ ] **Análise de Similitude:** Baseada na teoria dos grafos.
- [ ] **CHD (Método Reinert):** Classificação Hierárquica Descendente e análise de clusters.
- [ ] **Análise de Correspondência (AFC):** Visualização fatorial de classes e segmentos.

## Fonte dos Scripts
Os códigos contidos aqui são adaptações e melhorias baseadas nos scripts originais de **Pierre Ratinaud**, disponíveis no repositório oficial:
🔗 [GitLab Huma-Num - IRAMUTEQ Rscripts](https://gitlab.huma-num.fr/pratinaud/iramuteq/-/tree/master/Rscripts)

## Como Começar (Work in Progress)

1. **Pré-requisitos:**
   Certifique-se de ter o R instalado e as dependências necessárias:
   ```R
   install.packages(c("tm", "igraph", "wordcloud", "proxy", "cluster"))