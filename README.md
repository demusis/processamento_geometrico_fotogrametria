# Sistema de Retificação e Análise Comparativa para Fotogrametria

SRAC é uma aplicação web interativa, desenvolvida em **R/Shiny**, que oferece um conjunto de ferramentas para a correção geométrica e análise visual de imagens, com foco em tarefas de pré-processamento para fotogrametria.

-----

## Funcionalidades

A aplicação é organizada em três módulos principais, cada um focado em uma tarefa específica de processamento de imagem.

### 1\. Correção de Distorção de Lente

Este módulo é projetado para corrigir distorções radiais (em barril ou almofada) comumente introduzidas por lentes de câmeras. Oferece dois modos de operação:

  * **Ajuste Manual:** Controle preciso dos coeficientes de distorção radial de 3ª ordem ($k\_1, k\_2, k\_3$) através de *sliders* interativos.
  * **Otimização Automática:** Capacidade de desenhar linhas de referência sobre feições da imagem que deveriam ser retas. Um otimizador numérico (`optim`) calcula os parâmetros $k$ que melhor retificam as linhas desenhadas, propondo uma correção automática.
  * **Gestão de Pontos:** Os conjuntos de polilinhas podem ser salvos em um arquivo `.rds` e carregados posteriormente para reaplicar a mesma transformação.

### 2\. Análise Comparativa

Uma ferramenta para a inspeção visual e comparação de duas imagens lado a lado. É ideal para verificar o resultado de correções ou comparar imagens de diferentes épocas ou sensores.

  * **Divisor Interativo:** Um *slider* que move uma linha divisória sobre as imagens, permitindo uma comparação detalhada de áreas específicas.
  * **Anotações:** Adição de legendas personalizáveis em ambos os lados da imagem, com controle de fonte, cor e posição.

### 3\. Correção por Pontos de Controle (TPS)

Para distorções complexas e não radiais, este módulo utiliza a interpolação **Thin Plate Spline (TPS)**. O usuário pode corrigir deformações geométricas arbitrárias.

  * **Desenho de Vetores de Correção:** O usuário desenha polilinhas sobre feições distorcidas; o sistema as interpreta como vetores que mapeiam a geometria distorcida para uma geometria corrigida e retilínea.
  * **Gestão de Pontos:** Os conjuntos de polilinhas podem ser salvos em um arquivo `.rds` e carregados posteriormente para reaplicar a mesma transformação.
  * **Controle de Suavização:** Um parâmetro de regularização (λ) permite controlar a rigidez da transformação, evitando artefatos indesejados.

-----

## Tecnologias e Pacotes

A aplicação é construída inteiramente em **R**, utilizando os seguintes pacotes para alcançar sua funcionalidade:

  * **Framework:** `shiny`
  * **Interface de Usuário:** `bslib` (para theming com Bootstrap 5)
  * **Processamento de Imagem:** `magick` (módulos 1 e 2) e `imager` (módulo 3)
  * **Métodos Numéricos:** `fields` (para a interpolação TPS) e `stats::optim` (para a otimização de distorção)
  * **Utilitários:** `dplyr`, `shinycssloaders`

-----

## Instalação e Execução

Para executar a aplicação localmente, siga os passos abaixo.

### Pré-requisitos

  * [R](https://cran.r-project.org/) (versão 4.0 ou superior)
  * [RStudio](https://posit.co/download/rstudio-desktop/) (recomendado)

### Passos

1.  **Clone o repositório:**

2.  **Abra o RStudio** e, no console, instale as dependências necessárias executando o comando:

    ```r
    install.packages(c("shiny", "magick", "colourpicker", "shinycssloaders", "bslib", "imager", "dplyr", "fields"))
    ```

3.  **Execute a aplicação:**

    ```r
    shiny::runApp('app.R')
    ```

A aplicação será iniciada e estará acessível no seu navegador web local.
