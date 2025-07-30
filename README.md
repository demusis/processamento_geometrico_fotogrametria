# Corretor Manual de Distorção de Lente

Aplicação em R e Shiny desenvolvida para a correção interativa de distorções radiais em imagens digitais. A ferramenta permite que o utilizador ajuste manualmente os coeficientes de distorção e visualize o resultado em tempo real, sendo útil em áreas como fotografia e fotogrametria onde a precisão geométrica da imagem é necessária.

---

## Funcionalidades

* **Carregamento de Imagem**: Suporte para arquivos nos formatos `PNG` e `JPEG`.
* **Ajuste Interativo**: Controlo preciso sobre os três primeiros coeficientes de distorção radial ($k_1, k_2, k_3$) através de controlos deslizantes (*sliders*).
* **Pré-visualização Otimizada**: A manipulação é realizada sobre uma pré-visualização da imagem (com largura máxima de 1200 pixels) para garantir a fluidez da interface.
* **Processamento em Resolução Total**: A imagem final é processada com a resolução original no momento do download para assegurar a máxima qualidade.
* **Reinicialização de Parâmetros**: Um botão permite reverter todos os coeficientes aos seus valores iniciais (zero).
* **Documentação Integrada**: *Tooltips* informativos explicam o fundamento técnico e o efeito de cada coeficiente de distorção.

---

## Demonstração

A interface do utilizador é composta por um painel lateral para o carregamento do ficheiro e ajuste dos parâmetros, e um painel principal que exibe a imagem corrigida.

---

## Detalhes Técnicos

A correção da distorção é fundamentada no modelo polinomial de Brown-Conrady, que descreve o desvio dos pontos da imagem em função da sua distância ao centro ótico. A aplicação utiliza a função `image_distort` do pacote `magick`, implementando a correção para a distorção radial através da seguinte fórmula:

$$
\mathbf{x}_{u} = \mathbf{x}_{d} \cdot (1 + k_1 r^2 + k_2 r^4 + k_3 r^6)
$$

Onde:
- $\mathbf{x}_{u}$ representa as coordenadas do ponto corrigido (não distorcido).
- $\mathbf{x}_{d}$ representa as coordenadas do ponto na imagem original (distorcida).
- $r$ é a distância radial do ponto ao centro da imagem.
- $k_1, k_2, k_3$ são os coeficientes de distorção radial ajustados pelo utilizador:
    - **$k_1$**: Coeficiente de primeira ordem, principal responsável pela correção da **distorção de barril** (*barrel distortion*), comum em lentes grande-angulares, e da **distorção de almofada** (*pincushion distortion*), observada em teleobjetivas.
    - **$k_2$**: Coeficiente de segunda ordem, utilizado para ajustes finos em curvaturas não perfeitamente parabólicas.
    - **$k_3$**: Coeficiente de terceira ordem, que permite corrigir distorções complexas como a **distorção em bigode** (*mustache distortion*), onde a curvatura das linhas se inverte próximo às bordas.

Para otimizar a performance, a aplicação utiliza um *debounce timer* de 500 ms, atualizando a pré-visualização somente após o utilizador pausar o ajuste dos *sliders*.

---

## Instalação e Execução

Para executar esta aplicação localmente, são necessários o R e, preferencialmente, o RStudio.

1.  **Instalar as dependências:**
    Abra uma sessão R no diretório do projeto e execute o comando seguinte para instalar os pacotes necessários:
    ```r
    install.packages(c("shiny", "magick", "shinycssloaders", "shinyBS"))
    ```

2.  **Executar a aplicação:**
    Com o arquivo `app.R` (ou o nome correspondente) aberto no RStudio, clique em "Run App". Alternativamente, execute no console R:
    ```r
    shiny::runApp()
    ```

---

## Dependências

O projeto depende dos seguintes pacotes R:

* `shiny`: Framework para a criação de aplicações web.
* `magick`: Interface para a biblioteca de processamento de imagem ImageMagick.
* `shinycssloaders`: Adiciona indicadores de carregamento (*spinners*) a outputs Shiny.
* `shinyBS`: Componentes Bootstrap para Shiny, utilizados aqui para os *tooltips*.
