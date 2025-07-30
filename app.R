# Título: Corretor Manual de Distorção de Lente
# Data: 30 de julho de 2025


# --- Carregar Bibliotecas ---
library(shiny)
library(magick)
library(shinycssloaders)
library(shinyBS)

# --- Opções Globais ---
options(shiny.maxRequestSize = 100*1024^2)

# --- UI com Remoção do bsTooltip Problemático ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            .tooltip-inner { max-width: 350px !important; text-align: left !important; }
            .h4-style { font-size: 18px; font-weight: 500; line-height: 1.2; margin-top: 10px; margin-bottom: 10px; }
        "))
  ),
  
  titlePanel("Corretor Manual de Distorção de Lente"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("1. Carregar Imagem"),
                 fileInput("userImage", "Selecione um arquivo", accept = c("image/png", "image/jpeg")),
                 
                 hr(),
                 
                 tags$div(class = "h4-style",
                          "2. Ajuste da Distorção Radial ",
                          tags$span(id = "optim_help", shiny::icon("info-circle", style = "color: #17a2b8; cursor: help;"))
                 ),
                 # A LINHA bsTooltip() FOI REMOVIDA DAQUI
                 
                 p("Passe o mouse sobre os ícones (?) para uma explicação detalhada dos coeficientes."),
                 
                 tags$label("k₁:", tags$span(id = "k1_help", shiny::icon("question-circle", style = "color: #007bff; cursor: help;"))),
                 sliderInput("k1", label = NULL, min = -1.5, max = 1.5, value = 0, step = 0.001),
                 bsTooltip(id = "k1_help", title = "Coeficiente radial de 1ª ordem. É o termo dominante na correção. Geometricamente, controla a curvatura primária das linhas retas. Valor < 0 corrige a distorção de \"barril\" (linhas curvam-se para fora, comum em lentes grande-angulares). Valor > 0 corrige a distorção de \"almofada\" (linhas curvam-se para dentro, comum em lentes teleobjetivas).", placement = "right", trigger = "hover"),
                 
                 tags$label("k₂:", tags$span(id = "k2_help", shiny::icon("question-circle", style = "color: #007bff; cursor: help;"))),
                 sliderInput("k2", label = NULL, min = -1.0, max = 1.0, value = 0, step = 0.001),
                 bsTooltip(id = "k2_help", title = "Coeficiente radial de 2ª ordem. Modela desvios da curvatura primária. Seu efeito é proporcional à quarta potência do raio, sendo mais influente nas bordas da imagem. É usado para ajustes finos em distorções que não são perfeitamente parabólicas.", placement = "right", trigger = "hover"),
                 
                 tags$label("k₃:", tags$span(id = "k3_help", shiny::icon("question-circle", style = "color: #007bff; cursor: help;"))),
                 sliderInput("k3", label = NULL, min = -0.5, max = 0.5, value = 0, step = 0.001),
                 bsTooltip(id = "k3_help", title = "Coeficiente radial de 3ª ordem. Corrige distorções residuais de alta frequência. Seu efeito é proporcional à sexta potência do raio. É fundamental para corrigir padrões complexos como a distorção \"em bigode\" (mustache), onde a curvatura das linhas se inverte próximo às bordas.", placement = "right", trigger = "hover"),
                 
                 hr(),
                 actionButton("reset_all", "Reiniciar Ajustes", icon = icon("refresh")),
                 hr(),
                 downloadButton("save_image", "Salvar Imagem Corrigida", icon = icon("save"))
    ),
    mainPanel(
      imageOutput("corrected_image_output", height = "auto") %>% withSpinner(type = 6)
    )
  )
)

# --- Server com Adição do addTooltip ---
server <- function(input, output, session) {
  
  # Adiciona o tooltip para o ícone de otimização a partir do servidor
  addTooltip(session, id = "optim_help", 
             title = "Otimização para Imagens Grandes: Para garantir uma experiência fluida, os ajustes são feitos em uma pré-visualização de alta qualidade. A imagem em resolução total é processada apenas quando você clica em 'Salvar Imagem Corrigida'.",
             placement = "right", trigger = "hover")
  
  vals <- reactiveValues(original_image = NULL, preview_image = NULL)
  reset_sliders <- function() { updateSliderInput(session, "k1", value = 0); updateSliderInput(session, "k2", value = 0); updateSliderInput(session, "k3", value = 0) }
  observeEvent(input$userImage, { req(input$userImage); vals$original_image <- image_read(input$userImage$datapath); vals$preview_image <- image_scale(vals$original_image, "1200"); reset_sliders() })
  observeEvent(input$reset_all, { req(vals$original_image); reset_sliders(); showNotification("Ajustes reiniciados.", type="message") })
  processed_preview_image <- reactive({
    req(vals$preview_image)
    original_info <- image_info(vals$preview_image)
    dist_params_radial <- c(-input$k1, -input$k2, -input$k3)
    distorted_img <- image_distort(vals$preview_image, 'barrel', dist_params_radial, bestfit = TRUE)
    distorted_info <- image_info(distorted_img)
    offset_x <- (distorted_info$width - original_info$width) / 2
    offset_y <- (distorted_info$height - original_info$height) / 2
    crop_geometry <- paste0(original_info$width, "x", original_info$height, sprintf("%+d", offset_x), sprintf("%+d", offset_y))
    image_crop(distorted_img, crop_geometry)
  })
  debounced_preview <- debounce(processed_preview_image, 500)
  output$corrected_image_output <- renderImage({ final_preview_img <- debounced_preview(); tmpfile <- image_write(final_preview_img, tempfile(fileext = '.png'), format = 'png'); list(src = tmpfile, contentType = "image/png", width = "100%", height = "auto", alt = "Imagem corrigida (preview)") }, deleteFile = TRUE)
  output$save_image <- downloadHandler(
    filename = function() { original_filename <- tools::file_path_sans_ext(basename(input$userImage$name)); paste0(original_filename, "_corrigida.png") },
    content = function(file) {
      showNotification("Processando imagem em resolução total. Aguarde...", type = "message", duration = NULL, id = "processing_msg")
      original_info <- image_info(vals$original_image)
      dist_params_radial <- c(-input$k1, -input$k2, -input$k3)
      distorted_img <- image_distort(vals$original_image, 'barrel', dist_params_radial, bestfit = TRUE)
      distorted_info <- image_info(distorted_img)
      offset_x <- (distorted_info$width - original_info$width) / 2
      offset_y <- (distorted_info$height - original_info$height) / 2
      crop_geometry <- paste0(original_info$width, "x", original_info$height, sprintf("%+d", offset_x), sprintf("%+d", offset_y))
      final_high_res_img <- image_crop(distorted_img, crop_geometry)
      image_write(final_high_res_img, path = file, format = 'png')
      removeNotification("processing_msg")
    }
  )
}

# --- Executar o Aplicativo ---
shinyApp(ui = ui, server = server)