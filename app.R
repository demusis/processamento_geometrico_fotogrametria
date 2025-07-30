# Título: Ferramentas de Imagem (Corretor de Lente e Combinador)
# Data: 30 de julho de 2025
# Versão: Correção dos Textos de Ajuda com bslib


# --- Carregar Bibliotecas ---
library(shiny)
library(magick)
library(colourpicker)
library(shinycssloaders)
library(bslib) # Único pacote necessário para UI moderna e tooltips

# --- Opções Globais ---
options(shiny.maxRequestSize = 100 * 1024^2)

# --- Definição do Tema (bslib) ---
royal_blue_theme_v2 <- bs_theme(
  version = 5,
  bg = "#EFF2F5",
  fg = "#212529",
  primary = "#4169E1",
  secondary = "#ADB5BD",
  base_font = font_google("Roboto", local = FALSE),
  heading_font = font_google("Roboto", local = FALSE)
)


# --- Interface do Usuário (UI) ---
ui <- navbarPage(
  "Ferramentas de Imagem",
  theme = royal_blue_theme_v2,
  
  # --- Aba 1: Corretor de Distorção de Lente ---
  tabPanel(
    "Corretor de Distorção",
    icon = icon("camera-retro"),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Controles"),
          card_body(
            h5("1. Carregar Imagem"),
            fileInput("userImage", NULL, accept = c("image/png", "image/jpeg"), placeholder = "Selecione um arquivo"),
            hr(),
            h5("2. Ajustar Distorção"),
            p("Passe o mouse sobre (?) para detalhes.", style = "font-size: 0.9em;"),
            
            # MODIFICADO: Tooltips recriados com bslib::tooltip
            tags$label("k₁:", 
                       bslib::tooltip(shiny::icon("question-circle", style = "cursor: help;"),
                                      "Coeficiente de 1ª ordem. Corrige a distorção primária de 'barril' (< 0) ou 'almofada' (> 0).",
                                      placement = "right")),
            sliderInput("k1", label = NULL, min = -1.5, max = 1.5, value = 0, step = 0.001),
            
            tags$label("k₂:", 
                       bslib::tooltip(shiny::icon("question-circle", style = "cursor: help;"),
                                      "Coeficiente de 2ª ordem. Realiza ajustes finos na curvatura, mais influente nas bordas.",
                                      placement = "right")),
            sliderInput("k2", label = NULL, min = -1.0, max = 1.0, value = 0, step = 0.001),
            
            tags$label("k₃:",
                       bslib::tooltip(shiny::icon("question-circle", style = "cursor: help;"),
                                      "Coeficiente de 3ª ordem. Corrige distorções complexas como 'bigode', onde a curvatura se inverte.",
                                      placement = "right")),
            sliderInput("k3", label = NULL, min = -0.5, max = 0.5, value = 0, step = 0.001),
            
            hr(),
            div(style = "display: flex; justify-content: space-between;",
                actionButton("reset_all", "Reiniciar", icon = icon("refresh")),
                downloadButton("save_image", "Salvar", icon = icon("save"))
            )
          )
        )
      ),
      mainPanel(
        width = 10,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Visualização"),
          card_body(imageOutput("corrected_image_output", height = "auto") %>% withSpinner(color = "#4169E1"))
        )
      )
    )
  ),
  
  # --- Aba 2: Combinador de Imagens ---
  tabPanel(
    "Combinador de Imagens",
    icon = icon("layer-group"),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Controles"),
          card_body(
            h5("1. Carregar Imagens"),
            p("Devem possuir as mesmas dimensões.", style = "font-size: 0.9em;"),
            fileInput("file1", "Imagem Esquerda", accept = c("image/png", "image/jpeg")),
            fileInput("file2", "Imagem Direita", accept = c("image/png", "image/jpeg")),
            
            conditionalPanel(
              condition = "output.combiner_images_loaded == true",
              hr(),
              h5("2. Ajustar Sobreposição"),
              sliderInput("split_position", "Posição:", min = 0, max = 100, value = 50, post = "%"),
              sliderInput("line_thickness", "Espessura (px):", min = 0, max = 50, value = 2),
              colourInput("line_color", "Cor da Linha:", "black", allowTransparent = TRUE),
              
              hr(),
              h5("3. Legendas"),
              textInput("caption_left", "Legenda Esquerda:", placeholder = "Opcional"),
              textInput("caption_right", "Legenda Direita:", placeholder = "Opcional"),
              
              numericInput("caption_size", "Tamanho da Fonte:", value = 16, min = 8, max = 48, step = 1),
              colourInput("caption_color", "Cor da Fonte:", value = "black", allowTransparent = TRUE),
              
              selectInput("caption_gravity_y", "Posição Vertical:", 
                          choices = c("Topo" = "top", "Centro" = "center", "Base" = "bottom"), 
                          selected = "bottom"),
              
              hr(),
              downloadButton("save_combined_image", "Salvar Imagem", icon = icon("save"))
            )
          )
        )
      ),
      mainPanel(
        width = 10,
        card(
          class = "h-100",
          card_header(class = "bg-primary text-white", "Visualização"),
          card_body(imageOutput("combined_image", height = "auto") %>% withSpinner(color = "#4169E1"))
        )
      )
    )
  )
)

# --- Lógica do Servidor (Server) ---
server <- function(input, output, session) {
  
  # --- Lógica do Combinador de Imagens ---
  
  img1_reactive <- reactive({ req(input$file1); magick::image_read(input$file1$datapath) })
  img2_reactive <- reactive({ req(input$file2); magick::image_read(input$file2$datapath) })
  
  output$combiner_images_loaded <- reactive({ !is.null(input$file1) && !is.null(input$file2) })
  outputOptions(output, "combiner_images_loaded", suspendWhenHidden = FALSE)
  
  observeEvent(c(input$file1, input$file2), {
    req(img1_reactive(), img2_reactive())
    info1 <- magick::image_info(img1_reactive()); info2 <- magick::image_info(img2_reactive())
    if (info1$width != info2$width || info1$height != info2$height) {
      showNotification("As imagens não possuem as mesmas dimensões.", type = "error", duration = 8)
    }
  })
  
  generate_combined_image <- reactive({
    req(img1_reactive(), img2_reactive(), input$split_position, input$line_thickness >= 0)
    
    img1 <- img1_reactive(); img2 <- img2_reactive()
    info <- magick::image_info(img1)
    img_w <- info$width; img_h <- info$height
    
    split_pos_px <- as.integer(round((input$split_position / 100) * img_w))
    line_thick <- as.integer(input$line_thickness)
    line_col <- input$line_color
    
    vertical_pos <- input$caption_gravity_y
    switch(vertical_pos,
           "top"    = { gravity_left <- "northwest"; gravity_right <- "northeast" },
           "center" = { gravity_left <- "west";      gravity_right <- "east" },
           "bottom" = { gravity_left <- "southwest"; gravity_right <- "southeast" }
    )
    
    if (input$split_position > 0 && input$split_position < 100) {
      geom_left <- sprintf("%dx%d+0+0", split_pos_px, img_h)
      img_left <- magick::image_crop(img1, geom_left)
      
      geom_right <- sprintf("%dx%d+%d+0", (img_w - split_pos_px), img_h, split_pos_px)
      img_right <- magick::image_crop(img2, geom_right)
      
      base_image <- magick::image_append(c(img_left, img_right))
      
      if (line_thick > 0) {
        divider <- magick::image_blank(width = line_thick, height = img_h, color = line_col)
        offset_string <- sprintf("+%d+0", (split_pos_px - round(line_thick / 2)))
        intermediate_img <- magick::image_composite(base_image, divider, offset = offset_string)
      } else {
        intermediate_img <- base_image
      }
      
      final_img <- intermediate_img
      if (!is.null(input$caption_left) && input$caption_left != "") {
        final_img <- magick::image_annotate(final_img, input$caption_left, gravity = gravity_left, location = "+10+5", size = input$caption_size, color = input$caption_color)
      }
      if (!is.null(input$caption_right) && input$caption_right != "") {
        final_img <- magick::image_annotate(final_img, input$caption_right, gravity = gravity_right, location = "+10+5", size = input$caption_size, color = input$caption_color)
      }
      
    } else if (input$split_position <= 0) {
      final_img <- img2
      if (!is.null(input$caption_right) && input$caption_right != "") {
        final_img <- magick::image_annotate(final_img, input$caption_right, gravity = gravity_right, location = "+10+5", size = input$caption_size, color = input$caption_color)
      }
    } else {
      final_img <- img1
      if (!is.null(input$caption_left) && input$caption_left != "") {
        final_img <- magick::image_annotate(final_img, input$caption_left, gravity = gravity_left, location = "+10+5", size = input$caption_size, color = input$caption_color)
      }
    }
    return(final_img)
  })
  
  output$combined_image <- renderImage({
    final_img <- generate_combined_image()
    tmpfile <- magick::image_write(final_img, tempfile(fileext = '.png'), format = 'png')
    list(src = tmpfile, contentType = "image/png", width = "100%", height = "auto", alt = "Imagem combinada")
  }, deleteFile = TRUE)
  
  output$save_combined_image <- downloadHandler(
    filename = function() {
      paste0("imagem_combinada_com_legenda_", Sys.Date(), ".png")
    },
    content = function(file) {
      final_img_to_save <- generate_combined_image()
      magick::image_write(final_img_to_save, path = file, format = 'png')
    }
  )
  
  
  # --- Lógica do Corretor de Distorção ---
  
  vals <- reactiveValues(original_image = NULL, preview_image = NULL)
  
  reset_sliders <- function() {
    updateSliderInput(session, "k1", value = 0)
    updateSliderInput(session, "k2", value = 0)
    updateSliderInput(session, "k3", value = 0)
  }
  
  observeEvent(input$userImage, {
    req(input$userImage)
    vals$original_image <- magick::image_read(input$userImage$datapath)
    vals$preview_image <- magick::image_scale(vals$original_image, "1200")
    reset_sliders()
  })
  
  observeEvent(input$reset_all, {
    req(vals$original_image)
    reset_sliders()
    showNotification("Ajustes reiniciados.", type = "message")
  })
  
  processed_preview_image <- reactive({
    req(vals$preview_image)
    original_info <- magick::image_info(vals$preview_image)
    dist_params <- c(-input$k1, -input$k2, -input$k3)
    
    distorted_img <- magick::image_distort(vals$preview_image, 'barrel', dist_params, bestfit = TRUE)
    
    distorted_info <- magick::image_info(distorted_img)
    offset_x <- (distorted_info$width - original_info$width) / 2
    offset_y <- (distorted_info$height - original_info$height) / 2
    crop_geometry <- paste0(original_info$width, "x", original_info$height, sprintf("%+d", offset_x), sprintf("%+d", offset_y))
    
    magick::image_crop(distorted_img, crop_geometry)
  })
  
  debounced_preview <- debounce(processed_preview_image, 500)
  
  output$corrected_image_output <- renderImage({
    final_preview_img <- debounced_preview()
    tmpfile <- magick::image_write(final_preview_img, tempfile(fileext = '.png'), format = 'png')
    list(src = tmpfile, contentType = "image/png", width = "100%", height = "auto", alt = "Preview da imagem corrigida")
  }, deleteFile = TRUE)
  
  output$save_image <- downloadHandler(
    filename = function() {
      original_filename <- tools::file_path_sans_ext(basename(input$userImage$name))
      paste0(original_filename, "_corrigida.png")
    },
    content = function(file) {
      showNotification("Processando imagem em resolução total...", type = "message", duration = NULL, id = "processing_msg")
      
      original_info <- magick::image_info(vals$original_image)
      dist_params <- c(-input$k1, -input$k2, -input$k3)
      distorted_img <- magick::image_distort(vals$original_image, 'barrel', dist_params, bestfit = TRUE)
      
      distorted_info <- magick::image_info(distorted_img)
      offset_x <- (distorted_info$width - original_info$width) / 2
      offset_y <- (distorted_info$height - original_info$height) / 2
      crop_geometry <- paste0(original_info$width, "x", original_info$height, sprintf("%+d", offset_x), sprintf("%+d", offset_y))
      
      final_high_res_img <- magick::image_crop(distorted_img, crop_geometry)
      magick::image_write(final_high_res_img, path = file, format = 'png')
      removeNotification("processing_msg")
    }
  )
}

# --- Executar o Aplicativo ---
shinyApp(ui = ui, server = server)