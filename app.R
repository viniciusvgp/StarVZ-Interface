# --- Libraries ---
# Ensure all these libraries are installed
# install.packages(c("shiny", "shinyjs", "shinyFiles", "ggplot2", "plotly", "dplyr", "tidyr", "fs", "tibble", "starvz"))
library(shiny)
library(shinyjs)
library(shinyFiles)
library(ggplot2)
library(starvz)
library(tibble)
library(plotly)
library(dplyr)
library(tidyr)
library(fs)

# ----------------------
# UI - User Interface
# ----------------------

# Função para criar o sidebar da aplicação com seleção de workflow, diretório, painéis e opções
sidebar_ui <- function() {
  div(class = "sidebar",
      h3("1. Workflow and Data"),
      
      selectInput("workflow_type", "Select Workflow",
                  choices = c("StarVZ", "Tikki"), selected = "StarVZ"),
      
      shinyDirButton("data_dir", "Select Directory", "Choose data folder"),
      verbatimTextOutput("selected_dir", placeholder = TRUE),
      
      fileInput("config_file", "Configuration File (Optional)", accept = c(".yaml", ".yml")),
      
      actionButton("load_data", "Load Data", class = "btn"),
      hr(),
      
      h3("2. Analysis Panels"),
      checkboxInput("panel_st_active", "Space-Time", value = TRUE),
      checkboxInput("panel_kiteration_active", "K-Iterations", value = FALSE),
      
      conditionalPanel(
        condition = "input.workflow_type == 'StarVZ'",
        checkboxInput("panel_submitted_active", "Submitted Tasks", value = FALSE),
        checkboxInput("panel_starpu_active", "StarPU Activity", value = FALSE),
        checkboxInput("panel_ready_active", "Ready Queue", value = FALSE)
      ),
      
      h3("3. Options (Space-Time Panel)"),
      conditionalPanel("input.panel_st_active == true",
                       checkboxInput("st_legend", "Legend", FALSE),
                       checkboxInput("st_makespan", "Makespan", FALSE),
                       checkboxInput("st_outliers", "Outliers", FALSE),
                       
                       conditionalPanel(
                         condition = "input.workflow_type == 'StarVZ'",
                         checkboxInput("st_idleness", "Idleness", FALSE),
                         conditionalPanel("input.st_idleness == true",
                                          checkboxInput("st_idleness_all", "Idleness All", FALSE)
                         ),
                         checkboxInput("st_abe", "ABE", FALSE),
                         checkboxInput("st_cpb", "CPB", FALSE),
                         checkboxInput("st_tasks_active", "Highlight Tasks", FALSE),
                         conditionalPanel("input.st_tasks_active == true",
                                          numericInput("st_tasks_levels", "Levels", value = 3, min = 1, step = 1),
                                          # RÓTULO ATUALIZADO AQUI
                                          numericInput("st_tasks_list", "Task ID to Highlight", value = "1")
                         )
                       ),
                       
                       selectInput("st_labels", "Resource Labels:",
                                   choices = c("ALL", "1CPU_per_NODE", "1GPU_per_NODE", "FIRST_LAST", "NODES_only", "NODES_1_in_10", "1CPU_1GPU", "ALL_nompi"),
                                   selected = "FIRST_LAST")
      ),
      
      hr(),
      actionButton("plotly_button", "Generate Interactive Plot", class = "btn"),
      # BOTÃO DE DOWNLOAD ADICIONADO AQUI
      downloadButton("download_plot", "Download Static Plot (PNG)", class = "btn"),
      shinyjs::hidden(
        actionButton("back_button", "Back to Static Plot", class = "btn")
      )
  )
}

# Função auxiliar para calcular altura dinâmica do gráfico baseado nos painéis ativos
calculate_height <- function(panels_active) {
  height <- sum(unlist(panels_active)) * 400 
  return(paste0(max(height, 600), "px"))
}

# Define a UI principal com sidebar e área de gráficos
ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/style.css")),
  div(class = "container",
      sidebar_ui(),
      div(class = "main", uiOutput("plot_ui"))
  )
)

# ----------------------
# SERVER - Lógica da aplicação
# ----------------------

server <- function(input, output, session) {
  
  shinyDirChoose(input, "data_dir", roots = c(home = fs::path_home()), session = session)
  
  output$selected_dir <- renderText({
    req(input$data_dir)
    parseDirPath(c(home = fs::path_home()), input$data_dir)
  })
  
  rv <- reactiveValues(dado = NULL, data_dir = NULL, log_text = "Ready to load data.")
  
  observeEvent(input$load_data, {
    req(input$data_dir)
    rv$data_dir <- parseDirPath(c(home = fs::path_home()), input$data_dir)
    config_file <- if (!is.null(input$config_file)) input$config_file$datapath else NULL
    
    if (input$workflow_type == "StarVZ") {
      parquet_files <- fs::dir_ls(rv$data_dir, glob = "*.parquet")
      if (length(parquet_files) == 0) {
        showModal(modalDialog(
          title = ".parquet files not found",
          "Do you want to run StarVZ Phase 1 to generate the files?",
          footer = tagList(modalButton("Cancel"), actionButton("run_starvz_phase1", "Run Phase 1 (StarVZ)", class = "btn-primary"))
        ))
        return()
      }
      withProgress(message = 'Loading StarVZ data...', value = 0.5, {
        rv$dado <- starvz_read(rv$data_dir, config_file, selective = FALSE)
      })
      showNotification("StarVZ data loaded successfully!", type = "message")
      
    } else if (input$workflow_type == "Tikki") {
      processed_file <- file.path(rv$data_dir, "paje.worker_state.csv.gz")
      if (!file.exists(processed_file)) {
        showModal(modalDialog(
          title = "Tikki pre-processed files not found",
          "Do you want to run the Tikki Phase 1 script?",
          footer = tagList(modalButton("Cancel"), actionButton("run_tikki_phase1", "Run Phase 1 (Tikki)", class = "btn-primary"))
        ))
        return()
      }
      withProgress(message = 'Processing Tikki data (Phase 2)...', value = 0.5, {
        tryCatch({
          load_env <- new.env()
          load_env$trace_dir <- rv$data_dir
          load_env$conf <- config_file
          source("starvz-tikki-phase2.R", local = load_env)
          rv$dado <- load_env$dados
          rv$log_text <- "Tikki Phase 2 script executed successfully."
          showNotification("Tikki data loaded and processed!", type = "message")
        }, error = function(e){
          showModal(modalDialog(
            title = "Detailed Error in Phase 2",
            p("An error occurred during the execution of 'starvz-tikki-phase2.R' script."),
            tags$pre(paste(capture.output(print(e)), collapse = "\n"))
          ))
          rv$log_text <- paste("Error running Tikki Phase 2:", e$message)
        })
      })
    }
  })
  
  observeEvent(input$run_starvz_phase1, {
    removeModal()
    req(rv$data_dir)
    withProgress(message = "Executing StarVZ Phase 1...", value = 0.5, {
      rv$log_text <- tryCatch({
        starvz_path <- file.path(system.file("tools", package = "starvz"), "starvz")
        if (!file.exists(starvz_path)) stop("Executable 'starvz' not found.")
        args <- c("--use-paje-trace", "--keep", "--phase1", rv$data_dir)
        result <- system2(command = starvz_path, args = args, stdout = TRUE, stderr = TRUE)
        paste(result, collapse = "\n")
      }, error = function(e) paste("Error:", e$message))
    })
    if (length(fs::dir_ls(rv$data_dir, glob = "*.parquet")) > 0) {
      shinyjs::alert("StarVZ Phase 1 finished! Click 'Load Data' again.")
    } else {
      shinyjs::alert("Phase 1 executed, but no .parquet files generated.")
    }
  })
  
  observeEvent(input$run_tikki_phase1, {
    removeModal()
    req(rv$data_dir)
    withProgress(message = "Executing Tikki Phase 1...", value = 0.5, {
      rv$log_text <- tryCatch({
        result <- system2("Rscript", args = c("starvz-tikki-phase1.R", rv$data_dir), stdout = TRUE, stderr = TRUE)
        print(result)
        print(rv$data_dir)
      }, error = function(e) paste("Error:", e$message))
    })
    if (file.exists(file.path(rv$data_dir, "paje.worker_state.csv.gz"))) {
      shinyjs::alert("Tikki Phase 1 finished! Click 'Load Data' again.")
    } else {
      shinyjs::alert("Phase 1 executed, but no expected file.")
    }
  })
  
  data_to_plot <- reactive({
    req(rv$dado)
    temp_data <- rv$dado
    
    if (input$workflow_type == "StarVZ") {
      if (!is.null(temp_data$config$submitted)) temp_data$config$submitted$active <- input$panel_submitted_active
      if (!is.null(temp_data$config$starpu)) temp_data$config$starpu$active <- input$panel_starpu_active
      if (!is.null(temp_data$config$ready)) temp_data$config$ready$active <- input$panel_ready_active
    }
    
    if (!is.null(temp_data$config$st)) {
      temp_data$config$st$active <- input$panel_st_active
      temp_data$config$st$legend <- input$st_legend
      temp_data$config$st$makespan <- input$st_makespan
      temp_data$config$st$outliers <- input$st_outliers
      temp_data$config$st$labels <- input$st_labels
      #remover
      temp_data$config$kiteration$subite = FALSE
      
      if (input$workflow_type == "StarVZ") {
        temp_data$config$st$idleness <- input$st_idleness
        temp_data$config$st$idleness_all <- input$st_idleness_all
        temp_data$config$st$cpb <- input$st_cpb
        temp_data$config$st$abe$active <- input$st_abe
        
        if (is.null(temp_data$config$st$tasks)) {
          temp_data$config$st$tasks <- list()
        }
        
        temp_data$config$st$tasks$active <- FALSE
        temp_data$config$st$tasks$list <- character(0)
        temp_data$config$st$tasks$levels <- input$st_tasks_levels
        
        task_id_input <- input$st_tasks_list
        
        if (input$st_tasks_active) {
            temp_data$config$st$tasks$active <- TRUE
            temp_data$config$st$tasks$list <- c(as.character(task_id_input))
        }
      }
    }
    
    if (!is.null(temp_data$config$kiteration)) temp_data$config$kiteration$active <- input$panel_kiteration_active
    
    return(temp_data)
  })
  
  # NOVO: Objeto reativo para o gráfico estático. Evita recálculo.
  static_plot_object <- reactive({
    req(data_to_plot())
    starvz_plot(data_to_plot())
  })
  
  # renderPlot agora usa o objeto reativo.
  output$plot <- renderPlot({
    static_plot_object()
  })
  
  output$plotly_plot <- renderPlotly({
    d <- data_to_plot()
    req(d, d$Application, nrow(d$Application) > 0)
    
    plotly_raw_data <- d$Application %>%
      arrange(End) %>%
      select(Start, End, Position, Height, Value, JobId) %>%
      mutate(
        xP0 = Start, yP0 = Position, xP1 = End, yP1 = Position,
        xP2 = End, yP2 = Position + Height - 0.2, xP3 = Start, yP3 = Position + Height - 0.2,
        xP4 = Start, yP4 = Position, xP5 = NA, yP5 = NA
      ) %>%
      select(-Start, -End, -Position, -Height)
    
    plotly_data_xy <- plotly_raw_data %>%
      pivot_longer(
        cols = starts_with("x") | starts_with("y"),
        names_to = c(".value", "group"), names_pattern = "(.)P(\\d)"
      )
    
    plotly_data <- plotly_data_xy %>%
      left_join(d$Colors %>% select(Value, Color), by = "Value")
    
    p_plotly <- plot_ly(
      data = plotly_data, x = ~x, y = ~y, mode = "lines", fill = "toself",
      text = ~paste("JobId:", JobId, "<br>Value:", Value), customdata = ~JobId,
      hoveron = "points+fills", hoverinfo = "text",
      type = "scatter", split = ~Value, color = I(plotly_data$Color),
      line = list(width = 0.5)
    ) %>% layout(
      xaxis = list(title = "Time (ms)", rangeslider = list(visible = TRUE)),
      yaxis = list(title = "Resource"),
      showlegend = input$st_legend, hovermode = "closest", dragmode = "zoom"
    )
    
    event_register(p_plotly, "plotly_click")
    return(p_plotly)
  })
  
  output$plot_ui <- renderUI({
    if (is.null(rv$dado)) {
      return(h3("Select a directory and load data to get started."))
    }
    active_panels <- list(st = input$panel_st_active, kiteration = input$panel_kiteration_active)
    if (input$workflow_type == "StarVZ") {
      active_panels$submitted <- input$panel_submitted_active
      active_panels$starpu <- input$panel_starpu_active
      active_panels$ready <- input$panel_ready_active
    }
    tagList(
      plotOutput("plot", width = "100%", height = calculate_height(active_panels)),
      shinyjs::hidden(div(id = "plotly_container", plotlyOutput("plotly_plot", width = "100%"))),
      hr(), h4("Execution Log"), verbatimTextOutput("log")
    )
  })
  
  # NOVO: Handler para o botão de download
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("starvz-plot-", Sys.Date(), ".png")
    },
    content = function(file) {
      # Recalcula a altura do gráfico para garantir que a imagem salva tenha o tamanho correto
      active_panels <- list(st = input$panel_st_active, kiteration = input$panel_kiteration_active)
      if (input$workflow_type == "StarVZ") {
        active_panels$submitted <- input$panel_submitted_active
        active_panels$starpu <- input$panel_starpu_active
        active_panels$ready <- input$panel_ready_active
      }
      
      plot_height_px <- max(sum(unlist(active_panels)) * 400, 600)
      # Define uma largura padrão para o arquivo
      plot_width_px <- 1200 
      
      # Usa ggsave para salvar o gráfico (que está no objeto reativo) em um arquivo PNG
      ggsave(
        file, 
        plot = static_plot_object(), 
        device = "png",
        width = plot_width_px,
        height = plot_height_px,
        units = "px",
        dpi = 96 
      )
    }
  )
  
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    
    if (!is.null(click_data) && !is.null(click_data$customdata)) {
      clicked_job_id <- as.character(click_data$customdata[[1]])
      
      updateTextInput(session, "st_tasks_list", value = clicked_job_id)
      
      showNotification(paste("Highlighting JobId:", clicked_job_id), type = "message")
    }
  })
  
  observeEvent(input$plotly_button, {
    req(rv$dado)
    shinyjs::hide("plot")
    shinyjs::show("plotly_container")
    shinyjs::hide("plotly_button")
    shinyjs::show("back_button")
    showNotification("Interactive plot generated.", type = "message")
  })
  
  observeEvent(input$back_button, {
    shinyjs::show("plot")
    shinyjs::hide("plotly_container")
    shinyjs::show("plotly_button")
    shinyjs::hide("back_button")
  })
  
  output$log <- renderText({
    rv$log_text
  })
}

args = commandArgs(trailingOnly=TRUE)

# Initialize the Shiny app
if (length(args)==0) {
    shinyApp(ui = ui, server = server)
} else if (length(args)==1) {
    port = as.integer(args[1])
    print(paste("Running on port:", port))
    options(shiny.port = port)
    shinyApp(ui = ui, server = server)
}
