## app.R ##
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(geomorph)
  library(facefuns)
  library(dplyr)
})

## functions ----

debug_msg <- function(txt) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if (is_local) message(txt)
}

## tabs ----

intro_tab <- tabItem(
  tabName = "intro_tab",
  h3("Intro"),
  p("This shiny app is under development and likely to have a lot of bugs."),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Load and Visualise",
      fileInput("load_tps", "Load from TPS", width = "100%"),
      # rotation buttons
      actionButton("rotate_orig", "Original Orientation"),
      actionButton("flipX", "Flip X"),
      actionButton("flipY", "Flip Y"),
      actionButton("rotateC", "Rotate Clockwise"),
      actionButton("rotateCC", "Rotate CounterClockwise"),
      # plotting buttons
      selectInput("selected_pcs", "Choose PC", c()),
      numericInput("vis_sd", "SDs to Visualise", 3),
      actionButton("plot_pc", "Plot PC"),
      plotOutput("plot2d")
  )
)

## UI ----
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "facefuns"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Intro", tabName = "intro_tab")
    ),
    actionButton("demo_tps", "Demo TPS")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      intro_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  v <- reactiveValues()

  # demo_tps ----
  observeEvent(input$demo_tps, { debug_msg("demo_tps")
    v$tps_path <- system.file("extdata", "LondonSet.tps", package="facefuns")
  })

  # load_tps ----
  observeEvent(input$load_tps, { debug_msg("load_tps")
    v$tps_path <- input$load_tps$datapath
  }, ignoreNULL = TRUE)

  # tps_path ----
  observeEvent(v$tps_path, { debug_msg("tps_path")
    v$data <- geomorph::readland.tps(
      v$tps_path,
      specID = "ID",
      warnmsg = FALSE
    )

    data_gpa <- geomorph::gpagen(v$data, print.progress = FALSE)
    v$aligned <- data_gpa$coords
  }, ignoreNULL = TRUE)

  # v$aligned ----
  observeEvent(v$aligned, { debug_msg("v$aligned")
    v$pca <- geomorph::gm.prcomp(v$aligned)
    v$ref <- geomorph::mshape(v$aligned)

    # set up pcs
    x <- 1:length(v$pca$d)
    names(x) <- paste0("PC", x)
    updateSelectInput(session, "selected_pcs", choices = x)

    # plot when v$aligned changes
    output$plot2d <- renderPlot({
      plot(v$aligned[, , 1], asp = 1)
    })
  })

  ## rotate ----
  observeEvent(input$rotate_orig, { debug_msg("rotate_orig")
    if (is.null(v$data)) return()
    data_gpa <- geomorph::gpagen(v$data, print.progress = FALSE)
    v$aligned <- data_gpa$coords
  }, ignoreNULL = TRUE)

  observeEvent(input$flipX, { debug_msg("flipX")
    v$aligned <- geomorph::rotate.coords(v$aligned, type = "flipX")
  }, ignoreNULL = TRUE)

  observeEvent(input$flipY, { debug_msg("flipY")
    v$aligned <- geomorph::rotate.coords(v$aligned, type = "flipY")
  }, ignoreNULL = TRUE)

  observeEvent(input$rotateC, { debug_msg("rotateC")
    v$aligned <- geomorph::rotate.coords(v$aligned, type = "rotateC")
  }, ignoreNULL = TRUE)

  observeEvent(input$rotateCC, { debug_msg("rotateCC")
    v$aligned <- geomorph::rotate.coords(v$aligned, type = "rotateCC")
  }, ignoreNULL = TRUE)


  ## plot_pc ----
  observeEvent(input$plot_pc, { debug_msg("plot_pc")
    if (is.null(v$aligned)) return(NULL)

    which_pcs <- as.integer(input$selected_pcs)

    output$plot2d <- renderPlot({
      plot2DPCs(v$pca, v$ref, which_pcs, input$vis_sd)
   })
  })

} # end server()

shinyApp(ui, server)
