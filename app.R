##########################
##  app_dashboard.R     ##
##########################
library(shiny)
library(shinydashboard)
library(ggplot2)

blue  <- "#1E88E5"
light <- "#E3F2FD"

pretty_label <- function(x) if (identical(x, "hum")) "Human" else "AI"

# ── UI -----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = span(icon("eye"), "aiDetectR")),
  
  dashboardSidebar(
    fileInput("img", "Upload JPEG or PNG", accept = c(".jpg", ".jpeg", ".png")),
    numericInput("thr", "Flag if AI confidence ≥ (%)", 50, 0, 100, 5),
    actionButton("run", "Analyse", icon = icon("play")),
    width = 260
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(sprintf("
        .content-wrapper { background:%s; }
        #preview img { max-width:100%%; height:320px; object-fit:contain; }
    ", light)))),
    
    fluidRow(
      box(title = "Image preview", status = "primary",
          solidHeader = TRUE, width = 6,
          imageOutput("preview", height = "320px")),
      
      ## right column now has two stacked boxes
      column(
        width = 6,
        valueBoxOutput("predBox", width = 12),
        infoBoxOutput("flagBox", width = 12)   # <─ new
      )
    ),
    
    fluidRow(
      box(title = "Full probabilities", status = "info",
          solidHeader = TRUE, width = 6, verbatimTextOutput("stats")),
      box(title = "Probability bar", status = "info",
          solidHeader = TRUE, width = 6, plotOutput("probPlot", height = "220px"))
    )
  )
)

# ── server -------------------------------------------------------------------
server <- function(input, output, session) {
  
  img_path <- reactiveVal()
  
  observeEvent(input$img, {
    req(input$img)
    img_path(input$img$datapath)
  })
  
  result <- eventReactive(input$run, {
    req(img_path())
    detect_ai_image(img_path(), threshold = input$thr/100)
  })
  
  # preview -------------------------------------------------------------------
  output$preview <- renderImage({
    req(img_path())
    list(src = img_path(), contentType = mime::guess_type(img_path()), deleteFile = FALSE)
  }, deleteFile = FALSE)
  
  # headline valueBox ---------------------------------------------------------
  output$predBox <- renderValueBox({
    req(result())
    res <- result()
    lbl <- pretty_label(res$label)
    col <- if (lbl == "AI") "red" else "green"
    
    valueBox(
      paste0(round(res$confidence, 1), "%"),
      subtitle = paste("Predicted:", toupper(lbl)),
      icon     = icon(if (lbl == "AI") "robot" else "face-grin"),
      color    = col
    )
  })
  
  # NEW infoBox under valueBox ------------------------------------------------
  output$flagBox <- renderInfoBox({
    req(result())
    res <- result()
    
    if (res$flagged) {
      infoBox("Flagged", "AI confidence ≥ threshold",
              icon = icon("triangle-exclamation"),
              color = "red", fill = TRUE)
    } else {
      infoBox("Clear", "Below AI threshold",
              icon = icon("thumbs-up"),
              color = "green", fill = TRUE)
    }
  })
  
  # printed probs -------------------------------------------------------------
  output$stats <- renderPrint({
    req(result())
    res <- result()
    names(res$probs) <- vapply(names(res$probs), pretty_label, character(1))
    print(round(res$probs, 4))
  })
  
  # bar plot ------------------------------------------------------------------
  output$probPlot <- renderPlot({
    req(result())
    res <- result()
    names(res$probs) <- vapply(names(res$probs), pretty_label, character(1))
    
    df <- data.frame(label = names(res$probs),
                     prob  = as.numeric(res$probs))
    
    ggplot(df, aes(label, prob, fill = label)) +
      geom_col(width = 0.6, colour = "black") +
      scale_fill_manual(values = c(AI = "firebrick", Human = "steelblue")) +
      geom_text(aes(label = sprintf("%.1f %%", prob)), vjust = -0.4, size = 5) +
      labs(x = NULL, y = "Probability (%)") +
      ylim(0, 105) +
      theme_bw(base_size = 14) +
      theme(legend.position = "none",
            axis.text.x = element_text(face = "bold"))
  })
}

shinyApp(ui, server)
