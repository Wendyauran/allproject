library(shiny)
library(readr)
library(dplyr)
library(factoextra)
library(inaparc)
library(fclust)
library(plotly)
library(ppclust)
library(matrixStats)


ui <- fluidPage(
  titlePanel("Fuzzy Possibilistic C-Means Clustering"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Pilih File CSV', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      numericInput('clusters', 'Jumlah Cluster:', 3, min = 1, max = 9),
      numericInput('m', 'Parameter m:', 2, min = 1.1, max = 2, step = 0.1),
      actionButton("processButton", "Proses Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("data")),
        tabPanel("Summary Cluster", verbatimTextOutput("clusterSummary")),
        tabPanel("Indeks Validitas", verbatimTextOutput("indeksmpc")),
        tabPanel("Cluster Plot", plotOutput("clusterPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    read_csv(inFile$datapath)
  })
  
  processedData <- eventReactive(input$processButton, {
    Dat = as.matrix(data())

    # Clustering FCM
    fcm_res <- fcm(Dat, centers = input$clusters)
    
    # Clustering FPCM
    set.seed(20)
    fpcm_res <- fpcm(Dat, centers = fcm_res$v, memberships = fcm_res$u, m = input$m, con.val = 1e-05, iter.max = 1000)
    hasil <- summary(fpcm_res)
    
    # Nilai Indeks MPC
    nilai_mpc <- MPC(fpcm_res$u)
    
    # Return Data
    list(Dat = Dat, hasil = hasil, nilai_mpc = nilai_mpc, fpcm_res = fpcm_res, klaster = fpcm_res$cluster)
  })
  
  output$data <- renderTable({
    head(data(), 20)
  })
  
  output$clusterSummary <- renderPrint({
    hasil <- processedData()$hasil
    print(hasil)
  })
  
  output$indeksmpc <- renderPrint({
    nilai_mpc <- processedData()$nilai_mpc
    cat("Nilai Indeks MPC =",nilai_mpc)
  })
  
  output$clusterPlot <- renderPlot({
    Dat <- processedData()$Dat
    fpcm_res <- processedData()$fpcm_res
    fpcm2 <- ppclust2(fpcm_res, "kmeans")
    fviz_cluster(fpcm2, data = as.data.frame(Dat), ellipse.type = "convex", palette = "jco", repel = TRUE)
  })
}

shinyApp(ui = ui, server = server)