library(shiny)
library(shinythemes)
ui<-fluidPage(theme = shinytheme("darkly"),
  titlePanel(title="Uji Beda Mean (Uji t)"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Satu Sampel", br(),
                           textInput("n1", "Input Data",value = ""),
                           p("contoh: 15,43,66"),
                           textInput("mu", "mu",value = ""),
                           textInput("alpha1", "alpha",value = ""),
                           actionButton("hitung1","Hitung")),
                  
                  tabPanel("Dua Sampel", br(),
                           textInput("n2", "Input Data 1"),
                           p("contoh: 1,2,3"),
                           textInput("n3", "Input Data 2"),
                           p("contoh: 10,20,30"),
                           textInput("alpha2", "alpha"),
                           radioButtons(inputId="uji", "Pilih Uji",list("Independen","Berpasangan")),
                           actionButton("hitung2","Hitung"))
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Boxplot", plotOutput("box1"), plotOutput("box2")),
                  tabPanel("Normalitas secara visual", plotOutput("qq1"), plotOutput("qq2")),
                  tabPanel("Normalitas secara formal", verbatimTextOutput("norm1"), verbatimTextOutput("norm2"), uiOutput("ket")),
                  tabPanel("Output Uji t", verbatimTextOutput("ujit")),
                  tabPanel("Uji Hipotesis", uiOutput("uji"))
      )
    )
  )
)
server<-function(input,output){
  observeEvent(input$hitung1,{
    ekstrak <- function(text){
      text <- gsub(" ","",text)
      split <- strsplit(text,",",fixed = FALSE)[[1]]
      as.numeric(split)
    }
    
    x <- ekstrak(input$n1)
    miu <- as.numeric(input$mu)
    a <- as.numeric(input$alpha1)
    
    #Boxplot Data
    output$box1 <- renderPlot({
      boxplot(x,col = "blue", main = "Boxplot Data")
    })
    
    output$box2 <- renderPlot({ })
    
    #Uji Normalitas secara visual dengan Q-Q Plot
    output$qq1 <- renderPlot({
      qqnorm(x,main = "Normal Q-Q Plot")
      qqline(x)
    })
    
    output$qq2 <- renderPlot({ })
    
    #Uji Normalitas secara formal
    output$norm1 <- renderPrint({
      shapiro.test(x)
    })
    
    output$ket <- renderUI({
      withMathJax(
        br(),
        paste0("Note: Asumsi normalitas terpenuhi jika nilai p-value > alpha.")
      )
    })
    
    #Uji t
    output$ujit <- renderPrint({
      t.test(x,alternative = "two.sided",mu = miu,conf.level = 1-a)
    })
    
    output$uji <- renderUI({
      n <- length(x)
      xbar <- mean(x)
      S <- sqrt(var(x))
      thitung <- (xbar-miu)/(S/sqrt(n))
      ttabel <- qt(1-(a/2),df=n-1)
      pvalue <- 2*(1-pt(abs(thitung),df=n-1))
      
      #Uji Hipotesis
      withMathJax(
        br(),
        paste0(" Hipotesis"),
        br(),
        paste0(" H0 : mu sama dengan ",miu),
        br(),
        paste0(" H1 : mu tidak sama dengan ",miu),
        br(),
        br(),
        paste0(" Taraf signifikansi : ",a*100,"%"),
        br(),
        br(),
        paste0(" Statistik Uji"),
        br(),
        paste0(" Nilai t hitung = ", round(thitung,6)),
        br(),
        paste0(" Nilai p-value \t= ",round(pvalue,6)),
        br(),
        paste0(" Nilai t tabel \t= ",round(ttabel,6)),
        br(),
        br(),
        paste0(" Daerah Kritis"),
        br(),
        paste0(" Tolak H0 jika abs(t hitung) > t tabel atau nilai p-value < alpha"),
        br(),
        br(),
        paste0(" Kesimpulan"),
        br(),
        if (abs(thitung) > ttabel){
          paste0(" Ho ditolak sehingga rata-rata dari data tersebut tidak sama dengan ",miu)
        }
        else {
          paste0(" Ho gagal ditolak sehingga rata-rata dari data tersebut sama dengan ",miu)
        }
      )
    })
  })
  
  observeEvent(input$hitung2,{
    ekstrak <- function(text){
      text <- gsub(" ","",text)
      split <- strsplit(text,",",fixed = FALSE)[[1]]
      as.numeric(split)
    }
    
    x1 <- ekstrak(input$n2)
    x2 <- ekstrak(input$n3)
    a <- as.numeric(input$alpha2)
    n1 <- length(x1)
    n2 <- length(x2)
    xbar1 <- mean(x1)
    xbar2 <- mean(x2)
    s1 <- sqrt(var(x1))
    s2 <- sqrt(var(x2))
    
    #Boxplot Data 1
    output$box1 <- renderPlot({
      boxplot(x1,col = "blue", main = "Boxplot Data 1")
    })
    
    #Boxplot Data 2
    output$box2 <- renderPlot({
      boxplot(x2,col = "yellow", main = "Boxplot Data 2")
    })
    
    #Uji Normalitas secara visual dengan Q-Q Plot
    output$qq1 <- renderPlot({
      qqnorm(x1,main = "Normal Q-Q Plot Data 1")
      qqline(x1)
    })
    
    output$qq2 <- renderPlot({
      qqnorm(x2,main = "Normal Q-Q Plot Data 2")
      qqline(x2)
    })
    
    #Uji Normalitas secara formal
    output$norm1 <- renderPrint({
      shapiro.test(x1)
    })
    
    output$norm2 <- renderPrint({
      shapiro.test(x2)
    })
    
    output$ket <- renderUI({
      withMathJax(
        br(),
        paste0("Note: Asumsi normalitas terpenuhi jika nilai p-value > alpha.")
      )
    })
    
    #Uji t
    if (input$uji == "Independen"){
      if (s1 > s2){
        fhitung <- (s1^2)/(s2^2)
        ftabel <- qf(1-a,n1-1,n2-1)
      }
      else{
        fhitung <- (s2^2)/(s1^2)
        ftabel <- qf(1-a,n2-1,n1-1)
      }
      
      if (fhitung < ftabel){
        output$ujit <- renderPrint({
          t.test(x1,x2,alternative = "two.sided",paired = FALSE,var.equal = TRUE,conf.level = 1-a)
        })
        
        output$uji <- renderUI({
          Sp <- sqrt(((n1-1)*(s1^2)+(n2-1)*(s2^2))/(n1+n2-2))
          thitung <- (xbar1-xbar2)/(Sp*sqrt((1/n1)+(1/n2)))
          ttabel <- qt(1-(a/2),df=n1+n2-2)
          pvalue <- 2*(1-pt(abs(thitung),df=n1+n2-2))
          
          #Uji Hipotesis
          withMathJax(
            br(),
            paste0(" Hipotesis"),
            br(),
            paste0(" H0 : rata-rata kedua kelompok sama"),
            br(),
            paste0(" H1 : rata-rata kedua kelompok tidak sama"),
            br(),
            br(),
            paste0(" Taraf signifikansi : ",a*100,"%"),
            br(),
            br(),
            paste0(" Statistik Uji"),
            br(),
            paste0(" Nilai t hitung = ", round(thitung,6)),
            br(),
            paste0(" Nilai p-value \t= ",round(pvalue,6)),
            br(),
            paste0(" Nilai t tabel \t= ",round(ttabel,6)),
            br(),
            br(),
            paste0(" Daerah Kritis"),
            br(),
            paste0(" Tolak H0 jika abs(t hitung) > t tabel atau nilai p-value < alpha"),
            br(),
            br(),
            paste0(" Kesimpulan"),
            br(),
            if (abs(thitung) > ttabel){
              paste0(" Ho ditolak sehingga rata-rata kedua kelompok tidak sama")
            }
            else {
              paste0(" Ho gagal ditolak sehingga rata-rata kedua kelompok sama")
            }
          )
        })
      }
      else {
        output$ujit <- renderPrint({
          t.test(x1,x2,alternative = "two.sided",paired = FALSE,var.equal = FALSE,conf.level = 1-a)
        })
        
        output$uji <- renderUI({
          thitung <- (xbar1-xbar2)/sqrt(((s1^2)/n1)+((s2^2)/n2))
          k <- ((((s1^2)/n1)+((s2^2)/n2))^2)/(((((s1^2)/n1)^2)/(n1-1))+((((s2^2)/n2)^2)/(n2-1)))
          ttabel <- qt(1-(a/2),df=k)
          pvalue <- 2*(1-pt(abs(thitung),df=k))
          
          #Uji Hipotesis
          withMathJax(
            br(),
            paste0(" Hipotesis"),
            br(),
            paste0(" H0 : rata-rata kedua kelompok sama"),
            br(),
            paste0(" H1 : rata-rata kedua kelompok tidak sama"),
            br(),
            br(),
            paste0(" Taraf signifikansi : ",a*100,"%"),
            br(),
            br(),
            paste0(" Statistik Uji"),
            br(),
            paste0(" Nilai t hitung = ", round(thitung,6)),
            br(),
            paste0(" Nilai p-value \t= ",round(pvalue,6)),
            br(),
            paste0(" Nilai t tabel \t= ",round(ttabel,6)),
            br(),
            br(),
            paste0(" Daerah Kritis"),
            br(),
            paste0(" Tolak H0 jika abs(t hitung) > t tabel atau nilai p-value < alpha"),
            br(),
            br(),
            paste0(" Kesimpulan"),
            br(),
            if (abs(thitung) > ttabel){
              paste0(" Ho ditolak sehingga rata-rata kedua kelompok tidak sama")
            }
            else {
              paste0(" Ho gagal ditolak sehingga rata-rata kedua kelompok sama")
            }
          )
        })
      }
    }
    else {
      output$ujit <- renderPrint({
        t.test(x1,x2,alternative = "two.sided",paired = TRUE,var.equal = TRUE,conf.level = 1-a)
      })
      
      output$uji <- renderPrint({
        d=x1-x2
        nd=length(d)
        thitung=mean(d)/(sqrt(var(d)/nd))
        ttabel=qt(1-(a/2),df=nd-1)
        pvalue = 2*(1-pt(abs(thitung),df=nd-1))
        
        #Uji Hipotesis
        withMathJax(
          br(),
          paste0(" Hipotesis"),
          br(),
          paste0(" H0 : miu1-miu2 sama dengan 0"),
          br(),
          paste0(" H1 : miu1-miu2 tidak sama dengan 0"),
          br(),
          br(),
          paste0(" Taraf signifikansi : ",a*100,"%"),
          br(),
          br(),
          paste0(" Statistik Uji"),
          br(),
          paste0(" Nilai t hitung = ", round(thitung,6)),
          br(),
          paste0(" Nilai p-value \t= ",round(pvalue,6)),
          br(),
          paste0(" Nilai t tabel \t= ",round(ttabel,6)),
          br(),
          br(),
          paste0(" Daerah Kritis"),
          br(),
          paste0(" Tolak H0 jika abs(t hitung) > t tabel atau nilai p-value < alpha"),
          br(),
          br(),
          paste0(" Kesimpulan"),
          br(),
          if (abs(thitung) > ttabel){
            paste0(" Ho ditolak sehingga ada perbedaan rata-rata antara kedua kelompok")
          }
          else {
            paste0(" Ho gagal ditolak sehingga tidak ada perbedaan rata-rata antara kedua kelompok")
          }
        )
      })
    }
  })
}

shinyApp(ui=ui,server=server)