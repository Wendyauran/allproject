library(shiny)
library(shinydashboard)
library(matrixStats)
library(readxl)
library(psych)
library(car)
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Fuzzy Subtractive Clustering",titleWidth = 285),
  dashboardSidebar(width = 285,
    sidebarMenu(
      menuItem(h4(strong("Input Data")),tabName = "data"),
      menuItem(h4(strong("Uji Asumsi")),tabName = "asumsi"),
      menuItem(h4(strong("Perhitungan FSC Gauss")),tabName = "hitung"),
      numericInput("q1", "Masukkan Nilai q",value = 0),
      numericInput("ar1", "Masukkan Nilai ar",value = 0),
      numericInput("rr1", "Masukkan Nilai rr",value = 0),
      numericInput("r1", "Masukkan Nilai r",value = 0),
      actionButton("hitung2",strong("Hitung"),width = "70px",class = "btn-warning")
    )
  ),
  dashboardBody(
    tags$style(type="text/css",
      ".shiny-output-error{visibility: hidden;}",
      ".shiny-output-error:before{visibility: hidden;}"
    ),
    tags$style(
      HTML(".box.box-solid.box-success>.box-header{
            background:#3D9970}"
      )
    ),
    tags$style(
      HTML(".box.box-solid.box-primary>.box-header{
            background:#3E6D9C}"
      )
    ),
    tags$style(
      HTML(".box.box-solid.box-danger>.box-header{
            background:#468B97}"
      )
    ),
    tabItems(
      tabItem(tabName = "data",
        fluidPage(
          box(width = 4,background = "olive",fileInput("file1",h4(strong("Unggah File")),multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xlsx"))),
          box(width = 8,title = strong("Data"),status = "success",solidHeader = TRUE,dataTableOutput(outputId = "data1"))
        )
      ),
      tabItem(tabName = "asumsi",
        fluidPage(
          box(title = "Sampel yang mewakili (Sampel Representatif)",status = "primary",solidHeader = TRUE,verbatimTextOutput(outputId = "kmo")),
          box(title = "Non-Multikolinearitas",status = "primary",solidHeader = TRUE,verbatimTextOutput(outputId = "multiko"))
        ),
        fluidPage(
          box(h5(strong("Note : Asumsi terpenuhi jika nilai KMO > 0.5"))),
          box(h5(strong("Note : Asumsi terpenuhi jika nilai VIF < 10")))
        )
      ),
      tabItem(tabName = "hitung",
        fluidPage(
          column(width = 6,
            box(title = "Pusat Klaster",width = NULL,status = "danger",solidHeader = TRUE,verbatimTextOutput(outputId = "pusat")),
            box(title = "Indeks Validitas",width = NULL,status = "danger",solidHeader = TRUE,verbatimTextOutput(outputId = "pc"))
          ),
          box(title = "Tabel Derajat Keanggotaan",status = "danger",solidHeader = TRUE,verbatimTextOutput(outputId = "dk"))
        )
      )
    )
  )
)
server <- function(input,output){
  #Menampilkan Data
  output$data1 <- renderDataTable({
    read_excel(input$file1$datapath)
  })
  
  data2 <- reactive({
    d = read_excel(input$file1$datapath)
    return(d)
  })
  
  #Uji KMO
  output$kmo <- renderPrint({
    x = data2()
    KMO(x)
  })
  
  #Uji VIF
  output$multiko <- renderPrint({
    x = data2()
    cat("===== Nilai VIF =====\n\n")
    for (i in colnames(x)){
      v = vif(lm(paste0(i,"~."),data = x))
      print(v)
    }
  })
  
  observeEvent(input$hitung2,{
    q = as.numeric(input$q1)
    ar = as.numeric(input$ar1)
    rr = as.numeric(input$rr1)
    r = as.numeric(input$r1)
    Dat = as.matrix(data2())
    Xmin = colMins(Dat)
    Xmax = colMaxs(Dat)
    n = nrow(Dat)
    p = ncol(Dat)
    
    #Normalisasi Data
    x = Dat
    for(i in 1:n){
      for(j in 1:p){
        x[i,j] = (Dat[i,j]-Xmin[j])/(Xmax[j]-Xmin[j])
      } 
    }
    
    #Potensi Awal
    Jarak = x
    D = matrix(,n)
    for(k in 1:n){
      T = x[k,]
      for(i in 1:n){
        for(j in 1:p){
          Jarak[i,j] = (T[j]-x[i,j])/r
        } 
      }
      Jarak_kuadrat = Jarak^2
      if(p > 1){
        DS = c()
        for(i in 1:n){
          DS[i] = exp((-4)*sum(Jarak_kuadrat[i,]))
        }
        D[k] = sum(DS)
      }else{
        DS = c()
        for(i in 1:n){
          DS[i] = exp((-4)*Jarak_kuadrat[i,])
        }
        D[k] = sum(DS)
      }
    }
    colnames(D) = c("D[i]")
    
    M = max(D)
    h = which(D == M)
    Center = matrix(,,p)
    C = 0
    Kondisi = 1
    Z = M
    D_lama = D
    D_baru = matrix(,n)
    Iter = 0
    
    #Iterasi
    while(Kondisi != 0 & Z != 0){
      V = x[h,]
      Rasio = Z/M
      Iter = Iter+1
      
      #Menentukan Kondisi
      if(Rasio <= rr){
        Kondisi = 0
      }else if(Rasio > ar){
        Kondisi = 1
      }else{
        Md = -1
        G = matrix(,C,p)
        for(c in 1:C){
          for(j in 1:p){
            G[c,j] = (V[j]-Center[c,j])/r
          }
        }
        G_kuadrat = G^2
        Sd = matrix(,C)
        for(c in 1:C){
          Sd[c] = sum(G_kuadrat[c,])
        }
        for(c in 1:C){
          if(Md < 0 | Sd[c] < Md){
            Md = Sd[c]
          }else{
            Md = Md
          }
        }
        Smd = sqrt(Md)
        Rsmd = Rasio+Smd
        if(Rsmd >= 1){
          Kondisi = 1
        }else{
          Kondisi = 2
        }
      }
      
      #Kondisi
      if(Kondisi == 1){
        C = C+1
        Center = rbind(Center,c(0))
        Center[C,] = V
        S = matrix(,n,p)
        for(i in 1:n){
          for(j in 1:p){
            S[i,j] = (V[j]-x[i,j])/(r*q)
          } 
        }
        ST = S^2
        Dc = matrix(,n)
        for(i in 1:n){
          Dc[i] = M*exp((-4)*sum(ST[i,]))
        }
        D_baru = D_lama-Dc
        for(i in 1:n){
          if(D_baru[i] <= 0){
            D_baru[i] = 0
          }else{
            D_baru[i] = D_baru[i]
          }
        }
        D_lama = D_baru
        Z = max(D_baru)
        h = which(D_baru == Z)
      }else if(Kondisi == 2){
        D_baru[h,] = 0
        Z = max(D_baru)
        h = which(D_baru == Z)
      }else{
        break
      }
    }
    
    #Denormalisasi Data
    Denorm = matrix(,C,p)
    for(c in 1:C){
      for(j in 1:p){
        Denorm[c,j] = (Center[c,j]*(Xmax[j]-Xmin[j]))+Xmin[j]
      }
    }
    
    #Menghitung Nilai Sigma Klaster
    SK = matrix(,,p)
    for(j in 1:p){
      SK[j] = (r*(Xmax[j]-Xmin[j]))/sqrt(8)
    }
    
    #Menghitung Derajat Keanggotaan
    DK = matrix(,n,C)
    u = matrix(,,p)
    for(c in 1:C){
      for(i in 1:n){
        for(j in 1:p){
          u[j] = ((Dat[i,j]-Denorm[c,j])^2)/(2*(SK[j]^2))
        }
        DK[i,c] = exp(-(sum(u)))
      }
    }
    DK = round(DK,5)
    
    #Tabel Derajat Keanggotaan
    TDK = matrix(,n,C+1)
    for(i in 1:n){
      for(c in 1:C){
        TDK[i,c] = DK[i,c]
      }
      TDK[i,C+1] = which.max(DK[i,])
    }
    nk = c(1:C)
    colnames(TDK) = c(nk,"Hasil Klaster")
    
    #Pusat Kluster
    output$pusat <- renderPrint({
      cat("\nIterasi = ",Iter,"\n")
      cat("===== Pusat Klaster =====\n")
      
      #Denormalisasi Data
      Denorm = matrix(,C,p)
      for(c in 1:C){
        for(j in 1:p){
          Denorm[c,j] = (Center[c,j]*(Xmax[j]-Xmin[j]))+Xmin[j]
        }
      }
      colnames(Denorm) = colnames(x)
      return(Denorm)
    })
    
    #Tabel Derajat Keanggotaan
    output$dk <- renderPrint({
      cat("\n ===== Derajat Keanggotaan Pada Setiap Klaster =====\n")
      return(TDK)
    })
    
    #Indeks Validitas
    output$pc <- renderPrint({
      PCI = (1/n)*sum(DK^2)
      cat("Nilai Indeks PC =",PCI)
    })
  })
  
}

shinyApp(ui,server)