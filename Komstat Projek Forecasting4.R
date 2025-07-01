# --- SETUP ---
library(shiny)
library(fpp3)         
library(tsibble)      
library(readr)        
library(tidyverse)    
library(plotly)       
library(DT)           
library(bslib)        
library(shinycssloaders) 

# --- UI (Antarmuka Pengguna) ---
ui <- navbarPage(
  title = "Aplikasi Forecasting ARIMA", # --- DIUBAH: Judul lebih spesifik
  theme = bslib::bs_theme(bootswatch = "cerulean", base_font = font_google("Inter")),
  
  tabPanel("Analisis & Visualisasi",
           layout_sidebar(
             sidebar = sidebar(
               title = "Pengaturan Analisis",
               card(
                 fileInput("userfile", "1. Unggah File CSV", 
                           accept = ".csv", buttonLabel = "Cari...", placeholder = "Tidak ada file dipilih"),
                 helpText("File CSV harus memiliki kolom 'observation_date' (YYYY-MM atau YYYY-MM-DD) dan 'price'."),
                 numericInput("ahead", "2. Jumlah Tahun Ramalan:", 2, min = 1, max = 10),
                 sliderInput("ci_level", "Tingkat Kepercayaan (CI %):", 
                             min = 50, max = 99, value = 80, post = "%"),
                 actionButton("run_analysis", "3. Jalankan Analisis", icon = icon("play"), class = "btn-primary w-100")
               )
             ),
             card(
               card_header("Hasil Visualisasi"),
               tabsetPanel(
                 id = "main_tabs",
                 tabPanel("Plot Forecast ARIMA", plotlyOutput("forecastPlot") %>% withSpinner()),
                 # --- DIHAPUS: Tab perbandingan tidak relevan lagi karena hanya ada satu model ---
                 # tabPanel("Perbandingan Forecast", plotlyOutput("comparisonPlot") %>% withSpinner()),
                 tabPanel("Dekomposisi Deret Waktu", plotlyOutput("dcompPlot") %>% withSpinner())
               )
             )
           )
  ),
  
  tabPanel("Detail & Diagnostik Model",
           layout_sidebar(
             sidebar = sidebar(
               width = "40%",
               title = "Evaluasi Model ARIMA", # --- DIUBAH: Judul disesuaikan
               # --- DIUBAH: Value box tidak lagi menampilkan 'model terbaik', tapi info model yang digunakan
               value_box(
                 title = "Model Digunakan",
                 value = "Auto ARIMA",
                 showcase = icon("chart-line"),
                 theme = "primary",
                 p(textOutput("modelMAPE")) # Menampilkan metrik MAPE
               ),
               card(
                 card_header("Tabel Akurasi Forecast"),
                 DTOutput("accuracyTable") %>% withSpinner()
               )
             ),
             card(
               card_header("Diagnostik Residual Model ARIMA"), # --- DIUBAH: Judul lebih spesifik
               plotlyOutput("residualsPlot") %>% withSpinner()
             )
           )
  ),
  
  tabPanel("Tentang",
           card(
             card_header("Tentang Aplikasi"),
             # --- DIUBAH: Deskripsi disesuaikan dengan fokus ARIMA
             markdown("
#### Aplikasi Forecasting Interaktif dengan ARIMA
Aplikasi ini dirancang sebagai proyek akhir mata kuliah Komputasi Statistik. Tujuannya adalah untuk menyediakan alat interaktif untuk peramalan deret waktu menggunakan model ARIMA.

**Fitur Utama:**
- **Upload Data:** Mengunggah data CSV dengan kolom tanggal dan nilai.
- **Model Otomatis:** Membangun model `ARIMA` secara otomatis menggunakan fungsi dari paket `fpp3`.
- **Visual Interaktif:** Menggunakan `plotly` untuk eksplorasi plot hasil forecast, dekomposisi, dan diagnostik residual.
- **Evaluasi Model:** Menampilkan performa model dengan metrik akurasi forecast (out-of-sample) seperti Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), dan Mean Absolute Percentage Error (MAPE).

**Tim Pengembang:**
- Siti Fadilah Nurkhotimah
- Alfachino Maulana
- Ayda Syifa Ul Aliyah
- Rachmawati Tefaaulia
- Oki Ramadhan Pramono
- Septiani Amalia Wulandari
      ")
           )
  )
)

# --- SERVER (Logika Aplikasi) ---
server <- function(input, output, session) {
  
  dataset_ts <- eventReactive(input$userfile, {
    req(input$userfile)
    df <- read_csv(input$userfile$datapath)
    
    if (!all(c("observation_date", "price") %in% names(df)) && ncol(df) >= 2) {
      showNotification("Kolom tidak standar, diasumsikan kolom 1 adalah 'observation_date', dan kolom 2 adalah 'price'", type="warning")
      names(df)[1:2] <- c("observation_date", "price")
    }
    validate(
      need(all(c("observation_date", "price") %in% names(df)),
           "CSV gagal diproses: butuh kolom 'observation_date' dan 'price'.")
    )
    
    df %>%
      select(observation_date, price) %>%
      mutate(observation_date = yearmonth(observation_date), price = as.numeric(price)) %>%
      drop_na() %>%
      as_tsibble(index = observation_date)
  })
  
  train_data <- eventReactive(input$run_analysis, {
    req(dataset_ts())
    if (nrow(dataset_ts()) < 48) {
      shiny::validate("Data tidak cukup untuk evaluasi. Butuh minimal 4 tahun data (48 observasi).")
    }
    dataset_ts() %>% slice(1:(n()-24))
  })
  
  model_fit <- reactive({
    req(train_data())
    showNotification("Melatih model ARIMA pada data Latihan...", type="message", duration=2)
    # --- DIUBAH: Hanya melatih model ARIMA, model ETS (Holt-Winters) dihapus ---
    train_data() %>%
      model(
        Auto_ARIMA = ARIMA(price)
      )
  })
  
  model_accuracy <- reactive({
    req(model_fit(), dataset_ts())
    fc_test <- model_fit() %>% forecast(h = 24)
    accuracy(fc_test, dataset_ts())
  })
  
  model_forecast <- reactive({
    req(model_fit())
    horizon <- paste0(input$ahead, " years")
    model_fit() %>% forecast(h = horizon)
  })
  
  # --- BAGIAN VISUALISASI ---
  
  output$forecastPlot <- renderPlotly({
    req(model_forecast(), dataset_ts())
    
    # --- DIUBAH: Logika plot disederhanakan untuk satu model ---
    p <- model_forecast() %>%
      autoplot(dataset_ts(), level = input$ci_level, alpha = 0.5) +
      labs(
        title = paste(input$ahead, "Tahun Ramalan ARIMA dengan Interval Kepercayaan", input$ci_level, "%"),
        y = "Harga", x = "Tahun dan Bulan",
        colour = "Legenda"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # --- DIHAPUS: Plot perbandingan tidak diperlukan lagi ---
  # output$comparisonPlot <- renderPlotly({...})
  
  output$dcompPlot <- renderPlotly({
    req(dataset_ts())
    p <- dataset_ts() %>%
      model(STL(price ~ season(window = "periodic"), robust = TRUE)) %>%
      components() %>%
      autoplot() +
      labs(title = "Dekomposisi Deret Waktu") +
      theme_minimal()
    ggplotly(p)
  })
  
  # --- BAGIAN DETAIL & DIAGNOSTIK MODEL ---
  
  output$accuracyTable <- renderDT({
    req(model_accuracy())
    datatable(
      model_accuracy() %>% select(.model, RMSE, MAE, MAPE, MASE),
      options = list(dom = 't', ordering = FALSE),
      rownames = FALSE,
      caption = "Tabel Akurasi Forecast ARIMA (Out-of-Sample)"
    )
  })
  
  # --- DIHAPUS: Logika untuk mencari model terbaik tidak diperlukan ---
  # output$bestModelName <- renderText({...})
  
  # --- DIUBAH: Logika ini sekarang hanya mengambil MAPE dari satu-satunya model yang ada ---
  output$modelMAPE <- renderText({
    req(model_accuracy())
    m <- model_accuracy() %>% pull(MAPE)
    paste0("MAPE: ", round(m, 2), "%")
  })
  
  output$residualsPlot <- renderPlotly({
    req(model_fit())
    # autoplot akan otomatis membuat plot diagnostik yang sesuai karena hanya ada satu model
    p <- model_fit() %>%
      gg_tsresiduals() +
      labs(title = "Diagnostik Residual Model ARIMA (pada Data Latihan)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # --- DIHAPUS: Opsi ini tidak lagi relevan ---
  # outputOptions(output, "bestModelName", suspendWhenHidden = FALSE)
  # Mengganti dengan output yang baru
  outputOptions(output, "modelMAPE", suspendWhenHidden = FALSE)
  outputOptions(output, "accuracyTable", suspendWhenHidden = FALSE)
  
}

# --- JALANKAN APLIKASI ---
shinyApp(ui = ui, server = server)
