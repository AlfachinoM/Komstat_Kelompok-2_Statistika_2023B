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
  title = "Aplikasi Forecasting Modern",
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
                 tabPanel("Forecast Gabungan", plotlyOutput("forecastPlot") %>% withSpinner()),
                 tabPanel("Perbandingan Forecast", plotlyOutput("comparisonPlot") %>% withSpinner()),
                 tabPanel("Dekomposisi Deret Waktu", plotlyOutput("dcompPlot") %>% withSpinner())
               )
             )
           )
  ),
  
  tabPanel("Detail & Diagnostik Model",
           layout_sidebar(
             sidebar = sidebar(
               width = "40%",
               title = "Evaluasi Model",
               value_box(
                 title = "Model Terbaik (Out-of-Sample MAPE)",
                 value = textOutput("bestModelName"),
                 showcase = icon("trophy"),
                 theme = "primary",
                 p(textOutput("bestModelMAPE"))
               ),
               card(
                 card_header("Tabel Akurasi Forecast"),
                 DTOutput("accuracyTable") %>% withSpinner()
               )
             ),
             card(
               card_header("Diagnostik Residual Model"),
               plotlyOutput("residualsPlot") %>% withSpinner()
             )
           )
  ),
  
  tabPanel("Tentang",
           card(
             card_header("Tentang Aplikasi"),
             markdown("
#### Aplikasi Forecasting Interaktif
Aplikasi ini dirancang sebagai proyek akhir mata kuliah Komputasi Statistik.

**Fitur Utama:**
- **Upload Data:** Mengunggah data CSV dengan kolom tanggal dan nilai.
- **Model Otomatis:** Membangun model ARIMA dan Holt-Winters secara otomatis.
- **Visual Interaktif:** Menggunakan `plotly` untuk eksplorasi plot.
- **Evaluasi Model:** Membandingkan performa model dengan metrik akurasi forecast (out-of-sample) seperti Root Mean Squared Error(RMSE), Mean Absolute Error(MAE), Mean Absolute Percentage Error(MAPE) dan Mean Squared Error(MSE). 

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
      showNotification("Kolom tidak standar, asumsi kolom 1=observation_date, 2=price", type="warning")
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
    showNotification("Melatih model pada set Latihan...", type="message", duration=2)
    train_data() %>%
      model(
        HW_Additive       = ETS(price ~ error("A") + trend("A") + season("A")),
        HW_Multiplicative = ETS(price ~ error("M") + trend("A") + season("M")),
        Auto_ARIMA        = ARIMA(price)
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
    
    level_req <- input$ci_level
    lower_p <- (100 - level_req) / 200
    upper_p <- (100 + level_req) / 200
    
    data_ribbon <- model_forecast() %>%
      mutate(
        .lower = quantile(price, lower_p),
        .upper = quantile(price, upper_p)
      ) %>%
      filter(!is.na(.lower) & !is.na(.upper))
    
    p <- ggplot() +
      geom_line(data = dataset_ts(), aes(x = observation_date, y = price, color = "Data Aktual")) +
      geom_line(data = model_forecast(), aes(x = observation_date, y = .mean, color = .model)) +
      {
        if(nrow(data_ribbon) > 0)
          geom_ribbon(data = data_ribbon, aes(x = observation_date, ymin = .lower, ymax = .upper, fill = .model), alpha = 0.2, linetype = 0)
      } +
      labs(
        title = paste(input$ahead, "Tahun Ramalan dengan Interval Kepercayaan", level_req, "%"),
        y = "Harga", x = "Tahun dan Bulan"
        # Hapus 'color' dan 'fill' dari sini
      ) +
      scale_color_manual(
        name = "Legenda",
        values = c("Data Aktual" = "black", "Auto_ARIMA" = "#F8766D", "HW_Additive" = "#00BFC4", "HW_Multiplicative" = "#7CAE00")
      ) +
      scale_fill_manual(
        name = "Legenda",
        values = c("Auto_ARIMA" = "#F8766D", "HW_Additive" = "#00BFC4", "HW_Multiplicative" = "#7CAE00")
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$comparisonPlot <- renderPlotly({
    req(model_forecast())
    p <- model_forecast() %>%
      autoplot(level = NULL) +
      labs(title = "Perbandingan Titik Ramalan Antar Model", y = "Harga", x = "Tahun dan Bulan") +
      theme_minimal()
    ggplotly(p)
  })
  
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
      caption = "Tabel Akurasi Forecast (Out-of-Sample)"
    )
  })
  
  output$bestModelName <- renderText({
    req(model_accuracy())
    model_accuracy() %>% arrange(MAPE) %>% slice(1) %>% pull(.model)
  })
  
  output$bestModelMAPE <- renderText({
    req(model_accuracy())
    m <- model_accuracy() %>% arrange(MAPE) %>% slice(1) %>% pull(MAPE)
    paste0("MAPE: ", round(m, 2), "%")
  })
  
  output$residualsPlot <- renderPlotly({
    req(model_fit())
    res <- model_fit() %>% residuals()
    p <- autoplot(res) +
      labs(title = "Residual Diagnostics (pada Training Set)", y = "Residual", x = "Tahun dan Bulan") +
      theme_minimal()
    ggplotly(p)
  })
  
  outputOptions(output, "accuracyTable", suspendWhenHidden = FALSE)
  outputOptions(output, "bestModelName", suspendWhenHidden = FALSE)
  outputOptions(output, "bestModelMAPE", suspendWhenHidden = FALSE)
  
}

# --- JALANKAN APLIKASI ---
shinyApp(ui = ui, server = server)