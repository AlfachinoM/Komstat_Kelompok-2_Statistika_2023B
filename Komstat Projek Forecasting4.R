# --- SETUP ---
library(shiny)
library(fpp3)         
library(readr)
library(tidyverse)
library(plotly)
library(DT)
library(bslib)
library(shinycssloaders)
library(distributional) 
library(patchwork) 
library(tsibble) 
library(slider) 

# --- UI (User Interface) ---
ui <- navbarPage(
  title = "Aplikasi Forecasting ARIMA",
  theme = bslib::bs_theme(bootswatch = "cerulean", base_font = font_google("Inter")),
  
  tabPanel("Eksplorasi Data",
           layout_sidebar(
             sidebar = sidebar(
               title = "Pengaturan Analisis",
               card(
                 fileInput("userfile", "1. Unggah File CSV", 
                           accept = ".csv", buttonLabel = "Cari...", placeholder = "Tidak ada file dipilih"),
                 uiOutput("date_col_selector"),
                 uiOutput("value_col_selector"),
                 hr(),
                 h5("Pengaturan untuk Tab Forecast:"),
                 numericInput("ahead", "Jumlah Tahun Ramalan:", 2, min = 1, max = 10),
                 sliderInput("ci_level", "Tingkat Kepercayaan (CI %):", 
                             min = 50, max = 99, value = 80, post = "%"),
                 actionButton("run_analysis", "Jalankan Analisis", icon = icon("play"), class = "btn-primary w-100")
               )
             ),
             mainPanel(
               uiOutput("exploration_instructions"),
               tabsetPanel(
                 type = "tabs",
                 id = "eda_tabs",
                 tabPanel("Ringkasan & Plot Data", 
                          br(), 
                          card(
                            card_header("Plot Time Series Awal"),
                            plotlyOutput("tsPlot") %>% withSpinner()
                          ),
                          card(
                            card_header("Ringkasan Statistik"),
                            verbatimTextOutput("summaryStats") %>% withSpinner()
                          )
                 ),
                 tabPanel("Pola Data (ACF/PACF)",
                          br(),
                          card(
                            card_header("Plot ACF & PACF"),
                            plotOutput("acfPacfPlot", height = "600px") %>% withSpinner()
                          )
                 ),
                 tabPanel("Dekomposisi & Kestasioneran",
                          br(),
                          card(
                            card_header("Dekomposisi Deret Waktu"),
                            plotlyOutput("dcompPlot", height = "600px") %>% withSpinner()
                          ),
                          card(
                            card_header("Analisis Kestasioneran (Rolling Mean & Std Dev)"),
                            plotlyOutput("rollingStatsPlot") %>% withSpinner()
                          )
                 )
               )
             )
           )
  ),
  
  tabPanel("Hasil Forecast",
           card(
             card_header("Plot Hasil Forecast ARIMA"),
             plotlyOutput("forecastPlot") %>% withSpinner()
           )
  ),
  
  tabPanel("Detail & Diagnostik Model",
           layout_sidebar(
             sidebar = sidebar(
               width = "40%",
               title = "Evaluasi Model ARIMA",
               value_box(
                 title = "Model Digunakan",
                 value = "Auto ARIMA",
                 showcase = icon("chart-line"),
                 theme = "primary",
                 p(textOutput("modelMAPE"))
               ),
               card(
                 card_header("Tabel Akurasi Forecast"),
                 DTOutput("accuracyTable") %>% withSpinner()
               )
             ),
             mainPanel(
               width = 7,
               card(
                 card_header("Laporan Model ARIMA"),
                 verbatimTextOutput("arimaReport") %>% withSpinner()
               ),
               card(
                 card_header("Diagnostik Residual Model ARIMA"),
                 plotOutput("residualsPlot") %>% withSpinner()
               )
             )
           )
  ),
  
  tabPanel("Tentang",
           card(
             card_header("Tentang Aplikasi"),
             markdown("
#### Aplikasi Forecasting Interaktif dengan ARIMA
Aplikasi ini dirancang sebagai proyek akhir mata kuliah Komputasi Statistik. Tujuannya adalah untuk menyediakan alat interaktif untuk analisis dan peramalan deret waktu menggunakan model ARIMA.

**Fitur Utama:**
- **Eksplorasi Data Awal:** Menyediakan ringkasan statistik, plot time series, ACF/PACF, dekomposisi, dan analisis kestasioneran visual.
- **Upload Data Fleksibel:** Mengunggah data CSV dan memungkinkan pengguna memilih kolom tanggal dan nilai secara manual.
- **Pemodelan ARIMA Otomatis:** Membangun model `ARIMA` secara otomatis dan menampilkan laporan detail modelnya.
- **Visualisasi Interaktif:** Menggunakan `plotly` untuk eksplorasi plot hasil forecast dan dekomposisi data.
- **Evaluasi Model:** Menampilkan performa model dengan metrik akurasi forecast (out-of-sample) dan diagnostik residual.

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

# --- SERVER ---
server <- function(input, output, session) {
  
  uploaded_data_header <- eventReactive(input$userfile, {req(input$userfile); names(read_csv(input$userfile$datapath, n_max = 0, show_col_types = FALSE))})
  output$date_col_selector <- renderUI({req(uploaded_data_header()); guess <- grep("date|time|yyyymm", uploaded_data_header(), value = TRUE, ignore.case = TRUE)[1]; selectInput("date_col", "2. Pilih Kolom Tanggal:", choices = uploaded_data_header(), selected = guess)})
  output$value_col_selector <- renderUI({req(uploaded_data_header()); guess <- grep("price|value|harga|jumlah", uploaded_data_header(), value = TRUE, ignore.case = TRUE)[1]; selectInput("value_col", "3. Pilih Kolom Nilai:", choices = uploaded_data_header(), selected = guess)})
  
  dataset_ts <- eventReactive(input$run_analysis, {
    req(input$userfile, input$date_col, input$value_col)
    showNotification("Memproses data...", type = "message", duration = 2)
    
    # --- PERBAIKAN DI SINI: Menambahkan progress = FALSE ---
    df <- read_csv(input$userfile$datapath, show_col_types = FALSE, progress = FALSE)
    
    validate(need(nrow(df) > 0, "File kosong atau tidak valid."))
    df %>%
      select(observation_date = all_of(input$date_col), price = all_of(input$value_col)) %>%
      mutate(
        observation_date = tryCatch({yearmonth(observation_date)}, error = function(e) {shiny::validate(paste("Format tanggal tidak valid:", e$message))}),
        price = as.double(price)
      ) %>% drop_na() %>% as_tsibble(index = observation_date)
  })
  
  train_data <- reactive({req(dataset_ts()); data_for_training <- dataset_ts(); if (nrow(data_for_training) < 48) {shiny::validate("Data tidak cukup. Min. 48 observasi diperlukan.")}; data_for_training %>% slice(1:(n() - 24))})
  model_fit <- reactive({req(train_data()); showNotification("Melatih model ARIMA...", type = "message", duration = 2); train_data() %>% model(Auto_ARIMA = ARIMA(price))})
  model_accuracy <- reactive({req(model_fit(), dataset_ts()); fc_test <- model_fit() %>% forecast(h = 24); accuracy(fc_test, dataset_ts())})
  model_forecast <- reactive({req(model_fit()); horizon <- paste0(input$ahead, " years"); model_fit() %>% forecast(h = horizon)})
  
  output$exploration_instructions <- renderUI({
    if (!isTruthy(input$run_analysis) || input$run_analysis == 0) {
      h4("Silakan unggah file, pilih kolom, dan klik 'Jalankan Analisis' untuk melihat eksplorasi data.", style = "text-align: center; color: grey;")
    }
  })
  
  output$summaryStats <- renderPrint({
    req(dataset_ts())
    data <- dataset_ts()
    cat("Ringkasan Statistik untuk Kolom '", input$value_col, "'\n\n", sep="")
    cat("Jumlah Observasi :", nrow(data), "\n")
    cat("Mean             :", round(mean(data$price, na.rm = TRUE), 2), "\n")
    cat("Standar Deviasi  :", round(sd(data$price, na.rm = TRUE), 2), "\n")
    cat("Minimum          :", round(min(data$price, na.rm = TRUE), 2), "\n")
    cat("Maksimum         :", round(max(data$price, na.rm = TRUE), 2), "\n")
    cat("Jumlah Nilai NA  :", sum(is.na(data$price)), "\n")
  })
  
  output$tsPlot <- renderPlotly({
    req(dataset_ts())
    p <- dataset_ts() %>%
      ggplot(aes(x = observation_date, y = price)) +
      geom_line(color = "#0072B2") +
      labs(title = paste("Time Series Plot untuk", input$value_col),
           x = "Tanggal", y = "Nilai") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$acfPacfPlot <- renderPlot({
    req(dataset_ts())
    dataset_ts() %>%
      gg_tsdisplay(y = price, plot_type = 'partial', lag_max = 36) +
      labs(title = paste("ACF & PACF untuk", input$value_col))
  })
  
  output$dcompPlot <- renderPlotly({
    req(dataset_ts())
    p <- dataset_ts() %>%
      model(STL(price ~ season(window = "periodic"), robust = TRUE)) %>%
      components() %>%
      autoplot() +
      labs(title = "Dekomposisi STL")
    ggplotly(p)
  })
  
  output$rollingStatsPlot <- renderPlotly({
    req(dataset_ts())
    rolling_data <- dataset_ts() %>%
      mutate(
        rolling_mean = slide_dbl(price, .f = ~mean(.x, na.rm = TRUE), .size = 12, .align = "center"),
        rolling_sd = slide_dbl(price, .f = ~sd(.x, na.rm = TRUE), .size = 12, .align = "center")
      )
    p <- ggplot(rolling_data, aes(x = observation_date)) +
      geom_line(aes(y = price, color = "Data Asli", linetype = "Data Asli")) +
      geom_line(aes(y = rolling_mean, color = "Rolling Mean", linetype = "Rolling Mean")) +
      geom_line(aes(y = rolling_sd, color = "Rolling Std Dev", linetype = "Rolling Std Dev")) +
      labs(title = "Analisis Kestasioneran Visual (Window 12 Bulan)",
           x = "Tanggal", y = "Nilai", color = "Legenda", linetype = "Legenda") +
      scale_color_manual(
        values = c("Data Asli" = "grey", "Rolling Mean" = "blue", "Rolling Std Dev" = "red")
      ) +
      scale_linetype_manual(
        values = c("Data Asli" = "solid", "Rolling Mean" = "dashed", "Rolling Std Dev" = "dotted")
      ) + 
      theme_minimal()
    ggplotly(p)
  })
  
  output$forecastPlot <- renderPlotly({
    req(model_forecast(), dataset_ts())
    forecast_data <- model_forecast()
    alpha <- (100 - input$ci_level) / 100
    lower_prob <- alpha / 2; upper_prob <- 1 - (alpha / 2)
    plot_data <- forecast_data %>% mutate(.lower = quantile(price, p = lower_prob), .upper = quantile(price, p = upper_prob))
    p <- ggplot() + geom_line(data = dataset_ts(), aes(x = observation_date, y = price, color = "Data Aktual")) + geom_line(data = plot_data, aes(x = observation_date, y = .mean, color = "Forecast ARIMA"), linetype = "dashed") + geom_ribbon(data = plot_data, aes(x = observation_date, ymin = .lower, ymax = .upper), fill = "skyblue", alpha = 0.4) + labs(title = paste(input$ahead, "Tahun Ramalan ARIMA"), y = "Harga", x = "Tahun dan Bulan", color = "Legenda") + scale_color_manual(values = c("Data Aktual" = "black", "Forecast ARIMA" = "#0072B2")) + theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  output$accuracyTable <- renderDT({req(model_accuracy()); datatable(model_accuracy() %>% select(.model, RMSE, MAE, MAPE, MASE), options = list(dom = 't', ordering = FALSE), rownames = FALSE, caption = "Tabel Akurasi Forecast ARIMA (Out-of-Sample)")})
  output$modelMAPE <- renderText({req(model_accuracy()); m <- model_accuracy() %>% pull(MAPE); paste0("MAPE: ", round(m, 2), "%")})
  output$arimaReport <- renderPrint({req(model_fit()); report(model_fit())})
  output$residualsPlot <- renderPlot({ req(model_fit()); model_fit() %>% gg_tsresiduals() })
  
  outputOptions(output, "modelMAPE", suspendWhenHidden = FALSE)
  outputOptions(output, "accuracyTable", suspendWhenHidden = FALSE)
  outputOptions(output, "arimaReport", suspendWhenHidden = FALSE)
}

# --- JALANKAN APLIKASI ---
shinyApp(ui = ui, server = server)
