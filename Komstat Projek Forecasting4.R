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

# --- UI (User Interface) ---
ui <- navbarPage(
  title = "Aplikasi Forecasting ARIMA",
  theme = bslib::bs_theme(bootswatch = "cerulean", base_font = font_google("Inter")),
  
  tabPanel("Analisis & Visualisasi",
           layout_sidebar(
             sidebar = sidebar(
               title = "Pengaturan Analisis",
               card(
                 fileInput("userfile", "1. Unggah File CSV", 
                           accept = ".csv", buttonLabel = "Cari...", placeholder = "Tidak ada file dipilih"),
                 uiOutput("date_col_selector"),
                 uiOutput("value_col_selector"),
                 hr(),
                 numericInput("ahead", "Jumlah Tahun Ramalan:", 2, min = 1, max = 10),
                 sliderInput("ci_level", "Tingkat Kepercayaan (CI %):", 
                             min = 50, max = 99, value = 80, post = "%"),
                 actionButton("run_analysis", "Jalankan Analisis", icon = icon("play"), class = "btn-primary w-100")
               )
             ),
             card(
               card_header("Hasil Visualisasi"),
               uiOutput("plot_instructions"),
               tabsetPanel(
                 id = "main_tabs",
                 tabPanel("Plot Forecast ARIMA", plotlyOutput("forecastPlot") %>% withSpinner()),
                 tabPanel("Dekomposisi Deret Waktu", plotlyOutput("dcompPlot") %>% withSpinner())
               )
             )
           )
  ),

  tabPanel("Peramalan Menggunakan ARIMA",
         layout_sidebar(
           sidebar = sidebar(
             title = "Set Ordo ARIMA",
             card(
               numericInput("manual_p", "Ordo p (AR):", 1, min = 0, max = 5),
               numericInput("manual_d", "Ordo d (Difference):", 1, min = 0, max = 2),
               numericInput("manual_q", "Ordo q (MA):", 0, min = 0, max = 5),
               numericInput("manual_h", "Jumlah Tahun Ramalan:", 2, min = 1, max = 10),
               sliderInput("manual_ci", "Confidence Interval (%):", 50, 99, 80, post = "%"),
               actionButton("run_manual_arima", "Jalankan Forecast", class = "btn-primary w-100")
             )
           ),
           card(
             card_header("Hasil Forecast ARIMA"),
             plotlyOutput("plot_manual_arima") %>% withSpinner(),
             DTOutput("table_manual_arima") %>% withSpinner()
           )
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
             card(
               card_header("Diagnostik Residual Model ARIMA"),
               # ============================================
               # PERUBAHAN 1: Menggunakan plotOutput, bukan plotlyOutput
               # ============================================
               plotOutput("residualsPlot") %>% withSpinner()
             )
           )
  ),
  
  tabPanel("Tentang",
           card(
             card_header("Tentang Aplikasi"),
             markdown("
#### Aplikasi Forecasting Interaktif dengan ARIMA
Aplikasi ini dirancang sebagai proyek akhir mata kuliah Komputasi Statistik. Tujuannya adalah untuk menyediakan alat interaktif untuk peramalan deret waktu menggunakan model ARIMA.

**Fitur Utama:**
- **Upload Data:** Mengunggah data CSV dengan format kolom yang fleksibel.
- **Pemilihan Kolom:** Memungkinkan pengguna memilih kolom mana yang berisi tanggal dan nilai.
- **Model Otomatis:** Membangun model `ARIMA` secara otomatis menggunakan fungsi dari paket `fpp3`.
- **Visual Interaktif:** Menggunakan `plotly` untuk eksplorasi plot hasil forecast, dekomposisi, dan diagnostik residual.
- **Evaluasi Model:** Menampilkan performa model dengan metrik akurasi forecast (out-of-sample).

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
  
  uploaded_data_header <- eventReactive(input$userfile, {
    req(input$userfile)
    names(read_csv(input$userfile$datapath, n_max = 0, show_col_types = FALSE))
  })
  
  output$date_col_selector <- renderUI({
    req(uploaded_data_header())
    guess <- grep("date|time|yyyymm", uploaded_data_header(), value = TRUE, ignore.case = TRUE)[1]
    selectInput("date_col", "2. Pilih Kolom Tanggal:", choices = uploaded_data_header(), selected = guess)
  })
  
  output$value_col_selector <- renderUI({
    req(uploaded_data_header())
    guess <- grep("price|value|harga|jumlah", uploaded_data_header(), value = TRUE, ignore.case = TRUE)[1]
    selectInput("value_col", "3. Pilih Kolom Nilai:", choices = uploaded_data_header(), selected = guess)
  })
  
  output$plot_instructions <- renderUI({
    if (is.null(input$userfile)) {
      h4("Silakan unggah file CSV dan jalankan analisis untuk melihat visualisasi.", style = "text-align: center; color: grey;")
    }
  })
  
  dataset_ts <- eventReactive(input$run_analysis, {
    req(input$userfile, input$date_col, input$value_col)
    showNotification("Memproses data...", type = "message", duration = 2)
    
    df <- read_csv(input$userfile$datapath, show_col_types = FALSE)
    validate(need(nrow(df) > 0, "File kosong atau tidak valid."))
    
    df %>%
      select(observation_date = all_of(input$date_col), price = all_of(input$value_col)) %>%
      mutate(
        observation_date = tryCatch({
          yearmonth(observation_date)
        }, error = function(e) {
          shiny::validate(paste("Format tanggal tidak valid:", e$message))
        }),
        price = as.double(price)
      ) %>%
      drop_na() %>%
      as_tsibble(index = observation_date)
  })
  
  train_data <- reactive({
    req(dataset_ts())
    data_for_training <- dataset_ts()
    
    if (nrow(data_for_training) < 48) {
      shiny::validate("Data tidak cukup untuk evaluasi. Minimal 4 tahun (48 observasi) diperlukan.")
    }
    
    data_for_training %>% slice(1:(n() - 24))
  })
  
  model_fit <- reactive({
    req(train_data())
    showNotification("Melatih model ARIMA...", type = "message", duration = 2)
    train_data() %>% model(Auto_ARIMA = ARIMA(price))
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
  
  output$forecastPlot <- renderPlotly({
    req(model_forecast(), dataset_ts())
    
    forecast_data <- model_forecast()
    
    alpha <- (100 - input$ci_level) / 100
    lower_prob <- alpha / 2
    upper_prob <- 1 - (alpha / 2)
    
    plot_data <- forecast_data %>%
      mutate(
        .lower = quantile(price, p = lower_prob),
        .upper = quantile(price, p = upper_prob)
      )
    
    p <- ggplot() +
      geom_line(data = dataset_ts(), aes(x = observation_date, y = price, color = "Data Aktual")) +
      geom_line(data = plot_data, aes(x = observation_date, y = .mean, color = "Forecast ARIMA"), linetype = "dashed") +
      geom_ribbon(data = plot_data, aes(x = observation_date, ymin = .lower, ymax = .upper), fill = "skyblue", alpha = 0.4) +
      labs(title = paste(input$ahead, "Tahun Ramalan ARIMA"),
           y = "Harga", x = "Tahun dan Bulan", color = "Legenda") +
      scale_color_manual(values = c("Data Aktual" = "black", "Forecast ARIMA" = "#0072B2")) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "color"))
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
  
  output$accuracyTable <- renderDT({
    req(model_accuracy())
    datatable(model_accuracy() %>% select(.model, RMSE, MAE, MAPE, MASE),
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE,
              caption = "Tabel Akurasi Forecast ARIMA (Out-of-Sample)")
  })
  
  output$modelMAPE <- renderText({
    req(model_accuracy())
    m <- model_accuracy() %>% pull(MAPE)
    paste0("MAPE: ", round(m, 2), "%")
  })
  
  # ============================================
  # PERUBAHAN 2: Menggunakan renderPlot dan hanya mengembalikan objek plot
  # ============================================
  output$residualsPlot <- renderPlot({ 
    req(model_fit())
    
    # gg_tsresiduals() sendiri sudah cukup informatif
    # Kita tidak perlu menambahkan judul lagi karena sudah ada di header card
    model_fit() %>% gg_tsresiduals()
  })
  
  outputOptions(output, "modelMAPE", suspendWhenHidden = FALSE)
  outputOptions(output, "accuracyTable", suspendWhenHidden = FALSE)
}

# --- JALANKAN APLIKASI ---
shinyApp(ui = ui, server = server)
