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
library(urca)
library(forecast) 
library(tseries)
library(lmtest)

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
  
  tabPanel("Uji Stasioneritas",
           br(),
           card(
             card_header("Uji Stasioneritas (ADF Test)"),
             htmlOutput("stationarity_result")
           )
  ),
  
  tabPanel("Hasil Forecast",
           layout_sidebar(
             sidebar = sidebar(
               title = "Pengaturan Ordo ARIMA (Manual)",
               numericInput("p_order", "Ordo p:", value = 1, min = 0, max = 5),
               numericInput("d_order", "Ordo d:", value = 1, min = 0, max = 2),
               numericInput("q_order", "Ordo q:", value = 1, min = 0, max = 5),
               actionButton("run_manual_arima", "Gunakan Ordo Manual", icon = icon("cog"), class = "btn-info w-100")
             ),
             mainPanel(
               card(
                 card_header("Plot Hasil Forecast ARIMA"),
                 plotlyOutput("forecastPlot") %>% withSpinner()
               )
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
                 value = textOutput("modelSpecText_detail"),
                 showcase = icon("chart-line"),
                 theme = "primary",
                 p(textOutput("modelMAPE"))
               ),
               # KARTU BARU UNTUK INTERPRETASI
               card(
                 card_header("Interpretasi Metrik"),
                 uiOutput("eval_interpretation")
               ),
               card(
                 card_header("Tabel Evaluasi Model"),
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
  
  # --- UI Dinamis & Data Processing ---
  uploaded_data_header <- eventReactive(input$userfile, {req(input$userfile); names(read_csv(input$userfile$datapath, n_max = 0, show_col_types = FALSE))})
  output$date_col_selector <- renderUI({req(uploaded_data_header()); guess <- grep("date|time|yyyymm", uploaded_data_header(), value = TRUE, ignore.case = TRUE)[1]; selectInput("date_col", "2. Pilih Kolom Tanggal:", choices = uploaded_data_header(), selected = guess)})
  output$value_col_selector <- renderUI({req(uploaded_data_header()); guess <- grep("price|value|harga|jumlah", uploaded_data_header(), value = TRUE, ignore.case = TRUE)[1]; selectInput("value_col", "3. Pilih Kolom Nilai:", choices = uploaded_data_header(), selected = guess)})
  
  dataset_ts <- eventReactive(input$run_analysis, {
    req(input$userfile, input$date_col, input$value_col)
    showNotification("Memproses data...", type = "message", duration = 2)
    df <- read_csv(input$userfile$datapath, show_col_types = FALSE, progress = FALSE)
    validate(need(nrow(df) > 0, "File kosong atau tidak valid."))
    df %>%
      select(observation_date = all_of(input$date_col), price = all_of(input$value_col)) %>%
      mutate(
        observation_date = tryCatch({yearmonth(observation_date)}, error = function(e) {shiny::validate(paste("Format tanggal tidak valid:", e$message))}),
        price = as.double(price)
      ) %>% drop_na() %>% as_tsibble(index = observation_date)
  })
  
  # --- Reaktif untuk Data Latih & Model ---
  train_data <- reactive({req(dataset_ts()); data_for_training <- dataset_ts(); if (nrow(data_for_training) < 48) {shiny::validate("Data tidak cukup. Min. 48 observasi diperlukan.")}; data_for_training %>% slice(1:(n() - 24))})
  model_fit <- reactive({req(train_data()); showNotification("Melatih model ARIMA...", type = "message", duration = 2); train_data() %>% model(Auto_ARIMA = ARIMA(price))})
  model_manual <- eventReactive(input$run_manual_arima, {
    req(train_data())
    showNotification("Melatih model ARIMA dengan ordo manual...", type = "message", duration = 2)
    train_data() %>% model(Manual_ARIMA = ARIMA(price ~ pdq(input$p_order, input$d_order, input$q_order)))
  })
  
  # --- Reaktif untuk Evaluasi Model (Akurasi + AIC) ---
  model_evaluation <- reactive({
    active_model <- if (isTruthy(input$run_manual_arima) && !is.null(isolate(model_manual()))) {
      isolate(model_manual())
    } else {
      req(model_fit())
      model_fit()
    }
    
    acc <- active_model %>% forecast(h = 24) %>% accuracy(dataset_ts())
    glc <- active_model %>% glance()
    
    full_eval <- acc %>%
      left_join(glc, by = ".model") %>%
      select(.model, AIC, MAPE, RMSE, MAE, MASE, AICc, BIC) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
    
    return(full_eval)
  })
  
  # --- Reaktif untuk Forecast ---
  model_forecast <- reactive({
    horizon <- paste0(input$ahead, " years")
    active_model <- if (isTruthy(input$run_manual_arima) && !is.null(isolate(model_manual()))) {
      isolate(model_manual())
    } else {
      req(model_fit())
      model_fit()
    }
    active_model %>% forecast(h = horizon)
  })
  
  # --- Output: Eksplorasi Data ---
  output$exploration_instructions <- renderUI({if (!isTruthy(input$run_analysis)) h4("Silakan unggah file...", style = "text-align: center; color: grey;")})
  
  output$summaryStats <- renderPrint({ req(dataset_ts()); data <- dataset_ts(); cat("Ringkasan Statistik untuk Kolom '", input$value_col, "'\n\n", sep=""); cat("Jumlah Observasi :", nrow(data), "\n"); cat("Mean             :", round(mean(data$price, na.rm = TRUE), 2), "\n"); cat("Standar Deviasi  :", round(sd(data$price, na.rm = TRUE), 2), "\n"); cat("Minimum          :", round(min(data$price, na.rm = TRUE), 2), "\n"); cat("Maksimum         :", round(max(data$price, na.rm = TRUE), 2), "\n"); cat("Jumlah Nilai NA  :", sum(is.na(data$price)), "\n")})
  
  output$tsPlot <- renderPlotly({ req(dataset_ts()); p <- dataset_ts() %>% autoplot(price) + labs(title = paste("Plot Time Series:", input$value_col)); ggplotly(p, tooltip = "y") })
  output$acfPacfPlot <- renderPlot({ req(dataset_ts()); dataset_ts() %>% gg_tsdisplay(y = price, plot_type = 'partial', lag_max = 36) + labs(title = "ACF & PACF") })
  output$dcompPlot <- renderPlotly({ req(dataset_ts()); p <- dataset_ts() %>% model(STL(price)) %>% components() %>% autoplot() + labs(title = "Dekomposisi STL"); ggplotly(p) })
  output$rollingStatsPlot <- renderPlotly({ req(dataset_ts()); rolling_data <- dataset_ts() %>% mutate(rolling_mean = slide_dbl(price, .f = mean, .size = 12, .align = "center"), rolling_sd = slide_dbl(price, .f = sd, .size = 12, .align = "center")); p <- ggplot(rolling_data, aes(x = observation_date)) + geom_line(aes(y = price, text = paste("Nilai Aktual:", price)), color = "grey") + geom_line(aes(y = rolling_mean, text = paste("Rolling Mean:", round(rolling_mean,2))), color = "blue", linetype = "dashed") + geom_line(aes(y = rolling_sd, text = paste("Rolling SD:", round(rolling_sd,2))), color = "red", linetype = "dotted") + labs(title = "Analisis Kestasioneran Visual", x = "Tanggal", y = "Nilai") + theme_minimal(); ggplotly(p, tooltip = "text")})
  
  # --- Output: Uji Stasioneritas ---
  output$stationarity_result <- renderUI({ req(dataset_ts()); ts_data <- na.omit(as.numeric(dataset_ts()$price)); d_rekomendasi <- ndiffs(ts_data); ts_diff <- if (d_rekomendasi > 0) diff(ts_data, differences = d_rekomendasi) else ts_data; adf_result <- adf.test(ts_diff); status <- if (adf_result$p.value < 0.05) "<span style='color:green;'>STASIONER</span>" else "<span style='color:orange;'>TIDAK STASIONER</span>"; HTML(paste0("<h4>Hasil Uji Stasioneritas (ADF)</h4><ul><li>Statistik Uji: ", round(adf_result$statistic, 4), "</li><li>p-value: ", round(adf_result$p.value, 4), "</li><li>Rekomendasi differencing (d): ", d_rekomendasi, "</li><li>Hasil setelah differencing: ", status, "</li></ul>"))})
  
  # --- Output: Hasil Forecast ---
  output$forecastPlot <- renderPlotly({ req(model_forecast(), dataset_ts()); fc <- model_forecast(); data <- dataset_ts(); judul <- if (isTruthy(input$run_manual_arima) && !is.null(isolate(model_manual()))) {paste(input$ahead, "Tahun Ramalan ARIMA Manual (", input$p_order, ",", input$d_order, ",", input$q_order, ")")} else {req(model_evaluation()); spec <- model_evaluation()$.model; paste(input$ahead, "Tahun Ramalan Model", spec)}; p <- fc %>% autoplot(data, level = input$ci_level) + labs(title = judul, y = "Harga", x = "Tahun dan Bulan") + theme_minimal(); ggplotly(p, tooltip = c("x", "y"))})
  
  # --- Output: Detail & Diagnostik Model ---
  output$accuracyTable <- renderDT({ req(model_evaluation()); datatable(model_evaluation(), options = list(dom = 't', ordering = FALSE), rownames = FALSE, caption = "Tabel Evaluasi Model (Akurasi Out-of-Sample & Info In-Sample)")})
  output$modelSpecText_detail <- renderText({ req(model_evaluation()); model_evaluation()$.model })
  output$modelMAPE <- renderText({ req(model_evaluation()); aic_val <- model_evaluation()$AIC; mape_val <- model_evaluation()$MAPE; paste0("AIC: ", aic_val, " | MAPE: ", mape_val, "%")})
  
  # OUTPUT BARU UNTUK INTERPRETASI
  output$eval_interpretation <- renderUI({
    req(model_evaluation())
    eval_data <- model_evaluation()
    mape_val <- eval_data$MAPE
    
    mape_interp <- case_when(
      mape_val < 10  ~ "<b>Sangat Baik</b>: Rata-rata kesalahan peramalan kurang dari 10% dari nilai aktual.",
      mape_val < 20  ~ "<b>Baik</b>: Rata-rata kesalahan peramalan berada antara 10% - 20%.",
      mape_val < 50  ~ "<b>Cukup</b>: Rata-rata kesalahan peramalan berada antara 20% - 50%.",
      TRUE           ~ "<b>Lemah</b>: Rata-rata kesalahan peramalan di atas 50%, model mungkin tidak cocok."
    )
    
    HTML(paste(
      "<h5>Penjelasan:</h5>",
      "<ul>",
      "<li><b>AIC (Akaike Information Criterion):</b>",
      "<ul>",
      "<li>Nilai ini mengukur seberapa baik model cocok dengan data latih.</li>",
      "<li><b>Tidak ada nilai absolut 'baik' atau 'buruk'.</b> AIC hanya berguna untuk <b>membandingkan beberapa model</b> pada data yang sama. Model dengan <b>nilai AIC yang lebih rendah</b> dianggap lebih baik.</li>",
      "</ul></li>",
      "<br>",
      "<li><b>MAPE (Mean Absolute Percentage Error):</b>",
      "<ul>",
      "<li>Mengukur rata-rata persentase kesalahan absolut dari hasil forecast dibandingkan dengan data aktual (out-of-sample).</li>",
      "<li>Nilai saat ini adalah <b>", mape_val, "%</b>, yang interpretasinya adalah: <b>", mape_interp, "</b></li>",
      "</ul></li>",
      "</ul>"
    ))
  })
  
  output$arimaReport <- renderPrint({ active_model <- if (isTruthy(input$run_manual_arima) && !is.null(isolate(model_manual()))) {isolate(model_manual())} else {req(model_fit()); model_fit()}; report(active_model)})
  output$residualsPlot <- renderPlot({ active_model <- if (isTruthy(input$run_manual_arima) && !is.null(isolate(model_manual()))) {isolate(model_manual())} else {req(model_fit()); model_fit()}; active_model %>% gg_tsresiduals()})
  
  outputOptions(output, "modelMAPE", suspendWhenHidden = FALSE)
  outputOptions(output, "accuracyTable", suspendWhenHidden = FALSE)
  outputOptions(output, "arimaReport", suspendWhenHidden = FALSE)
  outputOptions(output, "modelSpecText_detail", suspendWhenHidden = FALSE)
  # Jangan lupa tambahkan yang baru
  outputOptions(output, "eval_interpretation", suspendWhenHidden = FALSE)
}

# --- JALANKAN APLIKASI ---
shinyApp(ui = ui, server = server)

