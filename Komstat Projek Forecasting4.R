# --- SETUP ---
# Pastikan semua paket ini sudah terinstal
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
  
  # --- TAB 1: EKSPLORASI DATA (DIUBAH) ---
  tabPanel("Eksplorasi Data",
           # Mengganti layout_sidebar dengan layout_columns untuk full-width
           layout_columns(
             col_widths = c(3, 9), # Sidebar 25%, Main Panel 75%
             
             # --- KONTEN UNTUK KOLOM 1 (Sidebar) ---
             div(
               h4("Pengaturan Analisis"),
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
             
             # --- KONTEN UNTUK KOLOM 2 (Main Panel) ---
             div(
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
                            plotlyOutput("rollingStatsPlot", height = "600px") %>% withSpinner()
                          )
                 )
               )
             )
           )
  ),
  
  # --- TAB 2: UJI STASIONERITAS ---
  tabPanel("Uji Stasioneritas",
           br(),
           
           card(
             card_header("Apa Itu Uji ADF?"),
             HTML("
             <p>Augmented Dickey-Fuller (ADF) Test adalah uji statistik untuk mengetahui apakah sebuah deret waktu <b>stasioner</b> atau tidak.</p>
             <ul>
               <li><b>Hipotesis nol (H₀):</b> Data tidak stasioner</li>
               <li><b>Hipotesis alternatif (H₁):</b> Data stasioner</li>
               <li><b>Kriteria:</b> Jika p-value < 0.05, maka data dianggap stasioner</li>
             </ul>
           ")
           ),
           
           card(
             card_header("Uji Stasioneritas (ADF Test)"),
             htmlOutput("stationarity_result"),
             uiOutput("diff_plot_ui")
           )
  ),
  
  # --- TAB 3: HASIL FORECAST (DIUBAH) ---
  tabPanel("Hasil Forecast",
           layout_columns(
             col_widths = c(3, 9),
             
             # --- KOLOM 1: Sidebar (25%) ---
             div(
               h4("Pengaturan Ordo ARIMA (Manual)"),
               numericInput("p_order", "Ordo p:", value = 1, min = 0, max = 5),
               numericInput("d_order", "Ordo d:", value = 1, min = 0, max = 2),
               numericInput("q_order", "Ordo q:", value = 1, min = 0, max = 5),
               numericInput("P_order", "Ordo P:", value = 0, min = 0, max = 5),
               numericInput("D_order", "Ordo D:", value = 0, min = 0, max = 2),
               numericInput("Q_order", "Ordo Q:", value = 0, min = 0, max = 5),
               actionButton("run_manual_arima", "Gunakan Ordo Manual", icon = icon("cog"), class = "btn-info w-100")
             ),
             
             # --- KOLOM 2: Main panel (75%) ---
             div(
               card(
                 card_header("Plot Hasil Forecast ARIMA Manual"),
                 plotlyOutput("forecastPlotManual") %>% withSpinner()
               ),
               card(
                 card_header("Plot Hasil Forecast ARIMA Otomatis"),
                 plotlyOutput("forecastPlotAuto") %>% withSpinner()
               )
             )
           )
  ),
  
  # --- TAB 4: DETAIL & DIAGNOSTIK (DIUBAH) ---
  tabPanel("Detail & Diagnostik Model",
           layout_columns(
             # Menggunakan rasio 5:7 agar mendekati 40% untuk sidebar
             col_widths = c(5, 7),
             
             # --- KONTEN KOLOM 1 (Sidebar) ---
             div(
               h4("Evaluasi Model ARIMA"),
               value_box(
                 title = "Model Digunakan",
                 value = textOutput("modelSpecText_detail"),
                 showcase = icon("chart-line"),
                 theme = "primary",
                 p(textOutput("modelMAPE"))
               ),
               card(
                 card_header("Interpretasi Metrik"),
                 uiOutput("eval_interpretation")
               ),
               card(
                 card_header("Tabel Evaluasi Model"),
                 DTOutput("accuracyTable") %>% withSpinner()
               ),
               card(
                 card_header("Diagnostik Lanjutan"),
                 plotOutput("residualsDiagnostics", height="600px") %>% withSpinner(),
                 tags$hr(),
                 verbatimTextOutput("ljung_box_test") %>% withSpinner(),
                 verbatimTextOutput("residual_summary") %>% withSpinner()
               )
             ),
             
             # --- KONTEN KOLOM 2 (Main Panel) ---
             div(
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
- Siti Fadilah Nurkhotimah (1314623019)
- Alfachino Maulana (1314623043)
- Ayda Syifa Ul Aliyah (1314623064)
- Rachmawati Tefaaulia (1314623066)
- Oki Ramadhan Pramono (1314623067)
- Septiani Amalia Wulandari (1314623069)
      ")
           )
  )
)

# --- SERVER (GABUNGAN & FINAL) ---
# SERVER TETAP SAMA, TIDAK PERLU DIUBAH
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
  train_data <- reactive({
    req(dataset_ts())
    data_for_training <- dataset_ts()
    validate(need(nrow(data_for_training) >= 36, "Data tidak cukup. Minimum 36 observasi diperlukan untuk analisis."))
    n_test <- min(24, floor(nrow(data_for_training) * 0.2))
    data_for_training %>% slice(1:(n() - n_test))
  })
  
  active_model <- reactiveVal()
  
  observeEvent(train_data(), {
    req(train_data())
    showNotification("Melatih model ARIMA otomatis...", type = "message", duration = 3)
    model_auto <- train_data() %>% model(Auto_ARIMA = ARIMA(price))
    active_model(model_auto)
  })
  
  observeEvent(input$run_manual_arima, {
    req(train_data())
    showNotification("Melatih model ARIMA dengan ordo manual...", type = "message", duration = 3)
    model_manual <- train_data() %>% model(Manual_ARIMA = ARIMA(price ~ pdq(input$p_order, input$d_order, input$q_order)))
    active_model(model_manual)
  })
  
  # --- Reaktif untuk Evaluasi, Forecast, dll. ---
  model_evaluation <- reactive({
    req(active_model(), dataset_ts())
    n_total <- nrow(dataset_ts())
    n_train <- nrow(train_data())
    h_test <- n_total - n_train
    
    acc <- active_model() %>% forecast(h = h_test) %>% accuracy(dataset_ts())
    glc <- active_model() %>% glance()
    
    acc %>%
      left_join(glc, by = ".model") %>%
      select(.model, AIC, MAPE, RMSE, MAE, MASE, AICc, BIC) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
  })
  
  model_forecast <- reactive({
    req(active_model())
    horizon <- paste0(input$ahead, " years")
    active_model() %>% forecast(h = horizon)
  })
  
  # --- Output: Eksplorasi Data ---
  output$exploration_instructions <- renderUI({if (!isTruthy(input$run_analysis)) h4("Silakan unggah file...", style = "text-align: center; color: grey;")})
  output$summaryStats <- renderPrint({ req(dataset_ts()); data <- dataset_ts(); cat("Ringkasan Statistik untuk Kolom '", input$value_col, "'\n\n", sep=""); summary(data$price) })
  output$tsPlot <- renderPlotly({ req(dataset_ts()); p <- dataset_ts() %>% autoplot(price) + labs(title = paste("Plot Time Series:", input$value_col), y = input$value_col, x = "Tanggal"); ggplotly(p, tooltip = "y") })
  output$acfPacfPlot <- renderPlot({ req(dataset_ts()); dataset_ts() %>% gg_tsdisplay(y = price, plot_type = 'partial', lag_max = 36) + labs(title = "ACF & PACF") })
  output$dcompPlot <- renderPlotly({ req(dataset_ts()); p <- dataset_ts() %>% model(STL(price)) %>% components() %>% autoplot() + labs(title = "Dekomposisi STL"); ggplotly(p) })
  
  output$rollingStatsPlot <- renderPlotly({
    req(dataset_ts())
    validate(need(nrow(dataset_ts()) >= 2, "Data tidak cukup untuk statistik bergerak (min. 2)."))
    
    rolling_data <- dataset_ts() %>%
      mutate(
        rolling_mean = slider::slide_dbl(price, ~mean(.x, na.rm = TRUE), .before = 11),
        rolling_sd   = slider::slide_dbl(price, ~sd(.x, na.rm = TRUE), .before = 11)
      ) %>% drop_na()
    
    plot_data_mean <- rolling_data %>%
      select(observation_date, `Nilai Aktual` = price, `Rata-rata Bergerak` = rolling_mean) %>%
      pivot_longer(cols = -observation_date, names_to = "Metrik", values_to = "Nilai")
    
    p1 <- ggplot(plot_data_mean, aes(x = observation_date, y = Nilai, color = Metrik)) +
      geom_line(linewidth = 0.8) +
      labs(title = "Nilai Aktual & Rata-rata Bergerak", x = NULL, y = "Nilai") +
      theme_minimal() +
      scale_color_manual(values = c("Rata-rata Bergerak" = "#007bff", "Nilai Aktual" = "grey50"))
    
    p2 <- ggplot(rolling_data, aes(x = observation_date, y = rolling_sd)) +
      geom_line(linewidth = 0.8, color = "#dc3545") +
      labs(title = "Standar Deviasi Bergerak (12-Bulan)", x = "Tanggal", y = "St. Deviasi") +
      theme_minimal()
    
    subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
      layout(title = list(text = "Analisis Kestasioneran Visual", y = 0.98),
             legend = list(orientation = "h", y = 1.05, x = 0.5, xanchor = "center"))
  })
  
  # --- Output: Hasil Uji Stasioneritas ADF ---
  output$stationarity_result <- renderUI({ 
    req(dataset_ts())
    
    ts_data <- na.omit(as.numeric(dataset_ts()$price))
    validate(need(length(ts_data) > 0, "Kolom nilai tidak memiliki data numerik yang valid."))
    
    # Gunakan test ADF
    d_rekomendasi <- ndiffs(ts_data, test = "adf")
    ts_diff <- if (d_rekomendasi > 0) diff(ts_data, differences = d_rekomendasi) else ts_data
    
    adf_result <- tryCatch(adf.test(ts_diff), error = function(e) NULL)
    if (is.null(adf_result)) {
      return(HTML("<h4>Uji ADF tidak dapat dilakukan.</h4><p>Kemungkinan data terlalu sedikit setelah differencing.</p>"))
    }
    
    p_val <- adf_result$p.value
    status <- if (p_val < 0.05) "<span style='color:green; font-weight:bold;'>STASIONER</span>" else "<span style='color:orange; font-weight:bold;'>TIDAK STASIONER</span>"
    p_val_text <- if(p_val < 0.01) "< 0.01" else round(p_val, 4)
    
    HTML(paste0(
      "<h4>Hasil Uji Stasioneritas (ADF)</h4>",
      "<ul>",
      "<li><b>Rekomendasi differencing (d):</b> ", d_rekomendasi, " → ", 
      if (d_rekomendasi == 0) {
        "data sudah stasioner, tidak perlu dilakukan differencing."
      } else {
        paste("data belum stasioner, perlu dilakukan differencing sebanyak", d_rekomendasi, "kali.")
      }, 
      "</li>",
      "<li><b>Statistik Uji (setelah differencing):</b> ", round(adf_result$statistic, 4), "</li>",
      "<li><b>p-value:</b> ", p_val_text, "</li>",
      "<li><b>Kesimpulan:</b> Setelah differencing ke-", d_rekomendasi, ", data dianggap ", status, ".</li>",
      "</ul>"
    ))
  })
  
  # --- Output: UI Plot Differencing (hanya jika d > 0) ---
  output$diff_plot_ui <- renderUI({
    req(dataset_ts())
    
    ts_data <- dataset_ts()$price
    ts_data <- na.omit(as.numeric(ts_data))
    d <- ndiffs(ts_data, test = "adf")  # konsisten ADF
    
    ts_diff <- if (d > 0) diff(ts_data, differences = d) else ts_data
    adf_result <- tryCatch(adf.test(ts_diff), error = function(e) NULL)
    req(adf_result)
    
    if (adf_result$p.value >= 0.05) {
      return(NULL)  # Tidak stasioner, belum ditindaklanjuti
    }
    
    if (d == 0) {
      return(card(
        card_header("Differencing Tidak Diperlukan"),
        HTML("<p>Data sudah stasioner, sehingga <b>tidak dilakukan differencing</b>.</p>")
      ))
    } else {
      return(card(
        card_header(paste("Plot Setelah Differencing ke-", d)),
        plotlyOutput("diff_plot") %>% withSpinner()
      ))
    }
  })
  
  # --- Output: Plot Differencing ---
  output$diff_plot <- renderPlotly({
    req(dataset_ts())
    
    ts_data <- dataset_ts()$price
    ts_data <- na.omit(as.numeric(ts_data))
    d <- ndiffs(ts_data, test = "adf")  # konsisten ADF
    req(d > 0)
    
    ts_diff <- diff(ts_data, differences = d)
    tanggal_diff <- dataset_ts()$observation_date[-(1:d)]
    
    df <- tibble(
      Tanggal = tanggal_diff,
      Data_Differenced = ts_diff
    )
    
    p <- ggplot(df, aes(x = Tanggal, y = Data_Differenced)) +
      geom_line(color = "#E69F00") +
      labs(title = paste("Plot Data Setelah Differencing ke-", d),
           x = "Tanggal", y = "Nilai") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  #---HASIL FORECASST---
  active_model <- reactiveVal()
  
  observeEvent(train_data(), {
    req(train_data())
    showNotification("Melatih model ARIMA otomatis...", type = "message", duration = 3)
    model_auto <- train_data() %>% model(Auto_ARIMA = ARIMA(price))
    active_model(model_auto)
  })
  
  observeEvent(input$run_manual_arima, {
    req(train_data())
    showNotification("Melatih model ARIMA dengan ordo manual...", type = "message", duration = 3)
    
    model_manual <- train_data() %>% model(Manual_ARIMA = ARIMA(price ~ pdq(input$p_order, input$d_order, input$q_order) + PDQ(input$P_order, input$D_order, input$Q_order)))
    active_model(model_manual)
  })
  
  # --- Output: Hasil Forecast untuk Model Manual ---
  output$forecastPlotManual <- renderPlotly({
    req(active_model())
    horizon <- paste0(input$ahead, " years")
    fc_manual <- active_model() %>% forecast(h = horizon)
    
    p_manual <- fc_manual %>% autoplot(train_data(), level = input$ci_level) +
      labs(title = "Hasil Forecast ARIMA Manual", y = input$value_col, x = "Tahun dan Bulan") +
      theme_minimal()
    
    ggplotly(p_manual, tooltip = c("x", "y"))
  })
  
  # --- Output: Hasil Forecast untuk Model Auto ARIMA ---
  output$forecastPlotAuto <- renderPlotly({
    req(active_model())
    horizon <- paste0(input$ahead, " years")
    fc_auto <- active_model() %>% forecast(h = horizon)
    
    p_auto <- fc_auto %>% autoplot(train_data(), level = input$ci_level) +
      labs(title = "Hasil Forecast ARIMA Otomatis", y = input$value_col, x = "Tahun dan Bulan") +
      theme_minimal()
    
    ggplotly(p_auto, tooltip = c("x", "y"))
  })
  
  # --- Output: Detail & Diagnostik Model ---
  output$accuracyTable <- renderDT({ req(model_evaluation()); datatable(model_evaluation(), options = list(dom = 't', ordering = FALSE), rownames = FALSE, caption = "Tabel Evaluasi Model")})
  output$modelSpecText_detail <- renderText({ req(model_evaluation()); model_evaluation()$.model })
  output$modelMAPE <- renderText({ req(model_evaluation()); aic_val <- model_evaluation()$AIC; mape_val <- model_evaluation()$MAPE; paste0("AIC: ", aic_val, " | MAPE: ", mape_val, "%")})
  output$arimaReport <- renderPrint({
    req(train_data())
    ts_data <- ts(train_data()$price, frequency = 12)
    model_summary <- auto.arima(ts_data)
    print(summary(model_summary))
    
    cat("\n--- Interpretasi Umum ---\n")
    cat("Model ARIMA(", paste(model_summary$arma[1:3], collapse = ","), ")",
        "dengan ordo:\n",
        "• p (AR)  =", model_summary$arma[1], "\n",
        "• d       =", model_summary$arma[6], "\n",
        "• q (MA)  =", model_summary$arma[2], "\n\n")
    cat("Interpretasi:\n")
    cat("- Nilai AIC:", round(model_summary$aic, 2), "menunjukkan kualitas model (lebih kecil lebih baik).\n")
    cat("- Periksa residual (lihat tab diagnostik) untuk validasi asumsi white noise.\n")
  })
  output$residualsPlot <- renderPlot({ req(active_model()); active_model() %>% gg_tsresiduals(lag_max = 24) })
  
  output$residualsDiagnostics <- renderPlot({
    req(active_model())
    
    resid_df <- residuals(active_model()) %>% as_tibble()
    fitted_df <- fitted(active_model()) %>% as_tibble()
    
    residuals_vec <- resid_df$.resid
    fitted_vec <- fitted_df$.fitted
    
    p1 <- ggplot(data.frame(fitted = fitted_vec, residuals = residuals_vec), aes(x = fitted, y = residuals)) + 
      geom_point(color = "steelblue") + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
      labs(title = "Plot Residual vs Fitted", x = "Fitted Values", y = "Residuals") + 
      theme_minimal()
    
    p2 <- ggplot(data.frame(residuals = residuals_vec), aes(x = residuals)) + 
      geom_histogram(bins = 30, fill = "orange", alpha = 0.7, color = "black") + 
      labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") + 
      theme_minimal()
    
    p3 <- ggplot(data.frame(residuals = residuals_vec), aes(sample = residuals)) + 
      stat_qq() + 
      stat_qq_line() + 
      labs(title = "Q-Q Plot of Residuals") + 
      theme_minimal()
    
    (p1 / p2 / p3) + plot_layout(ncol = 1)
  })
  
  output$ljung_box_test <- renderPrint({
    req(active_model())
    resids_vec <- residuals(active_model()) %>% pull(.resid)
    validate(need(length(na.omit(resids_vec)) > 0, "Residual tidak dapat dihitung."))
    model_params <- active_model() %>% glance()
    p <- if ("p" %in% names(model_params)) model_params$p else 0
    q <- if ("q" %in% names(model_params)) model_params$q else 0
    fitdf <- p + q
    lb_test <- Box.test(resids_vec, lag = 12, type = "Ljung-Box", fitdf = fitdf)
    cat("Uji Ljung-Box untuk Autokorelasi Residual:\n\n")
    cat("  X-squared  :", round(lb_test$statistic, 4), "\n")
    cat("  lag        :", 12, "\n")
    cat("  df         :", lb_test$parameter, "\n")
    cat("  p-value    :", round(lb_test$p.value, 4), "\n")
    if(lb_test$p.value > 0.05) {cat("\n→ Kesimpulan: Tidak ada bukti kuat autokorelasi pada residual.")} else {cat("\n→ Kesimpulan: Terdapat autokorelasi pada residual.")}
  })
  
  output$residual_summary <- renderPrint({
    req(active_model())
    resids <- na.omit(residuals(active_model())$.resid)
    fitted_vals <- na.omit(fitted(active_model())$.fitted)
    validate(need(length(resids) > 5, "Tidak cukup residual untuk pengujian."))
    
    acf_res <- acf(resids, plot = FALSE)
    acf_table <- data.frame(Lag = acf_res$lag, ACF = round(acf_res$acf, 3))
    
    shapiro_test <- shapiro.test(resids)
    bp_test_result <- tryCatch(bptest(resids ~ fitted_vals), error = function(e) NULL)
    
    cat("Ringkasan Diagnostik Asumsi Residual:\n\n")
    cat("1. Uji Normalitas (Shapiro-Wilk Test):\n")
    cat("   - p-value:", round(shapiro_test$p.value, 4), "→", if (shapiro_test$p.value < 0.05) "Residual TIDAK normal" else "Residual normal", "\n")
    
    cat("\n2. Uji Homoskedastisitas (Breusch-Pagan Test):\n")
    if (is.null(bp_test_result)) {
      cat("   - Uji tidak dapat dilakukan (kemungkinan nilai fitted konstan).\n")
    } else {
      cat("   - p-value:", round(bp_test_result$p.value, 4), "→", if (bp_test_result$p.value < 0.05) "Terdapat Heteroskedastisitas" else "Tidak ada Heteroskedastisitas (Homoskedastis)", "\n")
    }
    
    cat("\n3. Autokorelasi (ACF 10 lag pertama):\n")
    print(head(acf_table, 10))
  })
  
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
    
    HTML(paste("<h5>Penjelasan:</h5>", "<ul>", "<li><b>AIC (Akaike Information Criterion):</b>", "<ul>", "<li>Nilai ini mengukur seberapa baik model cocok dengan data latih.</li>", "<li><b>Tidak ada nilai absolut 'baik' atau 'buruk'.</b> AIC hanya berguna untuk <b>membandingkan beberapa model</b> pada data yang sama. Model dengan <b>nilai AIC yang lebih rendah</b> dianggap lebih baik.</li>", "</ul></li>", "<br>", "<li><b>MAPE (Mean Absolute Percentage Error):</b>", "<ul>", "<li>Mengukur rata-rata persentase kesalahan absolut dari hasil forecast dibandingkan dengan data aktual (out-of-sample).</li>", "<li>Nilai saat ini adalah <b>", mape_val, "%</b>, yang interpretasinya adalah: <b>", mape_interp, "</b></li>", "</ul></li>", "</ul>"))
  })
  
  # --- Opsi Output ---
  outputOptions(output, "modelMAPE", suspendWhenHidden = FALSE)
  outputOptions(output, "accuracyTable", suspendWhenHidden = FALSE)
  outputOptions(output, "arimaReport", suspendWhenHidden = FALSE)
  outputOptions(output, "modelSpecText_detail", suspendWhenHidden = FALSE)
  outputOptions(output, "eval_interpretation", suspendWhenHidden = FALSE)
}

# --- JALANKAN APLIKASI ---
shinyApp(ui = ui, server = server)
