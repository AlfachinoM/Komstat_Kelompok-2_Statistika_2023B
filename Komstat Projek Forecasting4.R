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
library(shinyjs)

# --- UI (User Interface) ---
ui <- navbarPage(
  title = div(
    img(src = "logo.png", height = "40px", style = "margin-right:10px;"),
    span("Aplikasi Forecasting ARIMA", style = "font-weight: 600;")
  ),
  id = "main_navbar",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#87CEEB",
    base_font = font_google("Inter"),
    bg = "#f8f9fa",
    fg = "#212529"
  ),
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("body {background-image: url('bg-blue.jpg'); background-size: cover; background-attachment: fixed;}"))
    )
  ),
  collapsible = TRUE,
  inverse = FALSE,
  
  # --- TAB 1: EKSPLORASI DATA ---
  tabPanel("Eksplorasi Data",
           layout_columns(
             col_widths = c(3, 9),
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
                 actionButton("run_analysis", "Jalankan Analisis", icon = icon("play"), class = "btn-primary w-100")
               )
             ),
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
                            card_header("Plot ACF & PACF Interaktif"),
                            ### --- DIPERBAIKI: Menggunakan plotlyOutput --- ###
                            plotlyOutput("acfPacfPlot", height = "600px") %>% withSpinner()
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
  
  # --- TAB 3: DETAIL & DIAGNOSTIK ---
  tabPanel("Detail & Diagnostik Model",
           layout_columns(
             col_widths = c(5, 7),
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
                 plotOutput("residualsDiagnostics", height="500px") %>% withSpinner(),
                 tags$hr(),
                 verbatimTextOutput("ljung_box_test") %>% withSpinner(),
                 verbatimTextOutput("residual_summary") %>% withSpinner()
               )
             ),
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
  
  # --- TAB 4: HASIL FORECAST ---
  tabPanel("Hasil Forecast",
           layout_columns(
             col_widths = c(3, 9),
             div(), 
             div(
               card(
                 card_header("Plot Hasil Forecast ARIMA Otomatis"),
                 plotlyOutput("forecastPlotAuto") %>% withSpinner(),
                 htmlOutput("interpretasiForecastAuto")
               ),
               card(
                 card_header("Tabel Hasil Peramalan"),
                 DTOutput("forecastTable") %>% withSpinner()
               )
             )
           )
  ),
  
  # --- TAB 5: TENTANG ---
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
           ),
           
           card(
             card_header("Penjelasan Model ARIMA dan SARIMA"),
             markdown("
### ARIMA (Autoregressive Integrated Moving Average)

Model ARIMA adalah metode peramalan deret waktu yang menggabungkan tiga komponen utama:

- **AR (Autoregressive):** Hubungan antara nilai saat ini dengan nilai-nilai sebelumnya (lag).
- **I (Integrated):** Proses differencing data untuk menjadikannya stasioner, yaitu menghilangkan tren atau perubahan musiman.
- **MA (Moving Average):** Hubungan antara nilai saat ini dan galat (kesalahan) model sebelumnya.

Model ini ditulis dalam bentuk **ARIMA(p, d, q)**, di mana:
- `p` = jumlah lag untuk komponen AR,
- `d` = jumlah differencing untuk mencapai kestasioneran,
- `q` = jumlah lag untuk komponen MA.

ARIMA sangat efektif untuk data deret waktu yang **tidak memiliki pola musiman**.

---

### SARIMA (Seasonal ARIMA)

Model SARIMA merupakan perluasan dari ARIMA untuk menangani pola **musiman** dalam data deret waktu. Ia menambahkan komponen musiman ke struktur ARIMA.

SARIMA ditulis dalam bentuk **SARIMA(p, d, q)(P, D, Q)[s]**, di mana:

#### Komponen Non-Musiman:
- `p` = Autoregressive (jumlah lag nilai masa lalu)
- `d` = Differencing (pengurangan agar stasioner)
- `q` = Moving Average (lag galat masa lalu)

#### Komponen Musiman:
- `P` = Seasonal AR (lag musiman nilai masa lalu)
- `D` = Seasonal Differencing
- `Q` = Seasonal MA (galat musiman masa lalu)
- `s` = Musim (jumlah periode per siklus, contoh `s=12` untuk data bulanan)

SARIMA sangat cocok untuk data dengan **pola berulang musiman**, seperti data bulanan atau kuartalan.

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
  
  ### --- DIPERBAIKI: Menggunakan renderPlotly dan subplot() --- ###
  output$acfPacfPlot <- renderPlotly({
    req(dataset_ts())
    p_acf <- dataset_ts() %>% ACF(y = price, lag_max = 36) %>% autoplot() + labs(title = "Autocorrelation Function (ACF)")
    p_pacf <- dataset_ts() %>% PACF(y = price, lag_max = 36) %>% autoplot() + labs(title = "Partial Autocorrelation Function (PACF)")
    subplot(ggplotly(p_acf), ggplotly(p_pacf), nrows = 2, shareX = TRUE, titleY = TRUE) %>%
      layout(title = list(text = "ACF & PACF untuk Data Asli", y = 0.95))
  })
  
  output$dcompPlot <- renderPlotly({ req(dataset_ts()); p <- dataset_ts() %>% model(STL(price)) %>% components() %>% autoplot() + labs(title = "Dekomposisi STL"); ggplotly(p) })
  
  output$rollingStatsPlot <- renderPlotly({
    req(dataset_ts())
    validate(need(nrow(dataset_ts()) >= 12, "Data tidak cukup untuk statistik bergerak (min 12)."))
    
    rolling_data <- dataset_ts() %>%
      mutate(
        rolling_mean = slider::slide_dbl(price, ~mean(.x, na.rm = TRUE), .before = 11, .complete = TRUE),
        rolling_sd   = slider::slide_dbl(price, ~sd(.x, na.rm = TRUE), .before = 11, .complete = TRUE)
      ) %>% drop_na()
    
    p1 <- plot_ly(rolling_data, x = ~observation_date) %>%
      add_lines(y = ~price, name = "Nilai Aktual", line = list(color = 'grey')) %>%
      add_lines(y = ~rolling_mean, name = "Rata-rata Bergerak", line = list(color = '#007bff')) %>%
      layout(title = "Nilai Aktual & Rata-rata Bergerak", yaxis = list(title = "Nilai"))
    
    p2 <- plot_ly(rolling_data, x = ~observation_date, y = ~rolling_sd, type = 'scatter', mode = 'lines', name = "St. Deviasi Bergerak", line = list(color = '#dc3545')) %>%
      layout(title = "Standar Deviasi Bergerak (12-Bulan)", yaxis = list(title = "St. Deviasi"))
    
    subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
      layout(title = list(text = "Analisis Kestasioneran Visual", y = 0.98),
             legend = list(orientation = "h", y = 1.1, x = 0.5, xanchor = "center"))
  })
  
  # --- Output: Hasil Uji Stasioneritas ADF ---
  d_order <- reactive({ req(dataset_ts()); ndiffs(na.omit(as.numeric(dataset_ts()$price)), test = "adf") })
  
  data_diff <- reactive({
    req(dataset_ts(), d_order())
    if(d_order() == 0) return(NULL)
    dataset_ts() %>% mutate(diff_price = difference(price, d_order())) %>% drop_na()
  })
  
  output$stationarity_result <- renderUI({ 
    req(dataset_ts(), d_order())
    
    ts_data <- na.omit(as.numeric(dataset_ts()$price))
    ts_diff <- if (d_order() > 0) diff(ts_data, differences = d_order()) else ts_data
    
    adf_result <- tryCatch(adf.test(ts_diff), error = function(e) NULL)
    if (is.null(adf_result)) return(HTML("<h4>Uji ADF tidak dapat dilakukan.</h4>"))
    
    p_val <- adf_result$p.value
    status <- if (p_val < 0.05) "<span style='color:green; font-weight:bold;'>STASIONER</span>" else "<span style='color:orange; font-weight:bold;'>TIDAK STASIONER</span>"
    p_val_text <- if(p_val < 0.01) "< 0.01" else round(p_val, 4)
    
    HTML(paste0(
      "<h4>Hasil Uji Stasioneritas (ADF)</h4><ul>",
      "<li><b>Rekomendasi differencing (d):</b> ", d_order(), "</li>",
      "<li><b>Statistik Uji (setelah differencing):</b> ", round(adf_result$statistic, 4), "</li>",
      "<li><b>p-value:</b> ", p_val_text, "</li>",
      "<li><b>Kesimpulan:</b> Setelah differencing, data dianggap ", status, ".</li></ul>"
    ))
  })
  
  #plot data yang sudah stasioner
  output$diff_plot_ui <- renderUI({
    req(d_order())
    if (d_order() == 0) return(card(card_header("Differencing Tidak Diperlukan"), HTML("<p>Data sudah stasioner.</p>")))
    
    tagList(
      card(card_header(paste("Plot Setelah Differencing ke-", d_order())), plotlyOutput("diff_plot") %>% withSpinner()),
      card(card_header(paste("ACF & PACF Setelah Differencing ke-", d_order())), plotlyOutput("diff_acf_pacf_plot", height = "600px") %>% withSpinner())
    )
  })
  
  output$diff_plot <- renderPlotly({
    req(data_diff())
    p <- data_diff() %>% autoplot(diff_price) + labs(title = NULL, y = "Nilai Hasil Differencing", x = "Tanggal")
    ggplotly(p)
  })
  
  #plot acf pacf difference
  output$diff_acf_pacf_plot <- renderPlotly({
    req(data_diff())
    p_acf <- data_diff() %>% ACF(y = diff_price, lag_max = 36) %>% autoplot() + labs(title = NULL)
    p_pacf <- data_diff() %>% PACF(y = diff_price, lag_max = 36) %>% autoplot() + labs(title = NULL)
    subplot(ggplotly(p_acf), ggplotly(p_pacf), nrows = 2, shareX = TRUE, titleY = TRUE)
  })
  
  #---HASIL FORECAST---
  output$forecastPlotAuto <- renderPlotly({
    req(model_forecast())
    p_auto <- model_forecast() %>% autoplot(dataset_ts()) +
      labs(title = "Hasil Forecast ARIMA Otomatis", y = input$value_col, x = "Tahun dan Bulan") +
      theme_minimal()
    ggplotly(p_auto, tooltip = c("x", "y"))
  })
  
  output$interpretasiForecastAuto <- renderUI({
    req(model_evaluation(), input$value_col)
    eval <- model_evaluation()
    interpret_mape <- case_when(
      eval$MAPE < 10 ~ "<b>Sangat Baik</b>: Error peramalan sangat rendah.",
      eval$MAPE < 20 ~ "<b>Baik</b>: Error peramalan cukup rendah dan dapat diterima.",
      eval$MAPE < 50 ~ "<b>Cukup:</b> Hasil forecast dapat digunakan dengan kehati-hatian.",
      TRUE          ~ "<b>Buruk:</b> Model mungkin kurang cocok, error tinggi."
    )
    HTML(paste0(
      "<div style='margin-top:20px; padding: 15px;'>",
      "<h4>📈 Interpretasi Hasil Forecast</h4>",
      "<p>Model <b>", eval$.model, "</b> digunakan untuk memprediksi <b>", input$ahead, " tahun ke depan</b>.</p>",
      "<ul><li><b>MAPE:</b> ", eval$MAPE, "% → ", interpret_mape, "</li></ul></div>"
    ))
  })
  
  output$forecastTable <- renderDT({
    req(model_forecast())
    
    model_forecast() %>%
      hilo(level = c(80, 95)) %>%
      unpack_hilo(`80%`) %>%
      unpack_hilo(`95%`) %>%
      as_tibble() %>%
      transmute(
        Tanggal = as.character(observation_date),
        Forecast = round(.mean, 2),
        `Batas Bawah (80%)` = round(`80%_lower`, 2),
        `Batas Atas (80%)` = round(`80%_upper`, 2),
        `Batas Bawah (95%)` = round(`95%_lower`, 2),
        `Batas Atas (95%)` = round(`95%_upper`, 2)
      ) %>%
      datatable(rownames = FALSE, options = list(pageLength = 12, dom = 'tip'), caption = "Tabel Rincian Hasil Peramalan")
  })
  
  # --- Output: Detail & Diagnostik Model ---
  output$accuracyTable <- renderDT({ req(model_evaluation()); datatable(model_evaluation(), options = list(dom = 't', ordering = FALSE), rownames = FALSE, caption = "Tabel Evaluasi Model")})
  output$modelSpecText_detail <- renderText({ req(model_evaluation()); model_evaluation()$.model })
  output$modelMAPE <- renderText({ req(model_evaluation()); aic_val <- model_evaluation()$AIC; mape_val <- model_evaluation()$MAPE; paste0("AIC: ", aic_val, " | MAPE: ", mape_val, "%")})
  
  output$arimaReport <- renderPrint({ req(active_model()); report(active_model()) })
  output$residualsPlot <- renderPlot({ req(active_model()); active_model() %>% gg_tsresiduals(lag_max = 24) })
  
  output$residualsDiagnostics <- renderPlot({
    req(active_model())
    diag_data <- left_join(residuals(active_model()), fitted(active_model()), by = c(".model", "observation_date"))
    
    p1 <- ggplot(diag_data, aes(x = .fitted, y = .resid)) + geom_point(color = "steelblue") + geom_hline(yintercept = 0, linetype = "dashed", color = "red") + labs(title = "Plot Residual vs Fitted", x = "Fitted Values", y = "Residuals")
    p2 <- ggplot(diag_data, aes(x = .resid)) + geom_histogram(bins = 30, fill = "orange", alpha = 0.7) + labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")
    p3 <- ggplot(diag_data, aes(sample = .resid)) + stat_qq() + stat_qq_line() + labs(title = "Q-Q Plot of Residuals")
    (p1 / p2 / p3) + plot_layout(ncol = 1) & theme_minimal()
  })
  
  output$ljung_box_test <- renderPrint({
    req(active_model())
    lb_test <- residuals(active_model()) %>% features(.resid, ljung_box, lag = 12)
    cat("Uji Ljung-Box untuk Autokorelasi Residual:\n\n")
    cat("  X-squared  :", round(lb_test$lb_stat, 4), "\n")
    cat("  df         :", lb_test$dof, "\n")
    cat("  p-value    :", round(lb_test$lb_pvalue, 4), "\n")
    if(lb_test$lb_pvalue > 0.05) {cat("\n→ Kesimpulan: Tidak ada bukti kuat autokorelasi pada residual.")} else {cat("\n→ Kesimpulan: Terdapat autokorelasi pada residual.")}
  })
  
  output$residual_summary <- renderPrint({
    req(active_model())
    resids <- na.omit(residuals(active_model())$.resid)
    validate(need(length(resids) > 5, "Tidak cukup residual untuk pengujian."))
    
    shapiro_test <- if(length(resids) > 5000) list(p.value = NA) else shapiro.test(resids)
    bp_test_result <- tryCatch(bptest(resids ~ fitted(active_model())$.fitted), error = function(e) NULL)
    
    cat("Ringkasan Diagnostik Asumsi Residual:\n\n")
    cat("1. Uji Normalitas (Shapiro-Wilk Test):\n")
    if(is.na(shapiro_test$p.value)) {
      cat("   - Uji tidak dilakukan (data > 5000).\n")
    } else {
      cat("   - p-value:", round(shapiro_test$p.value, 4), "→", if (shapiro_test$p.value < 0.05) "Residual TIDAK normal" else "Residual normal", "\n")
    }
    
    cat("\n2. Uji Homoskedastisitas (Breusch-Pagan Test):\n")
    if (is.null(bp_test_result)) {
      cat("   - Uji tidak dapat dilakukan.\n")
    } else {
      cat("   - p-value:", round(bp_test_result$p.value, 4), "→", if (bp_test_result$p.value < 0.05) "Terdapat Heteroskedastisitas" else "Homoskedastis", "\n")
    }
  })
  
  output$eval_interpretation <- renderUI({
    req(model_evaluation())
    mape_val <- model_evaluation()$MAPE
    
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
