# ============================================================
# PRAKTIKUM R 
# Alur R for Data Science (Hadley):
# 1) Membuat / mengimpor data
# 2) Transformasi dengan dplyr (tidyverse)
# 3) Visualisasi dengan ggplot2
# 4) Model regresi linear dasar + cek residual
# Data: BANGKITAN (50 "kabupaten"), aman untuk latihan
# ============================================================

# ------------------------------------------------------------
# 0. PERSIAPAN PAKET
# ------------------------------------------------------------
# tidyverse = kumpulan paket inti (ggplot2, dplyr, readr, tibble, dll.)
# Buku R4DS menggunakan tidyverse sebagai "bahasa utama" data science. 
install.packages("tidyverse")   # cukup sekali di komputer; bisa di-comment setelah terpasang
library(tidyverse)

# ------------------------------------------------------------
# 1. MEMBANGKITKAN DATA SINTETIS (SEPERTI CONTOH BAB MODEL) 
# ------------------------------------------------------------
# Di R4DS, Hadley sering membuat data simulasi untuk memahami model. 
# Di sini: 50 kabupaten dengan 3 variabel penjelas dan 1 variabel target.

set.seed(123)      # agar angka acak bisa diulang (reproducible)
n_kab <- 50        # jumlah baris data

# tibble(): versi modern dari data.frame, digunakan di R4DS. 
data_sim <- tibble(
  KAB        = paste("Kab", 1:n_kab),             # nama kabupaten fiktif
  LOWEDU     = runif(n_kab, 10, 40),              # % pendidikan rendah (runif: angka acak seragam antara 10 dan 40.)
  NOELECTRIC = runif(n_kab, 0, 20),               # % rumah tanpa listrik (runif: angka acak antara 0 dan 20.)
  GROWTH     = rnorm(n_kab, mean = 5, sd = 2)     # pertumbuhan ekonomi (rnorm: angka acak normal dengan rata-rata 5 dan simpangan baku 2.)
)

# Bangkitkan POVERTY dengan fungsi linear + noise:
#   POVERTY = 5 + 0.4*LOWEDU + 0.6*NOELECTRIC - 0.3*GROWTH + error
# Ide ini sama dengan contoh "model dari data simulasi" di bab model basics. 
data_sim <- data_sim |>
  mutate(                 #mutate() menambah kolom baru bernama POVERTY.
    POVERTY = 5 +
      0.4 * LOWEDU +
      0.6 * NOELECTRIC +
      -0.3 * GROWTH +
      rnorm(n_kab, mean = 0, sd = 3)   # error acak
  )

glimpse(data_sim)         # mirip yang digunakan di R4DS untuk melihat struktur data 
summary(data_sim$POVERTY)

# ------------------------------------------------------------
# 2. TRANSFORMASI DATA DENGAN DPLYR
# ------------------------------------------------------------
# Bagian ini paralel dengan bab "Data transformation" (select, filter, mutate, summarise). 

# 2.1. Pembersihan ringan (drop_na)
data_sim_clean <- data_sim |>
  drop_na()
# drop_na(): dipakai umum di R4DS untuk menghapus baris dengan NA sebelum analisis. 

# 2.2. Ringkasan sederhana: rata-rata tiap variabel numerik
ringkasan_mean <- data_sim_clean |>
  summarise(
    mean_poverty   = mean(POVERTY),
    mean_lowedu    = mean(LOWEDU),
    mean_noelectric= mean(NOELECTRIC),
    mean_growth    = mean(GROWTH)
  )

ringkasan_mean

# 2.3. Contoh group_by + summarise (misal: bagi dua kelompok GROWTH)
data_sim_clean <- data_sim_clean |>
  mutate(
    GRP_GROWTH = if_else(GROWTH >= median(GROWTH),
                         "Tinggi", "Rendah")
  )

# group_by() + summarise() = pola yang sangat ditekankan di R4DS. 
ringkasan_kelompok <- data_sim_clean |>
  group_by(GRP_GROWTH) |>
  summarise(
    mean_poverty = mean(POVERTY),
    mean_lowedu  = mean(LOWEDU),
    mean_noelec  = mean(NOELECTRIC),
    .groups = "drop"
  )

ringkasan_kelompok

# ------------------------------------------------------------
# 3. VISUALISASI DENGAN GGPLOT2
# ------------------------------------------------------------
# Mengikuti pola bab "Data visualization" (mpg) tapi dengan data kita sendiri. 

# 3.1. Scatter plot LOWEDU vs POVERTY + garis regresi
ggplot(data_sim_clean,
       aes(x = LOWEDU, y = POVERTY)) +
  geom_point(alpha = 0.8, color = "steelblue") +               # geom_point(...):Menambahkan titik untuk setiap kabupaten (scatter plot).
  geom_smooth(method = "lm", se = FALSE, color = "red") +      # geom_smooth(method = "lm", se = FALSE): Menggambar garis regresi linear (lm = linear model) tanpa area ketidakpastian (se = FALSE).
  labs(
    title = "LOWEDU vs POVERTY (data bangkitan)",              # labs(title = ..., x = ..., y = ...): Mengatur judul dan label sumbu.
    x     = "Persentase pendidikan rendah (LOWEDU)",
    y     = "Persentase kemiskinan (POVERTY)"
  ) +
  theme_minimal()                                              # theme_minimal():Mengganti tema grafik agar tampilan lebih bersih.

# 3.2. Scatter plot NOELECTRIC vs POVERTY, warna menurut GRP_GROWTH + facet
ggplot(data_sim_clean,
       aes(x = NOELECTRIC, y = POVERTY, color = GRP_GROWTH)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ GRP_GROWTH) +
  labs(
    title = "NOELECTRIC vs POVERTY difacet menurut GROWTH (rendah/tinggi)",
    x     = "% rumah tanpa listrik (NOELECTRIC)",
    y     = "% kemiskinan (POVERTY)",
    color = "Kelompok GROWTH"
  ) +
  theme_minimal()
# facet_wrap(): sama seperti contoh facetting di bab visualisasi (memecah grafik per grup).

# 3.3. Histogram POVERTY
ggplot(data_sim_clean,
       aes(x = POVERTY)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "white") +
  labs(
    title = "Distribusi POVERTY (data bangkitan)",
    x     = "Persentase kemiskinan",
    y     = "Jumlah kabupaten"
  ) +
  theme_minimal()
# Histogram/density sering dipakai di R4DS untuk mengecek bentuk distribusi. 

# ------------------------------------------------------------
# 4. MODEL REGRESI LINEAR DASAR
# ------------------------------------------------------------
# Sangat paralel dengan bab "Model basics" (fit model ke data simulasi, lalu visualisasi residual). 

# 4.1. Fit model linear 
# lm() adalah fungsi di R untuk membuat model linier
model_sim <- lm(
  POVERTY ~ LOWEDU + NOELECTRIC + GROWTH,
  data = data_sim_clean
)

summary(model_sim)  
# Interpretasi:
# untuk melihat koefisien, R-squared, p-value, dsb.
# - Intercept mendekati 5, koefisien LOWEDU ~0.4, NOELECTRIC ~0.6, GROWTH ~-0.3,
#   karena memang itu yang kita tanam saat membangkitkan data.

# 4.2. Tambahkan fitted values & residuals ke data (mengikuti R4DS)
data_sim_model <- data_sim_clean |>
  mutate(
    fitted = fitted(model_sim),   # nilai POVERTY yang diprediksi model (prediksi)
    resid  = resid(model_sim)     # Y asli - Y prediksi (residual)
  )

# 4.3. Plot residual vs fitted
ggplot(data_sim_model,
       aes(x = fitted, y = resid)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_point(alpha = 0.8) +
  labs(
    title = "Residual vs Fitted (model_sim)",
    x     = "Nilai fitted (prediksi POVERTY)",
    y     = "Residual"
  ) +
  theme_minimal()
# Di R4DS, grafik ini dipakai untuk mengecek apakah asumsi linearitas dan
# varians konstan kira-kira terpenuhi (pola residual acak di sekitar garis 0). 

# 4.4. QQ-plot residual (cek normalitas)
qqnorm(data_sim_model$resid)
qqline(data_sim_model$resid, col = "red")
# Di bab model diagnostics, R4DS memakai QQ-plot untuk menilai apakah residual
# qqnorm(...); qqline(...) QQ-plot residual untuk mengecek apakah residual mendekati distribusi normal.
# mendekati distribusi normal (titik mengikuti garis lurus). 
