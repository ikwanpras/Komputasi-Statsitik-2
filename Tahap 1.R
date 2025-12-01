# ==========================================
# 0. PERSIAPAN PAKET
# ==========================================
# tidyverse : kumpulan paket utama untuk data science (dplyr, ggplot2, dsb).
# readxl    : khusus untuk membaca file Excel (.xls/.xlsx).
# broom     : merapikan hasil model (lm) menjadi tabel (tibble) yang mudah dibaca.
install.packages("tidyverse")
install.packages("readxl")
install.packages("broom")

library(tidyverse)
library(readxl)
library(broom)

# ==========================================
# 1. IMPORT DATA EXCEL  (IMPORT)
# ==========================================
# Menggunakan read_excel karena sumber data adalah file Excel, sesuai
# rekomendasi R for Data Science untuk data dari spreadsheet. [web:54][web:60]
data_sovi <- read_excel(
  path  = "sovi_data_Kab.xlsx",  # pastikan nama file sama persis
  sheet = "Data_Sovi"            # ganti jika nama sheet berbeda
)

# DISTRICT dan DISTRICTCODE dibiarkan supaya hasil bisa dihubungkan ke nama kabupaten.
glimpse(data_sovi)

# ==========================================
# 2. RAPIKAN & PILIH VARIABEL  (TIDY + TRANSFORM)
# ==========================================
# select(): memilih hanya variabel yang relevan agar analisis fokus dan
# mengurangi kebisingan. [web:21][web:58]
# drop_na(): menghapus baris dengan data hilang supaya grafik dan model tidak error.
data_sovi_clean <- data_sovi |>
  select(
    DISTRICTCODE,
    DISTRICT,
    POVERTY,      # variabel kemiskinan (target)
    CHILDREN,
    FEMALE,
    ELDERLY,
    FHEAD,
    FAMILYSIZE,
    NOELECTRIC,
    LOWEDU,
    GROWTH,
    ILLITERATE,
    NOTRAINING,
    DPRONE,
    RENTED,
    NOSEWER,
    TAPWATER
  ) |>
  drop_na()

glimpse(data_sovi_clean)
summary(data_sovi_clean$POVERTY)

# ==========================================
# 3. BUAT KATEGORI KEMISKINAN  (TRANSFORM)
# ==========================================
# POVERTY adalah angka kontinu; untuk deskripsi, sering praktis dibagi
# menjadi kategori. Di sini digunakan kuantil (33% dan 66%) supaya batas
# kategori mengikuti distribusi data, bukan asal. [web:60][web:58]
quantile_pov <- quantile(
  data_sovi_clean$POVERTY,
  probs = c(0.33, 0.66),
  na.rm = TRUE
)

batas1 <- quantile_pov[1]
batas2 <- quantile_pov[2]

data_sovi_clean <- data_sovi_clean |>
  mutate(
    KATEGORI_POV = case_when(
      POVERTY <= batas1                    ~ "Rendah",
      POVERTY > batas1 & POVERTY <= batas2 ~ "Sedang",
      POVERTY > batas2                     ~ "Tinggi"
    )
  )
# mutate(): menambah kolom baru.
# case_when(): memudahkan membuat beberapa kategori berdasarkan kondisi logika. [web:21][web:58]

count(data_sovi_clean, KATEGORI_POV)

# ==========================================
# 4. VISUALISASI EKSPLORATORI  (VISUALIZE)
# ==========================================
# Tujuan: memahami pola hubungan antara POVERTY dan faktor lain sebelum modelling.
# ggplot2 dipakai karena merupakan standar visualisasi di tidyverse. [web:21][web:38]

# 4a. LOWEDU vs POVERTY
plot_lowedu <- ggplot(
  data_sovi_clean,
  aes(x = LOWEDU, y = POVERTY, color = KATEGORI_POV)
) +
  geom_point(alpha = 0.8, size = 2) +   # titik = kabupaten, alpha agar tidak terlalu pekat
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  # geom_smooth(method = "lm"): menambahkan garis regresi linear untuk
  # menunjukkan tren umum hubungan LOWEDU dan POVERTY. [web:38][web:79]
  labs(
    title = "Pendidikan rendah (LOWEDU) vs Kemiskinan (POVERTY) per kabupaten",
    x     = "Persentase berpendidikan rendah (LOWEDU)",
    y     = "Persentase kemiskinan (POVERTY)",
    color = "Kategori POVERTY"
  ) +
  theme_minimal()  # tema minimalis agar grafik bersih dan mudah dibaca.

plot_lowedu

# 4b. NOELECTRIC vs POVERTY
plot_noelectric <- ggplot(
  data_sovi_clean,
  aes(x = NOELECTRIC, y = POVERTY, color = KATEGORI_POV)
) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Rumah tanpa listrik (NOELECTRIC) vs Kemiskinan (POVERTY)",
    x     = "Persentase rumah tanpa listrik (NOELECTRIC)",
    y     = "Persentase kemiskinan (POVERTY)",
    color = "Kategori POVERTY"
  ) +
  theme_minimal()

plot_noelectric

# ==========================================
# 5. MODEL REGRESI LINIER  (MODEL)
# ==========================================
# Tujuan: mengukur secara kuantitatif bagaimana faktor-faktor sosial
# berhubungan dengan POVERTY. lm() dipakai karena ini model regresi
# linear klasik yang dijelaskan di R4DS. [web:79][web:80]

# Model 1: hanya faktor pendidikan
model_edukasi <- lm(
  POVERTY ~ LOWEDU + ILLITERATE,
  data = data_sovi_clean
)

# Model 2: model lebih lengkap dengan demografi dan fasilitas
model_lengkap <- lm(
  POVERTY ~ LOWEDU + ILLITERATE +
    CHILDREN + ELDERLY +
    FHEAD + FAMILYSIZE +
    NOELECTRIC + NOTRAINING +
    RENTED + NOSEWER + TAPWATER,
  data = data_sovi_clean
)

# summary(): melihat koefisien, signifikansi, dan R-squared. [web:79]
summary(model_edukasi)
summary(model_lengkap)

# broom::tidy(): merapikan output ke bentuk tabel (tibble) sehingga
# mudah difilter / diekspor / disajikan. [web:22][web:79]
tabel_model_edukasi <- tidy(model_edukasi)
tabel_model_lengkap <- tidy(model_lengkap)

tabel_model_edukasi
tabel_model_lengkap

# ==========================================
# 6. RINGKASAN PERFORMA MODEL
# ==========================================
# Dibuat tibble kecil untuk membandingkan R-squared antar model,
# seperti ide "model comparison" di bab modelling R4DS. [web:79][web:80]
perbandingan_model <- tibble(
  model         = c("Edukasi", "Lengkap"),
  r_squared     = c(
    summary(model_edukasi)$r.squared,
    summary(model_lengkap)$r.squared
  ),
  adj_r_squared = c(
    summary(model_edukasi)$adj.r.squared,
    summary(model_lengkap)$adj.r.squared
  )
)

perbandingan_model
