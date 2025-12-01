# ==========================================
# SESI 3 – MODEL & EVALUASI
# ==========================================
# Asumsi: data_sovi_clean sudah ada dan berisi:
# POVERTY, LOWEDU, ILLITERATE, CHILDREN, ELDERLY,
# FHEAD, FAMILYSIZE, NOELECTRIC, NOTRAINING, RENTED, NOSEWER, TAPWATER.

# ------------------------------------------
# 3.1 MODEL DASAR VS MODEL LENGKAP
# ------------------------------------------
# Model 1: hanya faktor pendidikan (lebih sederhana).
model_edukasi <- lm(
  POVERTY ~ LOWEDU + ILLITERATE,
  data = data_sovi_clean
)

# Model 2: model lebih lengkap (pendidikan + demografi + fasilitas).
model_lengkap <- lm(
  POVERTY ~ LOWEDU + ILLITERATE +
    CHILDREN + ELDERLY +
    FHEAD + FAMILYSIZE +
    NOELECTRIC + NOTRAINING +
    RENTED + NOSEWER + TAPWATER,
  data = data_sovi_clean
)

summary(model_edukasi)
summary(model_lengkap)
# Alasan:
# - Dua model ini mengikuti ide R4DS: mulai dari model sederhana,
#   lalu tambahkan prediktor untuk melihat apakah model membaik. [web:79][web:80]

# Ringkas R-squared dan adj R-squared untuk perbandingan cepat.
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
# Alasan:
# - R-squared: seberapa besar variasi POVERTY yang dijelaskan model.
# - Adjusted R-squared: mengoreksi jumlah prediktor, cocok untuk membandingkan
#   model dengan jumlah variabel berbeda. [web:79][web:81]

# ------------------------------------------
# 3.2 CEK ASUMSI DASAR MODEL (RESIDUAL)
# ------------------------------------------
# Ide dari R4DS: lihat residual vs fitted dan QQ-plot untuk memeriksa
# linearitas dan normalitas residual. [web:79]

# Tambah kolom fitted dan residual ke data.
data_resid <- data_sovi_clean |>
  mutate(
    fitted_lengkap = fitted(model_lengkap),
    resid_lengkap  = resid(model_lengkap)
  )

# Plot residual vs fitted
ggplot(data_resid,
       aes(x = fitted_lengkap, y = resid_lengkap)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(
    title = "Residual vs Fitted (model_lengkap)",
    x     = "Nilai fitted (diprediksi)",
    y     = "Residual"
  ) +
  theme_minimal()
# Alasan:
# - Jika pola residual acak di sekitar 0, asumsi linearitas/homoskedastis
#   lebih masuk akal; kalau ada pola lengkung/kipas, model bisa kurang tepat. [web:79]

# QQ-plot residual (uji normalitas visual sederhana)
qqnorm(data_resid$resid_lengkap)
qqline(data_resid$resid_lengkap, col = "red")
# Alasan:
# - Titik dekat garis lurus → residual mendekati normal, sesuai asumsi lm. [web:79]

# ------------------------------------------
# 3.3 PREDIKSI DAN INTERPRETASI SEDERHANA
# ------------------------------------------
# Contoh: buat data “hipotetik” untuk melihat pengaruh LOWEDU & NOELECTRIC
# sambil menahan variabel lain di sekitar rata-rata. [web:79][web:81]

# Hitung nilai rata-rata beberapa variabel lain.
mean_children   <- mean(data_sovi_clean$CHILDREN,   na.rm = TRUE)
mean_elderly    <- mean(data_sovi_clean$ELDERLY,    na.rm = TRUE)
mean_fhead      <- mean(data_sovi_clean$FHEAD,      na.rm = TRUE)
mean_familysize <- mean(data_sovi_clean$FAMILYSIZE, na.rm = TRUE)
mean_notrain    <- mean(data_sovi_clean$NOTRAINING, na.rm = TRUE)
mean_rented     <- mean(data_sovi_clean$RENTED,     na.rm = TRUE)
mean_nosewer    <- mean(data_sovi_clean$NOSEWER,    na.rm = TRUE)
mean_tapwater   <- mean(data_sovi_clean$TAPWATER,   na.rm = TRUE)
mean_illiterate <- mean(data_sovi_clean$ILLITERATE, na.rm = TRUE)

# Buat skenario: kombinasi LOWEDU dan NOELECTRIC rendah/tinggi.
newdata_skenario <- tibble(
  LOWEDU     = c(quantile(data_sovi_clean$LOWEDU, 0.25, na.rm = TRUE),
                 quantile(data_sovi_clean$LOWEDU, 0.75, na.rm = TRUE)),
  NOELECTRIC = c(quantile(data_sovi_clean$NOELECTRIC, 0.25, na.rm = TRUE),
                 quantile(data_sovi_clean$NOELECTRIC, 0.75, na.rm = TRUE)),
  ILLITERATE = mean_illiterate,
  CHILDREN   = mean_children,
  ELDERLY    = mean_elderly,
  FHEAD      = mean_fhead,
  FAMILYSIZE = mean_familysize,
  NOTRAINING = mean_notrain,
  RENTED     = mean_rented,
  NOSEWER    = mean_nosewer,
  TAPWATER   = mean_tapwater
)

pred_skenario <- newdata_skenario |>
  mutate(
    pred_poverty = predict(model_lengkap, newdata = newdata_skenario)
  )

pred_skenario
# Alasan:
# - Pendekatan ini mengikuti ide “what-if” di R4DS: melihat bagaimana nilai
#   POVERTY yang diprediksi berubah ketika beberapa variabel kunci dinaikkan,
#   sementara yang lain ditahan di rata-ratanya. [web:79][web:81]

# ------------------------------------------
# 3.4 MODEL ALTERNATIF SEDERHANA (INTERAKSI)
# ------------------------------------------
# Contoh: tambahkan interaksi LOWEDU:NOELECTRIC untuk melihat apakah
# pengaruh pendidikan terhadap kemiskinan berbeda pada daerah dengan
# akses listrik yang berbeda. [web:79][web:80]

model_interaksi <- lm(
  POVERTY ~ LOWEDU * NOELECTRIC + ILLITERATE,
  data = data_sovi_clean
)

summary(model_interaksi)

# Visualisasi garis regresi berbeda untuk NOELECTRIC rendah vs tinggi.
# Bagi dua: kuartil bawah dan atas NOELECTRIC.
data_interaksi_plot <- data_sovi_clean |>
  mutate(
    grup_listrik = if_else(
      NOELECTRIC <= quantile(NOELECTRIC, 0.5, na.rm = TRUE),
      "Listrik rendah",
      "Listrik tinggi"
    )
  )

ggplot(data_interaksi_plot,
       aes(x = LOWEDU, y = POVERTY, color = grup_listrik)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaksi LOWEDU dan NOELECTRIC terhadap POVERTY",
    x     = "Persentase berpendidikan rendah (LOWEDU)",
    y     = "Persentase kemiskinan (POVERTY)",
    color = "Kelompok NOELECTRIC"
  ) +
  theme_minimal()
# Alasan:
# - Tanda * di formula lm menambahkan LOWEDU, NOELECTRIC, dan interaksi
#   LOWEDU:NOELECTRIC. Ini mengikuti contoh interaksi dalam bab model building
#   di R4DS. [web:79][web:80]
