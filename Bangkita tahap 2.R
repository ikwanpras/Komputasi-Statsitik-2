# ============================================================
# LANJUTAN DATA BANGKITAN – WRANGLING LANJUT & MODEL LANJUT
# (jalankan setelah data_sim_clean & model_sim sudah dibuat)
# ============================================================

# ------------------------------------------------------------
# 5. MENAMBAH KATEGORI KEMISKINAN & WRANGLING LANJUT
# ------------------------------------------------------------

# 5.1. Buat kategori POVERTY (Rendah / Sedang / Tinggi)
#      Menggunakan kuantil, seperti di contoh kita pada data asli.
# Menghitung nilai ambang di mana:
# - 0.33 → sekitar 33% data ada di bawah nilai itu (batas bawah).
# - 0.66 → sekitar 66% data ada di bawah nilai itu (batas tengah).
# - Jadi data dibagi kira‑kira menjadi tiga bagian sama besar: 33% terendah, 33% tengah, 33% tertinggi.
# Kalau ada NA di POVERTY, diabaikan saat menghitung kuantil.
quant_pov <- quantile(data_sim_clean$POVERTY,
                      probs = c(0.33, 0.66),
                      na.rm = TRUE)

batas1 <- quant_pov[1]
batas2 <- quant_pov[2]
# quant_pov[1] = kuantil 0.33 (batas antara Rendah dan Sedang).
# quant_pov[2] = kuantil 0.66 (batas antara Sedang dan Tinggi).

data_sim_cat <- data_sim_clean |>
  mutate(
    KAT_POV = case_when(
      POVERTY <= batas1                    ~ "Rendah",
      POVERTY > batas1 & POVERTY <= batas2 ~ "Sedang",
      POVERTY > batas2                     ~ "Tinggi"
    )
  )

count(data_sim_cat, KAT_POV)
# Menghitung berapa kabupaten di tiap kategori kemiskinan.
# Konsepnya sama dengan bab “data tidying + transform”: numeric → kategori dengan mutate + case_when. [web:131][web:98]

# 5.2. Bentuk long dengan pivot_longer (indikator → baris)
data_long <- data_sim_cat |>
  select(KAB, KAT_POV, POVERTY, LOWEDU, NOELECTRIC, GROWTH) |>  # select(): memilih hanya kolom yang akan dipakai.
  pivot_longer(
    cols      = c(POVERTY, LOWEDU, NOELECTRIC, GROWTH),
    names_to  = "indikator",
    values_to = "nilai"
  )

glimpse(data_long)
# Melihat struktur data long (cek bahwa sudah ada kolom indikator dan nilai).
# Mirip bab “Data tidying”: pivot_longer mengubah banyak kolom indikator menjadi 1 kolom nama + 1 kolom nilai. [web:98][web:84]

# 5.3. Boxplot per indikator (pakai data long)
# Membuat boxplot distribusi nilai untuk setiap indikator.
ggplot(data_long,
       aes(x = indikator, y = nilai)) +
  geom_boxplot() +
  labs(
    title = "Sebaran nilai indikator (data bangkitan)",
    x     = "Indikator",
    y     = "Nilai"
  ) +
  theme_minimal()

# 5.4. Ringkasan rata-rata indikator per kategori kemiskinan
rata_per_kat <- data_long |>
  group_by(KAT_POV, indikator) |>
  summarise(
    mean_nilai = mean(nilai, na.rm = TRUE),
    .groups = "drop"
  )

rata_per_kat

# Ubah kembali ke wide supaya mudah dibaca sebagai “tabel laporan”
rata_per_kat_wide <- rata_per_kat |>
  pivot_wider(
    names_from  = indikator,
    values_from = mean_nilai
  )

rata_per_kat_wide
# siklus long → ringkasan → wide yang ditekankan di R4DS. [web:98][web:84]


# ------------------------------------------------------------
# 6. MODEL LANJUT: VIF & REGRESI KUADRATIK
# ------------------------------------------------------------

# 6.1. VIF (multikolinearitas) untuk model_sim
install.packages("car")
library(car)

vif(model_sim)
# VIF dipakai luas untuk mendeteksi multikolinearitas di regresi,
# seperti dibahas di banyak referensi statistik lanjutan. [web:110][web:115]

# 6.2. Tambah komponen kuadrat LOWEDU (hubungan non-linear)
data_sim_quad <- data_sim_clean |>
  mutate(LOWEDU2 = LOWEDU^2)

model_quad <- lm(
  POVERTY ~ LOWEDU + LOWEDU2 + NOELECTRIC + GROWTH,
  data = data_sim_quad
)

summary(model_quad)
# Ini contoh “model yang lebih kaya” seperti di bagian model building:
# menambah term non-linear dan membandingkan dengan model linear awal. [web:79][web:111]

# 6.3. Visualisasi garis kuadratik
ggplot(data_sim_quad,
       aes(x = LOWEDU, y = POVERTY)) +
  geom_point(alpha = 0.8) +
  stat_smooth(
    method  = "lm",
    formula = y ~ x + I(x^2),   # rumus kuadratik
    se      = FALSE,
    color   = "red"
  ) +
  labs(
    title = "Regresi kuadratik: POVERTY ~ LOWEDU + LOWEDU²",
    x     = "Persentase pendidikan rendah (LOWEDU)",
    y     = "Persentase kemiskinan (POVERTY)"
  ) +
  theme_minimal()

