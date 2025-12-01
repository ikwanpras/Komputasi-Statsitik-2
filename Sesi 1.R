# ==========================================
# SESI 1 – EKSPLORASI & VISUALISASI LANJUTAN
# ==========================================
# Catatan: bagian ini mengasumsikan data_sovi_clean dan KATEGORI_POV
# sudah dibuat seperti di script sebelumnya.

# ------------------------------------------
# 1. Histogram & density POVERTY
#    (melihat distribusi dan efek transformasi skala)
# ------------------------------------------

# Histogram POVERTY per kategori (facetting)
ggplot(data_sovi_clean,
       aes(x = POVERTY, fill = KATEGORI_POV)) +
  geom_histogram(alpha = 0.7, bins = 20, color = "white") +
  facet_wrap(~ KATEGORI_POV, scales = "free_y") +
  labs(
    title = "Distribusi POVERTY per kategori",
    x     = "Persentase kemiskinan (POVERTY)",
    y     = "Jumlah kabupaten",
    fill  = "Kategori POVERTY"
  ) +
  theme_minimal()
# Alasan:
# - geom_histogram: untuk melihat distribusi nilai POVERTY.
# - facet_wrap: memisahkan histogram per kategori kemiskinan agar perbandingan lebih jelas.
# - scales = "free_y": tinggi sumbu Y bebas per panel, berguna jika jumlah kabupaten
#   per kategori tidak sama. [web:21][web:38]

# Density plot POVERTY (tanpa facet) untuk melihat bentuk distribusi keseluruhan
ggplot(data_sovi_clean,
       aes(x = POVERTY)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(
    title = "Kepadatan distribusi POVERTY",
    x     = "Persentase kemiskinan (POVERTY)",
    y     = "Kepadatan"
  ) +
  theme_minimal()
# Alasan:
# - geom_density: alternatif histogram yang halus, sering dipakai di eksplorasi awal. [web:38]

# ------------------------------------------
# 2. Scatter plot dengan facet dan transformasi skala
# ------------------------------------------

# LOWEDU vs POVERTY difacet per kategori
ggplot(data_sovi_clean,
       aes(x = LOWEDU, y = POVERTY, color = KATEGORI_POV)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ KATEGORI_POV) +
  labs(
    title = "LOWEDU vs POVERTY dengan facet kategori kemiskinan",
    x     = "Persentase berpendidikan rendah (LOWEDU)",
    y     = "Persentase kemiskinan (POVERTY)",
    color = "Kategori POVERTY"
  ) +
  theme_minimal()
# Alasan:
# - facet_wrap(~KATEGORI_POV): membagi scatter plot per kelompok kemiskinan,
#   sehingga pola di tiap kelompok bisa dibandingkan. [web:21][web:38]

# NOELECTRIC vs POVERTY dengan transformasi skala (log10 pada X jika perlu)
ggplot(data_sovi_clean,
       aes(x = NOELECTRIC, y = POVERTY)) +
  geom_point(alpha = 0.8, color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_continuous(trans = "log10") +
  labs(
    title = "NOELECTRIC (log10) vs POVERTY",
    x     = "Persentase rumah tanpa listrik (NOELECTRIC, skala log10)",
    y     = "Persentase kemiskinan (POVERTY)"
  ) +
  theme_minimal()
# Alasan:
# - scale_x_continuous(trans = "log10"): dipakai ketika distribusi NOELECTRIC
#   sangat miring (skewed). Skala log bisa membuat pola linear lebih terlihat. [web:21][web:38]

# ------------------------------------------
# 3. Plot garis GROWTH vs POVERTY yang diurutkan
# ------------------------------------------

# Urutkan kabupaten berdasarkan POVERTY, lalu plot GROWTH dan POVERTY
data_growth <- data_sovi_clean |>
  arrange(POVERTY) |>
  mutate(
    urut = row_number()
  )
# Alasan:
# - arrange(): mengurutkan dari POVERTY terendah ke tertinggi.
# - row_number(): membuat indeks urutan untuk sumbu X pada plot garis. [web:21][web:58]

# Plot garis POVERTY
ggplot(data_growth,
       aes(x = urut, y = POVERTY)) +
  geom_line(color = "steelblue") +
  labs(
    title = "POVERTY diurutkan per kabupaten",
    x     = "Kabupaten (diurut berdasarkan POVERTY)",
    y     = "Persentase kemiskinan (POVERTY)"
  ) +
  theme_minimal()

# Plot garis GROWTH (misal pertumbuhan ekonomi) pada urutan yang sama
ggplot(data_growth,
       aes(x = urut, y = GROWTH)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "GROWTH diurutkan mengikuti POVERTY",
    x     = "Kabupaten (urutan POVERTY)",
    y     = "GROWTH"
  ) +
  theme_minimal()
# Alasan:
# - Dengan mengurutkan dan melihat dua garis ini, kamu bisa secara visual
#   mempertimbangkan apakah ada pola antara pertumbuhan dan kemiskinan. [web:21][web:38]

# ------------------------------------------
# 4. Ringkasan numerik tambahan (korelasi)
# ------------------------------------------
# Korelasi antara POVERTY dan tiga faktor utama
cor_matrix_sesi1 <- data_sovi_clean |>
  select(POVERTY, LOWEDU, NOELECTRIC, GROWTH) |>
  cor(use = "complete.obs")

cor_matrix_sesi1
# Alasan:
# - cor(): memberi gambaran awal kekuatan hubungan linear, untuk melengkapi
#   informasi dari grafik. [web:41][web:49]

