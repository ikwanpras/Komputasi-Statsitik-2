# ==========================================
# SESI 2 – MANIPULASI DATA LANJUTAN
# ==========================================
# Asumsi: paket tidyverse & readxl sudah di-load
# dan file "sovi_data_Kab.xlsx" ada di folder kerja.

# ------------------------------------------
# 2.0 Baca kedua sheet: Data_Sovi dan Data
# ------------------------------------------
data_sovi <- read_excel("sovi_data_Kab.xlsx", sheet = "Data_Sovi")
data_info <- read_excel("sovi_data_Kab.xlsx", sheet = "Data")

# Alasan:
# - Kita pisahkan dua sheet menjadi dua objek:
#   * data_sovi: berisi indikator SOVI per kabupaten.
#   * data_info: berisi info tambahan (misal nama kabupaten, kode, dll.). [attached_file:83]

glimpse(data_sovi)
glimpse(data_info)

# Misal di data_info ada kolom DISTRICTCODE dan DISTRICT yang juga ada di data_sovi.
# (Kalau nama kolom sedikit berbeda, nanti bisa disesuaikan.)

# ------------------------------------------
# 2A. JOIN antar sheet (RELATIONAL DATA)
# ------------------------------------------
# Tujuan: menggabungkan indikator SOVI dengan info tambahan kabupaten,
# menggunakan kunci bersama (misal DISTRICTCODE). [web:89][web:92]

# Contoh: pakai left_join supaya semua baris di data_sovi tetap ada.
data_gabung <- data_sovi |>
  left_join(
    data_info,
    by = "DISTRICTCODE"   # ganti jika kunci join lain (misal "DISTRICT")
  )

glimpse(data_gabung)

# Alasan:
# - left_join(): dipilih karena data utama ada di data_sovi (indikator),
#   dan kita hanya menambahkan kolom dari data_info tanpa menghilangkan kabupaten. [web:89][web:91]
# - by = "DISTRICTCODE": memastikan penggabungan berdasarkan kode kabupaten,
#   bukan urutan baris.

# ------------------------------------------
# 2B. PIVOT LONGER – ubah beberapa indikator jadi format panjang
# ------------------------------------------
# Tujuan: mengubah beberapa kolom indikator menjadi baris (long format),
# sehingga lebih fleksibel untuk grafik dan ringkasan. [web:84][web:98]

# Pilih beberapa indikator penting
colnames(data_gabung)
data_pivot_long <- data_gabung |>
  select(
    DISTRICTCODE,
    POVERTY,
    LOWEDU,
    NOELECTRIC,
    GROWTH
  ) |>
  pivot_longer(
    cols = c(POVERTY, LOWEDU, NOELECTRIC, GROWTH),
    names_to  = "indikator",
    values_to = "nilai"
  )

data_pivot_long <- data_gabung |>
  select(
    DISTRICTCODE,
    POVERTY,
    LOWEDU,
    NOELECTRIC,
    GROWTH
  ) |>
  pivot_longer(
    cols = c(POVERTY, LOWEDU, NOELECTRIC, GROWTH),
    names_to  = "indikator",
    values_to = "nilai"
  )

glimpse(data_pivot_long)

# Alasan:
# - pivot_longer(): mengubah kolom-kolom indikator menjadi dua kolom:
#   * indikator: nama variabel (POVERTY, LOWEDU, dll.)
#   * nilai    : angka masing-masing indikator. [web:84][web:94]
# - Format long ini sangat cocok untuk:
#   * facet per indikator.
#   * ringkasan rata-rata per indikator.

# Contoh visualisasi pakai data long: boxplot per indikator
ggplot(data_pivot_long,
       aes(x = indikator, y = nilai)) +
  geom_boxplot() +
  labs(
    title = "Sebaran nilai beberapa indikator per kabupaten",
    x     = "Indikator",
    y     = "Nilai"
  ) +
  theme_minimal()
# Alasan:
# - Boxplot di format long memungkinkan membandingkan distribusi
#   POVERTY vs LOWEDU vs NOELECTRIC vs GROWTH dalam satu gambar. [web:21][web:88]

# ------------------------------------------
# 2C. PIVOT WIDER – kembali ke format lebar dari ringkasan
# ------------------------------------------
# Tujuan: setelah data long, kita bisa buat ringkasan lalu kembalikan lagi ke wide. [web:84][web:93]

# Ringkasan rata-rata tiap indikator
rata_indikator <- data_pivot_long |>
  group_by(indikator) |>
  summarise(
    mean_nilai = mean(nilai, na.rm = TRUE)
  )

rata_indikator

# Misal kita punya ringkasan per kabupaten & indikator,
# lalu ingin kembali ke bentuk lebar (satu baris per kabupaten).
data_rata_kab <- data_pivot_long |>
  group_by(DISTRICTCODE, indikator) |>
  summarise(
    mean_nilai = mean(nilai, na.rm = TRUE),
    .groups = "drop"
  )

# Kembalikan ke wide: kolom per indikator
data_rata_kab_wide <- data_rata_kab |>
  pivot_wider(
    names_from  = indikator,
    values_from = mean_nilai
  )

glimpse(data_rata_kab_wide)

# Alasan:
# - pivot_wider(): kebalikan dari pivot_longer, dipakai ketika kita ingin kembali
#   ke bentuk “satu baris per kabupaten” setelah menghitung ringkasan. [web:84][web:99]
# - Contoh berguna ketika ingin menyimpan ringkasan ke Excel atau lanjut modelling.

# ------------------------------------------
# 2D. Contoh kecil: filter & arrange untuk fokus kabupaten tertentu
# ------------------------------------------
# Misal kita ingin melihat 10 kabupaten dengan POVERTY tertinggi.
top10_poverty <- data_gabung |>
  arrange(desc(POVERTY)) |>
  slice_head(n = 10) |>
  select(DISTRICTCODE, POVERTY, LOWEDU, NOELECTRIC, GROWTH)

top10_poverty

# Alasan:
# - arrange(desc(POVERTY)): mengurutkan dari kemiskinan tertinggi.
# - slice_head(n = 10): mengambil 10 teratas.
# - select(): hanya menampilkan kolom yang relevan untuk pelaporan. [web:21][web:58]
