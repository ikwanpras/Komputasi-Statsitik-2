# Install & load package writexl jika belum ada
install.packages("writexl")
library(writexl)

# Simpan data hasil analisis ke file Excel baru
write_xlsx(data_sovi_clean, "hasil_data_sovi_clean.xlsx")

# Alasan: write_xlsx() nyaman dan cepat untuk ekspor ke format .xlsx, 
# cocok untuk backup atau lanjut analisa di Excel. [web:104][web:107]
