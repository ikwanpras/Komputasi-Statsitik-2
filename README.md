                                            KOMPUTASI STATISTIK 2

project-kabupaten/
├── Data.xlsx          # File data Anda
├── analyze_data.R     # Script analisis R
├── README.md          # Awal kosong atau template
├── images/            # Folder untuk plot otomatis
└── .github/
    └── workflows/
        └── update-data.yml  # GitHub Actions
--------------------------------------------------------------------

# Load libraries
library(openxlsx)
library(ggplot2)
library(dplyr)
library(lubridate)

# Baca data kabupaten
data <- read.xlsx("Data.xlsx", sheet = "Data")

# Analisis sederhana (sesuaikan kolom Anda)
data$Date <- as.Date(data$Date)  # Asumsi ada kolom Date
latest_data <- data %>%
  group_by(Kabupaten) %>%
  summarise(
    Nilai_Rata = mean(Nilai, na.rm=TRUE),
    Pertumbuhan = round((tail(Nilai,1) - head(Nilai,1))/head(Nilai,1)*100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(Nilai_Rata))

# Buat plot
p1 <- ggplot(latest_data, aes(reorder(Kabupaten, Nilai_Rata), Nilai_Rata, fill = Pertumbuhan > 0)) +
  geom_col() + coord_flip() + theme_minimal() +
  labs(title = paste("Data Kabupaten - Update:", Sys.Date()),
       x = "Kabupaten", y = "Nilai Rata-rata")

ggsave("images/plot_kabupaten.png", p1, width=10, height=6, dpi=150)


