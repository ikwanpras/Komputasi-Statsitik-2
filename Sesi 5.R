# VIF = Variance Inflation Factor, nilai >5 menunjukkan multikolinearitas kuat.
install.packages("car")
library(car)

vif(model_lengkap)
# Alasan: Jika VIF tinggi, koefisien model jadi tidak stabil—penting untuk deteksi masalah korelasi antar prediktor. [web:110][web:115]
# Tambahkan kuadrat LOWEDU sebagai prediktor non-linear
data_sovi_clean <- data_sovi_clean |>
  mutate(LOWEDU2 = LOWEDU^2)

model_quad <- lm(POVERTY ~ LOWEDU + LOWEDU2 + ILLITERATE, data = data_sovi_clean)
summary(model_quad)

# Visualisasi hubungan non-linear
library(ggplot2)
ggplot(data_sovi_clean, aes(x = LOWEDU, y = POVERTY)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "red") +
  labs(title = "Quadratic Regression: POVERTY ~ LOWEDU + LOWEDU²")
# Alasan: Model kuadratik membantu jika hubungan tidak linier sempurna (misal efek LOWEDU pada POVERTY menguat/menurun pada ambang tertentu). [web:111][web:116]
