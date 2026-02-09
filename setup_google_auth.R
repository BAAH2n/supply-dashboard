#!/usr/bin/env Rscript
# ===============================================
# GOOGLE SHEETS AUTHENTICATION SETUP
# ===============================================
# Цей скрипт виконує інтерактивну авторизацію Google Sheets
# Запустіть його ОДИН РАЗ перед першим використанням Dashboard_generator.qmd
#
# Використання:
#   Rscript setup_google_auth.R
#   або у R консолі:
#   source("setup_google_auth.R")
# ===============================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║  Google Sheets Authentication Setup                     ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

# Перевірка наявності пакету
if (!require("googlesheets4", quietly = TRUE)) {
  cat("📥 Встановлення пакету googlesheets4...\n")
  install.packages("googlesheets4")
  library(googlesheets4)
}

cat("🔐 Запуск авторизації Google Sheets...\n\n")
cat("Зараз відкриється браузер для авторизації:\n")
cat("  1. Оберіть ваш Google акаунт\n")
cat("  2. Дозвольте доступ до Google Sheets\n")
cat("  3. Токени збережуться в папці .secrets/\n\n")

# Створити папку .secrets якщо не існує
if (!dir.exists(".secrets")) {
  dir.create(".secrets")
  cat("✅ Створено папку .secrets/\n")
}

# Інтерактивна авторизація
tryCatch({
  gs4_auth(
    email = TRUE,
    cache = ".secrets"
  )

  cat("\n")
  cat("╔══════════════════════════════════════════════════════════╗\n")
  cat("║  ✅ АВТОРИЗАЦІЯ УСПІШНА!                                ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")

  cat("Токени збережено в .secrets/\n")
  cat("Тепер ви можете запускати:\n")
  cat("  quarto render Dashboard_generator.qmd\n\n")

  # Перевірка токену
  if (gs4_has_token()) {
    cat("✓ Токен перевірено та працює!\n")
  }

}, error = function(e) {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════╗\n")
  cat("║  ❌ ПОМИЛКА АВТОРИЗАЦІЇ                                 ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n\n")
  cat("Повідомлення помилки:\n")
  cat(paste("  ", e$message, "\n\n"))
  cat("Можливі причини:\n")
  cat("  • Відмінено авторизацію в браузері\n")
  cat("  • Проблеми з інтернет-з'єднанням\n")
  cat("  • Браузер за замовчуванням не налаштований\n\n")
  cat("Спробуйте ще раз:\n")
  cat("  Rscript setup_google_auth.R\n\n")
})
