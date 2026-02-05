library(tidyverse)
library(patchwork)
library(stringr)
library(grid)

# ==============================================================================
# 1. ТЕМА І НАЛАШТУВАННЯ
# ==============================================================================
bg_dark <- "#2c3e50" 

theme_card <- function(border_col = "#bdc3c7") {
  theme_void(base_family = "sans") + 
    theme(
      # Відступи: 10px між картками
      plot.margin = margin(10, 10, 10, 10),
      # Картка
      plot.background = element_rect(fill = "white", color = border_col, linewidth = 1.5),
      panel.background = element_rect(fill = "white", color = NA),
      # Заголовок
      plot.title = element_text(face = "bold", size = 9, color = "#2c3e50", hjust = 0.5, margin = margin(b = 10))
    )
}

draw_header_text <- function(label) {
  ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = str_to_upper(label), 
             fontface = "bold", size = 6, color = "white") + 
    theme_void() +
    theme(
      plot.background = element_rect(fill = bg_dark, color = NA),
      plot.margin = margin(15, 0, 5, 0)
    )
}

# ==============================================================================
# 2. ФУНКЦІЇ МАЛЮВАННЯ (ПОВЕРНУВ СТАРІ СТРІЛКИ + ВИПРАВИВ ТЕКСТ)
# ==============================================================================

draw_kpi_card <- function(data, metric_name, suffix = "", invert = FALSE, is_headcount = FALSE) {
  
  subset <- data %>% filter(metric_name == !!metric_name) %>% arrange(date)
  if(nrow(subset) == 0) return(ggplot() + theme_void())
  
  curr_row <- tail(subset, 1); prev_row <- tail(subset, 2) %>% head(1)
  val <- curr_row$value_num; target <- curr_row$target_num
  prev_val <- if(nrow(subset) > 1) prev_row$value_num else NA
  
  if (is_headcount) {
    status_color <- "#2c3e50"; border_col <- "#bdc3c7"; fill_col <- "#ecf0f1"
  } else {
    if (is.na(val) || is.na(target)) { status_color <- "grey60"; border_col <- "#bdc3c7" } 
    else {
      is_good <- if(invert) val <= target else val >= target
      status_color <- if(is_good) "#27ae60" else "#e74c3c"
      border_col <- if(is_good) "#2ecc71" else "#e74c3c"
    }
    fill_col <- "white"
  }
  
  delta_text <- ""
  delta_col <- "grey60"
  if(!is.na(prev_val) && !is.na(val)) {
    diff <- val - prev_val
    if (is_headcount) {
      sign <- if(diff > 0) "+" else ""; delta_text <- paste0(sign, round(diff, 0), " vs prev")
    } else {
      # === ПОВЕРНУВ СТАРІ СИМВОЛИ СТРІЛОК ===
      arrow <- if(diff > 0) "▲" else if(diff < 0) "▼" else " "
      is_good_change <- if(invert) diff <= 0 else diff >= 0
      delta_col <- if(is_good_change) "#27ae60" else "#e74c3c"
      delta_text <- paste0(arrow, " ", if(diff>0) "+" else "", round(diff, 2), suffix)
    }
  }
  
  clean_title <- str_to_upper(str_replace_all(metric_name, "%", "")) %>% str_wrap(width = 20)
  display_val <- if(is.na(val)) "--" else paste0(val, suffix)
  
  ggplot() +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1)) +
    
    # 1. ЗАГОЛОВОК (Підняв на самий верх y=0.98)
    annotate("text", x=0.5, y=0.98, label=clean_title, size=2.8, fontface="bold", color="grey40", lineheight=0.9, vjust=1) +
    
    # 2. ЗНАЧЕННЯ (Опустив нижче y=0.45 і зменшив шрифт до 7)
    annotate("text", x=0.5, y=0.45, label=display_val, size=7, fontface="bold", color=status_color) +
    
    # 3. ДЕЛЬТА (В самому низу y=0.15)
    annotate("text", x=0.5, y=0.15, label=delta_text, size=3.5, fontface="bold", color=delta_col) +
    
    {if(!is.na(target) && !is_headcount) annotate("text", x=0.05, y=0.05, label=paste0("Tgt: ", target), size=2.2, color="grey60", hjust=0)} +
    
    theme_card(border_col = border_col) +
    theme(plot.background = element_rect(fill = fill_col, color = border_col, linewidth = 1.5))
}

draw_alert_card <- function(data, metric_name) {
  subset <- data %>% filter(metric_name == !!metric_name) %>% arrange(date) %>% tail(1)
  if(nrow(subset) == 0) return(ggplot() + theme_void())
  raw_text <- subset$value_raw; if(is.na(raw_text)) raw_text <- "No Data"
  
  # Проста логіка іконки текстом, щоб не ламалось
  if (str_detect(raw_text, "No incidents")) { 
    card_color <- "#27ae60"; icon <- "OK" 
  } else { 
    card_color <- "#c0392b"; icon <- "!" 
  }
  
  ggplot() +
    coord_cartesian(clip = "off") +
    scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1)) +
    annotate("text", x=0.5, y=0.9, label="NDA & AI SECURITY", size=2.5, fontface="bold", color=card_color) +
    annotate("text", x=0.5, y=0.6, label=icon, size=14, fontface="bold", color=card_color) + 
    annotate("text", x=0.5, y=0.35, label=ifelse(icon=="OK","CLEAN","ALERT"), size=5, fontface="bold", color=card_color) +
    annotate("text", x=0.5, y=0.15, label=raw_text, size=2.5, color="grey30") +
    theme_card(border_col = card_color)
}

draw_trend_chart <- function(data, metric_name, suffix = "", color_line = "#3498db") {
  plot_data <- data %>% filter(metric_name == !!metric_name) %>% arrange(date)
  if(nrow(plot_data) == 0) return(ggplot() + theme_void())
  target_val <- tail(plot_data$target_num, 1)
  clean_title <- str_to_upper(metric_name) %>% str_wrap(width = 25)
  
  p <- ggplot(plot_data, aes(x = date, y = value_num, group = 1)) +
    geom_line(color = color_line, linewidth = 1.1, alpha = 0.9) +
    geom_point(color = color_line, size = 2.5) + geom_point(color = "white", size = 1.2) +
    geom_text(aes(label = paste0(value_num, suffix)), vjust = -0.8, fontface = "bold", size = 3, color = "#2c3e50") +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.4))) +
    labs(title = clean_title) +
    theme_card(border_col = color_line) +
    theme(
      axis.text.x = element_text(size = 8, color = "grey50", face = "bold", margin = margin(t=5)),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  if (!is.na(target_val)) {
    p <- p + 
      geom_hline(yintercept = target_val, linetype = "dashed", color = "grey60", linewidth = 0.5) +
      annotate("text", x = 0.6, y = target_val, label = paste("Tgt:", target_val), 
               vjust = -0.5, hjust = 0, color = "grey60", size = 2.5, fontface = "italic")
  }
  return(p)
}

# ==============================================================================
# 3. КОНФІГУРАЦІЯ
# ==============================================================================
config <- list(
  header = c("Availability Average", "Availability Lowest", "%Completion", "PU Retention"),
  level_2 = c("Utilization", "%Force busy usage", "%Rejects"),
  l3_q    = c("CSAT overall", "CSAT trial", "CR Free to Paid"),
  l3_ops  = c("% Positive reviews", "FRT", "Pings coverage", "Messenger answer rate", "Engagement messages coverage")
)

get_params <- function(m) {
  list(
    suffix = if(str_detect(m, "%|Rate|Retention|Availability|Conversion|Utilization|Force|coverage|reviews|Messenger")) "%" else if(str_detect(m, "FRT")) "m" else "",
    invert = str_detect(m, "FRT|Rejects|Abuse|Force"),
    color  = case_when(str_detect(m, "CSAT") ~ "#27ae60", str_detect(m, "Utilization|Force") ~ "#8e44ad", str_detect(m, "CR|Retention") ~ "#d35400", str_detect(m, "FRT|Rejects|Abuse") ~ "#c0392b", TRUE ~ "#2980b9")
  )
}

# ==============================================================================
# 4. ГЕНЕРАЦІЯ
# ==============================================================================

h1 <- {p <- get_params(config$header[1]); draw_kpi_card(long_data, config$header[1], p$suffix, p$invert)}
h2 <- {p <- get_params(config$header[2]); draw_kpi_card(long_data, config$header[2], p$suffix, p$invert)}
h3 <- {p <- get_params(config$header[3]); draw_kpi_card(long_data, config$header[3], p$suffix, p$invert)}
h4 <- {p <- get_params(config$header[4]); draw_kpi_card(long_data, config$header[4], p$suffix, p$invert)}

l2_1 <- {m <- config$level_2[1]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
l2_2 <- {m <- config$level_2[2]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
l2_3 <- {m <- config$level_2[3]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}

l3q_1 <- {m <- config$l3_q[1]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
l3q_2 <- {m <- config$l3_q[2]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
l3q_3 <- {m <- config$l3_q[3]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}

l3o_1 <- {m <- config$l3_ops[1]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
l3o_2 <- {m <- config$l3_ops[2]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
l3o_3 <- {m <- config$l3_ops[3]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
l3o_4 <- {m <- config$l3_ops[4]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
l3o_5 <- {m <- config$l3_ops[5]; p <- get_params(m); draw_trend_chart(long_data, m, p$suffix, p$color)}
nda   <- draw_alert_card(long_data, "NDA & AI usage abuse")

hc <- draw_kpi_card(long_data, "Headcount", "", is_headcount = TRUE)

t1 <- draw_header_text("KEY METRICS SUMMARY")
t2 <- draw_header_text("OPERATIONAL EFFICIENCY")
t3 <- draw_header_text("QUALITY & BUSINESS IMPACT")
t4 <- draw_header_text("OPERATIONAL INDICATORS & RISK")

# ==============================================================================
# 5. СКЛЕЙКА (РУЧНА, НАДІЙНА)
# ==============================================================================

# Тут ми використовуємо оператори | (в ряд) і / (в стовпчик)
# Це гарантує правильну сітку без помилок wrap_dims

# Ряд карток (4)
row_cards <- (h1 | h2 | h3 | h4)

# Ряд ефективності (3)
row_eff <- (l2_1 | l2_2 | l2_3)

# Ряд якості (3)
row_qual <- (l3q_1 | l3q_2 | l3q_3)

# Ряд операційки (6 штук -> розбиваємо на 2 по 3)
row_ops_top <- (l3o_1 | l3o_2 | l3o_3)
row_ops_btm <- (l3o_4 | l3o_5 | nda)

# Фінальна конструкція
final_dashboard <- 
  t1 / row_cards / 
  t2 / row_eff / 
  t3 / row_qual / 
  t4 / row_ops_top / row_ops_btm / 
  hc +
  
  # Висоти налаштовані так, щоб текст не налізав
  plot_layout(heights = c(0.2, 0.8, 0.2, 1, 0.2, 1, 0.2, 1, 1, 0.4)) +
  
  plot_annotation(
    title = "Supply Performance Dashboard",
    subtitle = paste("Generated:", Sys.Date()),
    theme = theme(
      plot.background = element_rect(fill = bg_dark, color = NA),
      plot.margin = margin(20, 20, 20, 20),
      plot.title = element_text(size = 24, face = "bold", color = "white", margin = margin(10,0,10,20)),
      plot.subtitle = element_text(size = 14, color = "grey70", margin = margin(0,0,10,20))
    )
  )


final_dashboard