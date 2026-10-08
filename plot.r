# --- 1. LOAD DATA -------------------------------------------
df <- read.csv("[FILE_PATH.csv]")
# df <- read.delim("[FILE_PATH.tsv]", header = TRUE)
# df <- fromJSON("[FILE_PATH.json]", simplifyVector = TRUE)

# --- 2. DEFINE VARIABLES ------------------------------------
x_var     <- "[x_column_name]"
y_var     <- "[y_column_name]"
z_threshold <- 3

# --- 3. OUTLIER FLAGGING ------------------------------------
df$z_x <- (df[[x_var]] - mean(df[[x_var]], na.rm = TRUE)) / sd(df[[x_var]], na.rm = TRUE)
df$z_y <- (df[[y_var]] - mean(df[[y_var]], na.rm = TRUE)) / sd(df[[y_var]], na.rm = TRUE)
df$is_outlier <- (abs(df$z_x) > z_threshold) | (abs(df$z_y) > z_threshold)

# --- 4. BUILD PLOT ------------------------------------------
p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
  geom_point(
    alpha = 0.4,
    size  = 1.5,
    aes(color = ifelse(is_outlier, "Outlier", "Normal"),
        shape = ifelse(is_outlier, 4, 16))
  ) +
  scale_color_manual(values = c("Normal" = "steelblue", "Outlier" = "firebrick")) +
  labs(
    title = "[PLOT TITLE]",
    x     = "[X AXIS LABEL]",
    y     = "[Y AXIS LABEL]"
  ) +
  theme_minimal(base_size = 13)

# --- 5. OPTIONAL --------------------------------------------
# p <- p + scale_x_log10() + scale_y_log10()
# p <- p + geom_smooth(method = "lm", se = TRUE, linetype = "dashed")

# --- 6. RENDER / EXPORT -------------------------------------
print(p)
# ggsave("[OUTPUT.png]", p, width = 10, height = 6, dpi = 150)   
