# ============================================================
# Libraries
# ============================================================
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/harpermarshall/Desktop/Project 1/SCT_Data/")

# ============================================================
# Parameters
# ============================================================
offsets <- c(0)
excluded_participants <- c()
bins <- seq(200, 800, by = 10)
x_breaks <- seq(0, max(bins, na.rm = TRUE), by = 100)

# ============================================================
# 1) Load + filter dataset
# ============================================================
df_all <- read_csv("All_Trials_With_Survey_21TrueFAST.csv") %>%
  filter(
    modality            %in% c("A","V","AVC"),
    !participant_number %in% excluded_participants,
    rt_corrected        <= 800,
    offset_corrected    %in% offsets
  )

# ============================================================
# 2) Check for missing modality data per offset × participant
# ============================================================
df_all %>%
  group_by(offset_corrected, participant_number) %>%
  summarise(
    n_A   = sum(modality == "A"   & !is.na(rt_corrected)),
    n_V   = sum(modality == "V"   & !is.na(rt_corrected)),
    n_AVC = sum(modality == "AVC" & !is.na(rt_corrected)),
    .groups = "drop"
  ) %>%
  filter(n_A == 0 | n_V == 0 | n_AVC == 0)

# ============================================================
# 3) Per offset × subject: compute CDFs + Miller bound at each bin
# ============================================================
cdf_by_subj_all <- df_all %>%
  group_by(offset_corrected, participant_number) %>%
  do({
    dat <- .
    ecdf_A   <- ecdf(dat$rt_corrected[dat$modality=="A"])
    ecdf_V   <- ecdf(dat$rt_corrected[dat$modality=="V"])
    ecdf_AVC <- ecdf(dat$rt_corrected[dat$modality=="AVC"])
    tibble(
      rt_bin = bins,
      A      = ecdf_A(bins),
      V      = ecdf_V(bins),
      AVC    = ecdf_AVC(bins),
      Miller = pmin(ecdf_A(bins) + ecdf_V(bins), 1)
    )
  }) %>%
  ungroup()

# ============================================================
# 4) Average across subjects + pivot long + offset labels
# ============================================================
cdf_avg_all <- cdf_by_subj_all %>%
  group_by(offset_corrected, rt_bin) %>%
  summarise(
    A      = mean(A),
    V      = mean(V),
    AVC    = mean(AVC),
    Miller = mean(Miller),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(A, V, AVC, Miller),
    names_to  = "condition",
    values_to = "CDF"
  ) %>%
  mutate(offset_label = paste0(offset_corrected, " ms Onset"))

# ============================================================
# Shared scale settings (no behavior change)
# ============================================================
cond_order <- c("Miller","AVC","V","A")
linetypes  <- c("A"="solid","V"="solid","AVC"="solid","Miller"="dashed")

# ============================================================
# 5) Plot 1: Subject-averaged CDFs (publication light theme)
# ============================================================
cols_light <- c(
  "A"      = "#A63C3C",
  "V"      = "#4BA3C3",
  "AVC"    = "#8FBC8F",
  "Miller" = "#333333"
)

p <- ggplot(
  cdf_avg_all,
  aes(x = rt_bin, y = CDF, color = condition, linetype = condition)
) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Subject-Averaged Race Model CDF",
    subtitle = "Across audiovisual and unisensory conditions",
    x = "Response Time (ms)",
    y = "Cumulative Probability",
    color = "Condition",
    linetype = "Condition"
  ) +
  scale_color_manual(values = cols_light, breaks = cond_order) +
  scale_linetype_manual(values = linetypes, breaks = cond_order) +
  facet_wrap(~ offset_label, nrow = 1) +
  scale_x_continuous(name = "Response Time (ms)", breaks = x_breaks) +
  paper_light

p

# ============================================================
# 6) Build wide table ONCE (reuse for all downstream)
# ============================================================
cdf_wide <- cdf_avg_all %>%
  pivot_wider(names_from = condition, values_from = CDF)

# ============================================================
# 7) Plot 2: Shaded violation region where AVC > Miller + annotate max
# ============================================================
violation_ribbon <- cdf_wide %>%
  mutate(is_violation = AVC > Miller) %>%
  filter(is_violation) %>%
  transmute(
    offset_corrected,
    offset_label,
    rt_bin,
    ymin = Miller,
    ymax = AVC
  )

max_violations <- cdf_wide %>%
  mutate(diff = AVC - Miller) %>%
  group_by(offset_corrected, offset_label) %>%
  slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
  ungroup()

s <- ggplot() +
  geom_ribbon(
    data = violation_ribbon,
    aes(x = rt_bin, ymin = ymin, ymax = ymax),
    alpha = 0.35,
    fill = "#8FBC8F"
  ) +
  geom_line(
    data = cdf_avg_all,
    aes(x = rt_bin, y = CDF, color = condition, linetype = condition),
    linewidth = 1.2
  ) +
  geom_point(
    data = max_violations,
    aes(x = rt_bin, y = AVC),
    size = 3.5,
    color = "#333333",
    stroke = 0,
    shape = 21,
    fill = "#333333"
  ) +
  geom_text(
    data = max_violations,
    aes(x = rt_bin, y = AVC, label = sprintf("+%.2f", AVC - Miller)),
    vjust = -1.0,
    color = "#333333",
    size = 6
  ) +
  labs(
    title = "Subject-Averaged Race Model CDF",
    x = "Response Time (ms)",
    y = "Cumulative Probability",
    color = "Condition",
    linetype = "Condition"
  ) +
  scale_color_manual(values = cols_light, breaks = cond_order) +
  scale_linetype_manual(values = linetypes, breaks = cond_order) +
  facet_wrap(~ offset_label, nrow = 1) +
  scale_x_continuous(name = "Response Time (ms)", breaks = x_breaks) +
  scale_y_continuous(name = "Cumulative Probability", limits = c(0, 1)) +
  paper_light

s

# ============================================================
# Build wide table ONCE (upstream of plotting)
# ============================================================
cdf_wide <- cdf_avg_all %>%
  pivot_wider(names_from = condition, values_from = CDF)

violation_ribbon <- cdf_wide %>%
  mutate(is_violation = AVC > Miller) %>%
  filter(is_violation) %>%
  transmute(
    offset_label,
    rt_bin,
    ymin = Miller,
    ymax = AVC
  )

max_violations <- cdf_wide %>%
  mutate(diff = AVC - Miller) %>%
  group_by(offset_label) %>%
  slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
  ungroup()


# ============================================================
# 8) ΔCDF (AVC − Miller) + AUC computations + plot
# ============================================================
cdf_wide <- cdf_wide %>%
  mutate(diff = AVC - Miller)

auc_df <- cdf_wide %>%
  arrange(offset_label, rt_bin) %>%
  group_by(offset_label) %>%
  summarise(
    AUC_pos = sum(
      (lead(rt_bin) - rt_bin) * (pmax(diff, 0) + pmax(lead(diff), 0)) / 2,
      na.rm = TRUE
    ),
    AUC_neg = sum(
      (lead(rt_bin) - rt_bin) * (pmax(-diff, 0) + pmax(-lead(diff), 0)) / 2,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

cdf_clean <- cdf_wide %>%
  arrange(offset_label, rt_bin) %>%
  filter(!is.na(diff), !is.na(rt_bin))

viol_clip <- cdf_clean %>%
  mutate(
    ymin = 0,
    ymax = pmax(diff, 0)
  )

lab_pos <- cdf_wide %>%
  filter(!is.na(diff)) %>%
  group_by(offset_label) %>%
  slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(auc_df, by = "offset_label")

max_abs <- max(abs(cdf_wide$diff), na.rm = TRUE)
y_lim <- c(-1.05 * max_abs, 1.05 * max_abs)

v2 <- ggplot() +
  geom_hline(yintercept = 0, color = "#333333", linewidth = 0.6, alpha = 0.7) +
  geom_ribbon(
    data = viol_clip,
    aes(x = rt_bin, ymin = ymin, ymax = ymax, group = offset_label),
    fill = "#8FBC8F", alpha = 0.35, na.rm = TRUE
  ) +
  geom_line(
    data = cdf_wide,
    aes(x = rt_bin, y = diff),
    color = "#8FBC8F", linewidth = 1.1, na.rm = TRUE
  ) +
  geom_point(
    data = lab_pos,
    aes(x = rt_bin, y = diff),
    size = 3.5, color = "#333333", shape = 21, fill = "#333333", stroke = 0,
    na.rm = TRUE
  ) +
  geom_text(
    data = lab_pos,
    aes(
      x = rt_bin, y = diff,
      label = paste0(
        "Max ΔCDF=", sprintf("%.3f", diff),
        "\nAUC=", sprintf("%.1f", AUC_pos)
      )
    ),
    vjust = -1.10, color = "#333333", size = 6, lineheight = 0.95,
    check_overlap = TRUE, na.rm = TRUE
  ) +
  labs(
    title = "Race Model Difference (AVC − Miller)",
    x = "Response Time (ms)",
    y = "ΔCDF (Proportion)"
  ) +
  facet_wrap(~ offset_label, nrow = 1) +
  scale_x_continuous(breaks = x_breaks, name = "Response Time (ms)") +
  scale_y_continuous(limits = y_lim, name = "ΔCDF (Proportion)") +
  theme_paper_light +
  theme(legend.position = "none")

v2

# ============================================================
# 9) Optional AUC table (positive violations only)
# ============================================================
auc_tbl <- cdf_avg_all %>%
  select(offset_label, rt_bin, condition, CDF) %>%
  pivot_wider(names_from = condition, values_from = CDF) %>%
  mutate(diff = pmax(AVC - Miller, 0)) %>%
  group_by(offset_label) %>%
  summarise(AUC_ms = sum(diff) * 10, .groups = "drop")

auc_tbl

