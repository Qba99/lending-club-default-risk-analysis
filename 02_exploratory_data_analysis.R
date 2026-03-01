mean(LCData_final$is_default)

# Summary table

model_validation <- LCData_final %>%
  pivot_longer(
    cols = c(
      int_rate_bucket,
      grade,
      sub_grade
    ),
    names_to = "variable",
    values_to = "category"
  ) %>%
  mutate(
    variable = recode(
      variable,
      int_rate_bucket = "Interest rate",
      grade = "Grade",
      sub_grade = "Sub grade"
    ),
    variable = factor(
      variable,
      levels = c("Interest rate", "Grade", "Sub grade")
    )
  ) %>%
  group_by(variable, category) %>%
  summarize(
    total_loans = n(),
    default_rate = mean(is_default, na.rm = TRUE) * 100,
    .groups = "drop"
  )

summary_table <- LCData_final %>%
  pivot_longer(
    cols = c(
      loan_amnt_bucket,
      installment_bucket,
      income_group,
      dti_bucket,
      purpose,
      emp_length,
    ),
    names_to = "variable",
    values_to = "category"
  ) %>%
  group_by(variable, category) %>%
  summarize(
    total_loans = n(),
    default_rate = mean(is_default, na.rm = TRUE) * 100,
    .groups = "drop"
  )

summary_table %>%
  filter(variable == "installment_bucket")

 # Overview of multiple variables

ggplot(
  model_validation,
  aes(
    x = fct_reorder(category, default_rate),
    y = default_rate,
    fill = default_rate
  )
) +
  geom_col() +
  facet_wrap(~ variable, scales = "free_x") +
  scale_fill_gradient(
    low = "lightgreen",
    high = "lightcoral"
  ) +
  labs(
    x = NULL,
    y = "Default Rate (%)",
    title = "Default Rates by Interest Rate and Grade",
    fill = "Default Rate"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

# A single variable plot

summary_table %>%
  filter(variable == "income_group") %>%
  ggplot(
    aes(
      x = fct_reorder(category, default_rate),
      y = default_rate,
      fill = default_rate
    )
  ) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Income Group",
    y = "Default Rate (%)",
    title = "Default Rate by Income Group"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

summary_table %>%
  filter(variable == "emp_length") %>%
  ggplot(
    aes(
      x = fct_reorder(category, default_rate),
      y = default_rate,
      fill = default_rate
    )
  ) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(limits = c(0, 30)) +
  labs(
    x = "Employment Length",
    y = "Default Rate (%)",
    title = "Default Rate by Employment Length"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

summary_table %>%
  filter(variable == "installment_bucket") %>%
  ggplot(
    aes(
      x = fct_reorder(category, default_rate),
      y = default_rate,
      fill = default_rate
    )
  ) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(limits = c(0, 30)) +
  labs(
    x = "Installment",
    y = "Default Rate (%)",
    title = "Default Rate by Installment"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

summary_table %>%
  filter(variable == "loan_amnt_bucket") %>%
  ggplot(
    aes(
      x = fct_reorder(category, default_rate),
      y = default_rate,
      fill = default_rate
    )
  ) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(limits = c(0, 30)) +
  labs(
    x = "Loan Amount",
    y = "Default Rate (%)",
    title = "Default Rate by Loan Amount"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

library(stringr)

summary_table %>%
  filter(variable == "purpose") %>%
  ggplot(
    aes(
      x = fct_reorder(
        str_replace_all(str_to_title(category), "_", " "),
        default_rate
      ),
      y = default_rate,
      fill = default_rate
    )
  ) +
  geom_col() +
  scale_fill_gradient(low = "lightgreen", high = "lightcoral") +
  labs(
    x = "Loan Purpose",
    y = "Default Rate (%)",
    title = "Default Rate by Purpose"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

summary_table %>%
  filter(variable == "dti_bucket") %>%
  ggplot(
    aes(
      x = fct_reorder(
        category,
        default_rate
      ),
      y = default_rate,
      fill = default_rate
    )
  ) +
  geom_col() +
  scale_fill_gradient(low = "lightgreen", high = "lightcoral") +
  labs(
    x = "DTI",
    y = "Default Rate (%)",
    title = "Default Rate by Debt-To-Income Ratio"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "none"
  )

summary_table %>%
  filter(variable == "int_rate_bucket") %>%
  ggplot(
    aes(
      x = fct_reorder(
        category,
        default_rate
      ),
      y = default_rate,
      fill = default_rate
    )
  ) +
  geom_col() +
  scale_fill_gradient(low = "lightgreen", high = "lightcoral") +
  labs(
    x = "Interest Rate",
    y = "Default Rate (%)",
    title = "Default Rate by Interest Rate"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Correlation Matrix Heatmap Plot

install.packages("ggcorrplot")
library(ggcorrplot)

corr_matrix <- LCData_final %>%
  select(is_default, loan_amnt, int_rate, annual_inc, dti, installment) %>% 
  rename(
    Default       = is_default,
    "Loan Amount"   = loan_amnt,
    "Interest Rate" = int_rate,
    "Annual Income" = annual_inc,
    DTI           = dti,
    Installment   = installment
  ) %>%
  cor(use = "complete.obs")

ggcorrplot(corr_matrix, lab = TRUE, title = "Correlation of Risk Factors")

# Risk Ranking Plot

library(forcats)

top_risk <- summary_table %>%
  filter(total_loans >= 500) %>%   # avoid tiny samples
  mutate(
    variable = recode(
      variable,
      dti_bucket         = "DTI",
      emp_length         = "Employment Length",
      income_group       = "Annual Income",
      installment_bucket = "Installment",
      loan_amnt_bucket   = "Loan Amount",
      purpose            = "Purpose"
    )
  ) %>%
  slice_max(default_rate, n = 15)

ggplot(top_risk,
       aes(
         x = fct_reorder(
           paste(variable, str_replace_all(str_to_title(category), "_", " "), sep = ": "),
           default_rate
         ),
         y = default_rate,
         fill = default_rate
       )) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(
    low = "lightgreen",
    high = "lightcoral"
  ) +
  labs(
    title = "Top 15 Behavioral Risk Drivers",
    x = NULL,
    y = "Default Rate (%)",
    fill = "Default Rate"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")

# Distribution Plot of Risk by Variable (Boxplot)

ggplot(summary_table,
       aes(
         x = variable,
         y = default_rate
       )) +
  geom_boxplot(outlier.alpha = 0.5) +
  scale_x_discrete(
    labels = c(
      dti_bucket         = "DTI",
      emp_length         = "Employment Length",
      income_group       = "Annual Income",
      installment_bucket = "Installment",
      loan_amnt_bucket   = "Loan Amount",
      purpose            = "Purpose"
    )
  ) +
  labs(
    title = "Distribution of Default Rates by Risk Driver",
    x = "Variable",
    y = "Default Rate (%)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

