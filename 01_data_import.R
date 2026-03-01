install.packages("tidyverse")
library(tidyverse)
library(readxl)

LCDataDictionary <- read_excel("Data/Raw/LCDataDictionary.xlsx")
LCData <- read_csv("Data/Raw/loan.csv")

LCData_clean <- LCData %>%
  filter(loan_status %in% c("Fully Paid", "Charged Off", "Default")) %>%
  mutate(is_default = if_else(loan_status == "Fully Paid", 0, 1))

LCData_final <- LCData_clean %>%
  select(is_default, loan_amnt, int_rate, installment, 
         grade, sub_grade, emp_length, annual_inc, 
         purpose, dti)

LCData_final %>%
  filter(dti > 50 | dti < 0) %>%
  nrow()

LCData_final %>%
  filter(is.na(dti)) %>%
  nrow()

LCData_final <- LCData_final %>%
  mutate(
    dti = ifelse(dti < 0 | dti > 50, NA, dti),
    emp_length = na_if(emp_length, "n/a")
  ) %>%
  filter(
    !is.na(dti),
    !is.na(emp_length)
  )

LCData_final <- LCData_final %>%
  mutate(
    income_group = cut(
      annual_inc,
      breaks = c(0, 40000, 70000, 100000, Inf),
      labels = c("<40k", "40k-70k", "70k-100k", ">100k")
    ),
    
    dti_bucket = cut(
      dti,
      breaks = c(0, 10, 20, 30, 40, Inf),
      labels = c("0-10", "11-20", "21-30", "31-40", "41+"),
      include.lowest = TRUE
    ),
    
    loan_amnt_bucket = cut(
      loan_amnt,
      breaks = c(0, 10000, 20000, 30000, Inf),
      labels = c("<10k", "10k-20k", "20k-30k", "30k-40k")
    ),
    
    int_rate_bucket = cut(
      int_rate,
      breaks = c(5, 10, 15, 20, 25, Inf),
      labels = c("5-10", "11-15", "15-20", "21-25", "26-31")
    ),
    
    installment_bucket = cut(
      installment,
      breaks = c(0, 250, 500, 750, 1000, Inf),
      labels = c("<=250", "250-500", "500-750", "750-1000", "1000+")
    )
  )

