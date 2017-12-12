library(tidyverse)
library(forcats)
library(choroplethr)
library(here)

subsidy_table <-
  data_frame(lower_percent = c(0, 1.33, 1.5, 2.0, 2.5, 3.0),
             upper_percent = c(1.33, 1.5, 2.0, 2.5, 3.0, 4.0),
             lower_cap = c(2.0, 3.0, 4.0, 6.3, 8.05, 9.5),
             upper_cap = c(2.0, 4.0, 6.3, 8.05, 9.5, 9.5))

annual_subsidy <- function(annual_income, federal_poverty_level, silver_monthly_premium) {
  income_percent_of_fpl <- (annual_income / federal_poverty_level)

  # This is only true for states that expanded medicaid
  if (income_percent_of_fpl < 1.33) {
    return(0.0)
  }

  if (income_percent_of_fpl > 4.0) {
    return(0.0)
  }

  applicable_caps <- subsidy_table %>%
    filter(lower_percent <= income_percent_of_fpl, upper_percent > income_percent_of_fpl)

  lower_percent <- applicable_caps$lower_percent
  upper_percent <- applicable_caps$upper_percent
  lower_cap <- applicable_caps$lower_cap
  upper_cap <- applicable_caps$upper_cap

  if (upper_cap > lower_cap) {
    cap <- lower_cap + ((income_percent_of_fpl - lower_percent) / (upper_percent - lower_percent)) * (upper_cap - lower_cap)
  } else {
    cap <- lower_cap
  }
  # cat("cap = ", cap, "\n")

  subsidy <- silver_monthly_premium * 12 - (cap * annual_income / 100.0)

  subsidy
}

aca2018 <- read_csv(here("data", "QHP_PY2018_Medi-_Indi-_Land.csv"))

premium_columns <- grepl("^Premium(?!.*Scenarios)|^Couple|^Individual", names(aca2018), perl = TRUE)
premium_column_names <- names(aca2018)[premium_columns]

tidy_aca_2018 <- aca2018 %>%
  gather(type, premium, premium_column_names) %>%
  select(`State Code`, `FIPS County Code`, `County Name`, `Metal Level`, premium, type) %>%
  mutate(`State Code` = as_factor(`State Code`),
         `FIPS County Code` = as_factor(`FIPS County Code`),
         `County Name` = as_factor(`County Name`),
         `Metal Level` = as_factor(`Metal Level`)) %>%
  mutate(insured = case_when(str_detect(type, "Premium Child") ~ "Child",
                             str_detect(type, "Premium Adult Individual|Individual\\+") ~ "Individual",
                             str_detect(type, "Premium Couple|Couple\\+") ~ "Couple",
                             TRUE ~ "NA"),
         age = str_extract(type, "\\d+(-\\d+)?$"),
         num_children = str_match(type, "(\\d+( or more)?) [Cc]hild(ren)?")[,2]) %>%
  select(-type) %>%
  mutate(num_children = ifelse(is.na(num_children), "0", num_children),
         premium = as.numeric(gsub("\\$", "", premium))) %>%
  mutate(insured = as_factor(insured),
         age = as_factor(age),
         num_children = as_factor(num_children)) %>%
  distinct()

silver_premiums <- tidy_aca_2018 %>%
  filter(`Metal Level` == "Silver") %>%
  select(`FIPS County Code`, insured, age, num_children, premium) %>%
  group_by(`FIPS County Code`, insured, age, num_children) %>%
  mutate(ref_silver_premium = nth(premium, n = -2, order_by = -premium, default = min(premium))) %>%
  select(-premium) %>%
  distinct()

tidy_aca_2018 <- inner_join(tidy_aca_2018, silver_premiums, by = c("FIPS County Code", "insured", "age", "num_children"))

# tidy_aca_2018 %>%
#   group_by(`FIPS County Code`, insured, age, num_children, `Metal Level`) %>%
#   summarize(count = n()) %>%
#   filter(`Metal Level` == "Silver", count > 2)

premiums_for <- function(df, fips_code, metal_level, insured_, age_, num_children_) {
  df %>%
    filter(`FIPS County Code` == fips_code,
           `Metal Level` == metal_level,
           `insured` == insured_,
           `age` == age_,
           `num_children` == num_children_)
}

# premiums_for(tidy_aca_2018, 19153, "Silver", "Couple", 60, 0)
# premiums_for(tidy_aca_2018, 19153, "Bronze", "Couple", 60, 0)

federal_poverty_level <- function(year, state, family_size) {
  switch (as.character(year),
          `2016` = {
            switch(state,
                   AK = 14720 + 5200 * (family_size - 1),
                   HI = 13550 + 4780 * (family_size - 1),
                   11770 + 4160 * (family_size - 1))
          },
          `2017` = {
            switch(state,
                   AK = 14840 + 5180 * (family_size - 1),
                   HI = 13670 + 4760 * (family_size - 1),
                   11880 + 4140 * (family_size - 1))
          },
          `2018` = {
            switch(state,
                   AK = 15060 + 5230 * (family_size - 1),
                   HI = 13860 + 4810 * (family_size - 1),
                   12060 + 4180 * (family_size - 1))
          }
  )
}

# Proof of concept for couple age 60 no children
poc <- tidy_aca_2018 %>%
  filter(`Metal Level` == "Bronze", insured == "Couple", age == 60, num_children == 0) %>%
  group_by(`FIPS County Code`) %>%
  filter(premium == min(premium)) %>%
  mutate(cliff = (ref_silver_premium * 12 - 0.095 * 4 * federal_poverty_level(2018, "IA", 2)))

summary(poc)

poc[poc$cliff == max(poc$cliff), ]
poc[poc$cliff == min(poc$cliff), ]

g <- ggplot(poc, aes(x = cliff))
g <- g + geom_histogram(binwidth = 1000)
g

c <- poc %>%
  mutate(region = as.numeric(as.character(`FIPS County Code`)), value = cliff) %>%
  distinct()

# c %>%
#   group_by(region) %>%
#   summarize(n = n()) %>%
#   filter(n > 1)

county_choropleth(c, num_colors = 1) + scale_fill_continuous(low="#eff3ff", high="#084594", na.value="white")
# end POC

incomes <- seq(0, 70000, by = 100)
fpl <- federal_poverty_level(2018, "IA", 2)
silver_premium <- premiums_for(tidy_aca_2018, 19153, "Silver", "Couple", 60, 0)$premium

subsidy_fn <- function(income) {
  annual_subsidy(income, fpl, silver_premium)
}

subsidies <- sapply(incomes, subsidy_fn)

df <- data_frame(income = incomes, subsidy = subsidies)
df$slope <- (df$subsidy - lag(df$subsidy)) / (df$income - lag(df$income))
df$pfpl <- df$income / fpl

g <- ggplot(df, aes(x = income, y = subsidy))
g <- g + geom_line()
g <- g + geom_vline(xintercept = fpl, linetype = 2)
g <- g + geom_text(label = "100%",
                   x = fpl,
                   y = 15000, angle = 90,
                   # fontface = "plain",
                   hjust = 1, vjust = -1)
g <- g + geom_vline(data = subsidy_table,
                    aes(xintercept = upper_percent * fpl),
                    linetype = 2)
g <- g + geom_text(data = subsidy_table,
                   aes(label = paste(upper_percent * 100, "%"),
                       x = upper_percent * fpl,
                       y = 15000),
                   angle = 90, hjust = 1, vjust = -1)
g <- g + theme_minimal()
g
