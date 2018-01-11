library(tidyverse)
library(forcats)
library(choroplethr)
library(here)
library(glue)
library(assertthat)

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

second_lowest_silver_premiums <- tidy_aca_2018 %>%
  filter(`Metal Level` == "Silver") %>%
  select(`FIPS County Code`, insured, age, num_children, premium) %>%
  group_by(`FIPS County Code`, insured, age, num_children) %>%
  mutate(second_lowest_silver_premium = nth(premium, n = -2, order_by = -premium, default = min(premium))) %>%
  select(-premium) %>%
  distinct()

tidy_aca_2018 <- inner_join(tidy_aca_2018, second_lowest_silver_premiums, by = c("FIPS County Code", "insured", "age", "num_children"))

# tidy_aca_2018 %>%
#   group_by(`FIPS County Code`, insured, age, num_children, `Metal Level`) %>%
#   summarize(count = n()) %>%
#   filter(`Metal Level` == "Silver", count > 2)

premiums_for <- function(df, fips_code, metal_level, insured, age, num_children) {
  df %>%
    filter(`FIPS County Code` == !!fips_code,
           `Metal Level` == !!metal_level,
           `insured` == !!insured,
           `age` == !!age,
           `num_children` == !!num_children)
}

# premiums_for(tidy_aca_2018, 19153, "Silver", "Couple", 60, 0)
# premiums_for(tidy_aca_2018, 19153, "Bronze", "Couple", 60, 0)
# premiums_for(tidy_aca_2018, 19153, "Bronze", "Couple", 50, 2)

# https://aspe.hhs.gov/poverty-guidelines
federal_poverty_level <- Vectorize(function(year, state, family_size) {
  switch (as.character(year),
          `2014` = {
            switch(as.character(state),
                   "AK" = 14580 + 5080 * (family_size - 1),
                   "HI" = 13420 + 4670 * (family_size - 1),
                   11670 + 4060 * (family_size - 1))
          },
          `2015` = {
            switch(as.character(state),
                   "AK" = 14720 + 5200 * (family_size - 1),
                   "HI" = 13550 + 4780 * (family_size - 1),
                   11770 + 4160 * (family_size - 1))
          },
          `2016` = {
            switch(as.character(state),
                   "AK" = 14720 + 5200 * (family_size - 1),
                   "HI" = 13550 + 4780 * (family_size - 1),
                   11770 + 4160 * (family_size - 1))
          },
          `2017` = {
            switch(as.character(state),
                   "AK" = 14840 + 5180 * (family_size - 1),
                   "HI" = 13670 + 4760 * (family_size - 1),
                   11880 + 4140 * (family_size - 1))
          },
          `2018` = {
            switch(as.character(state),
                   "AK" = 15060 + 5230 * (family_size - 1),
                   "HI" = 13860 + 4810 * (family_size - 1),
                   12060 + 4180 * (family_size - 1))
          }
  )
})

expanded_medicaid <- function(state) {
  # List of states not expanding Medicaid from tables at the end of
  # https://www.kff.org/uninsured/issue-brief/the-coverage-gap-uninsured-poor-adults-in-states-that-do-not-expand-medicaid/
  !(state %in% c("AL", "FL", "GA", "ID", "KS", "ME", "MS", "MO", "NE", "NC", "OK", "SC", "SD", "TN", "TX", "UT", "VA", "WI", "WY"))
}

subsidy_table <-
  rbind(
    data_frame(
      # IRC §36B(b)(3)(A)(i)
      year = c(2014, 2014, 2014, 2014, 2014, 2014),
      lower_percent = c(0.0, 1.33, 1.5, 2.0, 2.5, 3.0),
      upper_percent = c(1.33, 1.5, 2.0, 2.5, 3.0, 4.0),
      lower_cap = c(2.0, 3.0, 4.0, 6.3, 8.05, 9.5),
      upper_cap = c(2.0, 4.0, 6.3, 8.05, 9.5, 9.5)),

    data_frame(
      # Rev. Proc. 2014-37
      year = c(2015, 2015, 2015, 2015, 2015, 2015),
      lower_percent = c(0.0, 1.33, 1.5, 2.0, 2.5, 3.0),
      upper_percent = c(1.33, 1.5, 2.0, 2.5, 3.0, 4.0),
      lower_cap = c(2.01, 3.02, 4.02, 6.34, 8.10, 9.56),
      upper_cap = c(2.01, 4.02, 6.34, 8.10, 9.56, 9.56)),

    data_frame(
      # Rev. Proc. 2014-62
      year = c(2016, 2016, 2016, 2016, 2016, 2016),
      lower_percent = c(0.0, 1.33, 1.5, 2.0, 2.5, 3.0),
      upper_percent = c(1.33, 1.5, 2.0, 2.5, 3.0, 4.0),
      lower_cap = c(2.03, 3.05, 4.07, 6.41, 8.18, 9.66),
      upper_cap = c(2.03, 4.07, 6.41, 8.18, 9.66, 9.66)),

    data_frame(
      # Rev. Proc. 2016-24
      year = c(2017, 2017, 2017, 2017, 2017, 2017),
      lower_percent = c(0.0, 1.33, 1.5, 2.0, 2.5, 3.0),
      upper_percent = c(1.33, 1.5, 2.0, 2.5, 3.0, 4.0),
      lower_cap = c(2.04, 3.06, 4.08, 6.43, 8.21, 9.69),
      upper_cap = c(2.04, 4.08, 6.43, 8.21, 9.69, 9.69)),

    data_frame(
      # Rev. Proc. 2017-36
      year = c(2018, 2018, 2018, 2018, 2018, 2018),
      lower_percent = c(0.0, 1.33, 1.5, 2.0, 2.5, 3.0),
      upper_percent = c(1.33, 1.5, 2.0, 2.5, 3.0, 4.0),
      lower_cap = c(2.01, 3.02, 4.03, 6.34, 8.10, 9.56),
      upper_cap = c(2.01, 4.03, 6.34, 8.10, 9.5, 9.56)
    ))

# Rev. Proc. 2017-36
subsidy_table_2018 <-
  data_frame(lower_percent = c(0.0, 1.33, 1.5, 2.0, 2.5, 3.0),
             upper_percent = c(1.33, 1.5, 2.0, 2.5, 3.0, 4.0),
             lower_cap = c(2.01, 3.02, 4.03, 6.34, 8.10, 9.56),
             upper_cap = c(2.01, 4.03, 6.34, 8.10, 9.5, 9.56))

applicable_percent <- Vectorize(function(income_percent_of_fpl, state) {
  # https://www.kff.org/health-reform/issue-brief/explaining-health-care-reform-questions-about-health/
  if (expanded_medicaid(state)) {
    if (income_percent_of_fpl < 1.38) {
      return(0.0)
    }
  } else {
    if (income_percent_of_fpl < 1.0) {
      return(0.0)
    }
  }

  if (income_percent_of_fpl >= 4.0) {
    return(0.0)
  }

  applicable_caps <- subsidy_table_2018 %>%
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

  return(cap)
})

annual_subsidy <- function(annual_income, federal_poverty_level, silver_monthly_premium, state) {
  income_percent_of_fpl <- (annual_income / federal_poverty_level)
  apr <- applicable_percent(income_percent_of_fpl, state)
  if (apr == 0) {
    subsidy <- 0
  } else {
    subsidy <- silver_monthly_premium * 12 - (apr * annual_income / 100.0)
  }

  # if (length(subsidy) != 1) {
  #   print(subsidy)
  # }
  assert_that(length(subsidy) == 1)

  return(subsidy)
}

calc_cliff <- function(insured, age, num_children) {
  if (insured == "Couple") {
    family_size <- 2 + num_children
  } else {
    family_size <- 1 + num_children
  }

  tidy_aca_2018 %>%
    filter(insured == !!insured, age == !!age, num_children == !!num_children) %>%
    select(`FIPS County Code`, `State Code`, insured, age, num_children, second_lowest_silver_premium) %>%
    group_by(`FIPS County Code`) %>%
    mutate(
      cliff = max(
          # TODO: don't hard code percentage
          second_lowest_silver_premium * 12 - 0.0956 * 4 * federal_poverty_level(2018, `State Code`, family_size),
          0.0))
}

cliff_map <- function(insured, age, num_children) {
  cliff_df <- calc_cliff(insured, age, num_children) %>%
    mutate(region = as.numeric(as.character(`FIPS County Code`)), value = cliff) %>%
    distinct()

  # We have to use the CountyChoropleth R6 object to get Alaska and Hawaii to render correctly
  # See https://stackoverflow.com/questions/38938565/alaska-and-hawaii-not-formatting-correctly-for-county-choropleth-map-in-r
  choro = CountyChoropleth$new(cliff_df)
  choro$ggplot_scale = scale_fill_brewer(name="Subsidy loss at 400% FPL", palette = "Blues", drop=FALSE)
  choro$render() + ggtitle("2018 ACA Subsidy Cliff", subtitle = glue("{insured} age {age}, {num_children} children")) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
}

# cliff_map("Couple", 60, 0)
# cliff_map("Individual", 21, 0)
# cliff_map("Couple", 40, 2)

fips_to_state_and_county <- function(fips_code) {
  fips_row = tidy_aca_2018[tidy_aca_2018$`FIPS County Code` == fips_code,]
  list(state = as.character(fips_row$`State Code`[1]),
       county = as.character(fips_row$`County Name`[1]))
}

cliff_chart <- function(fips_code, insured, age, num_children) {
  if (insured == "Couple") {
    family_size <- 2 + num_children
  } else {
    family_size <- 1 + num_children
  }

  # TODO: calculate upper income bound based on cliff
  incomes <- seq(0, 125000, by = 100)
  state_and_county <- fips_to_state_and_county(fips_code)
  state = state_and_county$state
  county = state_and_county$county
  fpl <- federal_poverty_level(2018, state, family_size)
  silver_premium <- tidy_aca_2018 %>%
    filter(`FIPS County Code` == !!fips_code, insured == !!insured, age == !!age, num_children == !!num_children) %>%
    select(second_lowest_silver_premium) %>%
    distinct()

  subsidies <- unlist(lapply(incomes, annual_subsidy, fpl, silver_premium, state))

  df <- data_frame(income = incomes, subsidy = subsidies)

  fpl_df <- rbind(data_frame(upper_percent = 1.0), select(subsidy_table_2018, upper_percent))
  # TODO: don't hard code percentage
  cliff <- unlist(round(silver_premium * 12 - 0.0956 * 4 * fpl))
  cliff_df <- data_frame(x = (4 * fpl) + 1000, cliff = cliff)
  four_x_fpl_df <- data_frame(y = -1000, four_x_fpl = 4 * fpl)

  g <- ggplot(df, aes(x = income, y = subsidy))
  g <- g + geom_line()
  # g <- g + geom_vline(data = fpl_df,
  #                     aes(xintercept = upper_percent * fpl),
  #                     linetype = 2)
  # g <- g + geom_text(aes(label = paste(upper_percent * 100, "%"),
  #                        x = upper_percent * fpl,
  #                        y = 15000),
  #                    data = fpl_df,
  #                    angle = 90, hjust = 1, vjust = -1)
  g <- g + geom_segment(aes(x = 0, y = cliff, xend = 4 * fpl, yend = cliff, color = "red"), show.legend = FALSE)
  g <- g + geom_text(aes(label = cliff,
                         x = x, y = cliff,
                         color = "red",
                         hjust = "left"),
                     data = cliff_df,
                     show.legend = FALSE)
  g <- g + geom_text(aes(label = 4 * fpl, x = four_x_fpl, y = y, color = "red"), show.legend = FALSE,
                     data = four_x_fpl_df)
  g <- g + labs(title = "2018 ACA Subsidy \"Cliff\"", subtitle = glue("{county} County, {state}, {insured} age {age}, {num_children} children"))
  g <- g + xlab("Household Income") + ylab("Maximum Available Annual Subsidy")
  g <- g + theme_minimal() + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  return(g)
}

# cliff_chart(19153, "Couple", 60, 0)
# cliff_chart(19153, "Individual", 21, 0)
# cliff_chart(19153, "Couple", 50, 2)
# cliff_chart(19153, "Couple", 30, 1)

states_with_one_issuer <- aca2018 %>%
  group_by(`State Code`) %>%
  mutate(n_issuers = n_distinct(`Issuer Name`)) %>%
  filter(n_issuers == 1) %>%
  select(`State Code`, `Issuer Name`) %>%
  distinct()

counties_with_one_issuer <- aca2018 %>%
  group_by(`FIPS County Code`) %>%
  mutate(n_issuers = n_distinct(`Issuer Name`)) %>%
  filter(n_issuers == 1) %>%
  select('FIPS County Code', `State Code`, `County Name`, `Issuer Name`) %>%
  distinct()
