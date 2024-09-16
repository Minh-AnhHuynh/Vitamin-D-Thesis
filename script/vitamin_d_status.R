# Setup ------------------------------
librarian::shelf(dplyr, tibble, tidyr, magrittr, scales)
# library(chattr)
# chattr_use("copilot")
# chattr_app(as_job = TRUE)



# North America --------------------------------------------------------------

## Calculate the weighted mean and standard deviation for Canada (Community Dwelling) ====
# Define means and assume equal number of participants for each age group
means <- c(67.7, 75.0, 68.1, 65.0, 66.5, 72.0)
sd_individual <- c(67.7 - 65.3, 75.0 - 70.3, 68.1 - 63.8, 65.0 - 61.0, 66.5 - 61.0, 72.0 - 69.4) # SD only available for the first group
n_groups <- length(means) # Number of age groups

# Assuming equal distribution across age groups for simplicity
total_population <- 5306
n_per_group <- total_population / n_groups

# Calculate Weighted Mean
weighted_mean <- mean(means)

# Calculate Weighted SD
# Since SDs for most groups are not provided, we'll use only the available SD
# Calculate variance based on available SD
variances <- sd_individual^2
mean_variance <- mean(variances, na.rm = TRUE)

# Calculate the overall estimated SD
# For simplicity, we assume the mean of variances and the mean of means
# Adjust this step if you have more detailed data
estimated_variance <- mean_variance + mean((means - weighted_mean)^2)
estimated_sd <- sqrt(estimated_variance)

# Print the results
cat("Weighted Mean:", weighted_mean, "\n")
cat("Estimated SD:", estimated_sd, "\n")

## Create the updated tribble with summarized data for community-dwelling ====
north_america_vd_data <- tribble(
  ~Country, ~Population, ~Population_Number, ~Mean_25OHD, ~SD_25OHD, ~Percent_Below_50,
  "Canada", "Men and women", 11336, 58.3, 0.9, 36.8,
  "Canada", "Men and women", 1912, 70.4, 0.55, 20.4,
  "Canada", "Community-dwelling (Overall)", 5306, weighted_mean, estimated_sd, NA, # Summarized data (All of this to be dropped because we don't have % below 50)
  "Canada", "Pregnant women (end of winter)", 304, 52.1, NA, 89,
  "Canada", "Pregnant women (end of summer)", 289, 68.6, NA, 64,
  "Canada", "Workers", 6101, 84.0, 42, 8.0,
  "Canada", "Refugee women", 461, 46.2, 2.1, 61.0,
  "USA", "Men and women (20–39 years)", 3349, 64.6, 2.4, 30.0,
  "USA", "Men and women (40–59 years)", 3377, 67.9, 2.4, 24.0,
  "USA", "Men and women (60+ years)", 3602, 71.0, 2.4, 22.0,
  "USA", "Pregnant women", 2048, 51.2, 27.2, 55.4,
  "USA", "Older adults (70–81 years, Black)", 977, 52.5, 26.0, 54.0,
  "USA", "Older adults (70–81 years, White)", 1607, 73.0, 27.3, 18.0,
  "USA", "Nonpregnant adults", 5173, 59.0, 61.0, 42.0,
  "USA", "Older men", 1606, 62.8, 19.8, 25.7,
  "USA", "Yup’ik Alaskanative people", 743, 77.0, 31.5, NA,
  "USA", "Immigrant & refugee", 1378, NA, NA, 60.0,
  "USA", "Refugees", 2610, NA, NA, 43.0,
  "USA", "Rural postmenopausal white women", 1179, mean(71.8, 71.1), mean(20.3, 20.0), 14.4,
  "Mexico", "Healthy subjects", 585, 52.3, NA, 43.6
) %>% drop_na()


# Summarize the data with weighted mean and standard deviation and percentage
print("Summary for USA")
weighted_sd <- function(x, w) {
  wm <- weighted.mean(x, w) # Calculate the weighted mean
  wm_variance <- weighted.mean((x - wm)^2, w) # Calculate the weighted variance
  sqrt(wm_variance) # Return the square root of variance (standard deviation)
}
continent_summary <- tibble()
calculate_weighted_stats <- function(data, continent_name) {
  data %>%
    summarise(
      Continent = continent_name,
      Weighted_Mean = weighted.mean(Mean_25OHD, Population_Number),
      Weighted_SD = weighted_sd(Mean_25OHD, Population_Number),
      # The problem was that weighted_sd was used on SD_25OHD and not on Mean!
      Weighted_Percent_Below_50 = weighted.mean(Percent_Below_50, Population_Number)
    )
}
north_america_summary <- calculate_weighted_stats(north_america_vd_data, "North America")
continent_summary %<>% bind_rows(north_america_summary)

# Verify if the calculations by functions are correct
verify_calculations <- function(data, data_comparison, country) {
  # Verification that the functions work properly
  test_weighted_mean <- sum(data$Mean_25OHD * data$Population_Number) /
    sum(data$Population_Number)
  test_weighted_sd <- sqrt(
    sum(data$Population_Number * (data$Mean_25OHD - test_weighted_mean)^2) /
      sum(data$Population_Number)
  )

  extract_mean <- data_comparison[data_comparison$Continent == country, ]$Weighted_Mean
  test_mean <- identical(extract_mean, test_weighted_mean)

  if (test_mean) {
    cli::cli_alert_success("Weighted mean calculation is correct")
  } else {
    cli::cli_alert_danger("Weighted mean calculation is incorrect")
    cli::cli_alert_info(cat("Calculated:", test_weighted_mean, "vs", "Function:", extract_mean))
  }
  extract_sd <- data_comparison[data_comparison$Continent == country, ]$Weighted_SD
  test_sd <- identical(test_weighted_sd, extract_sd)

  if (test_sd) {
    cli::cli_alert_success("Weighted SD calculation is correct")
  } else {
    cli::cli_alert_danger("Weighted SD calculation is incorrect")
    cli::cli_alert_info(glue::glue("Calculated: ", test_weighted_sd, " vs ", "Function: ", extract_sd))
  }
}

verify_calculations(north_america_vd_data, continent_summary, "North America")



# South America -------------------------------------------------------------

south_america_vd_data <- tribble(
  ~Country, ~Study, ~Population, ~Population_Number, ~Mean_25OHD, ~SD_25OHD, ~Percent_Below_50,
  "Guatemala", "Sud et al, 2010", "Healthy older Mayans", 108, 53.3, 15.0, 46.3,
  "Ecuador", "Orces, 2015", "Older adults participating in national health survey", 2374, mean(c(69.0, 63.8, 64.8)), mean(c(28.8, 24.3, 33.0)), 21.6,
  "Brazil", "Issa et al, 2016", "Random sample of elderly", 142, 64.1, 8.2, 40.8,
  "Brazil", "Lopes et al, 2014", "Community-dwelling elderly", 908, 48.5, 23.3, 58.0,
  "Brazil (Sao Paulo)", "Martini et al, 2013", "Population-based sample", 636, mean(c(48.4, 50.9, 51.0, 53.9)), mean(c(22.9, 21.9, 26.1, 18.9)), NA,
  "Brazil", "Eloi et al, 2016", "Database of laboratory results", 39004, 63.9, 28.6, NA,
  "Chile", "Gonzalez et al, 2007", "Healthy women (Premenopausal", 30, 61.3, 19.5, 27,
  "Chile", "Gonzalez et al, 2007", "Healthy women (Postmenopausal)", 60, 48.8, 24.8, 60,
  "Argentina", "Portela et al, 2010", "Institutionalized women", 48, 34.0, 15.3, 86
) %>% drop_na()

# Calculate weighted mean and weighted SD
south_america_stats <- calculate_weighted_stats(south_america_vd_data, "South America")
continent_summary %<>% bind_rows(south_america_stats)

# Verify the calculations
verify_calculations(south_america_vd_data, continent_summary, "South America")


# Europe -----------------------------------------------------------------------

# Create the tribble with the extracted data, excluding the Latitude column
europe_vd_data <- tribble(
  ~Country, ~Reference, ~Study_Population, ~Population_Number, ~Mean_25OHD, ~SD_25OHD, ~Percent_Below_50,
  "Norway (Tromso)", "Cashman et al, 2016", "Regionally representative", 12817, 65.0, 17.6, 18.6,
  "Norway (Oslo)", "Cashman et al, 2015", "—", 866, 71.0, 19.5, 14.9,
  "Norway", "Snellman et al, 2009", "Twins", 204, 84.8, 27.4, 8,
  "Finland", "Cashman et al, 2015", "Nationally representative", 4102, 67.7, 13.2, 6.6,
  "Finland", "Kauppi et al, 2009", "Men", 2736, 45.1, 45.1, NA,
  "Finland", "Kauppi et al, 2009", "Women", 3299, 45.2, 45.2, NA,
  "Finland", "Viljakainen et al, 2010", "Mothers", 98, 30.5, 4, NA,
  "Finland", "Viljakainen et al, 2010", "Newborns", 98, 29.2, NA, NA,
  "Finland", "Pekkarinen et al, 2010", "Older women", 1604, 45, NA, 60.3,
  "Iceland", "Cashman et al, 2016", "Adult men and women", 5519, 57.0, 17.8, 33.6,
  "Sweden", "Melhus et al, 2010", "Older men", 1194, 68.7, 19.1, 17,
  "Sweden", "Buchebner et al, 2014", "OPRA women", 995, 78, 30, 16,
  "Denmark (Copenhagen)", "Cashman et al, 2015", "Regionally representative", 3409, 65.0, 19.2, 23.6,
  "UK", "Cashman et al, 2016", "Children, teens and adults", 1488, 47.4, 19.8, 56.4,
  "UK", "Roddam et al, 2007", "Patients with fractures", 730, 82, 40, 21.7,
  "UK", "Roddam et al, 2007", "Controls", 1445, 81, 38, 20.9,
  "Ireland", "Cashman et al, 2013", "Nationally representative", 1118, 56.4, 22.2, 45.0,
  "Germany", "Cashman et al, 2016", "Nationally representative", 6995, 50.1, 18.1, 54.5,
  "Netherlands", "Cashman et al, 2016", "LASA 2009", 915, 64.7, 22.6, 28.5,
  "Netherlands", "Cashman et al, 2016", "Regionally representative", 2625, 59.5, 21.7, 33.6,
  "Netherlands", "van Schoor et al, 2008", "LASA", 1311, 75.5, 6.6, 48.4,
  "Netherlands", "Van Dam et al, 2007", "Hoorn cohort Men", 271, 69.4, 6.3, 33.7,
  "Netherlands", "Van Dam et al, 2007", "Hoorn cohort Women", 267, 69.8, 6.7, 50.9,
  "Belgium", "Hoge et al, 2015", "Adults", 697, 42.7, 10.3, 51.1,
  "France", "Souberbielle et al, 2016", "VARIETE Study", 892, 60, 20, 34.6,
  "Spain", "Gonzales-Molero et al, 2011", "Adults", 1262, 56, NA, 37,
  "Italy", "Cadario et al, 2015", "Women at delivery", 342, 33.6, 5, 61.6,
  "Italy", "Cadario et al, 2015", "Migrant", 191, 29.8, 5.8, 89.7,
  "Greece", "Vallianou et al, 2012", "Adults", 472, 46, NA, 28.6
) %>% drop_na()



europe_summary <- calculate_weighted_stats(europe_vd_data, "Europe")
continent_summary %<>% bind_rows(europe_summary)

# Print the cleaned data and summary
print(europe_vd_data)
print(continent_summary)

verify_calculations(europe_vd_data, continent_summary, "Europe")




# Middle East ----------------------------------------------------------------

middle_east_vitamin_d_data <- tribble(
  ~Country, ~Study, ~Population, ~Population_Number, ~Mean_25OHD, ~SD_25OHD, ~Percent_Below_50,
  "Turkey", "Hekimsoy et al, 2010", "Men", 119, 51.8, 38.7, 66.4,
  "Turkey", "Hekimsoy et al, 2010", "Women", 272, 38.1, 28.7, 78.7,
  "Turkey", "Buyukuslu et al, 2014", "Female students", 100, 65.7, 25.0, 34.0,
  "Iran", "Omrani et al, 2006", "Adult women", 676, 28.9, 23.0, 52.2,
  "Iran", "Hosseinpanah et al, 2011", "Healthy adults", 251, 45.2, NA, 53.8,
  "Israel", "Saliba et al, 2012", "Men", 198834, 54.8, 24.2, 45.0,
  "Israel", "Saliba et al, 2012", "Women", 198834, 50.7, 24.6, 51.8,
  "Jordan", "Nichols et al, 2012", "Women", 2032, 27.5, NA, 95.7,
  "Saudi Arabia", "Hussain et al, 2014", "Men", 3363, 50.5, 23.7, NA,
  "Saudi Arabia", "Hussain et al, 2014", "Women", 7346, 41.9, NA, NA,
  "Saudi Arabia", "Alfawaz et al, 2014", "Men", 756, 35.5, 30.6, 72.4,
  "Saudi Arabia", "Alfawaz et al, 2014", "Women", 2719, 48.8, NA, 78.1,
  "Egypt", "Olama et al, 2013", "Healthy women", 50, 47.0, 13.5, 30.0
) %>% drop_na()

# Calculate the weighted mean and standard deviation
middle_east_summary <- calculate_weighted_stats(middle_east_vitamin_d_data, "Middle East")
continent_summary %<>% bind_rows(middle_east_summary)

# Verify the calculations
verify_calculations(middle_east_vitamin_d_data, continent_summary, "Middle East")




# Asia --------------------------------------------------------------------

asia_vitamin_d_data <- tribble(
  ~Country, ~Study, ~Population, ~Population_Number, ~Mean_25OHD, ~SD_25OHD, ~Percent_Below_25, ~Percent_Below_50,
  "China", "Zhao et al, 2011", "Postmenopausal women", 1724, 33.0, 13.5, NA, 89.7,
  "China", "Yu et al, 2015", "Men", 178, 51.0, 15.5, 2.2, 53.9,
  "China", "Yu et al, 2015", "Men", 191, 44.0, 15.7, 11.0, 66.5,
  "China", "Yu et al, 2015", "Men", 265, 58.5, 16.0, 0.8, 32.1,
  "China", "Yu et al, 2015", "Men", 220, 51.5, 14.5, 3.6, 49.1,
  "China", "Yu et al, 2015", "Men", 223, 55.7, 12.5, 0.4, 32.2,
  "China", "Yu et al, 2015", "Women", 224, 41.5, 16.5, 11.1, 77.2,
  "China", "Yu et al, 2015", "Women", 224, 37.5, 14.8, 20.1, 79.5,
  "China", "Yu et al, 2015", "Women", 215, 49.5, 15.5, 4.2, 54.4,
  "China", "Yu et al, 2015", "Women", 217, 43.2, 13.0, 6.0, 72.4,
  "China", "Yu et al, 2015", "Women", 216, 51.2, 11.5, 0, 47.2,
  "China", "Lu et al, 2012", "Men", 649, 57.0, NA, 2.0, 30.0,
  "China", "Lu et al, 2012", "Women", 1939, 50.2, NA, 3.6, 46.0,
  "China", "Song et al, 2013", "Pregnant women", 125, 28.4, 9.5, 44.8, 96.8,
  "China", "Xiao et al, 2015", "Pregnant women", 5823, 34.0, NA, 40.7, 78.7,
  "China", "Ke et al, 2015", "Adults", 566, 50.6, 17.0, NA, 55.0,
  "China", "Zhen et al, 2015", "Women", 7136, 39.2, 17.8, NA, 75.2,
  "China", "Zhen et al, 2015", "Men", 2902, 45.3, 15.7, NA, NA,
  "China", "Li et al, 2014", "Postmenopausal women", 578, 43.5, 14.3, NA, 72.1,
  "China", "Chan et al, 2011", "Men", 939, 77.9, 20.5, NA, 5.9,
  "China", "Xu et al, 2015", "Adults (18–44 years)", 933, mean(42, 57), NA, NA, NA,
  "China", "Xu et al, 2015", "Adults (45–64 years)", 544, mean(47, 69), NA, NA, NA,
  "China", "Xu et al, 2015", "Adults (65+ years)", 651, mean(41, 56), NA, NA, NA,
  "Japan", "Nakamura et al, 2015", "Men", 9084, 55.9, 18.8, NA, 53.6,
  "Japan", "Nakamura et al, 2015", "Women", 9084, 45.2, 16.6, NA, NA,
  "Korea", "Choi et al, 2011", "Women", 3878, 45.5, 17.7, 10.4, 64.5,
  "Korea", "Choi et al, 2011", "Men", 3047, 53.0, 18.7, 4.7, 47.3,
  "Korea", "Kim et al, 2012", "Adolescent boys", 1095, 45.9, 15.7, 11.7, 64.2,
  "Korea", "Kim et al, 2012", "Adolescent girls", 967, 42.4, 14.7, 15.4, 72.6,
  "Malaysia", "Chee et al, 2010", "Postmenopausal women", 178, 60.4, 15.6, NA, 50.6,
  "Malaysia", "Moy, 2011", "Adults (men)", 158, 56.2, 18.9, NA, 67.9,
  "Malaysia", "Moy, 2011", "Adults (women)", 222, 36.2, 13.4, NA, NA,
  "Vietnam", "Laillou et al, 2013", "Women", 541, 44.5, NA, 17.0, 57.0,
  "Vietnam", "Laillou et al, 2013", "Children", 485, 43.4, NA, 21.0, 58.0,
  "Thailand", "Pratumvinit et al, 2015", "Pregnant women", 147, 61.6, 19.3, 0.7, 34.0,
  "Cambodia", "Smith et al, 2016", "Women", 725, 69.7, 31.2, 4.1, 29.0,
  "Singapore", "Loy et al, 2015", "Pregnant women", 940, 81.0, 27.2, NA, NA,
  "India", "Marwaha et al, 2011", "Pregnant women", 541, 23.2, 12.2, NA, 96.3,
  "India", "Shivane et al, 2011", "Young men", 558, 47.2, 22.2, 12.0, 62.0,
  "India", "Shivane et al, 2011", "Young women", 579, 39.5, 22.7, 26.4, 76.1,
  "Pakistan", "Mehboobali et al, 2015", "Women", 507, 42.3, 17.2, NA, 76.0,
  "Pakistan", "Mehboobali et al, 2015", "Men", 351, 60.1, 19.3, NA, 33.0,
  "Pakistan", "Junaid et al, 2015", "Women", 215, 40.4, 34.4, 43.0, 73.0
) %>% drop_na()

continent_summary <- asia_vitamin_d_data %>%
  calculate_weighted_stats("Asia") %>%
  bind_rows(continent_summary)




# Africa ---------------------------------------------------------------------

library(tibble)

africa_vitamin_d_data <- tribble(
  ~Country, ~Study, ~Population, ~Population_Number, ~Mean_25OHD, ~SD_25OHD, ~Percent_Below_50,
  "Morocco", "El Maghraoui et al, 2012", "Women >50 y", 178, 58.8, 8.2, 65.7,
  "Ethiopia", "Gebreegziabher & Stoecker, 2013", "Women", 202, 30.8, 7.8, 84.2,
  "Nigeria", "Olayiwola et al, 2014", "Adults", 240, NA, NA, NA,
  "Tanzania", "Mehta et al, 2009", "HIV-infected women Low D", 347, 24.6, 5.0, NA,
  "Tanzania", "Mehta et al, 2009", "HIV-infected women Adequate D", 537, 24.6, 5.0, NA,
  "Tanzania", "Friis et al, 2013", "Healthy adults", 355, 84.4, 25.6, 4.3,
  "Tanzania", "Luxwolda et al, 2013", "Nonpregnant adults", 88, 33.0, 10.0, NA,
  "Tanzania", "Luxwolda et al, 2013", "Pregnant women", 139, 138.5, 35.0, NA,
  "Guinea-Bissau", "Wejse et al, 2009", "Tuberculosis patients", 365, 37.0, 14.0, NA,
  "South Africa", "Haarburger et al, 2009", "Unselected all ages", 216, 48.3, NA, 37.0,
  "South Africa", "George et al, 2013", "African", 373, 41.6, 13.1, 3.0,
  "South Africa", "George et al, 2013", "Asian Indian", 344, 43.5, 12.9, 15.0,
  "South Africa", "Kruger et al, 2011", "Adults <50", 179, 77.3, NA, NA,
  "South Africa", "Kruger et al, 2011", "Adults 50–60", 298, 71.2, NA, NA,
  "South Africa", "Kruger et al, 2011", "Adults 60–70", 129, 66.2, NA, NA,
  "South Africa", "Kruger et al, 2011", "Adults >70", 52, 64.7, NA, NA
) %>% drop_na()

# Calculate
continent_summary <- africa_vitamin_d_data %>%
  calculate_weighted_stats("Africa") %>%
  bind_rows(continent_summary)


# Format the tibble
formatted_summary <- continent_summary %>%
  mutate(
    Weighted_Mean = Weighted_Mean / 2.5,
    Weighted_SD = Weighted_SD / 2.5,
    # Weighted_Percent_Below_50 is a percentage, so no conversion needed
  ) %>%
  mutate(
    Weighted_Mean = number(Weighted_Mean, accuracy = 0.1),
    Weighted_SD = number(Weighted_SD, accuracy = 0.1),
    Weighted_Percent_Below_50 = number(Weighted_Percent_Below_50, accuracy = 0.1)
  ) %>%
  arrange(Weighted_Mean) %>%
  rename(`Percentage below 20 ng/mL` = Weighted_Percent_Below_50) |>
  mutate(Weighted_Mean = paste0(Weighted_Mean, " ± ", Weighted_SD))

# Print the converted summary
print(formatted_summary)

french_table <- formatted_summary |>
  select(-Weighted_SD) |>
  rename(`% en dessous de 20 ng/mL` = `Percentage below 20 ng/mL`) |>
  rename(`Moyenne ± écart-type pondéré (ng/mL)` = Weighted_Mean) |>
  mutate(Continent = case_match(Continent, "Asia" ~ "Asie",
                                "Europe" ~ "Europe",
                                "Middle East" ~ "Moyen-Orient",
                                "North America" ~ "Amérique du Nord",
                                "South America" ~ "Amérique du Sud",
                                "Africa" ~ "Afrique"))

write.table(french_table, "tables/formatted_summary_french.csv", sep = ",", row.names = FALSE)
print(french_table)
