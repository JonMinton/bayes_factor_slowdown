
# Load and save e0 for UK


# getHMDcountries()

#dta_uk <- HMDHFDplus::readHMDweb("GBR_NP", item = "E0per", username = userInput(), password = userInput())


# Can't download this for now. Will fudge instead (for now)

dta_e0_old <- read_csv("data/e0_uk.csv") %>% 
  filter(!year %in% c(2017, 2018))

# Now 2017
# link here: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/adhocs/0091452017singleyearnationallifetables
# https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/adhocs/0091452017singleyearnationallifetables/2017singleyearlifetables.xls

# Life expectancy for UK, 2017 and 2018

# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/singleyearlifetablesuk1980to2018

# Copied to data/singleyearlifetablesunitedkingdom1.xls

# The values 

# 2017:

# Saved to data 
# The only items needed are 
# e0 male   : 79.28
# e0 female : 82.98

e0_male_2017   <- 79.28
e0_female_2017 <- 82.98 

# 2018
# Saved to data 
# The only items needed are 
# e0 male   : 79.24
# e0 female : 82.93

e0_male_2018   <- 79.24
e0_female_2018 <- 82.93 


# Population from here: 

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestool


# For total, need male and female population in 2017

# According to wolfram alpha:
# https://www.wolframalpha.com/input/?i=UK+population+2017+by+sex
# male: 31.9 million
# female: 32.82 million

p_male_2017 <- 31.9
p_female_2017 <- 32.82

e0_total_2017 <- (e0_male_2017 * p_male_2017 + e0_female_2017 * p_female_2017) / (p_male_2017 + p_female_2017)

# and for 2018

#https://www.wolframalpha.com/input/?i=UK+population+2018+by+sex
# male: 32.96 million
# female: 33.74 million

p_male_2018 <- 32.96
p_female_2018 <- 33.74

e0_total_2018 <- (e0_male_2018 * p_male_2018 + e0_female_2018 * p_female_2018) / (p_male_2018 + p_female_2018)


# dta_tidy <- dta_uk %>% 
#   as_tibble() %>% 
#   magrittr::set_names(tolower(names(.))) %>% 
#   gather(-year, key = "sex", value = "e0")

dta_tidy <- 
  dta_e0_old %>% 
  bind_rows(
    tribble(
      ~year, ~sex, ~e0,
      2017, "female", e0_female_2017,
      2017, "male"  , e0_male_2017, 
      2017, "total" , e0_total_2017,
      2018, "female", e0_female_2018,
      2018, "male"  , e0_male_2018, 
      2018, "total" , e0_total_2018 
      
    )
  )

write_csv(dta_tidy, "data/e0_uk.csv")
