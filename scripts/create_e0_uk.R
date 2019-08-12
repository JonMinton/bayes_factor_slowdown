
# Load and save e0 for UK


# getHMDcountries()

dta_uk <- HMDHFDplus::readHMDweb("GBR_NP", item = "E0per", username = userInput(), password = userInput())

# Now 201&
# link here: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/adhocs/0091452017singleyearnationallifetables
# https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/adhocs/0091452017singleyearnationallifetables/2017singleyearlifetables.xls

# Saved to data 
# The only items needed are 
# e0 male   : 77.13
# e0 female : 81.16

# Population from here: 

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestool


# For total, need male and female population in 2017

# According to wolfram alpha:
# https://www.wolframalpha.com/input/?i=UK+population+2017+by+sex
# male: 31.9 million
# female: 32.82 million

e0_total <- (79.23 * 31.9 + 82.98 * 32.82) / (31.9 + 32.82)


dta_tidy <- dta_uk %>% 
  as_tibble() %>% 
  magrittr::set_names(tolower(names(.))) %>% 
  gather(-year, key = "sex", value = "e0")

dta_tidy <- 
  dta_tidy %>% 
  bind_rows(
    tribble(
      ~year, ~sex, ~e0,
      2017, "female", 82.98,
      2017, "male"  , 79.23, 
      2017, "total" , e0_total 
    )
  )


