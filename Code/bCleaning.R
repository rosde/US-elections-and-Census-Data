library(tidyverse)
library(tidycensus)
library(sf)
library(rvest)

#Loading and cleaning of election data results
election_data_pre <- read_csv('../data/1976-2020-president.csv')

election_data <- election_data_pre %>% 
  mutate(new_party = (ifelse(party_simplified == "DEMOCRAT", "Democrat", 
                             ifelse(party_simplified == "REPUBLICAN", "Republican", "Third_Party")))) %>% 
  group_by(year, state, new_party) %>% 
  summarize(party_total = sum(candidatevotes/totalvotes))

census_api_key <- readLines("../data/census_api_key.txt")

#Function to collect electoral votes by state for given year via web scraping
electoralVotesScrape <- function(year) {
  archive <- read_html(str_c("https://www.archives.gov/electoral-college/", year))
  
  if (year <= 2008) {
  electoralVotes <- archive %>% 
    html_elements("tr~ tr+ tr td+ td:nth-child(2)") %>% 
    html_text()
  } else {
    electoralVotes <- archive %>% 
      html_elements("td+ td a") %>% 
      html_text()
  }
  
  return(electoralVotes[-9])
}

#Function to predict election results based on demographic data for given year
predictiveModel <- function(df) {
  demModel <- lm(data = df %>% st_drop_geometry(), formula = Democrat ~ whiteperc + blackperc + aianperc +
                   asianperc + twoormoreperc + nhpiperc + hispanicperc)
  repModel <- lm(data = df %>% st_drop_geometry(), formula = Republican ~ whiteperc + blackperc + aianperc +
                   asianperc + twoormoreperc + nhpiperc + hispanicperc)
  df <- df %>% 
    mutate(demPred = demModel$fitted.values) %>% 
    mutate(repPred = repModel$fitted.values) 
  
  return(df %>% 
 			mutate(realWinner = ifelse(Democrat > Republican, "Democrat", "Republican")) %>% 
           mutate(predWinner = ifelse(demPred > repPred, "Democrat", "Republican"))) %>% 
    st_transform("epsg:4326")
}

#Large function to create dataset containing relevant information for requested year
electionDataCreation <- function(yr) {
  
  #filter election data set 
  data <- election_data %>% 
    pivot_wider(names_from = "new_party", values_from = "party_total") %>% 
    filter(year == yr) %>% 
    mutate(NAME = str_to_title(state))
  
  #Query census api for given year
  if (yr == 2008) {
    query <- get_acs(geography = "state", 
                     variables = c(total = "B02001_001", white = "B02001_002", black = "B02001_003", aian = "B02001_004",
                                   asian = "B02001_005", twoormore = "B02001_008", nhpi = "B02001_006", hispanic = "B03001_003"), 
                     year = 2008, geometry = FALSE, survey = "acs1", key = census_api_key) %>% rename(value = estimate) %>% select(-5)
    hisp <- get_decennial(geography = "state",
                          variables = c(t2 = "P003001", h2 = "P004003"),
                          year = 2010, geometry = TRUE, key = census_api_key) %>% 
      pivot_wider(names_from = variable, values_from = "value") %>% 
      mutate(hispperc2 = h2/t2)
  } else if (yr == 2012) {
    query <- get_acs(geography = "state", 
                     variables = c(total = "B02001_001", white = "B02001_002", black = "B02001_003", aian = "B02001_004",
                                   asian = "B02001_005", twoormore = "B02001_008", nhpi = "B02001_006", hispanic = "B03001_003"), 
                     year = 2012, geometry = FALSE, survey = "acs1", key = census_api_key) %>% rename(value = estimate) %>% select(-5)
    hisp <- get_decennial(geography = "state",
                          variables = c(t2 = "P003001", h2 = "P004003"),
                          year = 2010, geometry = TRUE, key = census_api_key) %>% 
      pivot_wider(names_from = variable, values_from = "value") %>% 
      mutate(hispperc2 = h2/t2)
  } else if (yr == 2016) {
    query <- get_acs(geography = "state", 
                     variables = c(total = "B02001_001", white = "B02001_002", black = "B02001_003", aian = "B02001_004",
                                   asian = "B02001_005", twoormore = "B02001_008", nhpi = "B02001_006", hispanic = "B03001_003"), 
                     year = 2016, geometry = FALSE, survey = "acs1", key = census_api_key) %>% rename(value = estimate) %>% select(-5)
    hisp <- get_decennial(geography = "state",
                          variables = c(t2 = "P1_001N", h2 = "P2_002N"),
                          year = 2020, geometry = TRUE, key = census_api_key) %>% 
      pivot_wider(names_from = variable, values_from = "value") %>% 
      mutate(hispperc2 = h2/t2)
  } else if (yr == 2020) {
    query <- get_decennial(geography = "state", 
                           variables = c(total = "P1_001N", white = "P1_003N", black = "P1_004N", aian = "P1_005N", 
                                         asian = "P1_006N", twoormore = "P1_009N", nhpi = "P1_007N", hispanic = "P2_002N"), year = 2020,
                           geometry = TRUE, key = census_api_key)
  } else {
    query <- get_decennial(geography = "state", 
                           variables = c(total = "P003001", white = "P003003", black = "P003004", aian = "P003005", 
                                         asian = "P003006", twoormore = "P003009", nhpi = "P003007", hispanic = "P004002"), year = 2000,
                           geometry = TRUE, key = census_api_key)
  }
  
  #Clean queried data
  clean <- query %>% 
    pivot_wider(names_from = "variable", values_from = "value") %>% 
    mutate(whiteperc = white/total) %>% 
    mutate(blackperc = black/total) %>% 
    mutate(aianperc = aian/total) %>% 
    mutate(asianperc = asian/total) %>% 
    mutate(twoormoreperc = twoormore/total) %>% 
    mutate(nhpiperc = nhpi/total) %>% 
    mutate(hispanicperc = hispanic/total) %>% 
    left_join(y=data, by = "NAME") %>% 
    arrange(NAME) %>% 
    filter(!(NAME %in% c("Puerto Rico", "District of Columbia"))) %>% 
    
    mutate(electoralVotes = electoralVotesScrape(yr))
  
  if (yr <= 2016 & yr >= 2008) {
    clean <- hisp %>% 
      left_join(y=clean, by = "NAME") %>% 
      filter(!(NAME %in% c("Puerto Rico", "District of Columbia")))
    
    clean$hispanicperc[is.na(clean$hispanicperc)] <- clean$hispperc2[is.na(clean$hispanicperc)]
  }
  
  #Generate and return predicted results
  return(predictiveModel(clean))
}

data2000 <- electionDataCreation(2000)
data2004 <- electionDataCreation(2004)
data2008 <- electionDataCreation(2008)
data2012 <- electionDataCreation(2012)
data2016 <- electionDataCreation(2016)
data2020 <- electionDataCreation(2020)

predictedResults <- list("2000" = data2000, "2004" = data2004, "2008" = data2008,
                          "2012" = data2012, "2016" = data2016, "2020" = data2020)

save(predictedResults, file = "bCleaning.rda")
