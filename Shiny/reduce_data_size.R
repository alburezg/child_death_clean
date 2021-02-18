

  data.table::fread("Data/datasetS1.csv", stringsAsFactors = F)  %>% 
  data.frame %>% 
  filter(cohort %in% seq(1950, 2000, 5)) %>%
  filter(age %in% seq(15, 100, 5)) %>% 
  write.csv(., "Data/datasetS1.csv", row.names = F)

  read.csv("Data/datasetS2.csv", stringsAsFactors = F) %>% 
  filter(cohort %in% seq(1950, 2000, 5)) %>%
  write.csv(., "Data/datasetS2.csv", row.names = F)


