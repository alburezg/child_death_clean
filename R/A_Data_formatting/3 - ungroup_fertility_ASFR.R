print("Running script: 3 - ungroup_fertility_ASFR.R")

# This script takes 5-year gropued age specific rates reported and projected by 
# the UN and returns 1x1 projected period ASFR for all countries in the world
# https://population.un.org/wpp/Download/Standard/Fertility/

visual_examination <- F
export <- T

fert_df <- asfr_wpp

allowed_types <- c("Country", "SDG region")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created with this script can be loaded with:

# 1. Read all into a uique file (recommended)
# fert_per_1_1 <- read.csv(file = paste0("output/","fert_per_1_1.csv"), stringsAsFactors = F)

# 2. Load separately by type of region
# fil <- list.files("output/")
# fil <- fil[grepl("^fert_per_1_1_", fil)]
#  
# for(f in fil) {
#   obj <- read.csv(file = paste0("output/",f), stringsAsFactors = F)
#   new_name <- gsub(".csv", "", f)
#   assign(new_name, obj)
#   print(paste(new_name, "loaded."))
# }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Reformat fert data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.1. Change column names ####

old_names <- colnames(fert_df)

new_names <- c("index", "variant", "country", "notes", "country_code", 
               "type","parent_code", "year", "X15.19", "X20.24", 
               "X25.29", "X30.34", "X35.39", "X40.44", "X45.49")

# Change colnames
colnames(fert_df) <- new_names

# Save for future reference
fert_codebook <- data.frame(old = old_names, new = new_names)
# print(fert_codebook)

# sum(is.na(fert_df$year))

fert_df[fert_df == "..."] <- NA

fert_5_5 <- 
  fert_df %>% 
  filter(type %in% allowed_types) %>% 
  select(country, year, dplyr::starts_with("X")) %>% 
  reshape2::melt(id = c("country", "year")) %>% 
  select(country, year, age = variable, value) %>% 
  mutate_all(list(as.character)) %>% 
  # Remove unwanted punctuation
  mutate(
    country = fix_un_countries(country)
    , age = gsub("\\.|\\?", "-", age)
    , value = as.numeric(value)
  ) %>% 
  arrange(country, year, age) 

# 1.2. Parameters ####

# Change period labels
# UN calendar year periods are non-exlusive; ie 1950-1955 and 1955-1960
# The should actually be only 5 yers long: 1950-1954 and 1955-1959

per_old <- unique(fert_5_5$year)

per_new <- unlist(lapply(per_old, function(p) {
  pe <- as.numeric(unlist(strsplit(p, "-")))
  pe[2] <- pe[2] -1
  paste(pe, collapse = "-")
}))

fert_5_5$year <- plyr::mapvalues(fert_5_5$year, from = per_old, to = per_new)

all_years <- as.numeric(unlist(strsplit(per_new, "-")))

min_y <- min(all_years)
max_y <- max(all_years)
year_range <- min_y:max_y

grouped_ages <- seq(15, 45, 5)
age_groups_range <- 15:49

countries <- unique(fert_df$country)

# 2. Ungroup age groups ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

l_5_5 <- split(fert_5_5, list(fert_5_5$country, fert_5_5$year))

# This returns a data frame with 1-year age bands but still grouped in 5-year long calendar year periods
fert_5_1 <- expand_asfr_age(
  l_5_5 = l_5_5
  , grouped_ages = grouped_ages
  , method = "linear"
  , col = 'value'
)

rm("l_5_5")

# View(fert_5_1)

# 3. Interpolate calendar years ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fert_per_1_1 <- interpolate_COLUMN_calendar_years(
  df_5_1 = fert_5_1
  , method = "linear"
  , col = 'value'
)

rm("fert_5_1")
 
# 4. Visual exam ----
# Plot for same country, one 

if(visual_examination) {
  
  country_keep <- c("sweden", "guatemala", "israel", "sri lanka")
  
  # 4.1. Compare ASFR ====
  
  (
    test_asfr_1_1 <- 
  cowplot::plot_grid(
    #  Original data
    fert_5_5 %>% 
      filter(country %in% country_keep) %>% 
      mutate(
        value = as.numeric(value)
      ) %>% 
      ggplot(aes(x = age, y = value, group = year, colour = year)) +
      geom_line(size=0.5) +
      facet_grid(~country) +
      theme(legend.position = "none")
    # Smoothed data
    , fert_per_1_1 %>% 
      filter(country %in% country_keep) %>% 
      mutate(source = "interpolated") %>% 
      ggplot(aes(x = age, y = value, group = year, colour = year)) +
      geom_line(size=0.5) +
      facet_grid(~country) +
      theme(legend.position = "none")
    , ncol = 1) 
  )
  
  # 4.2. Compare TFR ====
  
  # Random countries
  country_keep <- tolower( countries[sample(1:length(countries), 20)] )
  
  tfr_new <- 
    fert_per_1_1 %>% 
    mutate(year = year - 3) %>% 
    group_by(country, year) %>% 
    summarise(tfr = sum(value)/1000) %>% 
    ungroup %>% 
    mutate(
      country = as.character(country)
      , source= 'new'
      )
  
  tfr_old <- 
    fert_5_5 %>% 
    mutate(year = as.numeric(str_extract(year, "^[0-9]{4}"))) %>% 
    group_by(country, year) %>% 
    summarise(tfr = sum(value)/1000*5) %>% 
    ungroup %>% 
    mutate(
      source= 'old'
      )
  
  tfr_both <- 
    bind_rows(
      tfr_new
      , tfr_old
    ) %>% 
    filter(country %in% country_keep)
  
  (
    test_tfr_1_1 <- 
      tfr_both %>% 
      ggplot(aes(x = year, y = tfr, group = source, colour = source)) +
      geom_line(size = 0.5) +
      facet_wrap(~country)
  )
  
   #Export graphs
  
  ggsave("../../Output/A.3.test_asfr_1_1.pdf", test_asfr_1_1)
  ggsave("../../Output/A.3.test_tfr_1_1.pdf", test_tfr_1_1)
  
}

# 5. Export ####

if(export) {
  write.csv(x = fert_per_1_1, file = paste0("../../Data/derived/", "fert_per_1_1.csv"), row.names = F)
}

print("3 - ungroup_fertility_ASFR: success!")