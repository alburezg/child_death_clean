
print("Running script: 6 - ungroup_mortality_from_mx.R")

# This script takes abridged period life  tables from the UN and creates complete life tables 
# (ie with single age groups) for all countries in the world. The UN life tables are grouped
# by 5-year age groups and 5-year calendar years.
# It does so by interpolatnig values on the mx column and creating life tables based on that
# column later on. 
# Afterwards, we interpolate again the values to create single-year complete (period) life tables.

# Note that not all resulting life tables have values for the upper age groups
# In some cases, mortality is high enough that there would be no surviving individuals
# to age 100, for example. In this case, the reuslting life table will only have rows 
# going up to age 95

# Data: https://population.un.org/wpp/Download/Standard/Mortality/

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The files created with this script can be loaded with:
# Women only
# lt_1_1_F <- data.table::fread(file = paste0("../../Data/derived/","lt_per_1_1_F.csv"), stringsAsFactors = F) %>% data.frame
# Both sexes
# lt_1_1_B <- data.table::fread(file = paste0("../../Data/derived/","lt_per_1_1_B.csv"), stringsAsFactors = F) %>% data.frame
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Define a function to do the complete interpolation ----


# This function is stored here and not in _global_functions.R since it is pretty much 
# all the analysis

ungroup_mortality_from_mx <- function(lt_per, sex = "F", parallel = T, numCores = 4, export = F, run_graphic_tests = F) {

  # 1. Format life tables ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # I will do this for women initially, but the same can be repeated for men
  
  # lt_per is a df of period life tables
  # If running interactively, for women, this would be:
  # lt_per <- lt_per_F_small
  
  lt_per[lt_per == "…"] <- NA
  
  # Change column names of life tables
  
  old_names <- colnames(lt_per)
  
  new_names <- c("id", "variant", "country" , "notes", "country_code", "type", "parent_code",
                 "year", "age", "interval"
                 , "mx", "qx", "px", "lx", "dx", "Lx", "Sx", "Tx", "ex", "ax")
  
  # Change colnames
  colnames(lt_per) <- new_names
  
  # Save for future reference
  lt_codebook <- data.frame(old = old_names, new = new_names)
  # print(lt_codebook)
  
  lt_5_5 <- lt_per_5_5 <- 
    lt_per %>% 
    select(country, year, age, interval, mx, qx, ax, lx, dx, Lx, Tx, ex) %>% 
    # filter out rows with no data (headings)
    filter(!is.na(age)) %>% 
    mutate(
      country = fix_un_countries(country)
      , age = as.numeric(gsub("\\+", "", age))
      # Change period labels
      # UN calendar year periods are non-exlusive; ie 1950-1955 and 1955-1960
      # The should actually be only 5 yers long: 1950-1954 and 1955-1959
      , year = change_period_labels(year)
      , mx = as.numeric(mx)
      , ax = as.numeric(ax)
    )
  
  
  # 1.1. Parameters ====
  # ~~~~~~~~~~~~~~~~~~~~~
  
  all_years <- as.numeric(unlist(strsplit(unique(lt_5_5$year), "-")))
  
  min_y <- min(all_years)
  max_y <- max(all_years)
  year_range <- min_y:max_y
  
  grouped_years <- unique(lt_5_5$year)
  countries <- unique(lt_5_5$country)
  
  grouped_ages <- unique(lt_5_5$age)
  
  # Temporary 
  # change if you do chose to extrapolate to older ages
  grouped_ages_extrap <- grouped_ages
  
  # 2. Interpolate ages (mx col) ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Expand abridged life table to create a full life table
  # this is currently done by interpolating between the grouped mx values
  # A more sophisticated approach would expand on qx values (see below)
  
  # First, split tables into a list with country/year combinations
  
  lt_5_5_l <- split(lt_5_5, list(lt_5_5$country, lt_5_5$year))
  
  print("Interpolating ages from the mx column...")
  
  # Note on 20191009:
  # This inerpolates mx values within each age group
  # and returns an expanded df with mx values for ages 0:100
  # It assumes that 5Mx values represent the value
  # for the mid-interval age; ie 5_M_x = 1_M_{(x+2)}
  # (more details inside function)
  mx_5_1 <- expand_LT_age_by_mx_linear(
    l_5_5 = lt_5_5_l
    , grouped_ages = grouped_ages
  )
  
  rm("lt_5_5_l")
  
  # 2.2. Graphic check ====
  
  if(run_graphic_tests){
    
    # Reshpare data first
    
    # Random countries
    country_keep <- countries[sample(1:length(countries), 20)]
    y1 <- unique(lt_per_5_5$year)[seq(1, 30, length.out = 5)]
    y2 <- as.character(unique(mx_5_1$year)[seq(1, 30, length.out = 5)])
    
    # 2.2.2. Compare mx values
    
    # Plot mx only
    mx_new <-
      mx_5_1 %>%
      filter(year %in% y2) %>%
      select(country, year, age, mx) %>%
      mutate(
        year = plyr::mapvalues(year, from = y2, to = y1)
        , country = as.character(country)
        , year = as.character(year)
        , source = "new"
      )
    
    mx_old <-
      lt_per_5_5 %>%
      filter(year %in% y1) %>%
      mutate(
        country = as.character(country)
        , year = as.character(year)
        , mx = as.numeric(mx)
      ) %>%
      select(country, year, age, mx) %>%
      mutate(source = "old")
    
    # Fix ages to show that the grouped ages refer to mid-age-intervals
    mx_old$age[mx_old$age %in% 1:95] <- mx_old$age[mx_old$age %in% 1:95] + 2
    
    df_plot <-
      bind_rows(
        mx_new
        , mx_old
      ) %>%
      filter(country %in% country_keep)
    
    # 2.2.2. Compare mx values
    
    (
      test_5_1_mx <-
        df_plot %>%
        filter(country %in% country_keep[1:5]) %>%
        mutate(val = mx) %>%
        ggplot(aes(x = age, y = val, colour = source, group = source)) +
        geom_point(size = 0.5) +
        scale_y_continuous("log(nmx)",trans='log2') +
        facet_grid( country ~ year, scales = "free")
    )
    
    
    # 2.2.2. Compare ex values
    
    # Now, make sure that inputed values actually make sense compared to the real data
    # IN this case, I need to create life tables to get ex
    
    mx <- mx_5_1
    
    mx$country <- as.character(mx$country)
    mx_l <- split(mx, list(mx$country, mx$year))
    
    lt_l <- lapply(mx_l, function(df) {
      lt <- lt_mx(nmx = df$mx, age = min(df$age):max(df$age), radix = 1E5)
      cbind(
        df[1:nrow(lt) , 1:2]
        , lt
      )
    })
    
    lt_temp <- rbindlist(lt_l, use.names = T) %>% data.frame
    
    lt_new <-
      lt_temp %>%
      filter(year %in% y2) %>%
      select(country, year, age, mx, ex) %>%
      mutate(
        year = plyr::mapvalues(year, from = y2, to = y1)
        , country = as.character(country)
        , year = as.character(year)
        , source = "new"
      )
    
    lt_old <-
      lt_per_5_5 %>%
      filter(year %in% y1) %>%
      mutate(
        country = as.character(country)
        , year = as.character(year)
        , ex = as.numeric(ex)
        , mx = as.numeric(mx)
      ) %>%
      select(country, year, age, mx, ex) %>%
      mutate(source = "old")
    
    df_plot <-
      bind_rows(
        lt_new
        , lt_old
      ) %>%
      filter(country %in% country_keep)
    
    (
      test_5_1_ex <-
        df_plot %>%
        filter(age == 0) %>%
        mutate(val = ex) %>%
        ggplot(aes(x = year, y = val, colour = source, group = source)) +
        labs(y = "life expectancy", x = "birth cohort") +
        geom_line() +
        facet_wrap( ~ country)
    )
    
    # Export tests
    
    ggsave("../../Output/A.6.test_5_1_mx.pdf", test_5_1_mx, height = 15, width = 25, units = "cm")
    ggsave("../../Output/A.6.test_5_1_ex.pdf", test_5_1_ex, height = 15, width = 25, units = "cm")
    
    rm("mx_l")
    rm("lt_l")
    rm("tl_temp")
    rm("df_plot")
    
  }
  
  # 3. Interpolate calendar years  ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # This are qx per country/period
  # However, they are still grouped by 5-year calendar years
  # Therefore, we need to smooth these qx values over the 5 calendar years before 
  # gettin gthe values for the life tables
  
  print("Interpolating calendar years...")
  
  # Preferred option 20190628
  # Takes around 5-10 minutes
  # Spline is prefered as method since it gives more accurate values
  mx_1_1 <- expand_LT_year_by_mx(
    df_5_1 = mx_5_1
    , method = "spline"
    , parallel = parallel
    , numCores = numCores
  )
  
  print("mx_1_1 df created")
  rm("mx_5_1")
  
  # 3.2. Graphic checks ====
  
  if(run_graphic_tests){
    
    # Reshpare data first
    
    # Random countries
    country_keep <- countries[sample(1:length(countries), 20)]
    y1 <- unique(lt_per_5_5$year)[seq(1, 30, length.out = 5)]
    y2 <- as.character(unique(mx_1_1$year)[seq(1, 30, length.out = 5)])
    age_keep <- c(0, 5, 75 , 100)
    
    # 2.2.2. Compare mx values
    
    # Plot mx only
    mx_new <-
      mx_1_1 %>%
      filter(age %in% age_keep) %>%
      select(country, year, age, mx) %>%
      mutate(
        country = as.character(country)
        , year = as.character(year)
        , source = "new"
      )
    
    mx_old <-
      lt_per_5_5 %>%
      filter(age %in% age_keep) %>%
      mutate(
        country = as.character(country)
        , year = str_extract(year, "^[0-9]{4}")
        , mx = as.numeric(mx)
      ) %>%
      select(country, year, age, mx) %>%
      mutate(source = "old")
    
    df_plot <-
      bind_rows(
        mx_new
        , mx_old
      ) 
    
    # 2.2.2. Compare mx values
    
    (
      test_1_1_mx <-
        df_plot %>%
        filter(country %in% country_keep[1:4]) %>%
        mutate(val = mx) %>%
        ggplot(aes(x = year, y = val, colour = source, group = source)) +
        geom_point(size = 1.5) +
        scale_y_continuous("log(nmx)",trans='log2') +
        facet_grid( country ~ age, scales = "free") +
        theme_bw()
    )
    
  }
  
  # 4. Create period life tables from mx matrices----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Now I have a data frame with all mx values for every country/year combination
  # the next step is to create life tables for each of these combinations
  
  print("Creating life tables from new mx column...")
  
  mx_1_1_l <- split(mx_1_1, list(mx_1_1$country, mx_1_1$year))
  
if(parallel) {
  # Preferred option 20190628
  # Parallel version
  lt_1_1_l <- lt_mx_parallel(
    l_mx_1_1_l = mx_1_1_l
    , numCores = numCores
  )
} else {
  # non-parallel verion
  lt_1_1_l <- lapply(mx_1_1_l, function(df) {
    lt <- lt_mx(nmx = df$mx, age = 0:100, radix = 1E5)
    cbind(
      df[1:nrow(lt) , 1:2]
      , lt
    )
  })
}

  # sAVE AS DF
  lt_1_1 <- data.frame( rbindlist(lt_1_1_l, use.names = T))
  
  rm("mx_1_1_l")
  rm("lt_1_1_l")
  gc()
  
  # 4.2. Final edits ====
  
  # Set ax columns value for for upper age group
  # Currently it is NA, set to 1/qx
  
  lt_1_1$ax[lt_1_1$age == 100] <- 1/lt_1_1$mx[lt_1_1$age == 100]
  
  print("1x1 life tables created for all countries")
  
  # View(lt_1_1[1:500,])
  
  # 6. Graphic checks ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(run_graphic_tests){
    # Now, make sure that inputed values actually make sense compared to the real data
    # Reshpare data first 
    
    # Random countries
    country_keep <- countries[sample(1:length(countries), 20)][1:4]
    age_keep <- c(0, 5, 75 , 100)
    
    lt_new <- 
      lt_1_1 %>% 
      dplyr::mutate(year = year - 3) %>% 
      # filter(year_lagged %in% seq(min_y, max_y, 5)) %>% 
      filter(age %in% age_keep) %>% 
      select(country, year, age, mx, ex) %>% 
      mutate(
        year = as.numeric(year)
        , source = "new"
      )
    
    lt_old <- 
      lt_per_5_5 %>% 
      filter(age %in% age_keep) %>% 
      mutate(
        year = as.numeric(str_extract(year, "^[0-9]{4}"))
        , country = as.character(country)
        , ex = as.numeric(ex)
        , mx = as.numeric(mx)
      ) %>% 
      select(country, year, age, mx, ex) %>% 
      mutate(source = "old")
    
    df_plot <- 
      bind_rows(
        lt_new
        , lt_old
      ) %>% 
      filter(country %in% country_keep)
    
    # 6.1. Compare mx values ====
    
    (
      test_1_1_mx <- 
        df_plot %>%
        mutate(val = mx) %>%
        ggplot(aes(x = year, y = val, colour = source, group = source)) +
        geom_point(size = 0.5) +
        scale_y_continuous("log(nmx)",trans='log2') +
        facet_grid( country ~ age, scales = "free") +
        theme_bw()
    )
    
    # 6.2. Compare life expectancy =====
    
    # Pending
    
    (
      test_1_1_ex <- 
        df_plot %>% 
        filter(age == 0) %>%
        mutate(val = ex) %>% 
        ggplot(aes(x = year, y = val, colour = source, group = source)) +
        geom_line() +
        facet_wrap( ~ country)
    )
    
    # Export tests
    
    ggsave("output/test_1_1_mx.pdf", test_1_1_mx)
    # ggsave("output/test_1_1_ex.pdf", test_1_1_ex)
    
  }
  
  # 7. Export ----
  # ~~~~~~~~~~~~~~~~~~
  
  if(export) {
    print("Saving to csv file...")
    file <- paste0("../../Data/derived/", "lt_per_1_1_", sex,".csv")
    write.csv(x = lt_1_1, file = file, row.names = F)
    
    print(paste("All period life tables saved to", file))
    
  }  
  
  return(lt_1_1)
  
}

# 2. Run with female life tables ----

numCores <- ifelse(detectCores() > 8, 25, detectCores())

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print('6.1 - Expanding life tables for women.')
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

allowed_types <- c("Country", "SDG region")

lt_per_F_small <- lt_per_F %>% 
  filter(Type %in% c("Country", "SDG region"))

# The function does not return a value
# but exports lt_1_1_sex to the output folder
lt_1_1_F <- 
  ungroup_mortality_from_mx(
  lt_per = lt_per_F_small
  , sex = "F"
  , parallel = T
  , numCores = numCores
  , export = T
  , run_graphic_tests = F
)

# 3. Run with both-sexes life tables ----

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print('6.2 - Expanding life tables for both sexes.')
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

lt_per_B_small <- lt_per_B %>% 
  filter(Type %in% c("Country", "SDG region"))

lt_1_1_B <- 
  ungroup_mortality_from_mx(
  lt_per = lt_per_B_small
  , sex = "B"
  , parallel = T
  , numCores = numCores
  , export = T
  , run_graphic_tests = F
)
