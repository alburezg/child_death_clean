# Compare ex to original

lt_per <- lt_per_F_small

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

or <- lt_5_5 %>% 
  arrange(country, year) %>% 
  filter(age == 100) %>% 
  filter(country == "niger") %>% 
  select(country, year, age, ex_or = ex)

new <- lt_1_1_F %>% 
  arrange(country, year) %>% 
  filter(age == 100) %>% 
  filter(country == "niger") %>% 
  select(country, year, age, ex_new = ex)

View(or)
View(new)

sum(new$ex_new)

# Check niger ====

df <- mx_1_1 %>% 
  filter(country == "niger") %>% 
  filter(year == 1950)

lt <- lt_mx(nmx = df$mx, age = 0:100, radix = 1E5)
lt$ex6 <- lt_mx(nmx = df$mx, age = 0:100, radix = 1E6)$ex
lt$ex7 <- lt_mx(nmx = df$mx, age = 0:100, radix = 1E7)$ex

View(lt)

# Denmark ====

df <- lt_1_1_F %>% 
  filter(country == "denmark")

View(df)

df <- LTC_df %>% filter(Country == "denmark")
