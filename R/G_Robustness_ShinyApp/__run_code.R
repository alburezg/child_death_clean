

# *~^**~^**~^**~^**~^**~^**~^*#
# Code by                     #
# Diego Alburez-Gutierrez     #
# gatemonte@gmail.com         #
# @d_alburez                  #
# unless stated otherwise.    #
# Last edited 20200110        #
# GNU GENERAL PUBLIC LICENSE  #
# Version 3, 29 June 2007     #
# *~^**~^**~^**~^**~^**~^**~^*#


# This should be a self-contained code that takes as input UNWPP rates downloaded
# directly from the website and performs robustness checks for the main outcome of
# the study: cumulative child death for a woman surviving to age a
# I want to do this for a single country under three fertility assumptions
# and three mortality assumptions. The plot should look like this:
# Rows (mortality assumptions): Normal, 80 PI (prediction interval), and constant
# cols (fert): low, medium, high, constant
# constant/constant is stable population

library(parallel)

# country_keep <- c("Guatemala", "Zimbabwe")
baseline_year_constant_rates <- "2000-2005"
numCores <- ifelse(detectCores() > 8, 25, 3)
re_estimate_matrix_of_survival_probs <- F

country_keep <- c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya", 
                  "Madagascar", "Malawi", "Mauritius", "Mayotte", "Mozambique", 
                  "Réunion", "Rwanda", "Seychelles", "Somalia", "South Sudan", 
                  "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", 
                  "Angola", "Cameroon", "Central African Republic", "Chad", "Congo", 
                  "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", 
                  "Sao Tome and Principe", "Botswana", "Eswatini", "Lesotho", "Namibia", 
                  "South Africa", "Benin", "Burkina Faso", "Cabo Verde", "Côte d'Ivoire", 
                  "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", 
                  "Mauritania", "Niger", "Nigeria", "Senegal", "Sierra Leone", 
                  "Togo", "Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia", 
                  "Western Sahara", "Armenia", "Azerbaijan", "Bahrain", "Cyprus", 
                  "Georgia", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Oman", 
                  "Qatar", "Saudi Arabia", "State of Palestine", "Syrian Arab Republic", 
                  "Turkey", "United Arab Emirates", "Yemen", "Kazakhstan", "Kyrgyzstan", 
                  "Tajikistan", "Turkmenistan", "Uzbekistan", "Afghanistan", "Bangladesh", 
                  "Bhutan", "India", "Iran (Islamic Republic of)", "Maldives", 
                  "Nepal", "Pakistan", "Sri Lanka", "China", "China, Hong Kong SAR", 
                  "China, Macao SAR", "China, Taiwan Province of China", "Dem. People's Republic of Korea", 
                  "Japan", "Mongolia", "Republic of Korea", "Brunei Darussalam", 
                  "Cambodia", "Indonesia", "Lao People's Democratic Republic", 
                  "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", 
                  "Timor-Leste", "Viet Nam", "Antigua and Barbuda", "Aruba", "Bahamas", 
                  "Barbados", "Cuba", "Curaçao", "Dominican Republic", "Grenada", 
                  "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Puerto Rico", 
                  "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago", 
                  "United States Virgin Islands", "Belize", "Costa Rica", "El Salvador", 
                  "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Argentina", 
                  "Bolivia (Plurinational State of)", "Brazil", "Chile", "Colombia", 
                  "Ecuador", "French Guiana", "Guyana", "Paraguay", "Peru", "Suriname", 
                  "Uruguay", "Venezuela (Bolivarian Republic of)", "Australia", 
                  "New Zealand", "Fiji", "New Caledonia", "Papua New Guinea", "Solomon Islands", 
                  "Vanuatu", "Guam", "Kiribati", "Micronesia (Fed. States of)", 
                  "French Polynesia", "Samoa", "Tonga", "Belarus", "Bulgaria", 
                  "Czechia", "Hungary", "Poland", "Republic of Moldova", "Romania", 
                  "Russian Federation", "Slovakia", "Ukraine"
                  # , "Channel Islands"
                  , "Denmark", "Estonia", "Finland", "Iceland", "Ireland", "Latvia", 
                  "Lithuania", "Norway", "Sweden", "United Kingdom", "Albania", 
                  "Bosnia and Herzegovina", "Croatia", "Greece", "Italy", "Malta", 
                  "Montenegro", "North Macedonia", "Portugal", "Serbia", "Slovenia", 
                  "Spain", "Austria", "Belgium", "France", "Germany", "Luxembourg", 
                  "Netherlands", "Switzerland", "Canada", "United States of America"
)


country_iso_keep <- countrycode(country_keep, origin = "country.name", destination = "iso3c")

(files <- list.files(pattern = ".R$")[-1])

# A. Expand fertility and mortality rates ----------------

# 1. Load the functions and packages needed in the scripts ====

source(files[1])

# 2. Load the data needed for the analysis ====

source(files[2])

# 3. Format fertility rates ----

source(files[3])

# 4. Format Mortality ====

# Using the interpolated values from the previous script, we approximate cohort fertility 
# by taking values on the diagonal. 
# For example, for the 1950 "cohort", 1_f_15 would be based on fertility rates for 1965, 
# the 1_f_16 for the period data from 1966, etc.

source(files[4])

# B. Analysis -------------

# Estimate child death
source(files[4])

# Plot comparisson
source(files[5])