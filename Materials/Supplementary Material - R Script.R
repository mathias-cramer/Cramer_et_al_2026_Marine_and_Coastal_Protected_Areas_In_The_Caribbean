#_____________________________________________________________________________:

#R Script for Cramer et al. 2026: Marine and coastal protected areas in the 
#Caribbean: Numbers, areal extent, distributions, no-take status, and management 
#attributes

#https://doi.org/10.1016/j.marpol.2026.107140

#By: Dr. Mathias Cramer, FWC FWRI

#_____________________________________________________________________________:

# Load necessary libraries

library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(sf)
library(writexl)
library(viridis)

#_____________________________________________________________________________:

# Data Cleaning_______________________________________________________________:

# Step 1: Data Loading
setwd("~/WDPA/Collaborator Revisions/Final Revisions/Documents to Submit")
wdpa_data <- read.csv("Supplementary Material - ArcGIS_Marine_Area_by_MPA.csv")
print(nrow(wdpa_data))

# Step 2: Remove unnecessary columns and clean column names
names(wdpa_data)
wdpa_data = subset(wdpa_data, select = -c(OID_, FIRST_WDPAID, 
                                          Shape_Length, Shape_Area))
names(wdpa_data)

colnames(wdpa_data) <- c(
  "WDPAID", "NAME", "IUCN_CAT", "MARINE", "NO_TAKE", "NO_TK_AREA", "STATUS", 
  "STATUS_YR", "GOV_TYPE", "MANG_PLAN", "ISO3", "GIS_TOTAL_AREA"
)
names(wdpa_data)

# Step 3: Ensure that the necessary columns are in the correct data types
wdpa_data <- wdpa_data %>%
  mutate(
    WDPAID = as.factor(WDPAID),
    IUCN_CAT = as.factor(IUCN_CAT),    
    MARINE = as.factor(MARINE), 
    NO_TAKE = as.factor(NO_TAKE),
    STATUS = as.factor(STATUS),
    GOV_TYPE = as.factor(GOV_TYPE),
    MANG_PLAN = as.factor(MANG_PLAN),
    ISO3 = as.factor(ISO3)
  )

# Step 4: Apply and confirm sub-region categorizations. 
# ISO3 BES is spread across two sub-regions, so BES classification needs to occur at the MPA level. 
wdpa_data <- wdpa_data %>%
  mutate(Sub_Region = case_when(
    ISO3 %in% c("BHS", "TCA") ~ "Lucayan Archipelago",
    ISO3 %in% c("VGB", "CYM", "CUB", "DOM", "HTI", "JAM", "PRI", "UMI", "VIR") ~ "Greater Antilles",
    ISO3 %in% c("AIA", "ATG", "BES", "BRB", "BLM", "DMA", "GRD", "GLP", "MTQ", "KNA", "LCA", "MAF", "SXM", "VCT", "BLM;GLP;MAF;MTQ") ~ "Lesser Antilles",
    ISO3 %in% c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN") ~ "Central America",
    ISO3 %in% c("ABW", "COL", "CUW", "TTO", "VEN") ~ "Southern Caribbean",
    
    WDPAID %in% c("68111", "68112", "230", "233", "555703527") ~ "Southern Caribbean",
    WDPAID %in% c("14005", "220029", "555624212") ~ "Lesser Antilles",
    TRUE ~ NA_character_
  ))

wdpa_data %>%
  select(ISO3, Sub_Region) %>%
  distinct()

# Step 5: Confirm data set only includes marine or coastal MPAs
levels(wdpa_data$MARINE)

# Step 6: Confirm data set only includes Caribbean countries and territories
levels(wdpa_data$ISO3)

# Step 7: Confirm data set only includes designated or established MPAs
levels(wdpa_data$STATUS)

# Step 8: Confirm data set does not contain duplicates (according to WDPAID)
wdpa_data %>%
  filter(WDPAID %in% WDPAID[duplicated(WDPAID)])

# Step 9: Separate WDPAID 555587040 (Agoa) into its individual territories: BLM, GLP, MAF, MTQ. 
# The MPA covers the entire MTs all four territories.

# Filter the row to be split
split_row <- wdpa_data[wdpa_data$WDPAID == 555587040, ]

# Define the new ISO3 codes and GIS_TOTAL_AREA values 
# Values calculated proportionally to the four territories' terrestrial areas.
# See here for general source: https://maritimelimits.gouv.fr/sites/default/files/2023-03/Superficies_espaces_maritimes_En_230126.pdf?
territories <- data.frame(
  ISO3 = c("BLM", "GLP", "MAF", "MTQ"),
  GIS_TOTAL_AREA = c(4184.45, 90823.38, 1101.44, 47849.37)
)

# Duplicate the row for each territory and adjust the values
split_rows <- lapply(1:nrow(territories), function(i) {
  new_row <- split_row
  new_row$ISO3 <- territories$ISO3[i]
  new_row$GIS_TOTAL_AREA <- territories$GIS_TOTAL_AREA[i]
  return(new_row)
})

# Combine the modified rows into a new dataframe
split_rows_df <- do.call(rbind, split_rows)

# Remove the original row and add the new ones to the dataset
wdpa_split <- rbind(wdpa_data[wdpa_data$WDPAID != 555587040, ], split_rows_df)
wdpa_unsplit <- wdpa_data

print(nrow(wdpa_split))
print(nrow(wdpa_unsplit))

# Export Cleaned Dataset
#write.csv(wdpa_data, "WDPA_ArcGIS_R_Data.csv", row.names = FALSE)

#_____________________________________________________________________________:

# Data Visualization (minus Areal Extents)____________________________________: 

wdpa_unsplit <- read.csv("Supplementary Material - WDPA_ArcGIS_R_Data.csv")
wdpa_split$ISO3 <- as.character(wdpa_split$ISO3)

# Define a color palette
my_palette <- plasma(12)

# Trends in MPA Establishment_________________________________________________: 
# Step 1: Handle erroneous years (replace 0s with NAs)
wdpa_unsplit <- wdpa_unsplit %>%
  mutate(STATUS_YR = ifelse(STATUS_YR == 0, NA, STATUS_YR)) 

# Step 2: Create a plot to visualize the trend
trend_est_plot <- ggplot(na.omit(wdpa_unsplit), aes(x = STATUS_YR)) +
  geom_histogram(binwidth = 1, fill = my_palette[1], color = "black", alpha = 0.7) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE), 
                     breaks = seq(1900, max(wdpa_unsplit$STATUS_YR, na.rm = TRUE), by = 10)) +
  labs(title = "", 
       x = "Year of Designation", 
       y = "Number of MPAs") +
  theme_classic() +
  theme(text = element_text(size = 14))

# MPA Numbers by Sub-Region and Country_______________________________________:
# Step 1: Create sub-region count data to order sub-regions by the total number of MPAs (Lesser Antilles reduced by 3 due to Agoa split)
mpa_count_by_sub_region <- wdpa_split %>%
  group_by(Sub_Region) %>%
  summarise(Sub_Region_Count = n_distinct(WDPAID), .groups = "drop") %>%
  mutate(
    Sub_Region_Count = if_else(Sub_Region == "Lesser Antilles", Sub_Region_Count - 3, Sub_Region_Count),
    Level = "Sub-Region",
    ISO3 = Sub_Region
  )

# Step 2: Order sub-regions by count in descending order
sub_region_order <- mpa_count_by_sub_region %>%
  arrange(desc(Sub_Region_Count)) %>%
  pull(Sub_Region)

# Step 3: Create country-level data
mpa_count_by_country <- wdpa_split %>%
  group_by(ISO3, Sub_Region) %>%
  summarise(Country_Count = n_distinct(WDPAID), .groups = "drop") %>%
  mutate(Level = "Country")

# Step 4: Combine sub-region and country data
mpa_combined <- bind_rows(
  mpa_count_by_sub_region %>% rename(Country_Count = Sub_Region_Count),
  mpa_count_by_country
)

# Step 5: Create sub-region colors
sub_region_colors <- setNames(my_palette, sub_region_order)

# Step 6: Generate country colors as shades of sub-region colors
country_colors <- mpa_count_by_country %>%
  group_by(Sub_Region) %>%
  mutate(
    Color = sub_region_colors[Sub_Region]
  ) %>%
  ungroup() %>%
  select(ISO3, Color)

# Step 7: Combine sub-region and country colors
fill_colors <- c(
  setNames(sub_region_colors, sub_region_order),
  setNames(country_colors$Color, country_colors$ISO3)
)

# Step 8: Order x-axis by sub-region count (descending), followed by countries (descending)
country_order <- mpa_count_by_country %>%
  arrange(factor(Sub_Region, levels = sub_region_order), desc(Country_Count)) %>%
  pull(ISO3)

x_order <- c(sub_region_order, "", country_order)

# Step 9: Create a vector that maps ISO3 codes to full country names
country_name_mapping <- c(
  "ABW" = "Aruba",
  "AIA" = "Anguilla",
  "ATG" = "Antigua and Barbuda",
  "BES" = "Bonaire, Sint Eustatius and Saba",
  "BHS" = "Bahamas",
  "BLM" = "Saint Barthelemy",
  "BLZ" = "Belize",
  "BRB" = "Barbados",
  "COL" = "Colombia",
  "CRI" = "Costa Rica",
  "CUB" = "Cuba",
  "CUW" = "Curacao",
  "CYM" = "Cayman Islands",
  "DMA" = "Dominica",
  "DOM" = "Dominican Republic",
  "GLP" = "Guadeloupe",
  "GRD" = "Grenada",
  "GTM" = "Guatemala",
  "HND" = "Honduras",
  "HTI" = "Haiti",
  "JAM" = "Jamaica",
  "KNA" = "Saint Kitts and Nevis",
  "LCA" = "Saint Lucia",
  "MAF" = "Saint Martin",
  "MEX" = "Mexico",
  "MSR" = "Montserrat",
  "MTQ" = "Martinique",
  "NIC" = "Nicaragua",
  "PAN" = "Panama",
  "PRI" = "Puerto Rico",
  "SXM" = "Sint Maarten",
  "TCA" = "Turks and Caicos",
  "TTO" = "Trinidad and Tobago",
  "VCT" = "Saint Vincent and the Grenadines",
  "UMI" = "Navassa Island",
  "VEN" = "Venezuela",
  "VGB" = "British Virgin Islands",
  "VIR" = "US Virgin Islands"
)

# Step 10: Create plot
mpa_number_plot <- ggplot(mpa_combined) +
  geom_bar(
    aes(x = ISO3, y = Country_Count, fill = ISO3),
    stat = "identity",
    color = "black",
    width = 0.7
  ) +
  scale_fill_manual(values = fill_colors) +
  scale_x_discrete(
    limits = x_order,
    labels = function(x) {
      sapply(x, function(val) {
        if (val == "" || val %in% sub_region_order) {
          return(val)
        } else {
          return(country_name_mapping[val])
        }
      })
    }
  ) +
  labs(
    title = "",
    x = "Subregion or Nation",
    y = "Number of MPAs"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma)

# NTA Numbers by Sub-Region and Country_______________________________________:
# Step 1: Filter data for no-take MPAs and create sub-region and country counts (Agoa no-take status is not reported, so modification not required)
no_take_mpa_count_by_sub_region <- wdpa_split %>%
  filter(NO_TAKE %in% c("All", "Part")) %>%
  group_by(Sub_Region) %>%
  summarise(Sub_Region_NoTakeCount = n_distinct(WDPAID), .groups = "drop") %>%
  mutate(Level = "Sub-Region", ISO3 = Sub_Region)

no_take_mpa_count_by_country <- wdpa_split %>%
  filter(NO_TAKE %in% c("All", "Part")) %>%
  group_by(ISO3, Sub_Region) %>%
  summarise(Country_NoTakeCount = n_distinct(WDPAID), .groups = "drop") %>%
  mutate(Level = "Country")

# Step 2: Combine sub-region and country data
no_take_mpa_combined <- bind_rows(
  no_take_mpa_count_by_sub_region %>% rename(Country_Count = Sub_Region_NoTakeCount),
  no_take_mpa_count_by_country %>% rename(Country_Count = Country_NoTakeCount)
)

# Step 3: Order sub-regions and countries by descending counts
sub_region_order_no_take <- no_take_mpa_count_by_sub_region %>%
  arrange(desc(Sub_Region_NoTakeCount)) %>%
  pull(Sub_Region)

country_order_no_take <- no_take_mpa_count_by_country %>%
  arrange(factor(Sub_Region, levels = sub_region_order_no_take), desc(Country_NoTakeCount)) %>%
  pull(ISO3)

x_order_no_take <- c(sub_region_order_no_take, "", country_order_no_take)

# Step 4: Reuse colors for sub-regions and generate shades for countries
sub_region_colors_no_take <- sub_region_colors
country_colors_no_take <- no_take_mpa_count_by_country %>%
  group_by(Sub_Region) %>%
  mutate(Color = sub_region_colors_no_take[Sub_Region]) %>%
  ungroup() %>%
  select(ISO3, Color)

fill_colors_no_take <- c(
  sub_region_colors_no_take,
  setNames(country_colors_no_take$Color, country_colors_no_take$ISO3)
)

# Step 5: Create the no-take MPA plot
nta_number_plot <- ggplot(no_take_mpa_combined) +
  geom_bar(
    aes(x = ISO3, y = Country_Count, fill = ISO3),
    stat = "identity",
    color = "black",
    width = 0.7
  ) +
  scale_fill_manual(values = fill_colors_no_take) +
  scale_x_discrete(
    limits = x_order_no_take,
    labels = function(x) {
      sapply(x, function(val) {
        if (val == "" || val %in% sub_region_order_no_take) {
          return(val)
        } else {
          return(country_name_mapping[val])
        }
      })
    }
  ) +
  labs(
    title = "",
    x = "Subregion or Nation",
    y = "Number of No-Take MPAs"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma)

# MPA Size____________________________________________________________________:
# Step 1: Handle erroneous MPA sizes (NAs or <0)
total_rows_before <- nrow(wdpa_unsplit)

na_marine_area <- wdpa_unsplit %>%
  filter(is.na(GIS_TOTAL_AREA)) %>%
  nrow()

invalid_value_marine_area <- wdpa_unsplit %>%
  filter(GIS_TOTAL_AREA <= 0) %>%
  nrow()

invalid_marine_area <- na_marine_area + invalid_value_marine_area

valid_marine_area <- total_rows_before - invalid_marine_area

cat("Total number of rows in dataset:", total_rows_before, "\n")
cat("Number of rows removed due to NA values for GIS_TOTAL_AREA:", na_marine_area, "\n")
cat("Number of rows removed due to GIS_TOTAL_AREA <= 0:", invalid_value_marine_area, "\n")
cat("Total number of rows removed:", invalid_marine_area, "\n")
cat("Number of valid rows (after filtering):", valid_marine_area, "\n")

# Step 2: Filter data to only include valid marine areas (GIS_TOTAL_AREA > 0)
valid_mpa_data <- wdpa_unsplit %>%
  filter(MARINE %in% c(1, 2), !is.na(GIS_TOTAL_AREA), GIS_TOTAL_AREA > 0)

# Step 3: Create histogram for the size distribution of valid marine areas (GIS_TOTAL_AREA)
mpa_size_plot <- ggplot(valid_mpa_data, aes(x = GIS_TOTAL_AREA)) +
  geom_histogram(binwidth = 100, fill = my_palette[1], color = "black") +
  scale_x_continuous(labels = scales::comma,
                     breaks = seq(0, max(valid_mpa_data$GIS_TOTAL_AREA), by = 25000)) +
  labs(title = "", 
       x = "Area (sq. km)", 
       y = "Number of MPAs") +
  theme_classic() +
  theme(text = element_text(size = 14))

# MPA IUCN Categories_________________________________________________________:
# Define the custom order for IUCN categories
category_order <- c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Applicable", "Not Reported")

# Create the frequency table of MPAs per IUCN category
iucn_count_df <- as.data.frame(table(wdpa_unsplit$IUCN_CAT))
colnames(iucn_count_df) <- c("IUCN_CAT", "count")

# Remove rows where IUCN_CAT is NA or not part of category_order
iucn_count_df <- iucn_count_df[!is.na(iucn_count_df$IUCN_CAT), ]
iucn_count_df <- iucn_count_df[iucn_count_df$IUCN_CAT %in% category_order, ]

# Convert the IUCN_CAT column to a factor with the specified order
iucn_count_df$IUCN_CAT <- factor(iucn_count_df$IUCN_CAT, levels = category_order)

# Plot the data
iucn_plot <- ggplot(iucn_count_df, aes(x = IUCN_CAT, y = count, fill = IUCN_CAT)) +
  geom_bar(stat = "identity", color = "black", width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "",
    x = "IUCN Classification",
    y = "Number of MPAs"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Governance Types____________________________________________________________:
# Step 1: Count occurrences of each GOV_TYPE
gov_type_count_df <- as.data.frame(table(wdpa_unsplit$GOV_TYPE))

# Rename the columns for clarity
colnames(gov_type_count_df) <- c("GOV_TYPE", "count")

# Step 2: Remove GOV_TYPE categories with a count of 0
gov_type_count_df <- gov_type_count_df %>%
  filter(count > 0)

# Define the custom order for GOV_TYPE
gov_type_order <- c("Not Reported", 
                    "Collaborative governance", 
                    "Federal or national ministry or agency", 
                    "Government-delegated management", 
                    "Indigenous peoples", 
                    "Joint governance", 
                    "Local communities", 
                    "Non-profit organisations", 
                    "Sub-national ministry or agency")

# Step 3: Convert GOV_TYPE to a factor with the specified order
gov_type_count_df$GOV_TYPE <- factor(gov_type_count_df$GOV_TYPE, levels = gov_type_order)

# Step 4: Plot the data
gov_plot <- ggplot(gov_type_count_df, aes(x = GOV_TYPE, y = count, fill = GOV_TYPE)) +
  geom_bar(stat = "identity", color = "black", width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "",
    x = "Governance Type",
    y = "Number of MPAs"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Management Plans____________________________________________________________:
# Step 1: Define your management plan categories
categorize_management_plan <- function(plan) {
  if (plan %in% c("No", "No Management Plan", 
                  "Management plan is not implemented and not available", 
                  "Management plan is not implented and not available")) {
    return("No Plan")
  } else if (plan %in% c("Draft management plan for wider area incorporating Ramsar site and partially implemented", 
                         "In process")) {
    return("Draft/Development")
  } else if (plan %in% c("Management plan is available but not implemented", 
                         "Management plan is implemented and is available", 
                         "Management plan is implented and available", 
                         "Management plan is not implented but is available", 
                         "Existing", "Exists", "Si", "Site-Specific Management Plan", 
                         "Yes, 2013", "Yes, 2014", "Yes, 2015", "MPA Programmatic Management Plan") || 
             grepl("http|www", plan)) { 
    return("Available")
  } else if (plan == "Not Reported") {
    return("Not Reported")
  } else {
    return(NA)
  }
}

# Step 2: Apply the categorization function to the 'MANG_PLAN' column of your 'wdpa_unsplit' dataset
wdpa_unsplit$Management_Plan_Category <- sapply(wdpa_unsplit$MANG_PLAN, categorize_management_plan)

# Step 3: Count occurrences of each Management_Plan_Category
management_plan_count_df <- as.data.frame(table(wdpa_unsplit$Management_Plan_Category))

# Step 4: Rename the columns for clarity
colnames(management_plan_count_df) <- c("Management_Plan_Category", "count")

# Step 5: Remove categories with a count of 0
management_plan_count_df <- management_plan_count_df %>%
  filter(count > 0)

# Step 6: Define the custom order for Management_Plan_Category
management_plan_order <- c("Available",
                           "Draft/Development",
                           "No Plan",
                           "Not Reported"
                           )

# Step 7: Convert Management_Plan_Category to a factor with the specified order
management_plan_count_df$Management_Plan_Category <- factor(management_plan_count_df$Management_Plan_Category, levels = management_plan_order)

# Step 8: Plot the data
man_plot <- ggplot(management_plan_count_df, aes(x = Management_Plan_Category, y = count, fill = Management_Plan_Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = my_palette) +
  labs(
    title = "",
    x = "Management Plan Status",
    y = "Number of MPAs"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "none"
  )

#_____________________________________________________________________________:

# Data Results (minus Areal Extents)__________________________________________: 

# Calculate Total Number of MPAs
total_mpa_number <- nrow(wdpa_unsplit)
total_mpa_number

# Calculate Total Number of MPAs by Subregion (Sub_Region)
mpa_number_by_region <- wdpa_unsplit %>%
  group_by(Sub_Region) %>%
  summarise(total_mpa_number = n(), .groups = 'drop') %>%
  arrange(Sub_Region)
print(mpa_number_by_region)

# Calculate Total Number of MPAs by Country (ISO3)
mpa_number_by_country <- wdpa_split %>%
  group_by(ISO3) %>%
  summarise(total_mpa_number = n(), .groups = 'drop') #%>%
  #arrange(desc(total_mpa_number))
print(mpa_number_by_country, n = Inf)

# Calculate Total Number of NTAs
total_nta_number <- wdpa_unsplit %>% 
  filter(NO_TAKE %in% c("All", "Part")) %>%
  nrow()
total_nta_number

total_nta_number_comp <- wdpa_unsplit %>%
  filter(NO_TAKE == "All") %>%
  nrow()
total_nta_number_comp

# Calculate Total Number of NTAs by Subregion (Sub_Region)
nta_number_by_region <- wdpa_unsplit %>%
  filter(NO_TAKE %in% c("All", "Part")) %>%
  group_by(Sub_Region) %>%
  summarise(total_nta_number = n(), .groups = 'drop') %>%
  arrange(Sub_Region)
print(nta_number_by_region)

# Calculate Total Number of NTAs by Country (ISO3)
nta_number_by_country <- wdpa_split %>%
  filter(NO_TAKE %in% c("All", "Part")) %>%
  group_by(ISO3) %>%
  summarise(total_nta_number = n(), .groups = 'drop') #%>%
  #arrange(desc(total_nta_number))
print(nta_number_by_country, n = Inf)

# Calculate Number of MPAs with NA or Not Reported NTA Status
sum(is.na(wdpa_unsplit$NO_TAKE))
nta_nr_number <- wdpa_unsplit %>%
  count(NO_TAKE)
nta_nr_number

# Calculate Size Distribution of MPAs
mpa_size_nr_number <- sum(is.na(wdpa_unsplit$GIS_TOTAL_AREA))
mpa_size_nr_number

size_distribution <- wdpa_unsplit %>%
  summarise(
    min_size = min(GIS_TOTAL_AREA, na.rm = TRUE),
    max_size = max(GIS_TOTAL_AREA, na.rm = TRUE),
    mean_size = mean(GIS_TOTAL_AREA, na.rm = TRUE),
    median_size = median(GIS_TOTAL_AREA, na.rm = TRUE),
    q25_size = quantile(GIS_TOTAL_AREA, 0.25, na.rm = TRUE),
    q75_size = quantile(GIS_TOTAL_AREA, 0.75, na.rm = TRUE),
    iqr_size = IQR(GIS_TOTAL_AREA, na.rm = TRUE)
  )
size_distribution

# Count the number of MPAs with GIS_TOTAL_AREA less than 1
size_less_1km <- wdpa_unsplit %>%
  filter(GIS_TOTAL_AREA < 1) %>%
  summarise(count = n())
print(size_less_1km)

# Count the number of MPAs with GIS_TOTAL_AREA less than 100
size_less_100km <- wdpa_unsplit %>%
  filter(GIS_TOTAL_AREA < 100) %>%
  summarise(count = n())
print(size_less_100km)

# Calculate Size Classes and Proportions
tiny_threshold <- 1
very_small_threshold <- 5
small_threshold <- 10
medium_threshold <- 100
large_threshold <- 250
very_large_threshold <- 1000

mpa_size_category <- wdpa_unsplit %>%
  mutate(
    Size_Category = case_when(
      GIS_TOTAL_AREA < tiny_threshold ~ "Extremely Small",
      GIS_TOTAL_AREA >= tiny_threshold & GIS_TOTAL_AREA < very_small_threshold ~ "Very Small",
      GIS_TOTAL_AREA >= very_small_threshold & GIS_TOTAL_AREA < small_threshold ~ "Small",
      GIS_TOTAL_AREA >= small_threshold & GIS_TOTAL_AREA < medium_threshold ~ "Intermediate",
      GIS_TOTAL_AREA >= medium_threshold & GIS_TOTAL_AREA < large_threshold ~ "Large",
      GIS_TOTAL_AREA >= large_threshold & GIS_TOTAL_AREA < very_large_threshold ~ "Very Large",
      GIS_TOTAL_AREA >= very_large_threshold ~ "Extremely Large",
      TRUE ~ NA_character_
    )
  )

size_proportions <- mpa_size_category %>%
  group_by(Size_Category) %>%
  summarise(Proportion = round(n() / nrow(mpa_size_category), 3), .groups = "drop") %>%
  mutate(Proportion = sprintf("%.5f", Proportion))
print(size_proportions)

# IUCN Categories Summary
iucn_summary <- wdpa_unsplit %>%
  group_by(IUCN_CAT) %>%
  summarise(count = n())
iucn_summary

# Governance Types Summary
gov_type_summary <- wdpa_unsplit %>%
  group_by(GOV_TYPE) %>%
  summarise(count = n())
gov_type_summary

# Management Plan Summary
management_plan_count_df

#_____________________________________________________________________________:

# Areal Extent Analysis_______________________________________________________:

# Data Cleaning_______________________________________________________________:

# Step 1: Data Loading
mpa_data <- read.csv("Supplementary Material - Marine_Area_by_ISO3.csv")
nta_data <- read.csv("Supplementary Material - No_Take_Area_by_ISO3_NTA.csv")

# Step 2: Remove unnecessary columns and clean column names
names(mpa_data)
names(nta_data)
mpa_data = subset(mpa_data, select = -c(OID_, WDPAID, FIRST_WDPAID, FIRST_NAME, 
                                        FIRST_IUCN_CAT, FIRST_MARINE, FIRST_NO_TAKE,
                                        SUM_NO_TK_AREA, FIRST_STATUS, FIRST_STATUS_YR,
                                        FIRST_GOV_TYPE, FIRST_MANG_PLAN, Shape_Length, 
                                        Shape_Area))
nta_data = subset(nta_data, select = -c(OID_, Shape_Length, Shape_Area))

colnames(mpa_data) <- c(
  "ISO3", "GIS_AREA"
)
colnames(nta_data) <- c(
  "ISO3",   "GIS_AREA"
)

# Step 3: Apply subregion categorization
mpa_data <- mpa_data %>%
  mutate(Sub_Region = case_when(
    ISO3 %in% c("BHS", "TCA") ~ "Lucayan Archipelago",
    ISO3 %in% c("VGB", "CYM", "CUB", "DOM", "HTI", "JAM", "PRI", "UMI", "VIR") ~ "Greater Antilles",
    ISO3 %in% c("AIA", "ATG", "BES", "BRB", "BLM", "DMA", "GRD", "GLP", "MTQ", "KNA", "LCA", "MAF", "SXM", "VCT", "BLM;GLP;MAF;MTQ") ~ "Lesser Antilles",
    ISO3 %in% c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN") ~ "Central America",
    ISO3 %in% c("ABW", "COL", "CUW", "TTO", "VEN") ~ "Southern Caribbean",
    TRUE ~ NA_character_
  ))

nta_data <- nta_data %>%
  mutate(Sub_Region = case_when(
    ISO3 %in% c("BHS", "TCA") ~ "Lucayan Archipelago",
    ISO3 %in% c("VGB", "CYM", "CUB", "DOM", "HTI", "JAM", "PRI", "UMI", "VIR") ~ "Greater Antilles",
    ISO3 %in% c("AIA", "ATG", "BES", "BRB", "BLM", "DMA", "GRD", "GLP", "MTQ", "KNA", "LCA", "MAF", "SXM", "VCT", "BLM;GLP;MAF;MTQ") ~ "Lesser Antilles",
    ISO3 %in% c("BLZ", "CRI", "GTM", "HND", "MEX", "NIC", "PAN") ~ "Central America",
    ISO3 %in% c("ABW", "COL", "CUW", "TTO", "VEN") ~ "Southern Caribbean",
    TRUE ~ NA_character_
  ))

# Step 4: The Agoa MPA (WDPAID 555587040; ISO3 = BLM;GLP;MAF;MTQ) covers the entire 
# MTs of all four Dutch Caribbean territories (BLM, GLP, MAF, and MTQ). Therefore, 
# delete the individual entry, and adjust the individual territory areal extents. 

# The Agoa MPA's no-take status is not reported, so it does not appear in nta_data.
# So, these modifications only need to be made in mpa_data.

# Filter out the Agoa MPA (identified by ISO3 = "BLM;GLP;MAF;MTQ") from mpa_data
mpa_data <- mpa_data %>%
  filter(ISO3 != "BLM;GLP;MAF;MTQ")

# Define the updated MT areas for each territory (in km2)
# Values calculated by dividing Agoa MPA size proportionally to the four territories' areas. 

# Create a data frame for the territory areas
territory_areas_df <- data.frame(
  ISO3 = c("BLM", "GLP", "MAF", "MTQ"),
  NEW_GIS_AREA = c(4184.45, 90823.38, 1101.44, 47849.37)
)

# Perform a left join with mpa_data to bring in the new areas
mpa_data <- mpa_data %>%
  left_join(territory_areas_df, by = "ISO3") %>%
  mutate(
    GIS_AREA = ifelse(!is.na(NEW_GIS_AREA), NEW_GIS_AREA, GIS_AREA)
  ) %>%
  select(-NEW_GIS_AREA)

# Data Results________________________________________________________________:

# Calculate Areal Extent of all MPAs
mpa_area <- sum(mpa_data$GIS_AREA, na.rm = TRUE)
mpa_area

# Calculate Areal Extent of all MPAs by Sub-Region
mpa_area_by_region <- mpa_data %>%
  group_by(Sub_Region) %>%
  summarize(
    AREA_SUM = sum(GIS_AREA, na.rm = TRUE)
  )
print(mpa_area_by_region)

# Calculate Areal Extent of all MPAs by ISO3
mpa_area_by_country <- mpa_data %>%
  group_by(ISO3) %>%
  summarize(
    AREA_SUM = sum(GIS_AREA, na.rm = TRUE)
  )
print(mpa_area_by_country, n = Inf)

# Calculate Areal Extent of all NTAs
nta_area <- sum(nta_data$GIS_AREA, na.rm = TRUE)
nta_area

# Calculate Areal Extent of all NTAs by Sub-Region
nta_area_by_region <- nta_data %>%
  group_by(Sub_Region) %>%
  summarize(
    AREA_SUM = sum(GIS_AREA, na.rm = TRUE)
  )
print(nta_area_by_region)

# Calculate Areal Extent of all NTAs by ISO3
nta_area_by_country <- nta_data %>%
  group_by(ISO3) %>%
  summarize(
    AREA_SUM = sum(GIS_AREA, na.rm = TRUE)
  )
print(nta_area_by_country, n = Inf)

# Data Visualization__________________________________________________________:

# MPA Areal Extent by Sub-Region and Country__________________________________:

# Step 1: Aggregate sub-region MPA area
mpa_area_by_sub_region <- mpa_data %>%
  group_by(Sub_Region) %>%
  summarise(Sub_Region_Area = sum(GIS_AREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Level = "Sub-Region", ISO3 = Sub_Region)

# Step 2: Aggregate country-level MPA area
mpa_area_by_country <- mpa_data %>%
  group_by(ISO3, Sub_Region) %>%
  summarise(Country_Area = sum(GIS_AREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Level = "Country")

# Step 3: Combine sub-region and country area data
mpa_area_combined <- bind_rows(
  mpa_area_by_sub_region %>% rename(Country_Area = Sub_Region_Area),
  mpa_area_by_country
)

# Step 4: Order sub-regions and countries by descending MPA area
# Order sub-regions by descending area
sub_region_order <- mpa_area_by_sub_region %>%
  arrange(desc(Sub_Region_Area)) %>%
  pull(Sub_Region)

# Order countries within each sub-region by descending MPA area
country_order <- mpa_area_by_country %>%
  arrange(factor(Sub_Region, levels = sub_region_order), desc(Country_Area)) %>%
  pull(ISO3)

# Combine the sub-regions and countries into one ordered vector
x_order <- c(sub_region_order, "", country_order)

# Step 5: Ensure that ISO3 levels follow this custom order
mpa_area_combined <- mpa_area_combined %>%
  mutate(ISO3 = factor(ISO3, levels = x_order))

# Step 6: Create plot
mpa_area_plot <- ggplot(mpa_area_combined) +
  geom_bar(
    aes(x = ISO3, y = Country_Area, fill = ISO3),
    stat = "identity",
    color = "black",
    width = 0.7
  ) +
  scale_fill_manual(values = fill_colors) +
  scale_x_discrete(
    limits = x_order,
    labels = function(x) {
      sapply(x, function(val) {
        if (val == "" || val %in% sub_region_order) {
          return(val)
        } else {
          return(country_name_mapping[val])
        }
      })
    }
  ) +
  labs(
    title = "",
    x = "Subregion or Nation",
    y = bquote("MPA Extent (km"^2*")")
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma)

# NTA Areal Extent by Sub-Region and Country__________________________________:
# Step 1: Aggregate sub-region MPA area
nta_area_by_sub_region <- nta_data %>%
  group_by(Sub_Region) %>%
  summarise(Sub_Region_Area = sum(GIS_AREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Level = "Sub-Region", ISO3 = Sub_Region)

# Step 2: Aggregate country-level MPA area
nta_area_by_country <- nta_data %>%
  group_by(ISO3, Sub_Region) %>%
  summarise(Country_Area = sum(GIS_AREA, na.rm = TRUE), .groups = "drop") %>%
  mutate(Level = "Country")

# Step 3: Combine sub-region and country area data
nta_area_combined <- bind_rows(
  nta_area_by_sub_region %>% rename(Country_Area = Sub_Region_Area),
  nta_area_by_country
)

# Step 4: Order sub-regions and countries by descending MPA area
# Order sub-regions by descending area
sub_region_order <- nta_area_by_sub_region %>%
  arrange(desc(Sub_Region_Area)) %>%
  pull(Sub_Region)

# Order countries within each sub-region by descending MPA area
country_order <- nta_area_by_country %>%
  arrange(factor(Sub_Region, levels = sub_region_order), desc(Country_Area)) %>%
  pull(ISO3)

# Combine the sub-regions and countries into one ordered vector
x_order <- c(sub_region_order, "", country_order)

# Step 5: Ensure that ISO3 levels follow this custom order
nta_area_combined <- nta_area_combined %>%
  mutate(ISO3 = factor(ISO3, levels = x_order))

# Step 6: Create plot
nta_area_plot <- ggplot(nta_area_combined) +
  geom_bar(
    aes(x = ISO3, y = Country_Area, fill = ISO3),
    stat = "identity",
    color = "black",
    width = 0.7
  ) +
  scale_fill_manual(values = fill_colors) +
  scale_x_discrete(
    limits = x_order,
    labels = function(x) {
      sapply(x, function(val) {
        if (val == "" || val %in% sub_region_order) {
          return(val)
        } else {
          return(country_name_mapping[val])
        }
      })
    }
  ) +
  labs(
    title = "",
    x = "Subregion or Nation",
    y = bquote("No-Take Area Extent (km"^2*")")
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma)

#_____________________________________________________________________________:

# Proportional MPA Coverage to MT_____________________________________________:

# Data Cleaning_______________________________________________________________:

# Create the MT dataset
MT_data <- data.frame(
  ISO3 = c("ABW", "AIA", "ATG", "BES", "BHS", "BLM", "BLZ", "BRB", "COL", "CRI", "CUB",
           "CUW", "CYM", "DMA", "DOM", "GLP", "GRD", "GTM", "HND", "HTI", "JAM",
           "KNA", "LCA", "MAF", "MEX", "MTQ", "NIC", "PAN", "PRI", "SXM", "TCA",
           "TTO", "UMI", "VCT", "VEN", "VGB", "VIR"),
  MT = c(30015, 90164, 111568, 24668, 619785, 4179, 32862, 185007, 400119, 37447, 352259,
          25401, 118291, 28552, 350694, 90705, 25571, 2242, 208636, 103485, 256909,
          9502, 15413, 1100, 96419, 47787, 149695, 142702, 154661, 466, 91025,
          76562, 13890, 36244, 472546, 81555, 38275)
)

# Merge MT with mpa_data
merged_data <- merge(mpa_data, MT_data, by = "ISO3", all.x = TRUE)

# Create a new column for the proportion of MT that is protected
merged_data$MT_Protected_Proportion <- merged_data$GIS_AREA / merged_data$MT

# Ensure the proportion does not exceed 1.0
merged_data$MT_Protected_Proportion <- pmin(merged_data$GIS_AREA / merged_data$MT, 1.0)

print(merged_data)

# Data Visualization__________________________________________________________:

# Step 1: Aggregate sub-region MPA area
prop_area_by_sub_region <- merged_data %>%
  group_by(Sub_Region) %>%
  summarise(
    Sub_Region_Area = sum(GIS_AREA, na.rm = TRUE) / sum(MT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Level = "Sub-Region", ISO3 = Sub_Region)

# Step 2: Aggregate country-level MPA area
prop_area_by_country <- merged_data %>%
  group_by(ISO3, Sub_Region) %>%
  summarise(Country_Area = mean(MT_Protected_Proportion, na.rm = TRUE), .groups = "drop") %>%
  mutate(Level = "Country")

# Step 3: Combine sub-region and country area data
prop_area_combined <- bind_rows(
  prop_area_by_sub_region %>% rename(Country_Area = Sub_Region_Area),
  prop_area_by_country
)

# Step 4: Order sub-regions and countries by descending MPA area
# Order sub-regions by descending area
sub_region_order <- prop_area_by_sub_region %>%
  arrange(desc(Sub_Region_Area)) %>%
  pull(Sub_Region)

# Order countries within each sub-region by descending MPA area
country_order <- prop_area_by_country %>%
  arrange(factor(Sub_Region, levels = sub_region_order), desc(Country_Area)) %>%
  pull(ISO3)

# Combine the sub-regions and countries into one ordered vector
x_order <- c(sub_region_order, "", country_order)

# Step 5: Ensure that ISO3 levels follow this custom order
prop_area_combined <- prop_area_combined %>%
  mutate(ISO3 = factor(ISO3, levels = x_order))

# Step 6: Create plot
prop.area <- ggplot(prop_area_combined) +
  geom_bar(
    aes(x = ISO3, y = Country_Area, fill = ISO3),
    stat = "identity",
    color = "black",
    width = 0.7
  ) +
  scale_fill_manual(values = fill_colors) +
  scale_x_discrete(
    limits = x_order,
    labels = function(x) {
      sapply(x, function(val) {
        if (val == "" || val %in% sub_region_order) {
          return(val)
        } else {
          return(country_name_mapping[val])
        }
      })
    }
  ) +
  labs(
    title = "",
    x = "Subregion or Nation",
    y = "Proportion of MT Protected by MPAs"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma)

# Results_____________________________________________________________________: 

prop_area_by_sub_region
print(prop_area_by_country, n = Inf)
median <- print(prop_area_by_country[order(prop_area_by_country$Country_Area, decreasing = TRUE), ], n = Inf)
median(median$Country_Area)

#_____________________________________________________________________________: 

# Data Visualization and Results Summary______________________________________:

total_mpa_number
print(mpa_number_by_region, n = Inf)
print(mpa_number_by_country, n = Inf)
median(mpa_number_by_country$total_mpa_number)

total_nta_number
total_nta_number_comp
print(nta_number_by_region, n = Inf)
print(nta_number_by_country, n = Inf)

nta_nr_number

mpa_size_nr_number
size_less_1km
size_less_100km
print(size_proportions)
summary(wdpa_unsplit$GIS_TOTAL_AREA)

iucn_summary
gov_type_summary
management_plan_count_df

mpa_area
mpa_area_by_region
print(mpa_area_by_country, n = Inf)
median <- print(mpa_area_by_country[order(mpa_area_by_country$Country_Area, decreasing = TRUE), ], n = Inf)
median(median$Country_Area)

nta_area
nta_area_by_region
median <- print(nta_area_by_country[order(nta_area_by_country$Country_Area, decreasing = TRUE), ], n = Inf)
median(median$Country_Area)

prop_area_by_sub_region
print(prop_area_by_country, n = Inf)
median <- print(prop_area_by_country[order(prop_area_by_country$Country_Area, decreasing = TRUE), ], n = Inf)
median(median$Country_Area)

#Plots

trend_est_plot
mpa_number_plot
nta_number_plot
mpa_size_plot
iucn_plot
gov_plot
man_plot
mpa_area_plot
nta_area_plot
prop.area

#Save images as: 1200x500

#_____________________________________________________________________________:

# The End.

#_____________________________________________________________________________: 
