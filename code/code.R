### CAA 2006
## Pajdla - TKáč - Kozák
# From Zero to Plotting: R for Anyone
# March 31


# -------------------------------------------------------------------------
# BLOCK I (9:00 - 11:00) --------------------------------------------------
# -------------------------------------------------------------------------



# Introduction



# -------------------------------------------------------------------------
# BLOCK II (11:30 - 13:00) ------------------------------------------------
# -------------------------------------------------------------------------

# Environment preparation -------------------------------------------------

dir.create("./data")
dir.create("./data/raw")
dir.create("./data/processed")
dir.create("./code")
dir.create("./plots")

# packages ----------------------------------------------------------------

# install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

library(tidyverse)

library(readxl)


# Data import -------------------------------------------------------------

burials <- read_csv("./data/raw/burials.csv")

goods <- read_csv2("./data/raw/grave_goods.csv")      # csv2 for ";" delimeter

#goods_xlsx <- read_xlsx("./data/raw/grave_goods.xlsx")


# Explore data ------------------------------------------------------------

burials

nrow(burials)
ncol(burials)
str(burials)

head(burials,4)
tail(burials, 4)

names(burials)
# 
# goods
# 
# colnames(goods)
# 
# nrow(goods)
# ncol(goods)
# str(goods)
# 
# head(goods,4)
# tail(goods, 4)


# -------------------------------------------------------------------------
# Tidy data ---------------------------------------------------------------
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Burials -----------------------------------------------------------------


# Clearing data -----------------------------------------------------------

burials <- burials %>%                 #drops all observations with NAs
  drop_na()            

      # burials <- burials %>%        #drops NAs in selected column
      #   drop_na(Depth_cm)

burials <- burials %>%                #Not all missing values are coded as NA
  filter(Sex %in% c("M", "F"))


# 
# # Mutate - create or modify columns ---------------------------------------
# burials <- burials %>%               #create new column
#   mutate(Analysed = "CAA 2026")


# Rename columns ----------------------------------------------------------
colnames(burials)

burials <- rename(burials, Orientation = orientation)

burials <- rename(burials, Grave_ID = "grave id")

# pipe

# Recode variable ---------------------------------------------------------

#Preservation

unique(burials$Preservation)

# #OR
# 
# burials %>% 
#   group_by(Preservation) %>%    # Oh no! Different form of variable!
#   count()                       # R is case sensitive
# 
# #OR just
# 
# table(burials$Preservation)



burials <- burials %>%
  mutate(Preservation = recode(Preservation, 
                               GOOD = "Good",
                               good = "Good",
                               fair = "Fair",
                               poor = "Poor")) 

unique(burials$Preservation)


# OR
# 
# burials <- burials %>% 
#   mutate(Preservation = str_to_title(Preservation))   # str_to_title = function from "stringr" package



#Orientation

burials <- burials %>%              # recode() does not recognise numbers as argument name; "case_when()" instead!
  mutate(Orientation = case_when(
    Orientation == 0 ~ "N-S",
    Orientation == 90 ~ "E-W",
    Orientation == 180 ~ "S-N",
    Orientation == 270 ~ "W-E",
    .default = Orientation))

#Age_category

# burials %>%                         # Non-consistent categories 
#   group_by(Age_category) %>% 
#   count()

unique(burials$Age_category)


burials <- burials %>% mutate(Age_category = recode(Age_category,
                                                mature = "adult",
                                                infant = "child"))


# Select columns ----------------------------------------------------------

colnames(burials)

burials %>% select("Grave_ID", "Context", "Age_category", "Sex", "Orientation", "Depth_cm", "Preservation", "Excavation_year")

#OR

burials <- burials %>% select(-Analysed)  #OR select(-c(Excavation_year, Analysed))


# Filter values -----------------------------------------------------------

# burials %>% filter(Excavation_year == "2019")

adult <- burials %>% filter(Age_category == "adult")

adult_M <- burials %>% filter(Age_category == "adult" & Sex == "M")


# -------------------------------------------------------------------------
# Grave goods -------------------------------------------------------------

## INDEPENDENT WORK ##

goods
view(goods)


# Clearing data -----------------------------------------------------------


goods <- goods %>%                # NAs      
  drop_na()            

goods <- goods %>%                # Negativ weight values
  filter(weight_g > 0)

goods <- goods %>%                # numeric vs. description
  filter(length_mm != "fragment") 

goods <- goods %>% filter(dating != "undated")


# Rename columns ----------------------------------------------------------
names(goods)

goods <- rename(goods, 
                context = Context,
                category = supercategory)

# Recode variable ---------------------------------------------------------

unique(goods$dating)
table(goods$dating)                     # All early medieval. What is the "medieval" one?

goods %>% filter(dating == "medieval")  #Ok... medieval ceramic axe. Let´s ditch this observation
goods <- goods %>% filter(dating != "medieval")


goods <- goods %>%                       # All artefacts EMA; we can keep more the detail we have and unite the rest
  mutate(dating = recode(dating, 
                        "early medieval" = "EMA",
                        "Early Medieval" = "EMA",
                        "Viking Age"     = "EMA",
                        "9th-10th century" = "EMA"))

table(goods$dating)


#how many rings in each period?

goods %>% filter(artifact_type == "ring")  %>%                       
  group_by(dating) %>% 
  count()

table(goods$material)

iron <- goods %>% filter(material == "iron")

# -------------------------------------------------------------------------
# Export data -------------------------------------------------------------

write_csv(burials, "./data/processed/burials.csv")
write_csv(goods, "./data/processed/goods.csv")



# -------------------------------------------------------------------------
# Plotting ----------------------------------------------------------------
# -------------------------------------------------------------------------


# basics ------------------------------------------------------------------

# data
ggplot(data = iron)

# aesthetics mapping
ggplot(data = iron, mapping = aes(x = length_mm, y = weight_g))

# geometry
ggplot(data = iron, mapping = aes(x = length_mm, y = weight_g)) + 
  geom_point()

# shortened
ggplot(iron, aes(x = length_mm, y = weight_g)) +
  geom_point()

# using pipeline operator
iron %>% 
  ggplot(aes(x = length_mm, y = weight_g)) +
  geom_point()


# adjusting plot ----------------------------------------------------------

p1 <- iron %>% 
  ggplot(aes(x = length_mm, y = weight_g, colour = dating, shape = category)) + 
  geom_point(alpha = 0.8, size = 5) + 
  labs(x = "Length (mm)",
       y = " Weight (g)",
       title = "Early medieval artefacts",
       subtitle = "Iron",
       caption = "CAA 2026") +
  theme_light()

p1

ggsave("./plots/plot1.png", plot = p1, width = 8, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# BLOCK III (14:30 - 15:30) -----------------------------------------------
# -------------------------------------------------------------------------


# Joining data ------------------------------------------------------------

table(goods$material)   # Let´s ditch textile and amber + create material category "metal"

good_goods <- goods %>% 
  filter(material != "amber" , material != "textile")   # alternatively:   filter(!material %in% c("amber", "textile"))

good_goods <- good_goods %>%
  mutate(material = case_when(
    material %in% c("bronze", "copper alloy", "iron", "silver") ~ "metal",
    .default = material))

table(good_goods$material)

#Pivoting


goods_wide <- good_goods %>% 
  group_by(context) %>% 
  mutate(weight_g = sum(weight_g, na.rm = TRUE)) %>%  # total weight per context
  ungroup() %>% 
  count(context, material, weight_g) %>% 
  pivot_wider(names_from = material, 
              values_from = n, 
              values_fill = 0)

# LEFT join 

goods_wide <- goods_wide %>% rename(Context = context)

data <- left_join(burials, goods_wide, by = "Context")

str(data)     # Let´s consider that contexts not mentioned in "burials" had 0 goods

data <- data %>% 
  mutate(across(c(weight_g, metal, bone, ceramic, stone, glass), ~replace_na(.x, 0)))

# total number of grave goods

data <- data %>% mutate(goods_total = metal + bone + ceramic + stone + glass)


# -------------------------------------------------------------------------
# Plotting ----------------------------------------------------------------
# -------------------------------------------------------------------------


# Barplot -----------------------------------------------------------------

p2 <- data %>% 
  ggplot(aes(x = Sex, y = goods_total, fill = Age_category)) +
  geom_col(width = 0.5) +
  scale_fill_brewer(palette = "Set2") +             # name = "Age category"
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  labs(x = "Sex",
       y = "Number of grave goods", 
       title = "Grave goods by sex",
       subtitle = "Early medieval period",
       caption = "CAA 2026",
       fill = "Age category") +
  theme_light()
  

print(p2)


# Boxplot -----------------------------------------------------------------

p3 <- data %>% 
  ggplot(aes(x = Sex, y = weight_g)) +
  geom_boxplot() +
  labs(x = "Sex",
       y = "Weight of grave goods (g)", 
       title = "Grave goods by sex",
       subtitle = "Early medieval period",
       caption = "CAA 2026") +
  theme_light()


print(p3)



