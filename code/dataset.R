# Generate sample burial dataset for R workshop
# "From Zero to Plotting: R for Anyone"
# 2026-02-04
# by pajdla@arub.cz

set.seed(42) # For reproducibility

# Number of burials
n_burials <- 50

# Generate main burials dataset (CSV)
burials <- data.frame(
  # Intentionally inconsistent column naming
  Grave_ID = paste0(
    "G",
    stringr::str_pad(1:n_burials, 3, side = "left", pad = "0")
  ),
  Context = paste0("C", 1000 + 1:n_burials),

  # Age categories with some NAs
  Age_category = sample(
    c("infant", "child", "adult", "mature", NA),
    n_burials,
    replace = TRUE,
    prob = c(0.15, 0.2, 0.4, 0.2, 0.05)
  ),

  # Sex with various ways to express "unknown"
  Sex = sample(
    c("M", "F", "?", NA, "", "unknown"),
    n_burials,
    replace = TRUE,
    prob = c(0.35, 0.35, 0.1, 0.1, 0.05, 0.05)
  ),

  # Orientation - mix of degrees and text, with NAs
  orientation = sample(
    c("90", "180", "270", "0", "E-W", "N-S", "W-E", NA, ""),
    n_burials,
    replace = TRUE
  ),

  # Depth with some typos and NAs
  Depth_cm = c(
    sample(50:200, n_burials - 5, replace = TRUE),
    1500, # obvious typo (should be 150)
    45,
    NA,
    2200, # another typo (should be 220)
    NA
  )[sample(n_burials)], # shuffle

  # Preservation with inconsistent capitalization
  Preservation = sample(
    c("good", "Good", "GOOD", "fair", "Fair", "poor", "Poor", NA),
    n_burials,
    replace = TRUE
  ),

  # Excavation year
  Excavation_year = sample(2019:2023, n_burials, replace = TRUE)
)

# burials |> tibble::tibble()

# Introduce some column name variations for a few rows
# (simulating merged data from different seasons)
colnames(burials)[1] <- "grave id" # Will need renaming

# Write CSV
write.csv(
  burials,
  here::here("data", "burials.csv"),
  row.names = FALSE,
  na = ""
)


# Generate grave goods inventory dataset (XLSX - wide format that needs pivoting)
# Each row is an individual artifact find

# Define artifact types with their categories
artifact_types <- data.frame(
  artifact = c(
    "sword",
    "spearhead",
    "shield boss",
    "knife",
    "axe",
    "pottery fragment",
    "pottery vessel",
    "spindle whorl",
    "loom weight",
    "bead",
    "bracelet",
    "ring",
    "pendant",
    "fibula",
    "pin",
    "bone comb",
    "belt buckle",
    "textile fragment"
  ),
  supercategory = c(
    "weapons and armour",
    "weapons and armour",
    "weapons and armour",
    "tools",
    "tools",
    "common find",
    "common find",
    "common find",
    "common find",
    "adornment",
    "adornment",
    "adornment",
    "adornment",
    "clothing",
    "clothing",
    "clothing",
    "clothing",
    "clothing"
  ),
  stringsAsFactors = FALSE
)

# Generate individual artifact records
# Not all contexts have finds, and some have multiple
n_artifacts <- 85 # Total artifact count

grave_goods <- data.frame(
  Context = sample(burials$Context, n_artifacts, replace = TRUE),

  artifact_type = sample(artifact_types$artifact, n_artifacts, replace = TRUE),

  material = sample(
    c(
      "iron",
      "bronze",
      "copper alloy",
      "silver",
      "bone",
      "ceramic",
      "glass",
      "amber",
      "stone",
      "textile",
      NA,
      ""
    ),
    n_artifacts,
    replace = TRUE,
    prob = c(
      0.15,
      0.15,
      0.1,
      0.05,
      0.1,
      0.2,
      0.08,
      0.05,
      0.05,
      0.02,
      0.03,
      0.02
    )
  ),

  weight_g = round(
    c(
      rnorm(70, mean = 45, sd = 30), # Most artifacts
      rnorm(10, mean = 350, sd = 100), # Heavy items (weapons)
      rep(NA, 5)
    ), # Missing weights
    1
  ),

  # Length in mm (some missing, some as "fragment")
  length_mm = c(
    round(rnorm(60, mean = 85, sd = 40), 0),
    rep(NA, 20),
    rep("fragment", 5)
  )[sample(n_artifacts)],

  # Dating - mix of specific and broad periods, some contradictory, some NA
  dating = sample(
    c(
      "Early Medieval",
      "early medieval",
      "9th century",
      "10th century",
      "9th-10th century",
      "Viking Age",
      "Late Iron Age",
      "medieval",
      "undated",
      NA,
      ""
    ),
    n_artifacts,
    replace = TRUE,
    prob = c(0.15, 0.1, 0.15, 0.15, 0.2, 0.1, 0.05, 0.03, 0.03, 0.02, 0.02)
  )
)

# Add supercategory by matching artifact types
grave_goods$supercategory <- artifact_types$supercategory[
  match(grave_goods$artifact_type, artifact_types$artifact)
]

# Reorder columns for better readability
grave_goods <- grave_goods[, c(
  "Context",
  "artifact_type",
  "supercategory",
  "material",
  "weight_g",
  "length_mm",
  "dating"
)]

# Introduce some weight outliers/typos
grave_goods$weight_g[sample(nrow(grave_goods), 2)] <- c(4500, 8.5) # Obvious errors

# Write XLSX
readr::write_excel_csv2(grave_goods, here::here("data", "grave_goods.csv"))
