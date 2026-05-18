library(tidyverse)
library(readr)
library(broom)
library(effectsize)
# we use read.csv2 here because the separator is ";"
# we need to make D3 as characters because read_csv2 treat "," as the decimal mark, but in practice that is also just a separator
unclean = read_csv2(
  "CA_study.csv",
  col_types = cols(
    D3 = col_character()
  )
)

# check if there is any drama language other than English, Chinese and Korean
lancheck = any(unclean$A4 == 0, na.rm = TRUE)

# delete the columns that are irrelevant to data analysis
clean = unclean[, !names(unclean) %in% c("A4_4_TEXT", "B1_0_TEXT", "C2_0_TEXT", "C3_0_TEXT", "D1_0_TEXT", "D2_0_TEXT", "D3_0_TEXT", "E1_0_TEXT", "F1")]

# then remove the cases that are not supposed to be included for all data analysis. e.g. for question B1, any cases falls under non-binary or others should be removed
clean = clean[!(clean$B1 %in% c(0, 3)), ]

# relabel the genders
clean$B1 <- factor(clean$B1, levels = c(1, 2), labels = c("Male", "Female"))

# 1.create separate tibbles for different hypothesis
# 2.remove cases when they contain at least one "not mentioned / other" answers
# 3.further clean / alter the data set to fit the data analysis

cols_1a = c("A2", "B1", "C1", "C2", "C3")
clean_1a = clean[cols_1a]

clean_1a = clean_1a[!apply(clean_1a[, cols_1a] == 0, 1, any), ] |> 
  mutate(c_scale = round((C1 + C2 + C3)/3, 2)) # create a scale for social economic status

cols_1b = c("A2", "B1", "C4")
clean_1b = clean[cols_1b]

# for RQ 2 we have to make the variables dummy coded
cols_2 = c("A2", "B1", "D1", "D2", "D3")
clean_2 = clean[cols_2] 

cols_D <- c("D1", "D2", "D3")

clean_2long <- clean_2 |>
  mutate(row_id = row_number()) |>
  select(row_id, all_of(cols_D)) |>
  pivot_longer(
    cols = all_of(cols_D),
    names_to = "question",
    values_to = "answer"
  ) |>
  separate_longer_delim(answer, delim = ",") |>
  mutate(
    answer = trimws(answer),
    selected = 1
  )

clean_2dummy <- clean_2long |>
  unite("var", question, answer, sep = "_") |>
  pivot_wider(
    id_cols = row_id,
    names_from = var,
    values_from = selected,
    values_fill = 0
  )

clean_2final <- clean_2 |>
  mutate(row_id = row_number()) |>
  left_join(clean_2dummy, by = "row_id") |>
  select(-row_id)

# now rename the column names into actual labels
clean_2final <- clean_2final |>
  rename(
    relational_emotional_obstruction = D1_1,
    structural_material_obstruction = D1_2,
    physical_obstruction_or_threat = D1_3,
    personal_private_sphere = D3_1,
    public_professional_sphere = D3_2
  )

# now merge D2 labels into two non-mutually-exclusive motive variables
clean_2final = clean_2final |>
  mutate(
    relationally_embedded_motives = as.integer(
      rowSums(across(any_of(c("D2_1", "D2_2", "D2_5"))), na.rm = TRUE) > 0
    ),
    status_oriented_motives = as.integer(
      rowSums(across(any_of(c("D2_3", "D2_4", "D2_6"))), na.rm = TRUE) > 0
    )
  )|> 
  select(-any_of(c("D2_1", "D2_2", "D2_3", "D2_4", "D2_5", "D2_6")))

# and we do the same things for RQ 3
cols_3 = c("A2", "B1", "E1")
clean_3 = clean[cols_3]

clean_3final  = clean_3 |>
  mutate(
    E1 = case_when(
      E1 == 1 ~ "calm",
      E1 == 2 ~ "emotional",
      E1 == 3 ~ "mixed",
      TRUE ~ NA_character_
    )
  )

## t-test H1a
male_ses <- clean_1a$c_scale[clean_1a$B1 == "Male"]
female_ses <- clean_1a$c_scale[clean_1a$B1 == "Female"]
tidy(t.test(male_ses, female_ses, alternative = "greater", var.equal = FALSE))
cohens_d(c_scale ~ B1, data = clean_1a)



## chi-square H1b
# contingency table
table_gender_ambiguity <- table(clean_1b$B1, clean_1b$C4)

dimnames(table_gender_ambiguity) <- list(
  Gender = c("Male", "Female"),
  Ambiguity = c("Ambiguous", "Not Ambiguous")
)

table_gender_ambiguity

# run chi-square test
chisq.test(table_gender_ambiguity)

# effect size (Cramer's V)
cramers_v(table_gender_ambiguity)



## chi-square H2a
# contingency table for relational obstruction
table_relational_obstruction <- table(
  clean_2final$B1,
  clean_2final$relational_emotional_obstruction
)

dimnames(table_relational_obstruction) <- list(
  Gender = c("Male", "Female"),
  `Relational / emotional obstruction` = c("Absent", "Present")
)

table_relational_obstruction

# contingency table for structural obstruction
table_structural_obstruction <- table(
  clean_2final$B1,
  clean_2final$structural_material_obstruction
)

dimnames(table_structural_obstruction) <- list(
  Gender = c("Male", "Female"),
  `Structural / material obstruction` = c("Absent", "Present")
)

table_structural_obstruction

# contingency table for physical obstruction
table_physical_obstruction <- table(
  clean_2final$B1,
  clean_2final$physical_obstruction_or_threat
)

dimnames(table_physical_obstruction) <- list(
  Gender = c("Male", "Female"),
  `Physical obstruction or threat` = c("Absent", "Present")
)

table_physical_obstruction

# run chi-square test
chisq.test(table_relational_obstruction)
chisq.test(table_structural_obstruction)
chisq.test(table_physical_obstruction)

# effect size (Cramer's V)
cramers_v(table_relational_obstruction)



## chi-square H2b
# contingency table for relational motives
table_relational_motives <- table(
  clean_2final$B1,
  clean_2final$relationally_embedded_motives
)

dimnames(table_relational_motives) <- list(
  Gender = c("Male", "Female"),
  `Relationally embedded motives` = c("Absent", "Present")
)

table_relational_motives

# contingency table for status motives
table_status_motives <- table(
  clean_2final$B1,
  clean_2final$status_oriented_motives
)

dimnames(table_status_motives) <- list(
  Gender = c("Male", "Female"),
  `Status-oriented motives` = c("Absent", "Present")
)

table_status_motives

# run chi-square test
chisq.test(table_relational_motives)
chisq.test(table_status_motives)

# effect size (Cramer's V)
cramers_v(table_relational_motives)


## chi-square H2c
# contingency table for private sphere
table_personal_private_sphere <- table(
  clean_2final$B1,
  clean_2final$personal_private_sphere
)

dimnames(table_personal_private_sphere) <- list(
  Gender = c("Male", "Female"),
  `Personal / private sphere` = c("Absent", "Present")
)

table_personal_private_sphere

# contingency table for public sphere
table_public_professional_sphere <- table(
  clean_2final$B1,
  clean_2final$public_professional_sphere
)

dimnames(table_public_professional_sphere) <- list(
  Gender = c("Male", "Female"),
  `Public / professional sphere` = c("Absent", "Present")
)

table_public_professional_sphere

# run chi-sqaure
chisq.test(table_personal_private_sphere)
chisq.test(table_public_professional_sphere)



## chi-square H3
# contingency table
clean_3final_filtered <- clean_3final[clean_3final$E1 != "mixed", ]

table_emotion <- table(clean_3final_filtered$B1, clean_3final_filtered $E1)

# run chi-square test
chisq.test(table_emotion)

# effect size (Cramer's V)
cramers_v(table_emotion)







##-------------Exploratory Analysis---------------------

# Does certain narrative role pattern cluster?
table(clean_2final$B1,
      clean_2final$relational_emotional_obstruction,
      clean_2final$relationally_embedded_motives)

table(clean_2final$B1,
      clean_2final$structural_material_obstruction,
      clean_2final$status_oriented_motives)

table(clean_2final$B1,
      clean_2final$relational_emotional_obstruction,
      clean_2final$personal_private_sphere)

# Test the cluster relational obstruction X relational motives X private sphere in genders (according to the contingency tables)
clean_2final <- clean_2final |>
  mutate(
    relational_cluster = as.integer(
      relational_emotional_obstruction == 1 &
        relationally_embedded_motives == 1 &
        personal_private_sphere == 1
    )
  )

rel_cluster_table <- table(
  Gender = clean_2final$B1,
  RelationalCluster = clean_2final$relational_cluster
)

rel_cluster_table

prop.table(rel_cluster_table, margin = 1) * 100

chisq.test(rel_cluster_table, correct = FALSE)

cramers_v(rel_cluster_table)

# Logistic regression on the same one
model_rel_cluster <- glm(
  relational_cluster ~ B1,
  data = clean_2final,
  family = binomial
)

summary(model_rel_cluster)

exp(coef(model_rel_cluster)) # Odds ratio

exp(confint(model_rel_cluster))



## Construct mutual exclusive variables (for robustness test)
clean_2final <- clean_2final |>
  mutate(obstruction_type = case_when(
    relational_emotional_obstruction == 1 & structural_material_obstruction == 0 ~ "Relational only",
    relational_emotional_obstruction == 0 & structural_material_obstruction == 1 ~ "Structural only",
    relational_emotional_obstruction == 1 & structural_material_obstruction == 1 ~ "Both",
    relational_emotional_obstruction == 0 & structural_material_obstruction == 0 ~ "Neither"
  ))

obstruction_table <- table(clean_2final$B1, clean_2final$obstruction_type)
obstruction_table
chisq.test(obstruction_table)
cramers_v(obstruction_table)



clean_2final <- clean_2final |>
  mutate(motive_type = case_when(
    relationally_embedded_motives == 1 & status_oriented_motives == 0 ~ "Relational only",
    relationally_embedded_motives == 0 & status_oriented_motives == 1 ~ "Status only",
    relationally_embedded_motives == 1 & status_oriented_motives == 1 ~ "Both",
    relationally_embedded_motives == 0 & status_oriented_motives == 0 ~ "Neither"
  ))

motive_table <- table(clean_2final$B1, clean_2final$motive_type)
motive_table
chisq.test(motive_table)
cramers_v(motive_table)



clean_2final <- clean_2final |>
  mutate(sphere_type = case_when(
    personal_private_sphere == 1 & public_professional_sphere == 0 ~ "Personal/private only",
    personal_private_sphere == 0 & public_professional_sphere == 1 ~ "Public/professional only",
    personal_private_sphere == 1 & public_professional_sphere == 1 ~ "Both",
    personal_private_sphere == 0 & public_professional_sphere == 0 ~ "Neither"
  ))

sphere_table <- table(clean_2final$B1, clean_2final$sphere_type)
sphere_table
chisq.test(sphere_table)


# Sphere type not significant, try removing the cases that contains both sphere
sphere_pure <- clean_2final |>
  filter(sphere_type %in% c("Personal/private only", "Public/professional only"))

sphere_pure_table <- table(sphere_pure$B1, sphere_pure$sphere_type)

sphere_pure_table
prop.table(sphere_pure_table, margin = 1) * 100

chisq.test(sphere_pure_table)   # Still not significant, could be concerning could be


## Cultural difference
cols_lang_ses <- c("A2", "A4", "B1", "C1", "C2", "C3")
clean_lang_ses <- clean[cols_lang_ses]

# Remove cases with "not mentioned / other" answers in SES items
clean_lang_ses <- clean_lang_ses[!apply(clean_lang_ses[, c("C1", "C2", "C3")] == 0, 1, any), ]

# Create SES scale (not significant at all)
clean_lang_ses <- clean_lang_ses |>
  mutate(
    c_scale = round((C1 + C2 + C3) / 3, 2)
  )

clean_lang_ses <- clean_lang_ses |>
  mutate(
    language = case_when(
      A4 == 1 ~ "English",
      A4 == 2 ~ "Chinese",
      A4 == 3 ~ "Korean",
      TRUE ~ NA_character_
    ),
    language = factor(language)
  )

clean_lang_ses |>
  group_by(language) |>
  summarise(
    n = n(),
    mean_ses = mean(c_scale, na.rm = TRUE),
    sd_ses = sd(c_scale, na.rm = TRUE),
    median_ses = median(c_scale, na.rm = TRUE),
    min_ses = min(c_scale, na.rm = TRUE),
    max_ses = max(c_scale, na.rm = TRUE)
  )

model_lang_ses <- aov(c_scale ~ language, data = clean_lang_ses)

summary(model_lang_ses)

TukeyHSD(model_lang_ses)