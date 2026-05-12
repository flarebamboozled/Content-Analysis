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

## trying my best: logistic regression H2a
model_relational <- glm(relational_emotional_obstruction ~ B1 + structural_material_obstruction, 
                        data = clean_2final, family = binomial)
exp(coef(model_relational)); exp(confint(model_relational))

model_structural <- glm(structural_material_obstruction ~ B1 + relational_emotional_obstruction, 
                        data = clean_2final, family = binomial)
exp(coef(model_structural)); exp(confint(model_structural))



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

