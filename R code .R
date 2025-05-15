###########################################################################
##  EV‑Charger panel analysis  – single‑file version ######################
###########################################################################
##  EDIT THESE TWO LINES ONLY #############################################
root_dir   <- "/Users/farhanakhtar/Documents/Thesis/Last Daaayyyy/Raw Data"
output_doc <- file.path(root_dir, "EV_Regression_Results_UPDATED.docx")
###########################################################################

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(broom)
library(officer)
library(flextable)
library(skimr)
library(scales)

merged_csv <- file.path(root_dir, "merged_regression_data.csv")

# 1 ── read merged panel ---------------------------------------------------
df <- read_csv(merged_csv, show_col_types = FALSE) |>
  mutate(across(where(is.character), as.factor))

# 2 ── rescale EV penetration ---------------------------------------------
df <- df %>%
  mutate(
    ev_pct = ifelse(ev_pct <= 1 & !is.na(ev_pct),
                    ev_pct * 100, ev_pct)
  )

# 3 ── optional winsorise fast‑chargers for plots -------------------------
df <- df |>
  mutate(fast_chargers_w = squish(fast_chargers,
                                  quantile(fast_chargers, 0.99)))

# 4 ── build panel object --------------------------------------------------
pdata <- pdata.frame(df |> arrange(mun_code, year),
                     index = c("mun_code","year"))

# 5 ── model formulas ------------------------------------------------------
fe_form <- fast_chargers ~ subsidies_pct * ev_pct + education
re_form <- fe_form      # random‑effects same RHS

fast_fe <- plm(fe_form, data = pdata, model = "within")
fast_re <- plm(re_form, data = pdata, model = "random")

norm_fe <- plm(update(fe_form, normal_chargers ~ .), data = pdata, model = "within")
norm_re <- plm(update(re_form,  normal_chargers ~ .), data = pdata, model = "random")


###########################################################################
##  EV‑Charger panel analysis  – single‑file version ######################
###########################################################################
##  EDIT THESE TWO LINES ONLY #############################################
root_dir   <- "/Users/farhanakhtar/Documents/Thesis/Last Daaayyyy/Raw Data"
output_doc <- file.path(root_dir, "EV_Regression_Results_UPDATED.docx")
###########################################################################

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(broom)
library(officer)
library(flextable)
library(skimr)
library(scales)

merged_csv <- file.path(root_dir, "merged_regression_data.csv")

# 1 ── read merged panel ---------------------------------------------------
df <- read_csv(merged_csv, show_col_types = FALSE) |>
  mutate(across(where(is.character), as.factor))

# 2 ── rescale EV penetration ---------------------------------------------
df <- df %>%
  mutate(
    ev_pct = ifelse(ev_pct <= 1 & !is.na(ev_pct),
                    ev_pct * 100, ev_pct)
  )

# 3 ── optional winsorise fast‑chargers for plots -------------------------
df <- df |>
  mutate(fast_chargers_w = squish(fast_chargers,
                                  quantile(fast_chargers, 0.99)))

# 4 ── build panel object --------------------------------------------------
pdata <- pdata.frame(df |> arrange(mun_code, year),
                     index = c("mun_code","year"))

# 5 ── model formulas ------------------------------------------------------
fe_form <- fast_chargers ~ subsidies_pct * ev_pct + education
re_form <- fe_form      # random‑effects same RHS

fast_fe <- plm(fe_form, data = pdata, model = "within")
fast_re <- plm(re_form, data = pdata, model = "random")

norm_fe <- plm(update(fe_form, normal_chargers ~ .), data = pdata, model = "within")
norm_re <- plm(update(re_form,  normal_chargers ~ .), data = pdata, model = "random")


############################################################################
## 5b ── TWO‑WAY FIXED‑EFFECTS  (municipality + year)  ####################
############################################################################
##
##  ▸ Estimates FE with year dummies (effect = "twoways")
##  ▸ Keeps interaction term; the Subsidy_pc main effect is dropped
##    automatically by plm because it is collinear with the year dummies.
##  ▸ Writes a **separate** Word file so your original results stay intact.
############################################################################

# ----- model formulas -----------------------------------------------------
fe2_form   <- fast_chargers ~ subsidies_pct:ev_pct + ev_pct + education
norm2_form <- update(fe2_form, normal_chargers ~ .)

# ----- estimate two‑way FE models ----------------------------------------
fast_fe2 <- plm(fe2_form,  data = pdata, model = "within", effect = "twoways")
norm_fe2 <- plm(norm2_form, data = pdata, model = "within", effect = "twoways")

# ----- cluster‑robust SEs (same rob_se() you already defined) ------------
c_fast2 <- coeftest(fast_fe2, vcov = rob_se(fast_fe2))
c_norm2 <- coeftest(norm_fe2, vcov = rob_se(norm_fe2))

# ----- Word export (separate file) ---------------------------------------
output_doc_tw <- file.path(root_dir, "EV_Regression_Results_TWOWAY.docx")

doc2 <- read_docx() |>
  body_add_par("EV‑Charger Regression – TWO‑WAY FE (mun + year)", 
               style = "heading 1") |>
  body_add_par("Note: Subsidy main term is omitted (collinear with year dummies).", 
               style = "Normal")

add_section <- function(doc, tbl, heading){
  doc |>
    body_add_par(heading, style = "heading 2") |>
    body_add_flextable(tbl) |>
    body_add_par("")
}

make_ft <- function(df){
  df |>
    dplyr::select(where(is.numeric)) |>
    mutate(across(where(is.numeric), round, 4)) |>
    flextable() |>
    autofit()
}

doc2 <- add_section(doc2, make_ft(broom::tidy(fast_fe2)),
                    "Two‑way FE  Fast chargers")
doc2 <- add_section(doc2, make_ft(broom::tidy(norm_fe2)),
                    "Two‑way FE  Normal chargers")

print(doc2, target = output_doc_tw)

# ----- console summary (optional) ----------------------------------------
cat("\n==== Two‑way FE (cluster‑robust) – Fast chargers ====\n")
print(c_fast2)
cat("\n==== Two‑way FE (cluster‑robust) – Normal chargers ===\n")
print(c_norm2)
cat("\nTWO‑WAY FE Word file written to:\n", output_doc_tw, "\n")
############################################################################

# 6 ── cluster‑robust SEs --------------------------------------------------
rob_se <- function(m) vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
c_fast <- coeftest(fast_fe,  vcov = rob_se(fast_fe))
c_norm <- coeftest(norm_fe,  vcov = rob_se(norm_fe))

# 7 ── Hausman tests -------------------------------------------------------
haus_fast <- phtest(fast_re, fast_fe)
haus_norm <- phtest(norm_re,  norm_fe)

# 8 ── Word export ---------------------------------------------------------
# ── Word export (models + Hausman) ───────────────────────────────────────
make_ft <- function(df){
  df |>
    dplyr::select(where(is.numeric)) |>
    mutate(across(where(is.numeric), round, 4)) |>
    flextable() |>
    autofit()
}

doc <- read_docx() |>
  body_add_par("EV‑Charger Regression – updated single‑file run", style = "heading 1")

add_section <- function(doc, tbl, heading){
  doc |>
    body_add_par(heading, style = "heading 2") |>
    body_add_flextable(tbl) |>
    body_add_par("")
}

doc <- add_section(doc, make_ft(broom::tidy(fast_re)),  "Random‑effects Fast chargers")
doc <- add_section(doc, make_ft(broom::tidy(norm_re)),  "Random‑effects Normal chargers")
doc <- add_section(doc, make_ft(broom::tidy(fast_fe)),  "Fixed‑effects   Fast chargers")
doc <- add_section(doc, make_ft(broom::tidy(norm_fe)),  "Fixed‑effects   Normal chargers")

## Hausman tests – keep only numeric columns (statistic, parameter, p.value)
doc <- add_section(doc, make_ft(broom::glance(haus_fast)), "Hausman test Fast")
doc <- add_section(doc, make_ft(broom::glance(haus_norm)), "Hausman test Normal")

print(doc, target = output_doc)

# 9 ── descriptive stats for your Chapter 3 table --------------------------
skimr::skim(df %>%
              select(fast_chargers, normal_chargers,
                     subsidies_pct, ev_pct, education)) %>%
  write_csv(file.path(root_dir, "descriptive_stats_updated.csv"))

# 10 ── console summary ----------------------------------------------------
cat("\n==== FE model (cluster‑robust) – Fast chargers ====\n")
print(c_fast)
cat("\n==== FE model (cluster‑robust) – Normal chargers ===\n")
print(c_norm)
cat("\nWord tables and descriptive CSV written to:\n", root_dir, "\n")

# 6 ── cluster‑robust SEs --------------------------------------------------
rob_se <- function(m) vcovHC(m, method = "arellano", type = "HC1", cluster = "group")
c_fast <- coeftest(fast_fe,  vcov = rob_se(fast_fe))
c_norm <- coeftest(norm_fe,  vcov = rob_se(norm_fe))

# 7 ── Hausman tests -------------------------------------------------------
haus_fast <- phtest(fast_re, fast_fe)
haus_norm <- phtest(norm_re,  norm_fe)

# 8 ── Word export ---------------------------------------------------------
# ── Word export (models + Hausman) ───────────────────────────────────────
make_ft <- function(df){
  df |>
    dplyr::select(where(is.numeric)) |>
    mutate(across(where(is.numeric), round, 4)) |>
    flextable() |>
    autofit()
}

doc <- read_docx() |>
  body_add_par("EV‑Charger Regression – updated single‑file run", style = "heading 1")

add_section <- function(doc, tbl, heading){
  doc |>
    body_add_par(heading, style = "heading 2") |>
    body_add_flextable(tbl) |>
    body_add_par("")
}

doc <- add_section(doc, make_ft(broom::tidy(fast_re)),  "Random‑effects Fast chargers")
doc <- add_section(doc, make_ft(broom::tidy(norm_re)),  "Random‑effects Normal chargers")
doc <- add_section(doc, make_ft(broom::tidy(fast_fe)),  "Fixed‑effects   Fast chargers")
doc <- add_section(doc, make_ft(broom::tidy(norm_fe)),  "Fixed‑effects   Normal chargers")

## Hausman tests – keep only numeric columns (statistic, parameter, p.value)
doc <- add_section(doc, make_ft(broom::glance(haus_fast)), "Hausman test Fast")
doc <- add_section(doc, make_ft(broom::glance(haus_norm)), "Hausman test Normal")

print(doc, target = output_doc)

# 9 ── descriptive stats for your Chapter 3 table --------------------------
skimr::skim(df %>%
              select(fast_chargers, normal_chargers,
                     subsidies_pct, ev_pct, education)) %>%
  write_csv(file.path(root_dir, "descriptive_stats_updated.csv"))

# 10 ── console summary ----------------------------------------------------
cat("\n==== FE model (cluster‑robust) – Fast chargers ====\n")
print(c_fast)
cat("\n==== FE model (cluster‑robust) – Normal chargers ===\n")
print(c_norm)
cat("\nWord tables and descriptive CSV written to:\n", root_dir, "\n")