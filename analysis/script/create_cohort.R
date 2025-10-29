library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

ca_ind <- readr::read_csv(
  here(
    'data-raw',
    'bpc',
    'NSCLC',
    '3.1-consortium',
    'cancer_level_dataset_index.csv'
  )
)
cohort <- ca_ind %>%
  select(
    cohort,
    record_id,
    institution,
    ca_seq
  )

# flow_track monitors attrition at each step for us.
flow_track <- flow_record_helper(cohort, "BPC NSCLC v3.1")


dx_stage <- ca_ind %>%
  filter(stage_dx %in% c("Stage III", "Stage IV")) %>%
  mutate(
    stage_detailed_comb = case_when(
      !is.na(best_ajcc_stage_cd) ~ best_ajcc_stage_cd,
      !is.na(ca_path_group_stage) ~ ca_path_group_stage,
      T ~ NA_character_
    )
  ) %>%
  select(record_id, ca_seq, stage_detailed_comb, stage_dx)

cohort <- left_join(
  cohort,
  dx_stage,
  by = c('record_id', 'ca_seq')
)

# This is inductive - I just looked at the available codes first.
cohort %<>%
  filter(
    stage_dx %in% "Stage IV" | stage_detailed_comb %in% c("3B", "3C", "IIIB")
  )

flow_track <- flow_record_helper(cohort, "Stage 3B+ at dx")
