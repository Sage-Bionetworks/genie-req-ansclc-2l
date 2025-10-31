library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

cohort <- readr::read_rds(
  here('data', 'cohort', 'cohort_part_1.rds')
)
flow_track <- readr::read_rds(
  here('data', 'cohort', 'flow_track_part_1.rds')
)
index_therapies <- readr::read_rds(
  here('data', 'cohort', 'index_therapies.rds')
)
lot <- readr::read_rds(
  here('data', 'cohort', 'lot.rds')
)
# rad <- readr::read_csv(
#   here('data-raw', 'cohort'

prev_ther_checks <- lot %>%
  group_by(record_id, ca_seq) %>%
  summarize(
    had_plat = max(cumsum_plat) >= 1,
    had_io = max(cumsum_io) >= 1,
  )

cohort %<>%
  left_join(
    .,
    prev_ther_checks,
    by = c('record_id', 'ca_seq')
  )

# The people with NA for these variables were not in the lines dataset, meaning
#   they didn't have any therapies that counted as lines.
cohort %<>% replace_na(list(had_plat = FALSE, had_io = FALSE))

cohort %<>% filter(had_plat)
flow_track %<>% flow_record_helper(cohort, "Prior platinum exposure", .)
cohort %<>% filter(had_io)
flow_track %<>% flow_record_helper(cohort, "Prior IO exposure", .)

cohort %<>%
  left_join(
    .,
    index_therapies,
    by = c('record_id', 'ca_seq')
  )

cohort %<>% filter(no_inv_to_index)
flow_track %<>% flow_record_helper(cohort, "No investigational before index", .)

cohort %<>%
  mutate(
    dob_cpt_rep_days = dob_ca_dx_days + dx_cpt_rep_days,
    cpt_within_index_window = (dob_cpt_rep_days - index_dob_reg_start_int) < 30
  ) %>%
  relocate(.after = dx_cpt_rep_days, dob_cpt_rep_days, cpt_within_index_window)

cohort %<>% filter(cpt_within_index_window)
flow_track %<>% flow_record_helper(cohort, "NGS within window", .)

# cohort %<>% filter(no_tki_to_index)
# flow_track %<>% flow_record_helper(cohort, "No TKI before index", .)
#
# cohort %<>% filter(no_inv_to_index)
# flow_track %<>% flow_record_helper(cohort, "No investigational before index", .)
