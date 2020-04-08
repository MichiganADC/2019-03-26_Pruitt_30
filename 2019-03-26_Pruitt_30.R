# 2019-03-26_Pruitt_30.R

# USEFUL LIBRARIES ----

library(dplyr)
library(stringr)


# USEFUL GLOBALS / FUNCTIONS ---- 
source("~/Box/Documents/R_helpers/config.R")
source("~/Box/Documents/R_helpers/helpers.R")


# GET DATA ----

# _ UDS 3 ----

# _ _ Define Fields / Forms ----

fields_u3_hd_raw <-
  c(
    "ptid"
    , "form_date"
    , "dob"
  )

fields_u3_a1_raw <-
  c(
    "sex"
    , "race"
    , "educ"
    , "handed"
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

fields_u3_b6_raw <-
  c(
    "nogds"
    , "satis"
    , "dropact"
    , "empty"
    , "bored"
    , "spirits"
    , "afraid"
    , "happy"
    , "helpless"
    , "stayhome"
    , "memprob"
    , "wondrful"
    , "wrthless"
    , "energy"
    , "hopeless"
    , "better"
    , "gds"
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

# fields_u3_c2_raw <-
#   c(
#     "mocacomp"
#     , "mocareas"
#     , "mocaloc"
#     , "mocalan"
#     , "mocavis"
#     , "mocahear"
#     , "mocatots"
#     , "mocatrai"
#     , "mocacube"
#     , "mocacloc"
#     , "mocaclon"
#     , "mocacloh"
#     , "mocanami"
#     , "mocaregi"
#     , "mocadigi"
#     , "mocalett"
#     , "mocaser7"
#     , "mocarepe"
#     , "mocaflue"
#     , "mocaabst"
#     , "mocarecn"
#     , "mocarecc"
#     , "mocarecr"
#     , "mocaordt"
#     , "mocaormo"
#     , "mocaoryr"
#     , "mocaordy"
#     , "mocaorpl"
#     , "mocaorct"
#     , "crafturs"
#     , "craftdre"
#     , "udsbentd"
#     , "trailb_c2z"
#     , "digbacct"
#   ) %>% c(paste0("fu_", .), paste0("tele_", .))
forms_u3_c2 <-
  c(
    "ivp_c2"
    , "fvp_c2"
  )

fields_u3_d1_raw <-
  c(
    "normcog"
    , "demented"
    , "mciamem"
    , "mciaplus"
    , "mcinon1"
    , "mcinon2"
    , "impnomci"
    , "amndem"
    , "pca"
    , "ppasyn"
    , "ftdsyn"
    , "lbdsyn"
    , "namndem"
    , "alzdis"
    , "alzdisif"
    , "psp"
    , "pspif" 
    , "cort"
    , "cortif"
    , "ftldmo"
    , "ftldmoif" 
    , "ftldnos"
    , "ftldnoif"
    , "cvd"
    , "cvdif"
    , "lbdis"
    , "lbdif"
    , "park"
  ) %>% c(paste0("fu_", .), paste0("tele_", .))

fields_u3_ls_raw <-
  c(
    "see_hear"
    , "private"
    , "help"
    , "friend_see_hear"
    , "friend_private"
    , "friend_help"
    , "lubben_total"
  )

fields_u3_raw <-
  c(
    fields_u3_hd_raw
    , fields_u3_a1_raw
    , fields_u3_b6_raw
    # , fields_u3_c2_raw
    , fields_u3_d1_raw
    , fields_u3_ls_raw
  )

forms_u3_raw <-
  c(
    forms_u3_c2
  )

fields_u3 <- fields_u3_raw %>% paste(collapse = ",")
forms_u3 <- forms_u3_raw %>% paste(collapse = ",")

# _ _ Get Data via REDCap API ----

json_u3 <- 
  export_redcap_records(uri    = REDCAP_API_URI,
                        token  = REDCAP_API_TOKEN_UDS3n,
                        fields = fields_u3,
                        forms  = forms_u3,
                        vp     = TRUE)
df_u3 <- jsonlite::fromJSON(json_u3) %>% as_tibble() %>% na_if("")

# _ UMMAP General ----

# _ _ Define Fields / Forms

fields_ug_mri_raw <-
  c(
    "subject_id"
    , "exam_date"
    , "mri_sub_id"
    , "seq_num"
    , "mri_completed"
    , "mri_date"
  )

fields_ug_raw <- 
  c(
    fields_ug_mri_raw
  )

fields_ug <- fields_ug_raw %>% paste(collapse = ",")

# _ _ Get Data via REDCap API ----

json_ug <-
  export_redcap_records(uri    = REDCAP_API_URI,
                        token  = REDCAP_API_TOKEN_UMMAP_GEN,
                        fields = fields_ug,
                        vp     = TRUE,
                        filterLogic = paste0("(",
                                             "[subject_id] >= 'UM00000000'",
                                             " AND ",
                                             "[subject_id] <= 'UM00009999'",
                                             " AND ",
                                             "[exam_date]  >= '2017-03-01'",
                                             ")"))
df_ug <- jsonlite::fromJSON(json_ug) %>% as_tibble() %>% na_if("")


# _ MiNDSet Registry ----

# _ _ Define Fields / Forms ----

fields_ms_dm_raw <-
  c(
    "subject_id"
    , "race_value"
    , "ed_level"
    , "handedness"
    , "birth_date"
  )

fields_ms_raw <- 
  c(
    fields_ms_dm_raw
  )

fields_ms <- fields_ms_raw %>% paste(collapse = ",")

# _ _ Get Data via REDCap API ----

json_ms <-
  export_redcap_records(uri    = REDCAP_API_URI,
                        token  = REDCAP_API_TOKEN_MINDSET,
                        fields = fields_ms,
                        vp     = TRUE,
                        filterLogic = paste0("(",
                                             "[subject_id] >= 'UM00000000'",
                                             " AND ",
                                             "[subject_id] <= 'UM00009999'",
                                             ")"))
df_ms <- jsonlite::fromJSON(json_ms) %>% as_tibble() %>% na_if("")


# PROCESS DATA ----

# _ Clean Data ----

# _ _ UDS 3 ----

rel_fields <- names(df_u3) %>%
  str_replace_all("ptid|form_date|dob|redcap_event_name", NA_character_) %>%
  stringi::stri_remove_empty_na()

df_u3_cln <- df_u3 %>% 
  # Deselect useless field(s)
  select(-redcap_event_name) %>%
  # Keep only merged records
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  # Filter out records missing `form_date`s
  filter(!is.na(form_date)) %>% 
  # Get rid of records without any relevant data (likely milestoned pts)
  get_nonempty_records(rel_fields)

# _ _ UMMAP General ----

df_ug_cln <- df_ug %>% 
  # Deselect useless field(s)
  select(-redcap_event_name)


# _ _ MiNDSet Registry ----

df_ms_cln <- df_ms %>% 
  # Coerce fields to appropriate types
  mutate(educ_ms = as.integer(ed_level)) %>% 
  # Coerce `birth_date` to date class
  mutate(dob_ms = lubridate::as_date(birth_date)) %>% 
  select(-ed_level, -birth_date)

# _ Mutate Data ----

# _ _ UDS 3 ----

dx_vars <- fields_u3_d1_raw %>%
  str_replace_all("fu_|tele_", NA_character_) %>%
  stringi::stri_remove_empty_na()

df_u3_cln_mut <- df_u3_cln %>% 
  # Coerce `dob` to date class
  mutate(dob = lubridate::as_date(dob)) %>% 
  # Coalesce initial-followup-telephone fields
  coalesce_ift_cols() %>% 
  # Derive MADC Consensus Dx
  mutate_at(vars(all_of(dx_vars)), as.integer) %>% 
  derive_consensus_dx()

# _ _ MiNDSet Registry ----

df_ms_cln_mut <- df_ms_cln %>% 
  # Convert `race_value` to match UDS 3 `race`
  mutate(race_ms = case_when(
    race_value == 1 ~ 1L,  # White
    race_value == 2 ~ 2L,  # Black
    race_value == 3 ~ 5L,  # Asian
    race_value == 5 ~ 50L, # Other
    race_value == 6 ~ 99L, # Unknown
    TRUE ~ NA_integer_
  )) %>% 
  select(-race_value) %>% 
  # Convert `handedness___*` to match UDS 3 `handed`
  mutate(handed_ms = case_when(
    handedness___1 == 1 ~ 2L, # R
    handedness___2 == 1 ~ 1L, # L
    handedness___3 == 1 ~ 3L, # A
    TRUE ~ NA_integer_
  )) %>% 
  select(-handedness___1, -handedness___2, -handedness___3) 

# _ Join all three DFs

df_u3_ug_ms <- df_u3_cln_mut %>% 
  left_join(df_ug_cln, 
            by = c("ptid" = "subject_id", "form_date" = "exam_date")) %>% 
  left_join(df_ms_cln_mut,
            by = c("ptid" = "subject_id"))


# _ Spackle Missing Demographic Data ----

df_u3_ug_ms_cln <- df_u3_ug_ms %>% 
  mutate_at(vars(starts_with("race"),
                 starts_with("educ"),
                 starts_with("handed")),
            as.integer) %>% 
  mutate_at(vars(starts_with("dob")),
            lubridate::as_date) %>% 
  mutate(race   = coalesce(!!!select(., starts_with("race"))),
         educ   = coalesce(!!!select(., starts_with("educ"))),
         handed = coalesce(!!!select(., starts_with("handed"))),
         dob    = coalesce(!!!select(., starts_with("dob")))) %>% 
  select(-ends_with("_ms"))

# _ Mutate Spackled Data ----

df_u3_ug_ms_cln_mut <- df_u3_ug_ms_cln %>% 
  # Calculate age
  calculate_age(dob, form_date) %>% 
  # Mutate `mri_dir_name`
  rowwise() %>% 
  mutate(mri_dir_name = case_when(
    !is.na(mri_sub_id) & !is.na(seq_num) ~
      paste0(
        paste0(mri_sub_id,
               "_0",
               str_split(seq_num, pattern = "(,\\s*)")[[1]]),
        collapse = ","),
    !is.na(mri_sub_id) & is.na(seq_num) ~
      paste0(mri_sub_id, "_0????"),
    TRUE ~ NA_character_
  )) %>% 
  ungroup() %>% 
  # Reörder initial fields
  select(ptid, form_date, dob, starts_with("age"), madc_dx,
         mri_sub_id, seq_num, mri_completed,
         mri_date, mri_sub_id,
         mri_dir_name,
         everything()) %>% 
  select(-dob, -age_years, -age_units) %>% 
  select(-ends_with("_complete"))


# WRITE CSV ----

readr::write_csv(df_u3_ug_ms_cln_mut, 
                 paste0("df_u3_ug_ms_cln_mut_", Sys.Date(), ".csv"), 
                 na = "")


###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
