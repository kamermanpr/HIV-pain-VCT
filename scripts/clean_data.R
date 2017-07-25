############################################################
#                                                          #
#            Process the original datasets and             #
#           generate a single cleaned RDS output           #
#                                                          #
############################################################
# Load packages
library(dplyr)

############################################################
#                                                          #
#                   General information                    #
#                                                          #
############################################################
# Import data
general_info <- readr::read_csv('./original-data/general_info.csv')

# Quick look
head(general_info)
tail(general_info)
glimpse(general_info)
summary(general_info)

# Clean data
general_info <- general_info %>%
    # Convert to date
    mutate(date = lubridate::dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Convert all character columns to lower case
    mutate_if(is.character, stringr::str_to_lower) %>%
    # Recode sex
    mutate(sex = ifelse(sex == 1,
                        yes = 'male',
                        no = 'female')) %>%
    # Rename P_group column
    rename(population = P_group) %>%
    # Recode population
    mutate(population = ifelse(population == 'black',
                               yes = 'african',
                               no = 'mixed race')) %>%
    # Rename laguage column
    rename(language = laguage) %>%
    # Fix misspelling of Zulu
    mutate(language = ifelse(language == 'zolu',
                             yes = 'zulu',
                             no = paste(language))) %>%
    # Rename Education column
    rename(formal_schooling = Education) %>%
    # Recode formal schooling
    mutate(formal_schooling = ifelse(formal_schooling == 'schooled',
                                     yes = 'yes',
                                     no = ifelse(is.na(formal_schooling),
                                                 yes = NA,
                                                 no = 'no'))) %>%
    # Rename Employment column
    rename(employment = Employment) %>%
    # Collapse employment factors levels
    mutate(employment =
               ifelse(employment %in%
                          c('employed (part time/piece work) self employed',
                            'employed (part time/piece work)- volunteer'),
                               yes = 'employed (part time)',
                               no = ifelse(employment %in%
                                               c('employed',
                                                 'employed- self employed',
                                                 'employed- volunteer',
                                                 'self employed',
                                                 'self empoyed'),
                                           yes = 'employed',
                                           no = paste(employment)))) %>%
    # Rename Grant column
    rename(grant = Grants) %>%
    # Recode grant
    mutate(grant = ifelse(grant == 0,
                          yes = 'No',
                          no = 'Yes')) %>%
    # Rename G_specify
    rename(grant_type = G_specify) %>%
    # Recode grant_type
    mutate(grant_type = case_when(
        stringr::str_detect(.$grant_type, 'c...d') ~
            paste('child grant'),
        stringr::str_detect(.$grant_type, 'c..d') ~
            paste('child grant'),
        stringr::str_detect(.$grant_type, 'kids') ~
            paste('child grant'),
        stringr::str_detect(.$grant_type, 'pension') ~
            paste('pension'),
        stringr::str_detect(.$grant_type, 'disability') ~
            paste('disability grant'),
        stringr::str_detect(.$grant_type, 'medical') ~
            paste('disability grant'))) %>%
    # Recode employment based on whether on pension
    mutate(employment = case_when(
        stringr::str_detect(.$grant_type, 'pension') &
            !is.na(.$grant_type) ~ paste('on pension'),
        stringr::str_detect(.$grant_type, 'disability') &
            !is.na(.$grant_type) ~ paste('on disability'))) %>%
    # Rename Schooled_other1/2
    rename(school_grade = Schooled_other1,
           post_school_qualification = Schooled_other2) %>%
    # New column for 'in_school'
    mutate(still_in_school =
               ifelse(stringr::str_detect(school_grade, 'still') |
                          stringr::str_detect(
                              post_school_qualification, 'still'),
                      yes = 'yes',
                      no = ifelse(is.na(school_grade),
                                  yes = NA,
                                  no = 'no'))) %>%
    # Recode school_grade
    mutate(school_grade = as.numeric(
        case_when(
        stringr::str_detect(.$school_grade, 'm....') &
            !is.na(.$school_grade) ~ paste('12'),
        stringr::str_detect(.$school_grade, 'grade 12') &
            !is.na(.$school_grade) ~ paste('12'),
        stringr::str_detect(.$school_grade, 'grade 11') &
            !is.na(.$school_grade) ~ paste('11'),
        stringr::str_detect(.$school_grade, 'still') &
            !is.na(.$school_grade) ~ paste('11'),
        stringr::str_detect(.$school_grade, 'grade 10') &
            !is.na(.$school_grade) ~ paste('10'),
        stringr::str_detect(.$school_grade, 'grade 9') &
            !is.na(.$school_grade) ~ paste('9'),
        stringr::str_detect(.$school_grade, 'grade 8') &
            !is.na(.$school_grade) ~ paste('8'),
        stringr::str_detect(.$school_grade, 'grade 7') &
            !is.na(.$school_grade) ~ paste('7'),
        stringr::str_detect(.$school_grade, 'grade 6') &
            !is.na(.$school_grade) ~ paste('6'),
        stringr::str_detect(.$school_grade, 'grade 5') &
            !is.na(.$school_grade) ~ paste('5'),
        stringr::str_detect(.$school_grade, 'grade 4') &
            !is.na(.$school_grade) ~ paste('4'),
        stringr::str_detect(.$school_grade, 'grade 3') &
            !is.na(.$school_grade) ~ paste('3'),
        stringr::str_detect(.$school_grade, 'grade 2') &
            !is.na(.$school_grade) ~ paste('2'),
        stringr::str_detect(.$school_grade, 'grade 1') &
            !is.na(.$school_grade) ~ paste('1'),
        stringr::str_detect(.$school_grade, 'form') &
            !is.na(.$school_grade) ~ paste('3'),
        stringr::str_detect(.$school_grade, 'grade11') &
            !is.na(.$school_grade) ~ paste('11'),
        stringr::str_detect(.$school_grade, 'tertiary') &
            !is.na(.$school_grade) ~ paste('12'),
        TRUE ~ as.character(.$school_grade)))) %>%
    # Recode post_school_qualification
    mutate(post_school_qualification =
               ifelse(stringr::str_detect(post_school_qualification, 'n.ne') |
                          stringr::str_detect(post_school_qualification, 'no'),
                      yes = 'no',
                      no = ifelse(is.na(post_school_qualification),
                                  yes = NA,
                                  no = 'yes'))) %>%
    # Make schooling categories
    mutate(educational_level = case_when(
        .$school_grade > 0 & .$school_grade < 8 ~
            paste('primary school'),
        .$school_grade > 7 & .$post_school_qualification == 'no' ~
            paste('secondary school'),
        .$post_school_qualification == 'yes' ~
            paste('tertiary')
    )) %>%
    # Reorder columns
    select(PID, date, age, sex, population, language, countryOB,
           formal_schooling, school_grade, still_in_school,
           post_school_qualification, educational_level, employment,
           grant, grant_type)

# Write to CSV (flat file for data sharing)
# readr::write_csv(gen_info, './data/general_info.csv')

# Write to RDS (for data analysis)
# readr::write_rds(gen_info, './data/general_info.rds')

############################################################
#                                                          #
#                       HIV history                        #
#                                                          #
############################################################
# Import data
hiv_test <- readr::read_csv('./original-data/hiv_test_results.csv')

# Quick look
head(hiv_test)
tail(hiv_test)
glimpse(hiv_test)
summary(hiv_test)

# Clean data
hiv_test <- hiv_test %>%
    # Rename columns
    rename(test_1_date = test_dt1,
           test_2_date = Test_dt2,
           test_result = HIV_result) %>%
    # Convert to date
    mutate_at(vars(date, test_1_date, test_2_date), lubridate::dmy) %>%
    # Remove visit number
    select(-v_number) %>%
    # Remove empty columns
    select(PID, date, test_1_date, test_2_date, test_result, CD4_count) %>%
    # Recode test_result
    mutate(test_result = case_when(
        .$test_result == 1 & !is.na(.$test_result) ~ 'HIV positive',
        .$test_result == 2 & !is.na(.$test_result) ~ 'HIV negative'
    ))

# Write to CSV (flat file for data sharing)
# readr::write_csv(hiv_test, './data/hiv_test_results.csv')

# Write to RDS (for data analysis)
# readr::write_rds(hiv_test, './data/hiv_test_results.rds')

############################################################
#                                                          #
#                   Medical information                    #
#                                                          #
############################################################
# Import data
medical_info <- readr::read_csv('./original-data/medical_info.csv')

# Quick look
head(medical_info)
tail(medical_info)
glimpse(medical_info)
summary(medical_info)

# Clean data
medical_info <- medical_info %>%
    # Convert to date
    mutate(date = lubridate::dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Convert all character columns to lower case
    mutate_if(is.character, stringr::str_to_lower) %>%
    # Rename mass column
    rename(mass_kg = mass) %>%
    # Fix mass column formatting
    mutate(mass_kg = stringr::str_replace_all(mass_kg,
                                              pattern = 'no \\s?scale',
                                              replacement = 'NA'),
           mass_kg = stringr::str_replace_all(mass_kg,
                                              pattern = ',',
                                              replacement = '.'),
           mass_kg = ifelse(mass_kg == 'NA',
                         yes = NA,
                         no = paste(mass_kg)),
           mass_kg = as.numeric(mass_kg)) %>%
    # Rename height column
    rename(height_cm = height) %>%
    # Fix height column formatting
    mutate(height_cm = stringr::str_replace_all(height_cm,
                                                pattern = 'no \\s?scale',
                                                replacement = 'NA'),
           height_cm = stringr::str_replace_all(height_cm,
                                                pattern = ',,?',
                                                replacement = '.'),
           height_cm = ifelse(height_cm == 'NA',
                           yes = NA,
                           no = paste(height_cm)),
           height_cm = as.numeric(height_cm)) %>%
    # Rename alcohol column
    rename(alcohol_consumption = alcohol) %>%
    # Recode consumes_alcohol column
    mutate(alcohol_consumption = ifelse(alcohol_consumption == 1,
                                        yes = 'yes',
                                        no = ifelse(alcohol_consumption == 2,
                                                    yes = 'unknown',
                                                    no = 'no'))) %>%
    # Rename alcohol_often column
    rename(alcohol_freq = alcohol_often) %>%
    # Recode alcohol_freq column
    mutate(alcohol_freq = ifelse(alcohol_freq == 1,
                                 yes = 'daily',
                                 no = ifelse(alcohol_freq == 2,
                                             yes = 'weekly',
                                             no = ifelse(
                                                 is.na(alcohol_freq),
                                                 yes = NA,
                                                 no = 'monthly'))),
           # Convert to ordered factor
           alcohol_freq = factor(forcats::fct_relevel(alcohol_freq,
                                                      'daily',
                                                      'weekly',
                                                      'monthly'),
                                 ordered = TRUE)) %>%
    # Recode alcohol_type column
    mutate(alcohol_type = ifelse(alcohol_type == 1,
                                 yes = 'beer',
                                 no = ifelse(alcohol_freq == 2,
                                             yes = 'wine',
                                             no = ifelse(is.na(alcohol_type),
                                                         yes = NA,
                                                         no = 'spirits')))) %>%
    # Rename alcohol_average column
    rename(alcohol_per_sitting = alcohol_average) %>%
    # Recode alcohol_per_sitting column
    mutate(alcohol_per_sitting = ifelse(alcohol_per_sitting == 1,
                                 yes = '1-2',
                                 no = ifelse(alcohol_per_sitting == 2,
                                             yes = '3-4',
                                             no = ifelse(
                                                 is.na(alcohol_per_sitting),
                                                 yes = NA,
                                                 no = '>4'))),
           # Convert to ordered factor
           alcohol_per_sitting = factor(
               forcats::fct_relevel(alcohol_per_sitting,
                                    '1-2',
                                    '3-4',
                                    '>4'),
               ordered = TRUE)) %>%
    # Rename TB column
    rename(TB_ever = TB) %>%
    # Recode TB_infection column
    mutate(TB_ever = ifelse(TB_ever == 1,
                            yes = 'yes',
                            no = ifelse(TB_ever == 2,
                                        yes = 'unknown',
                                        no = 'no'))) %>%
    # Rename TB_specify column
    rename(TB_current = TB_specify) %>%
    # Recode TB_current column
    mutate(TB_current = ifelse(TB_current == 1,
                               yes = 'yes',
                               no = ifelse(is.na(TB_current),
                                                    yes = NA,
                                                    no = 'no'))) %>%
    # Recode diabetes column
    mutate(diabetes = ifelse(diabetes == 1,
                             yes = 'yes',
                             no = ifelse(diabetes == 2,
                                         yes = 'unknown',
                                         no = ifelse(is.na(diabetes),
                                                     yes = NA,
                                                     no = 'no'))))

# Write to CSV (flat file for data sharing)
# readr::write_csv(med_info, './data/medical_info.csv')

# Write to RDS (for data analysis)
# readr::write_rds(med_info, './data/medical_info.rds')


############################################################
#                                                          #
#                          EQ-5D 3L                        #
#                                                          #
############################################################
# Import data
eq5d <- readr::read_csv('./original-data/eq5d.csv')

# Quick look
head(eq5d)
tail(eq5d)
glimpse(eq5d)
summary(eq5d)

# Clean data
eq5d <- eq5d %>%
    # Convert to date
    mutate(date = lubridate::dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Convert all character columns to lower case
    mutate_if(is.character, stringr::str_to_lower) %>%
    # Recode mobility
    mutate(mobility = ifelse(mobility == 1,
                             yes = 'no problems',
                             no = ifelse(mobility == 2,
                                         yes = 'some problems',
                                         no = ifelse(is.na(mobility),
                                                     yes = NA,
                                                     no = 'confined to bed'))),
           # Convert to ordered factor
           mobility = factor(forcats::fct_relevel(mobility,
                                                  'no problems',
                                                  'some problems',
                                                  'confined to bed'),
                             ordered = TRUE)) %>%
    # Recode self_care
    mutate(self_care =
               ifelse(self_care == 1,
                      yes = 'no problems',
                      no = ifelse(self_care == 2,
                                  yes = 'some problems',
                                  no = ifelse(is.na(self_care),
                                              yes = NA,
                                              no = 'unable to wash or dress'))),
           # Convert to ordered factor
           self_care = factor(forcats::fct_relevel(self_care,
                                                  'no problems',
                                                  'some problems',
                                                  'unable to wash or dress'),
                             ordered = TRUE)) %>%
    # Rename usual_activ
    rename(usual_activities = usual_activ) %>%
    # Recode usual_activities
    mutate(usual_activities =
               ifelse(usual_activities == 1,
                      yes = 'no problems',
                      no = ifelse(usual_activities == 2,
                                  yes = 'some problems',
                                  no = ifelse(
                                      is.na(usual_activities),
                                      yes = NA,
                                      no = 'unable to perform usual activities'))),
           # Convert to ordered factor
           usual_activities = factor(
               forcats::fct_relevel(usual_activities,
                                    'no problems',
                                    'some problems',
                                    'unable to perform usual activities'),
               ordered = TRUE)) %>%
    # Rename p_discomfort
    rename(pain_discomfort = p_discomfort) %>%
    # Recode pain_discomfort
    mutate(pain_discomfort =
               ifelse(pain_discomfort == 1,
                      yes = 'no pain or discomfort',
                      no = ifelse(
                          pain_discomfort == 2,
                          yes = 'moderate pain or discomfort',
                          no = ifelse(
                              is.na(pain_discomfort),
                              yes = NA,
                              no = 'extreme pain or discomfort'))),
           # Convert to ordered factor
           pain_discomfort = factor(
               forcats::fct_relevel(pain_discomfort,
                                    'no pain or discomfort',
                                    'moderate pain or discomfort',
                                    'extreme pain or discomfort'),
               ordered = TRUE)) %>%
    # Recode anxiety
    mutate(anxiety =
               ifelse(anxiety == 1,
                      yes = 'no anxiety or depression',
                      no = ifelse(anxiety == 2,
                                  yes = 'moderate anxiety or depression',
                                  no = ifelse(
                                      is.na(anxiety),
                                      yes = NA,
                                      no = 'extreme anxiety or depression'))),
           # Convert to ordered factor
           anxiety = factor(
               forcats::fct_relevel(anxiety,
                                    'no anxiety or depression',
                                    'moderate anxiety or depression',
                                    'extreme anxiety or depression'),
               ordered = TRUE)) %>%
    # Rename health_code
    rename(qol_vas = health_code)

# Write to CSV (flat file for data sharing)
# readr::write_csv(eq5d, './data/eq5d.csv')

# Write to RDS (for data analysis)
# readr::write_rds(eq5d, './data/eq5d.rds')

############################################################
#                                                          #
#             Pain Catastrophizing Scale (PCS)             #
#                                                          #
############################################################
# Import data
pcs <- readr::read_csv('./original-data/pcs.csv')

# Quick look
head(pcs)
tail(pcs)
glimpse(pcs)
summary(pcs)

# Clean data
pcs <- pcs %>%
    # Convert to date
    mutate(date = lubridate::dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Create total and subscales scores
    mutate(rumination_score = rowSums(.[c(10, 11, 12, 13)]),
           magnification_score = rowSums(.[c(8, 9, 15)]),
           helplessness_score = rowSums(.[c(3, 4, 5, 6, 7, 14)]),
           total_score = rowSums(.[3:15]),
           total_score_30 = ifelse(total_score >= 30,
                                   yes = 'yes',
                                   no = 'no')) #%>%
    # Convert from numeric to ordinal factor
    #mutate_at(var(starts_with('pcs')),
     #        funs(factor(
      #           forcats::fct_relevel(as.character(.),
       #                               '1', '2', '3', '4', '5'),
        #         ordered = TRUE)))

# Write to CSV (flat file for data sharing)
# readr::write_csv(pcs, './data/pcs.csv')

# Write to RDS (for data analysis)
# readr::write_rds(pcs, './data/pcs.rds')

############################################################
#                                                          #
#           Hopkins Symptom Checklist 25 (HSCL)            #
#                                                          #
############################################################
# Import data
hscl <- readr::read_csv('./original-data/hscl.csv')

# Quick look
head(hscl)
tail(hscl)
glimpse(hscl)
summary(hscl)

# Clean data
hscl <- hscl %>%
    # Convert to date
    mutate(date = lubridate::dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Create total and subscales scores
    mutate(anxiety_score = rowSums(.[3:12]) / 10,
           depression_score = rowSums(.[13:27]) / 15,
           total_score = rowSums(.[3:27]) / 25,
           total_score_1.55 = ifelse(total_score >= 1.55,
                                     yes = 'yes',
                                     no = 'no'),
           total_score_1.75 = ifelse(total_score >= 1.75,
                                     yes = 'yes',
                                     no = 'no')) #%>%
    # Convert from numeric to ordinal factor
    #mutate_if(is.numeric, funs(factor(
        #forcats::fct_relevel(as.character(.),
                             #'1', '2', '3', '4'),
        #ordered = TRUE)))

# Write to CSV (flat file for data sharing)
# readr::write_csv(hscl, './data/hscl.csv')

# Write to RDS (for data analysis)
# readr::write_rds(hscl, './data/hscl.rds')

############################################################
#                                                          #
#                     Neuro exam signs                     #
#                                                          #
############################################################
# Import data
signs <- readr::read_csv('./original-data/signs_neuro.csv')

# Quick look
head(signs)
tail(signs)
glimpse(signs)
summary(signs)

# Clean data
signs <- signs %>%
    # Convert to date
    mutate(date = lubridate::dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Rename vibrationR?
    rename(vibration_R_s = vibrationR1,
           vibration_L_s = vibrationL1) %>%
    # Remove vibration 'interpretation' columns
    select(-vibrationR2,
           -vibrationL2) %>%
    # Make new vibration 'interpretation' column
    mutate(reduced_vibration_bilateral = case_when(
        .$vibration_R_s >= 10 | .$vibration_L_s >= 10 ~ 'no',
        .$vibration_R_s < 10 & .$vibration_L_s < 10 ~ 'yes'
    )) %>%
    # Rename Ankle_refelxesR?
    rename(ankle_reflex_R = Ankle_reflexesR,
           ankle_reflex_L = Ankle_reflexesL) %>%
    # Recode ankle reflex data
    mutate(ankle_reflex_R =
               ifelse(ankle_reflex_R == 1,
                      yes = 'present',
                      no = ifelse(is.na(ankle_reflex_R),
                                  yes = NA,
                                  no = ifelse(ankle_reflex_R == 0,
                                              yes = 'absent',
                                              no = 'not done'))),
           ankle_reflex_L =
               ifelse(ankle_reflex_L == 1,
                      yes = 'present',
                      no = ifelse(is.na(ankle_reflex_L),
                                  yes = NA,
                                  no = ifelse(ankle_reflex_L == 0,
                                              yes = 'absent',
                                              no = 'not done')))) %>%
    # Make an ankle_reflex 'interpretation' column
    mutate(absent_reflexes_bilateral = case_when(
        .$ankle_reflex_R == 'present' |
            .$ankle_reflex_L == 'present' ~ 'no',
        .$ankle_reflex_R == 'absent' &
            .$ankle_reflex_L == 'absent' ~ 'yes',
        .$ankle_reflex_R == 'not done' |
            .$ankle_reflex_L == 'not done' ~ 'not done')) %>%
    # Pin-prick proximal distribution data has too many holes
    # and is not required, so only analyse the yes/no coding for
    # presence vs absence of sensation.
    # Remove 'extent' columns
    select(-pinR_princk2,
           -pinL_princk4) %>%
    # Rename pin?_princk? columns
    rename(reduced_pinprick_R = pinR_princk1,
           reduced_pinprick_L = pinL_princk3) %>%
    # Recode pin-prick data
    mutate(reduced_pinprick_R =
               ifelse(is.na(reduced_pinprick_R),
                      yes = NA,
                      no = ifelse(reduced_pinprick_R == 1,
                                  yes = 'no',
                                  no = 'yes')),
           reduced_pinprick_L =
               ifelse(is.na(reduced_pinprick_L),
                      yes = NA,
                      no = ifelse(reduced_pinprick_L == 1,
                                  yes = 'no',
                                  no = 'yes'))) %>%
    # Make a pin-prick 'interpretation' column
    mutate(reduced_pinprick_bilateral = case_when(
        .$reduced_pinprick_R == 'no' |
            .$reduced_pinprick_L == 'no' ~ 'no',
        .$reduced_pinprick_R == 'yes' &
            .$reduced_pinprick_L == 'yes' ~ 'yes')) %>%
    # Make new column indicating whether DSP is present
    # Only evaluate individuals with 3/3 assessments, others = <NA>
    mutate(SN_present =
               ifelse(reduced_vibration_bilateral == 'yes' &
                      absent_reflexes_bilateral == 'yes' &
                      reduced_pinprick_bilateral == 'yes',
                      yes = 'yes',
                      no = ifelse(c(reduced_vibration_bilateral == 'yes' &
                                  absent_reflexes_bilateral == 'yes') |
                                  c(absent_reflexes_bilateral == 'yes' &
                                  reduced_pinprick_bilateral == 'yes') |
                                  c(reduced_vibration_bilateral == 'yes' &
                                  reduced_pinprick_bilateral == 'yes'),
                                  yes = 'yes',
                                  no = 'no')))

############################################################
#                                                          #
#                   Neuro exam symptoms                    #
#                                                          #
############################################################
# Import data
symptoms <- readr::read_csv('./original-data/symptoms_neuro.csv')

# Quick look
head(symptoms)
tail(symptoms)
glimpse(symptoms)
summary(symptoms)

# Clean data
symptoms <- symptoms %>%
    # Convert to date
    mutate(date = lubridate::dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Remove other unused columns (leftover from EQ-5D)
    select(-mobility,
           -self_care,
           -usual_activ,
           -anxiety,
           -p_discomfort,
           -health_code) %>%
    # Rename neurological?
    rename(foot_symptoms_ever = neurologicala,
           foot_symptoms_current = neurologicalb) %>%
    # Recode symptoms_?
    mutate_at(vars(starts_with('foot_symptoms')),
              funs(ifelse(. == 1,
                          yes ='yes',
                          no = 'no'))) %>%
    # Make new symptom history symmary column
    mutate(foot_symptoms = case_when(
        .$foot_symptoms_ever == 'yes' &
            .$foot_symptoms_current == 'yes' ~ 'current',
        .$foot_symptoms_ever == 'yes' &
            .$foot_symptoms_current == 'no' ~ 'past',
        .$foot_symptoms_ever == 'no' &
            .$foot_symptoms_current == 'yes' ~ 'unclear',
        .$foot_symptoms_ever == 'no' &
            .$foot_symptoms_current == 'no' ~ 'never')) %>%
    # Rename ?_other
    rename(hot_burning_intensity = hot_burning_other,
           cramping_intensity = cramping_other,
           painful_intensity = p_aching_other,
           itching_intensity = itching_other,
           numbness_intensity = numb_lack_other,
           cold_freezing_intensity = cold_freez_other,
           pins_needles_intensity = p_needles_other) %>%
    # Recode and order symptom intensity
    mutate_at(vars(ends_with('intensity')),
              funs(ifelse(. == 1,
                          yes = 'mild',
                          no = ifelse(. == 2,
                                      yes = 'moderate',
                                      no = 'severe')))) %>%
    mutate_at(vars(ends_with('intensity')),
              funs(factor(
                  forcats::fct_relevel(.,
                                       'mild', 'moderate', 'severe'),
                  ordered = TRUE))) %>%
    # Rename symptoms
    rename(painful = p_aching,
           numbness = numb_lack,
           cold_freezing = cold_freez,
           pins_needles = p_needles) %>%
    # Recode symptom occurrence
    mutate_at(vars(c(5, 7, 9, 11, 13, 15, 17)),
              funs(ifelse(. == 0,
                          yes = 'no',
                          no = ifelse(. == 1,
                                      yes = 'unsure',
                                      no = 'yes')))) %>%
    # Reorder columns
    select(c(1:4, 19, 5:18))

############################################################
#                                                          #
#                        WBPQ data                         #
#                                                          #
############################################################
# Import data
wbpq <- readr::read_csv('./original-data/wbpq.csv')

# Quick look
head(wbpq)
tail(wbpq)
glimpse(wbpq)
summary(wbpq)

# Clean data
wbpq2 <- wbpq %>%
    # Convert to date
    mutate(date = lubridate::dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Rename pain presence / sites columns
    rename(current_pain = paina,
           pain_in_last_month = painb,
           pain_reason_for_visit = painc,
           low_back = low_b,
           genitals = private_p,
           site_of_worst_pain = worst_pain) %>%
    # Recode pain presence / sites columns
    mutate_at(vars(current_pain, pain_in_last_month, pain_reason_for_visit,
                   head, shoulders, arms, hands,
                   chest, abdomen, low_back, genitals,
                   legs, feet, joints, muscles),
              factor,
              levels = c(0, 1),
              labels = c('no', 'yes')) %>%
    # Remove superficial/deep pain columns (won't use data)
    select(-starts_with('inside'),
           -starts_with('skin')) %>%
    # Convert text to lower-case for all character columns
    mutate_if(is.character,
              tolower) %>%
    # Rename pain_cause? columns
    rename(pain_cause_head = pain_cause1,
           pain_cause_shoulders = pain_cause2,
           pain_cause_arms = pain_cause3,
           pain_cause_hands = pain_cause4,
           pain_cause_chest = pain_cause5,
           pain_cause_abdomen = pain_cause6,
           pain_cause_low_back = pain_cause7,
           pain_cause_genitals = pain_cause8,
           pain_cause_legs = pain_cause9,
           pain_cause_feet = pain_cause10,
           pain_cause_joints = pain_cause11,
           pain_cause_muscles = pain_cause12) %>%
    # Simplify pain_cause? (requires manual inspection)
    ## Head pain
    mutate(pain_cause_head = ifelse(head == 'no' | is.na(head),
                                    yes = NA,
                                    no = pain_cause_head),
           pain_cause_head = case_when(
               stringr::str_detect(.$pain_cause_head, '^m.....[se]$') |
                   stringr::str_detect(.$pain_cause_head, 'unknown') ~ 'unspecified cause',
               stringr::str_detect(.$pain_cause_head, 'stress') |
                   stringr::str_detect(.$pain_cause_head, 'thinking')  ~ 'stress-related',
               TRUE ~ 'other')) %>%
    ## Shoulder pain
    mutate(pain_cause_shoulders = case_when(
        stringr::str_detect(.$pain_cause_shoulders, 'exercis') |
            stringr::str_detect(.$pain_cause_shoulders, 'sport') |
            stringr::str_detect(.$pain_cause_shoulders, 'heavy') |
            stringr::str_detect(.$pain_cause_shoulders, 'muscle') |
            stringr::str_detect(.$pain_cause_shoulders, 'work') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_shoulders, 'stress') |
            stringr::str_detect(.$pain_cause_shoulders, 'tense') |
            stringr::str_detect(.$pain_cause_shoulders, 'thinking')  ~ 'stress-related',
        stringr::str_detect(.$pain_cause_shoulders, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_shoulders = ifelse(shoulders == 'no' | is.na(shoulders),
                                      yes = NA,
                                      no = ifelse(is.na(pain_cause_shoulders),
                                                  yes = 'unspecified cause',
                                                  no = pain_cause_shoulders))) %>%
    ## Arm pain
    mutate(pain_cause_arms = case_when(
        stringr::str_detect(.$pain_cause_arms, 'blood') |
            stringr::str_detect(.$pain_cause_arms, 'injection') ~ 'procedural pain',
        stringr::str_detect(.$pain_cause_arms, 'exercis') |
            stringr::str_detect(.$pain_cause_arms, 'jogging') |
            stringr::str_detect(.$pain_cause_arms, 'scratch') |
            stringr::str_detect(.$pain_cause_arms, 'accident') |
            stringr::str_detect(.$pain_cause_arms, 'garden') |
            stringr::str_detect(.$pain_cause_arms, 'wsshing') |
            stringr::str_detect(.$pain_cause_arms, 'alcohol') |
            stringr::str_detect(.$pain_cause_arms, 'fell') |
            stringr::str_detect(.$pain_cause_arms, 'broke') |
            stringr::str_detect(.$pain_cause_arms, 'sport') |
            stringr::str_detect(.$pain_cause_arms, 'heavy') |
            stringr::str_detect(.$pain_cause_arms, 'muscle') |
            stringr::str_detect(.$pain_cause_arms, 'work') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_arms, 'arth') ~ 'arthritis',
        stringr::str_detect(.$pain_cause_arms, 'stress') ~ 'stress-related',
        stringr::str_detect(.$pain_cause_arms, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_arms = ifelse(arms == 'no' | is.na(arms),
                                      yes = NA,
                                      no = ifelse(is.na(pain_cause_arms),
                                                  yes = 'unspecified cause',
                                                  no = pain_cause_arms))) %>%
    ## Hand pain
    mutate(pain_cause_hands = case_when(
        stringr::str_detect(.$pain_cause_hands, 'burn') |
            stringr::str_detect(.$pain_cause_hands, 'cut') |
            stringr::str_detect(.$pain_cause_hands, 'washing') |
            stringr::str_detect(.$pain_cause_hands, 'broke') |
            stringr::str_detect(.$pain_cause_hands, 'washing') |
            stringr::str_detect(.$pain_cause_hands, 'accident') |
            stringr::str_detect(.$pain_cause_hands, 'sport') |
            stringr::str_detect(.$pain_cause_hands, 'fight') |
            stringr::str_detect(.$pain_cause_hands, 'washing') |
            stringr::str_detect(.$pain_cause_hands, 'heavy') |
            stringr::str_detect(.$pain_cause_hands, 'fell') |
            stringr::str_detect(.$pain_cause_hands, 'work') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_hands, 'arth') ~ 'arthritis',
        stringr::str_detect(.$pain_cause_hands, 'unknown') ~ 'unspecified cause',
        stringr::str_detect(.$pain_cause_hands, 'hiv') ~ 'HIV infection',
        TRUE ~ 'other'),
        pain_cause_hands = ifelse(hands == 'no' | is.na(hands),
                                 yes = NA,
                                 no = ifelse(is.na(pain_cause_hands),
                                             yes = 'unspecified cause',
                                             no = pain_cause_hands))) %>%
    ## Chest pain
    mutate(pain_cause_chest = case_when(
        stringr::str_detect(.$pain_cause_chest, 'exercise') |
            stringr::str_detect(.$pain_cause_chest, 'running') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_chest, 'blocked') |
            stringr::str_detect(.$pain_cause_chest, 'failer') ~ 'MI or heart failure',
        stringr::str_detect(.$pain_cause_chest, 'smoking') ~ 'smoking',
        stringr::str_detect(.$pain_cause_chest, 'cough') |
            stringr::str_detect(.$pain_cause_chest, 'caughing') |
            stringr::str_detect(.$pain_cause_chest, 'pneumonia') |
            stringr::str_detect(.$pain_cause_chest, 'flu') ~ 'cough or infection',
        stringr::str_detect(.$pain_cause_chest, 'unknown') |
            stringr::str_detect(.$pain_cause_chest, 'sharp') ~ 'unspecified cause',
        stringr::str_detect(.$pain_cause_chest, 'acid') |
            stringr::str_detect(.$pain_cause_chest, 'heartburn') |
            stringr::str_detect(.$pain_cause_chest, 'heart.burn') |
            stringr::str_detect(.$pain_cause_chest, 'ulcer') ~ 'ulcer\\heart-burn',
        TRUE ~ 'other'),
        pain_cause_chest = ifelse(chest == 'no' | is.na(chest),
                                  yes = NA,
                                  no = ifelse(is.na(pain_cause_chest),
                                              yes = 'unspecified cause',
                                              no = pain_cause_chest))) %>%
    ## Abdominal pain
    mutate(pain_cause_abdomen = case_when(
        stringr::str_detect(.$pain_cause_abdomen, 'sex') |
            stringr::str_detect(.$pain_cause_abdomen, 'condom') ~ 'sex-related',
        stringr::str_detect(.$pain_cause_abdomen, 'ulcer') ~ 'ulcer\\heart-burn',
        stringr::str_detect(.$pain_cause_abdomen, 'planning') |
            stringr::str_detect(.$pain_cause_abdomen, 'smear') |
            stringr::str_detect(.$pain_cause_abdomen, 'apendix') |
            stringr::str_detect(.$pain_cause_abdomen, 'sterilization') |
            stringr::str_detect(.$pain_cause_abdomen, 'oparation') |
            stringr::str_detect(.$pain_cause_abdomen, 'Ceaserian') ~ 'procedural pain',
        stringr::str_detect(.$pain_cause_abdomen, 'cramp') |
            stringr::str_detect(.$pain_cause_abdomen, 'crump') ~ 'cramps',
        stringr::str_detect(.$pain_cause_abdomen, 'constip') |
            stringr::str_detect(.$pain_cause_abdomen, 'costip') ~ 'constipation',
        stringr::str_detect(.$pain_cause_abdomen, 'period') |
            stringr::str_detect(.$pain_cause_abdomen, 'menstr') |
            stringr::str_detect(.$pain_cause_abdomen, 'mentr') ~ 'menstrual pain',
        stringr::str_detect(.$pain_cause_abdomen, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_abdomen = ifelse(abdomen == 'no' | is.na(abdomen),
                                  yes = NA,
                                  no = ifelse(is.na(pain_cause_abdomen),
                                              yes = 'unspecified cause',
                                              no = pain_cause_abdomen))) %>%
    ## Low-back pain
    mutate(pain_cause_low_back = case_when(
        stringr::str_detect(.$pain_cause_low_back, 'ulcer') ~ 'ulcer or heart-burn',
        stringr::str_detect(.$pain_cause_low_back, 'bending') |
            stringr::str_detect(.$pain_cause_low_back, 'exercis') |
            stringr::str_detect(.$pain_cause_low_back, 'running') |
            stringr::str_detect(.$pain_cause_low_back, 'standing') |
            stringr::str_detect(.$pain_cause_low_back, 'walk') |
            stringr::str_detect(.$pain_cause_low_back, 'lifting') |
            stringr::str_detect(.$pain_cause_low_back, 'accident') |
            stringr::str_detect(.$pain_cause_low_back, 'injur') |
            stringr::str_detect(.$pain_cause_low_back, 'work') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_low_back, 'period') |
            stringr::str_detect(.$pain_cause_low_back, 'menstr') |
            stringr::str_detect(.$pain_cause_low_back, 'mentr') ~ 'menstrual pain',
        stringr::str_detect(.$pain_cause_low_back, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_low_back = ifelse(low_back == 'no' | is.na(low_back),
                                    yes = NA,
                                    no = ifelse(is.na(pain_cause_low_back),
                                                yes = 'unspecified cause',
                                                no = pain_cause_low_back))) %>%
    ## Genital pain
    mutate(pain_cause_genitals = case_when(
        stringr::str_detect(.$pain_cause_genitals, 'piles') ~ 'piles',
        stringr::str_detect(.$pain_cause_genitals, 'sex') |
            stringr::str_detect(.$pain_cause_genitals, 'rape') ~ 'sex-related',
        stringr::str_detect(.$pain_cause_genitals, 'sores') |
            stringr::str_detect(.$pain_cause_genitals, 'rash')|
            stringr::str_detect(.$pain_cause_genitals, 'rush') |
            stringr::str_detect(.$pain_cause_genitals, 'piple') ~ 'infection',
        stringr::str_detect(.$pain_cause_genitals, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_genitals = ifelse(genitals == 'no' | is.na(genitals),
                                     yes = NA,
                                     no = ifelse(is.na(pain_cause_genitals),
                                                 yes = 'unspecified cause',
                                                 no = pain_cause_genitals))) %>%
    ## Leg pain
    mutate(pain_cause_legs = case_when(
        stringr::str_detect(.$pain_cause_legs, 'exercis') |
            stringr::str_detect(.$pain_cause_legs, 'running') |
            stringr::str_detect(.$pain_cause_legs, 'standing') |
            stringr::str_detect(.$pain_cause_legs, 'playing') |
            stringr::str_detect(.$pain_cause_legs, 'possition') |
            stringr::str_detect(.$pain_cause_legs, 'muscle') |
            stringr::str_detect(.$pain_cause_legs, 'fell') |
            stringr::str_detect(.$pain_cause_legs, 'walk') |
            stringr::str_detect(.$pain_cause_legs, 'lifting') |
            stringr::str_detect(.$pain_cause_legs, 'accident') |
            stringr::str_detect(.$pain_cause_legs, 'inju') |
            stringr::str_detect(.$pain_cause_legs, 'work') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_legs, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_legs = ifelse(legs == 'no' | is.na(legs),
                                      yes = NA,
                                      no = ifelse(is.na(pain_cause_legs),
                                                  yes = 'unspecified cause',
                                                  no = pain_cause_legs))) %>%
    ## Foot pain
    mutate(pain_cause_feet = case_when(
        stringr::str_detect(.$pain_cause_feet, 'exercis') |
            stringr::str_detect(.$pain_cause_feet, 'standing') |
            stringr::str_detect(.$pain_cause_feet, 'soccer') |
            stringr::str_detect(.$pain_cause_feet, 'twisted') |
            stringr::str_detect(.$pain_cause_feet, 'fell') |
            stringr::str_detect(.$pain_cause_feet, 'walk') |
            stringr::str_detect(.$pain_cause_feet, 'accident') |
            stringr::str_detect(.$pain_cause_feet, 'inju') |
            stringr::str_detect(.$pain_cause_feet, 'work') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_feet, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_feet = ifelse(feet == 'no' | is.na(feet),
                                 yes = NA,
                                 no = ifelse(is.na(pain_cause_feet),
                                             yes = 'unspecified cause',
                                             no = pain_cause_feet))) %>%
    ## Joint pain
    mutate(pain_cause_joints = case_when(
        stringr::str_detect(.$pain_cause_joints, 'not exercising') ~ 'other2',
        stringr::str_detect(.$pain_cause_joints, 'exercis') |
            stringr::str_detect(.$pain_cause_joints, 'standing') |
            stringr::str_detect(.$pain_cause_joints, 'soccer') |
            stringr::str_detect(.$pain_cause_joints, 'sport') |
            stringr::str_detect(.$pain_cause_joints, 'twisted') |
            stringr::str_detect(.$pain_cause_joints, 'fell') |
            stringr::str_detect(.$pain_cause_joints, 'walk') |
            stringr::str_detect(.$pain_cause_joints, 'accident') |
            stringr::str_detect(.$pain_cause_joints, 'inju') |
            stringr::str_detect(.$pain_cause_joints, 'work') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_joints, 'arthritis') |
            stringr::str_detect(.$pain_cause_joints, 'atratise') ~ 'arthritis',
        stringr::str_detect(.$pain_cause_joints, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_joints = ifelse(joints == 'no' | is.na(joints),
                                 yes = NA,
                                 no = ifelse(is.na(pain_cause_joints),
                                             yes = 'unspecified cause',
                                             no = pain_cause_joints))) %>%
    ## Muscle pain
    mutate(pain_cause_muscles = case_when(
        stringr::str_detect(.$pain_cause_muscles, 'exercis') |
            stringr::str_detect(.$pain_cause_muscles, 'standing') |
            stringr::str_detect(.$pain_cause_muscles, 'soccer') |
            stringr::str_detect(.$pain_cause_muscles, 'gym') |
            stringr::str_detect(.$pain_cause_muscles, 'sport') |
            stringr::str_detect(.$pain_cause_muscles, 'walk') |
            stringr::str_detect(.$pain_cause_muscles, 'accident') |
            stringr::str_detect(.$pain_cause_muscles, 'injur') |
            stringr::str_detect(.$pain_cause_muscles, 'work') ~ 'physical injury\\strain',
        stringr::str_detect(.$pain_cause_muscles, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_muscles = ifelse(muscles == 'no' | is.na(muscles),
                                   yes = NA,
                                   no = ifelse(is.na(pain_cause_muscles),
                                               yes = 'unspecified cause',
                                               no = pain_cause_muscles))) %>%
    ## Site of worst pain
    ### Replace 'private parts' with 'genitals'
    mutate(site_of_worst_pain =
               stringr::str_replace_all(site_of_worst_pain,
                                        pattern = 'private parts',
                                        replacement = 'genitals')) %>%
    ### Split columns (one site per column)
    tidyr::separate(col = site_of_worst_pain,
                    sep = '/',
                    into = c('site_of_worst_pain_1',
                             'site_of_worst_pain_2',
                             'site_of_worst_pain_3',
                             'site_of_worst_pain_4',
                             'site_of_worst_pain_5')) %>%
    ## Time of worst pain
    mutate(pain_times =
               stringr::str_replace_all(pain_times,
                                        pattern = 'norning',
                                        replacement = 'morning')) %>%
    ## Recode pain treatment
    mutate(pain_treatment = case_when(
        .$pain_treatment == 1 ~ 'yes',
        .$pain_treatment == 0 ~ 'no'
        )) %>%
    ## Drug prescriptions
    ### Rename columns
    rename(medication_1 = prescribed1,
           prescribed_1 = prescribed_Dr1,
           medication_2 = prescribed2,
           prescribed_2 = prescribed_Dr2,
           medication_3 = prescribed3,
           prescribed_3 = prescribed_Dr3,
           medication_4 = prescribed4,
           prescribed_4 = prescribed_Dr4) %>%
    ### Recode drugs in medication_* columns
    mutate_at(.vars = vars(starts_with('medication_')),
              .funs = funs(case_when(
                  stringr::str_detect(., 'grand') |
                      stringr::str_detect(., 'compral') ~ 'paracetamol + aspirin',
                  stringr::str_detect(., 'adcod') |
                      stringr::str_detect(., 'acod') |
                      stringr::str_detect(., 'alcod') |
                      stringr::str_detect(., 'alcad') |
                      stringr::str_detect(., 'cend') |
                      stringr::str_detect(., 'cind') |
                      stringr::str_detect(., 'lena') |
                      stringr::str_detect(., 'sillp') |
                      stringr::str_detect(., 'silp') |
                      stringr::str_detect(., 'sinut') |
                      stringr::str_detect(., 'betap') ~ 'paracetamol + codeine',
                  stringr::str_detect(., 'anad') |
                      stringr::str_detect(., 'aspr') |
                      stringr::str_detect(., 'dispr') ~ 'aspirin',
                  stringr::str_detect(., 'bruf') |
                      stringr::str_detect(., 'IB-pro') |
                      stringr::str_detect(., 'nurof') ~ 'ibuprofen',
                  stringr::str_detect(., 'diclo') |
                      stringr::str_detect(., 'declo') |
                      stringr::str_detect(., 'volta') ~ 'diclofenac',
                  stringr::str_detect(., 'microd') |
                      stringr::str_detect(., 'miprod') |
                      stringr::str_detect(., 'mybu')
                  ~ 'paracetamol + ibuprofen + codeine',
                  stringr::str_detect(., 'pain blo') |
                      stringr::str_detect(., 'painblo') |
                      stringr::str_detect(., 'pianblo') |
                      stringr::str_detect(., 'paunblo') |
                      stringr::str_detect(., 'painam') |
                      stringr::str_detect(., 'paracet') |
                      stringr::str_detect(., 'paracent') |
                      stringr::str_detect(., 'prarcent') |
                      stringr::str_detect(., 'go pain') |
                      stringr::str_detect(., 'panado') ~ 'paracetamol',
                  stringr::str_detect(., 'tramad') ~ 'tramadol',
                  stringr::str_detect(., 'napro') ~ 'naproxen',
                  stringr::str_detect(., 'trilp') ~ 'amitriptyline',
                  stringr::str_detect(., 'killer') ~ 'not specified'
                  ))) %>%
    ### Recode prescribed_* columns to 'yes' or 'no'
    mutate_at(.vars = vars(starts_with('prescribed_')),
              .funs = funs(ifelse(. == 1,
                                  yes = 'yes',
                                  no = 'no'))) %>%
    ### Clean-up prescribed_* columns to agree with medication_* columns
    mutate(prescribed_1 = ifelse(is.na(medication_1),
                                 yes = NA,
                                 no = prescribed_1),
           prescribed_2 = ifelse(is.na(medication_2),
                                 yes = NA,
                                 no = prescribed_2),
           prescribed_3 = ifelse(is.na(medication_3),
                                 yes = NA,
                                 no = prescribed_3),
           prescribed_4 = ifelse(is.na(medication_4),
                                 yes = NA,
                                 no = prescribed_4)) %>%
    mutate(prescribed_1 = ifelse(!is.na(medication_1) & is.na(prescribed_1),
                                 yes = 'not specified',
                                 no = prescribed_1),
           prescribed_2 = ifelse(!is.na(medication_2) & is.na(prescribed_2),
                                 yes = NA,
                                 no = prescribed_2),
           prescribed_3 = ifelse(!is.na(medication_3) & is.na(prescribed_3),
                                 yes = NA,
                                 no = prescribed_3),
           prescribed_4 = ifelse(!is.na(medication_4) & is.na(prescribed_4),
                                 yes = NA,
                                 no = prescribed_4)) %>%
    ### Clean-up medication_* to remove duplicates
    #### medication_2 / prescribed_2
    mutate(medication_2 = ifelse(prescribed_2 == prescribed_1 &
                                     medication_2 == medication_1,
                                 yes = NA,
                                 no = medication_2),
           prescribed_2 = ifelse(prescribed_2 == prescribed_1 &
                                     medication_2 == medication_1,
                                 yes = NA,
                                 no = prescribed_2)) %>%
    #### medication_3 / prescribed_3
    mutate(medication_3 = ifelse(prescribed_3 == prescribed_1 &
                                     medication_3 == medication_1,
                                 yes = NA,
                                 no = medication_3),
           prescribed_3 = ifelse(prescribed_3 == prescribed_1 &
                                     medication_3 == medication_1,
                                 yes = NA,
                                 no = prescribed_3)) %>%
    #### medication_4 / prescribed_4
    mutate(medication_4 = ifelse(prescribed_4 == prescribed_1 &
                                     medication_4 == medication_1,
                                 yes = NA,
                                 no = medication_4),
           prescribed_4 = ifelse(prescribed_4 == prescribed_1 &
                                     medication_4 == medication_1,
                                 yes = NA,
                                 no = prescribed_4))




############################################################
#                                                          #
#                    Combine dataframes                    #
#                                                          #
############################################################

clean_data <- general_info %>%
    left_join(medical_info) %>%
    left_join(hiv_test) %>%
    left_join(eq5d) %>%
    left_join(hscl) %>%
    left_join(pcs) %>%
    left_join(signs) %>%
    left_join(symptoms)

############################################################
#                                                          #
#                    Output data to RDS                    #
#                                                          #
############################################################
readr::write_rds(clean_data,
                 path = './data/clean_data.rds',
                 compress = 'xz')

############################################################
#                                                          #
#                         Clean-up                         #
#                                                          #
############################################################
rm(list = ls())

