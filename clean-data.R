############################################################
#                                                          #
#            Process the original datasets and             #
#           generate a single cleaned RDS output           #
#                                                          #
############################################################

# Load packages
library(magrittr)
library(tidyverse)
library(lubridate)

############################################################
#                                                          #
#                   General information                    #
#                                                          #
############################################################

# Import data
general_info <- read_csv('data-original/general_info.csv')

# Quick look
head(general_info)
tail(general_info)
glimpse(general_info)
summary(general_info)

# Fix impossible age value (max age = 234 years)
general_info$age[general_info$age == 234] <- NA

# Clean data
general_info %<>%
    # Convert to date
    mutate(date = dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Convert all character columns to lower case
    mutate_if(is.character, str_to_lower) %>%
    # Recode sex
    mutate(sex = ifelse(sex == 1,
                        yes = 'male',
                        no = 'female')) %>%
    # Rename P_group column
    rename(population = P_group) %>%
    # Recode population
    mutate(population = ifelse(population == 'black',
                               yes = 'black african',
                               no = 'mixed race')) %>%
    # Rename laguage column
    rename(language = laguage) %>%
    # Fix misspelling of Zulu
    mutate(language = ifelse(language == 'zolu',
                             yes = 'zulu',
                             no = paste(language))) %>%
    # Rename Employment column
    rename(employment = Employment) %>%
    # Collapse employment factors levels
    mutate(employment =
               ifelse(employment %in%
                          c('employed (part time/piece work) Self employed',
                            'employed (part time/piece work) self employed',
                            'employed (part time/piece work)- volunteer',
                            'employed (part time/piece work)'),
                               yes = 'employed (part time)',
                               no = ifelse(employment %in%
                                               c('employed',
                                                 'employed- self employed',
                                                 'employed- volunteer',
                                                 'self employed',
                                                 'self empoyed',
                                                 'SELF EMPLOYED'),
                                           yes = 'employed',
                                           no = employment))) %>%
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
        str_detect(.$grant_type, 'c.+d$') ~
            paste('child grant'),
        str_detect(.$grant_type, 'kids') ~
            paste('child grant'),
        str_detect(.$grant_type, 'support') ~
            paste('child grant'),
        str_detect(.$grant_type, 'children') ~
            paste('child grant'),
        str_detect(.$grant_type, 'disability') ~
            paste('disability grant'),
        str_detect(.$grant_type, 'medical') ~
            paste('disability grant'),
        str_detect(.$grant_type, 'pension') ~
            paste('pension'),
        TRUE ~ .$grant_type)) %>%
    # Recode employment based on whether on pension / disability
    mutate(employment = case_when(
        str_detect(.$grant_type, 'pension') &
            str_detect(.$employment, 'ployed') ~ paste('pension grant'),
        str_detect(.$grant_type, 'pension') &
            str_detect(.$employment, 'pensioner') ~ paste('pension grant'),
        str_detect(.$grant_type, 'disability') &
            str_detect(.$employment, 'ployed') ~ paste('disability grant'),
        TRUE ~ .$employment
        )) %>%
    # Remove Education column
    rename(formal_schooling = Education) %>%
    # Recode formal schooling
    mutate(formal_schooling = ifelse(formal_schooling == 'schooled',
                                     yes = 'yes',
                                     no = ifelse(is.na(formal_schooling),
                                                 yes = NA,
                                                 no = 'no'))) %>%
    # Rename Schooled_other1/2
    rename(school_grade = Schooled_other1,
           post_school_qualification = Schooled_other2) %>%
    # Recode school_grade
    mutate(school_grade = as.numeric(
        case_when(
        str_detect(.$school_grade, '^[m|M]at') ~ paste('12'),
        str_detect(.$school_grade, 'grade 12') ~ paste('12'),
        str_detect(.$school_grade, 'NCS') ~ paste('12'),
        str_detect(.$school_grade, 'grade 11') ~ paste('11'),
        str_detect(.$school_grade, 'still') ~ paste('11'),
        str_detect(.$school_grade, '10') ~ paste('10'),
        str_detect(.$school_grade, '9') ~ paste('9'),
        str_detect(.$school_grade, '8') ~ paste('8'),
        str_detect(.$school_grade, '7') ~ paste('7'),
        str_detect(.$school_grade, '6')~ paste('6'),
        str_detect(.$school_grade, '5') ~ paste('5'),
        str_detect(.$school_grade, '4') ~ paste('4'),
        str_detect(.$school_grade, '3') ~ paste('3'),
        str_detect(.$school_grade, 'grade 2') ~ paste('2'),
        str_detect(.$school_grade, 'grade 1') ~ paste('1'),
        str_detect(.$school_grade, 'tertiary') ~ '',
        TRUE ~ .$school_grade))) %>%
    mutate(school_grade = ifelse(formal_schooling == 'no',
                                 yes = 0,
                                 no = school_grade)) %>%
    # Recode post_school_qualification
    mutate(post_school_qualification =
               ifelse(str_detect(post_school_qualification, 'n.ne') |
                          str_detect(post_school_qualification, 'no'),
                      yes = 'no',
                      no = ifelse(is.na(post_school_qualification),
                                  yes = NA,
                                  no = 'yes'))) %>%
    mutate(school_grade = as.character(school_grade),
           school_grade = ifelse(school_grade == 12 &
                                     post_school_qualification == 'yes',
                                 yes = '+13',
                                 no = school_grade)) %>%
    mutate(school_grade = factor(school_grade,
                                 levels = c(as.character(0:12), '+13'),
                                 ordered = TRUE)) %>%
    # Make schooling categories
    mutate(educational_level = case_when(
        .$school_grade == '+13' ~ 'post-school qualification',
        str_detect(.$school_grade, '^1.') |
            str_detect(.$school_grade, '^9') |
            str_detect(.$school_grade, '^8') ~ 'secondary school',
        str_detect(.$school_grade, '^7') |
            str_detect(.$school_grade, '^6') |
            str_detect(.$school_grade, '^5') |
            str_detect(.$school_grade, '^4') |
            str_detect(.$school_grade, '^3') |
            str_detect(.$school_grade, '^2') |
            str_detect(.$school_grade, '^1') |
            str_detect(.$school_grade, '^0') ~ 'no/primary school',
        ),
        educational_level = factor(educational_level,
                                   levels = c('no/primary school',
                                              'secondary school',
                                              'post-school qualification'),
                                   ordered = TRUE)) %>%
    # Reorder columns
    select(PID, date, age, sex, population, language, countryOB,
           school_grade, educational_level, employment,
           grant, grant_type)

levels(factor(general_info$employment))

############################################################
#                                                          #
#                       HIV history                        #
#                                                          #
############################################################
# Import data
hiv_test <- read_csv('data-original/hiv_test_results.csv')

# Quick look
dim(hiv_test)
head(hiv_test)
tail(hiv_test)
glimpse(hiv_test)
summary(hiv_test)

# Clean data
hiv_test %<>%
    # Rename columns
    rename(test_1_date = test_dt1,
           test_2_date = Test_dt2,
           test_result = HIV_result) %>%
    # Convert to date
    mutate_at(vars(date, test_1_date, test_2_date), dmy) %>%
    # Remove visit number
    select(-v_number) %>%
    # Remove empty columns
    select(PID, date, test_1_date, test_2_date, test_result, CD4_count) %>%
    # Recode test_result
    mutate(test_result = case_when(
        .$test_result == 1 & !is.na(.$test_result) ~ 'HIV positive',
        .$test_result == 2 & !is.na(.$test_result) ~ 'HIV negative'
    ))

############################################################
#                                                          #
#                   Medical information                    #
#                                                          #
############################################################
# Import data
medical_info <- read_csv('data-original/medical_info.csv')

# Quick look
dim(medical_info)
head(medical_info)
tail(medical_info)
glimpse(medical_info)
summary(medical_info)

# Clean data
medical_info %<>%
    # Convert to date
    mutate(date = dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Convert all character columns to lower case
    mutate_if(is.character, str_to_lower) %>%
    # Rename mass column
    rename(mass_kg = mass) %>%
    # Fix mass column formatting
    mutate(mass_kg = str_replace_all(mass_kg,
                                     pattern = 'no \\s?scale',
                                     replacement = 'NA'),
           mass_kg = str_replace_all(mass_kg,
                                     pattern = ',',
                                     replacement = '.'),
           mass_kg = ifelse(mass_kg == 'NA',
                         yes = NA,
                         no = paste(mass_kg)),
           mass_kg = as.numeric(mass_kg)) %>%
    # Rename height column
    rename(height_cm = height) %>%
    # Fix height column formatting
    mutate(height_cm = str_replace_all(height_cm,
                                       pattern = 'no \\s?scale',
                                       replacement = 'NA'),
           height_cm = str_replace_all(height_cm,
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
           alcohol_freq = factor(fct_relevel(alcohol_freq,
                                             'daily',
                                             'weekly',
                                             'monthly'))) %>%
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
           alcohol_per_sitting = factor(fct_relevel(alcohol_per_sitting,
                                                    '1-2',
                                                    '3-4',
                                                    '>4'))) %>%
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

############################################################
#                                                          #
#                          EQ-5D 3L                        #
#                                                          #
############################################################
# Import data
eq5d <- read_csv('data-original/eq5d.csv')

# Quick look
dim(eq5d)
head(eq5d)
tail(eq5d)
glimpse(eq5d)
summary(eq5d)

# Clean data
eq5d %<>%
    # Convert to date
    mutate(date = dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Convert all character columns to lower case
    mutate_if(is.character, str_to_lower) %>%
    # Recode mobility
    mutate(mobility = ifelse(mobility == 1,
                             yes = 'no problems',
                             no = ifelse(mobility == 2,
                                         yes = 'some problems',
                                         no = ifelse(is.na(mobility),
                                                     yes = NA,
                                                     no = 'confined to bed'))),
           # Convert to ordered factor
           mobility = factor(fct_relevel(mobility,
                                         'no problems',
                                         'some problems',
                                         'confined to bed'))) %>%
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
           self_care = factor(self_care,
                              levels = c()),
           self_care = fct_relevel(self_care,
                                   'no problems',
                                   'some problems',
                                   'unable to wash or dress')) %>%
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
           usual_activities = factor(fct_relevel(usual_activities,
                                                 'no problems',
                                                 'some problems',
                                                 'unable to perform usual activities'))) %>%
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
           pain_discomfort = factor(fct_relevel(pain_discomfort,
                                                'no pain or discomfort',
                                                'moderate pain or discomfort',
                                                'extreme pain or discomfort'))) %>%
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
           anxiety = factor(fct_relevel(anxiety,
                                        'no anxiety or depression',
                                        'moderate anxiety or depression',
                                        'extreme anxiety or depression'))) %>%
    # Rename health_code
    rename(qol_vas = health_code)

############################################################
#                                                          #
#             Pain Catastrophizing Scale (PCS)             #
#                                                          #
############################################################
# Import data
pcs <- read_csv('data-original/pcs.csv')

# Quick look
dim(pcs)
head(pcs)
tail(pcs)
glimpse(pcs)
summary(pcs)

# Clean data
pcs %<>%
    # Convert to date
    mutate(date = dmy(date)) %>%
    # Remove visit number
    select(-v_number) %>%
    # Create total and subscales scores
    mutate(rumination_score = rowSums(.[c(10, 11, 12, 13)]),
           magnification_score = rowSums(.[c(8, 9, 15)]),
           helplessness_score = rowSums(.[c(3, 4, 5, 6, 7, 14)]),
           total_score = rowSums(.[3:15]),
           total_score_30 = ifelse(total_score >= 30,
                                   yes = 'yes',
                                   no = 'no'))

############################################################
#                                                          #
#           Hopkins Symptom Checklist 25 (HSCL)            #
#                                                          #
############################################################
# Import data
hscl <- read_csv('data-original/hscl.csv')

# Quick look
dim(hscl)
head(hscl)
tail(hscl)
glimpse(hscl)
summary(hscl)

# Clean data
hscl %<>%
    # Convert to date
    mutate(date = dmy(date)) %>%
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
                                     no = 'no'))

############################################################
#                                                          #
#                     Neuro exam signs                     #
#                                                          #
############################################################
# Import data
signs <- read_csv('data-original/signs_neuro.csv')

# Quick look
dim(signs)
head(signs)
tail(signs)
glimpse(signs)
summary(signs)

# Clean data
signs %<>%
    # Convert to date
    mutate(date = dmy(date)) %>%
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
symptoms <- read_csv('data-original/symptoms_neuro.csv')

# Quick look
head(symptoms)
tail(symptoms)
glimpse(symptoms)
summary(symptoms)

# Clean data
symptoms %<>%
    # Convert to date
    mutate(date = dmy(date)) %>%
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
                          yes = 'yes',
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
              funs(factor(fct_relevel(.,
                                      'mild',
                                      'moderate',
                                      'severe')))) %>%
    # Rename symptoms
    rename(painful = p_aching,
           numbness = numb_lack,
           cold_freezing = cold_freez,
           pins_needles = p_needles) %>%
    # Recode symptom occurrence
    mutate_at(vars(c(5, 7, 9, 11, 13, 15, 17)),
              list(~ ifelse(. == 0,
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
wbpq <- read_csv('data-original/wbpq.csv')

# Quick look
head(wbpq)
tail(wbpq)
glimpse(wbpq)
summary(wbpq)

# Clean data
wbpq %<>%
    # Convert to date
    mutate(date = dmy(date)) %>%
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
    # Convert all factors to character class
    mutate_if(is.factor, as.character) %>%
    # Recode pain_in_last_month so that if current_pain = 'yes', then it is 'yes'
    mutate(pain_in_last_month = ifelse(current_pain == 'yes',
                                       yes = 'yes',
                                       no = pain_in_last_month)) %>%
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
               str_detect(.$pain_cause_head, '^m.....[se]$') |
                   str_detect(.$pain_cause_head, 'unknown') ~ 'unspecified cause',
               str_detect(.$pain_cause_head, 'stress') |
                   str_detect(.$pain_cause_head, 'thinking')  ~ 'stress-related',
               TRUE ~ 'other'),
           pain_cause_head = ifelse(head == 'no' | is.na(head),
                                    yes = NA,
                                    no = ifelse(is.na(pain_cause_head),
                                                     yes = 'unspecified cause',
                                                     no = pain_cause_head))) %>%
    ## Shoulder pain
    mutate(pain_cause_shoulders = case_when(
        str_detect(.$pain_cause_shoulders, 'exercis') |
            str_detect(.$pain_cause_shoulders, 'sport') |
            str_detect(.$pain_cause_shoulders, 'heavy') |
            str_detect(.$pain_cause_shoulders, 'muscle') |
            str_detect(.$pain_cause_shoulders, 'work') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_shoulders, 'stress') |
            str_detect(.$pain_cause_shoulders, 'tense') |
            str_detect(.$pain_cause_shoulders, 'thinking')  ~ 'stress-related',
        str_detect(.$pain_cause_shoulders, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_shoulders = ifelse(shoulders == 'no' | is.na(shoulders),
                                      yes = NA,
                                      no = ifelse(is.na(pain_cause_shoulders),
                                                  yes = 'unspecified cause',
                                                  no = pain_cause_shoulders))) %>%
    ## Arm pain
    mutate(pain_cause_arms = case_when(
        str_detect(.$pain_cause_arms, 'blood') |
            str_detect(.$pain_cause_arms, 'injection') ~ 'procedural pain',
        str_detect(.$pain_cause_arms, 'exercis') |
            str_detect(.$pain_cause_arms, 'jogging') |
            str_detect(.$pain_cause_arms, 'scratch') |
            str_detect(.$pain_cause_arms, 'accident') |
            str_detect(.$pain_cause_arms, 'garden') |
            str_detect(.$pain_cause_arms, 'wsshing') |
            str_detect(.$pain_cause_arms, 'alcohol') |
            str_detect(.$pain_cause_arms, 'fell') |
            str_detect(.$pain_cause_arms, 'broke') |
            str_detect(.$pain_cause_arms, 'sport') |
            str_detect(.$pain_cause_arms, 'heavy') |
            str_detect(.$pain_cause_arms, 'muscle') |
            str_detect(.$pain_cause_arms, 'work') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_arms, 'arth') ~ 'arthritis',
        str_detect(.$pain_cause_arms, 'stress') ~ 'stress-related',
        str_detect(.$pain_cause_arms, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_arms = ifelse(arms == 'no' | is.na(arms),
                                      yes = NA,
                                      no = ifelse(is.na(pain_cause_arms),
                                                  yes = 'unspecified cause',
                                                  no = pain_cause_arms))) %>%
    ## Hand pain
    mutate(pain_cause_hands = case_when(
        str_detect(.$pain_cause_hands, 'burn') |
            str_detect(.$pain_cause_hands, 'cut') |
            str_detect(.$pain_cause_hands, 'washing') |
            str_detect(.$pain_cause_hands, 'broke') |
            str_detect(.$pain_cause_hands, 'washing') |
            str_detect(.$pain_cause_hands, 'accident') |
            str_detect(.$pain_cause_hands, 'sport') |
            str_detect(.$pain_cause_hands, 'fight') |
            str_detect(.$pain_cause_hands, 'washing') |
            str_detect(.$pain_cause_hands, 'heavy') |
            str_detect(.$pain_cause_hands, 'fell') |
            str_detect(.$pain_cause_hands, 'work') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_hands, 'arth') ~ 'arthritis',
        str_detect(.$pain_cause_hands, 'unknown') ~ 'unspecified cause',
        str_detect(.$pain_cause_hands, 'hiv') ~ 'HIV infection',
        TRUE ~ 'other'),
        pain_cause_hands = ifelse(hands == 'no' | is.na(hands),
                                 yes = NA,
                                 no = ifelse(is.na(pain_cause_hands),
                                             yes = 'unspecified cause',
                                             no = pain_cause_hands))) %>%
    ## Chest pain
    mutate(pain_cause_chest = case_when(
        str_detect(.$pain_cause_chest, 'exercise') |
            str_detect(.$pain_cause_chest, 'running') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_chest, 'blocked') |
            str_detect(.$pain_cause_chest, 'failer') ~ 'MI or heart failure',
        str_detect(.$pain_cause_chest, 'smoking') ~ 'smoking',
        str_detect(.$pain_cause_chest, 'cough') |
            str_detect(.$pain_cause_chest, 'caughing') |
            str_detect(.$pain_cause_chest, 'pneumonia') |
            str_detect(.$pain_cause_chest, 'flu') ~ 'cough or infection',
        str_detect(.$pain_cause_chest, 'unknown') |
            str_detect(.$pain_cause_chest, 'sharp') ~ 'unspecified cause',
        str_detect(.$pain_cause_chest, 'acid') |
            str_detect(.$pain_cause_chest, 'heartburn') |
            str_detect(.$pain_cause_chest, 'heart.burn') |
            str_detect(.$pain_cause_chest, 'ulcer') ~ 'ulcer\\heart-burn',
        TRUE ~ 'other'),
        pain_cause_chest = ifelse(chest == 'no' | is.na(chest),
                                  yes = NA,
                                  no = ifelse(is.na(pain_cause_chest),
                                              yes = 'unspecified cause',
                                              no = pain_cause_chest))) %>%
    ## Abdominal pain
    mutate(pain_cause_abdomen = case_when(
        str_detect(.$pain_cause_abdomen, 'sex') |
            str_detect(.$pain_cause_abdomen, 'condom') ~ 'sex-related',
        str_detect(.$pain_cause_abdomen, 'ulcer') ~ 'ulcer\\heart-burn',
        str_detect(.$pain_cause_abdomen, 'planning') |
            str_detect(.$pain_cause_abdomen, 'smear') |
            str_detect(.$pain_cause_abdomen, 'apendix') |
            str_detect(.$pain_cause_abdomen, 'sterilization') |
            str_detect(.$pain_cause_abdomen, 'oparation') |
            str_detect(.$pain_cause_abdomen, 'Ceaserian') ~ 'procedural pain',
        str_detect(.$pain_cause_abdomen, 'cramp') |
            str_detect(.$pain_cause_abdomen, 'crump') ~ 'cramps',
        str_detect(.$pain_cause_abdomen, 'constip') |
            str_detect(.$pain_cause_abdomen, 'costip') ~ 'constipation',
        str_detect(.$pain_cause_abdomen, 'period') |
            str_detect(.$pain_cause_abdomen, 'menstr') |
            str_detect(.$pain_cause_abdomen, 'mentr') ~ 'menstrual pain',
        str_detect(.$pain_cause_abdomen, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_abdomen = ifelse(abdomen == 'no' | is.na(abdomen),
                                  yes = NA,
                                  no = ifelse(is.na(pain_cause_abdomen),
                                              yes = 'unspecified cause',
                                              no = pain_cause_abdomen))) %>%
    ## Low-back pain
    mutate(pain_cause_low_back = case_when(
        str_detect(.$pain_cause_low_back, 'ulcer') ~ 'ulcer or heart-burn',
        str_detect(.$pain_cause_low_back, 'bending') |
            str_detect(.$pain_cause_low_back, 'exercis') |
            str_detect(.$pain_cause_low_back, 'running') |
            str_detect(.$pain_cause_low_back, 'standing') |
            str_detect(.$pain_cause_low_back, 'walk') |
            str_detect(.$pain_cause_low_back, 'lifting') |
            str_detect(.$pain_cause_low_back, 'accident') |
            str_detect(.$pain_cause_low_back, 'injur') |
            str_detect(.$pain_cause_low_back, 'work') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_low_back, 'period') |
            str_detect(.$pain_cause_low_back, 'menstr') |
            str_detect(.$pain_cause_low_back, 'mentr') ~ 'menstrual pain',
        str_detect(.$pain_cause_low_back, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_low_back = ifelse(low_back == 'no' | is.na(low_back),
                                    yes = NA,
                                    no = ifelse(is.na(pain_cause_low_back),
                                                yes = 'unspecified cause',
                                                no = pain_cause_low_back))) %>%
    ## Genital pain
    mutate(pain_cause_genitals = case_when(
        str_detect(.$pain_cause_genitals, 'piles') ~ 'piles',
        str_detect(.$pain_cause_genitals, 'sex') |
            str_detect(.$pain_cause_genitals, 'rape') ~ 'sex-related',
        str_detect(.$pain_cause_genitals, 'sores') |
            str_detect(.$pain_cause_genitals, 'rash')|
            str_detect(.$pain_cause_genitals, 'rush') |
            str_detect(.$pain_cause_genitals, 'piple') ~ 'infection',
        str_detect(.$pain_cause_genitals, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_genitals = ifelse(genitals == 'no' | is.na(genitals),
                                     yes = NA,
                                     no = ifelse(is.na(pain_cause_genitals),
                                                 yes = 'unspecified cause',
                                                 no = pain_cause_genitals))) %>%
    ## Leg pain
    mutate(pain_cause_legs = case_when(
        str_detect(.$pain_cause_legs, 'exercis') |
            str_detect(.$pain_cause_legs, 'running') |
            str_detect(.$pain_cause_legs, 'standing') |
            str_detect(.$pain_cause_legs, 'playing') |
            str_detect(.$pain_cause_legs, 'possition') |
            str_detect(.$pain_cause_legs, 'muscle') |
            str_detect(.$pain_cause_legs, 'fell') |
            str_detect(.$pain_cause_legs, 'walk') |
            str_detect(.$pain_cause_legs, 'lifting') |
            str_detect(.$pain_cause_legs, 'accident') |
            str_detect(.$pain_cause_legs, 'inju') |
            str_detect(.$pain_cause_legs, 'work') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_legs, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_legs = ifelse(legs == 'no' | is.na(legs),
                                      yes = NA,
                                      no = ifelse(is.na(pain_cause_legs),
                                                  yes = 'unspecified cause',
                                                  no = pain_cause_legs))) %>%
    ## Foot pain
    mutate(pain_cause_feet = case_when(
        str_detect(.$pain_cause_feet, 'exercis') |
            str_detect(.$pain_cause_feet, 'standing') |
            str_detect(.$pain_cause_feet, 'soccer') |
            str_detect(.$pain_cause_feet, 'twisted') |
            str_detect(.$pain_cause_feet, 'fell') |
            str_detect(.$pain_cause_feet, 'walk') |
            str_detect(.$pain_cause_feet, 'accident') |
            str_detect(.$pain_cause_feet, 'inju') |
            str_detect(.$pain_cause_feet, 'work') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_feet, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_feet = ifelse(feet == 'no' | is.na(feet),
                                 yes = NA,
                                 no = ifelse(is.na(pain_cause_feet),
                                             yes = 'unspecified cause',
                                             no = pain_cause_feet))) %>%
    ## Joint pain
    mutate(pain_cause_joints = case_when(
        str_detect(.$pain_cause_joints, 'not exercising') ~ 'other2',
        str_detect(.$pain_cause_joints, 'exercis') |
            str_detect(.$pain_cause_joints, 'standing') |
            str_detect(.$pain_cause_joints, 'soccer') |
            str_detect(.$pain_cause_joints, 'sport') |
            str_detect(.$pain_cause_joints, 'twisted') |
            str_detect(.$pain_cause_joints, 'fell') |
            str_detect(.$pain_cause_joints, 'walk') |
            str_detect(.$pain_cause_joints, 'accident') |
            str_detect(.$pain_cause_joints, 'inju') |
            str_detect(.$pain_cause_joints, 'work') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_joints, 'arthritis') |
            str_detect(.$pain_cause_joints, 'atratise') ~ 'arthritis',
        str_detect(.$pain_cause_joints, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_joints = ifelse(joints == 'no' | is.na(joints),
                                 yes = NA,
                                 no = ifelse(is.na(pain_cause_joints),
                                             yes = 'unspecified cause',
                                             no = pain_cause_joints))) %>%
    ## Muscle pain
    mutate(pain_cause_muscles = case_when(
        str_detect(.$pain_cause_muscles, 'exercis') |
            str_detect(.$pain_cause_muscles, 'standing') |
            str_detect(.$pain_cause_muscles, 'soccer') |
            str_detect(.$pain_cause_muscles, 'gym') |
            str_detect(.$pain_cause_muscles, 'sport') |
            str_detect(.$pain_cause_muscles, 'walk') |
            str_detect(.$pain_cause_muscles, 'accident') |
            str_detect(.$pain_cause_muscles, 'injur') |
            str_detect(.$pain_cause_muscles, 'work') ~ 'physical injury\\strain',
        str_detect(.$pain_cause_muscles, 'unknown') ~ 'unspecified cause',
        TRUE ~ 'other'),
        pain_cause_muscles = ifelse(muscles == 'no' | is.na(muscles),
                                   yes = NA,
                                   no = ifelse(is.na(pain_cause_muscles),
                                               yes = 'unspecified cause',
                                               no = pain_cause_muscles))) %>%
    # Pain at its worst
    mutate(pain_worst = ifelse(pain_in_last_month == 'no',
                               yes = 0,
                               no = pain_worst)) %>%
    # Pain at its least
    mutate(pain_least = ifelse(pain_in_last_month == 'no',
                               yes = 0,
                               no = pain_least)) %>%
    # Pain now
    mutate(pain_now = ifelse(current_pain == 'no',
                             yes = 0,
                             no = pain_now)) %>%
    # Site of worst pain
    ## Replace 'private parts' with 'genitals'
    mutate(site_of_worst_pain =
               str_replace_all(site_of_worst_pain,
                                        pattern = 'private parts',
                                        replacement = 'genitals')) %>%
    ## Split columns (one site per column)
    tidyr::separate(col = site_of_worst_pain,
                    sep = '/',
                    into = c('site_of_worst_pain_1',
                             'site_of_worst_pain_2',
                             'site_of_worst_pain_3',
                             'site_of_worst_pain_4',
                             'site_of_worst_pain_5')) %>%
    # Time of worst pain
    mutate(pain_times =
               str_replace_all(pain_times,
                                        pattern = 'norning',
                                        replacement = 'morning')) %>%
    # Recode pain treatment
    mutate(pain_treatment = case_when(
        .$pain_treatment == 1 ~ 'yes',
        .$pain_treatment == 0 ~ 'no'
        )) %>%
    # Drug prescriptions
    ## Rename columns
    rename(medication_1 = prescribed1,
           prescribed_1 = prescribed_Dr1,
           medication_2 = prescribed2,
           prescribed_2 = prescribed_Dr2,
           medication_3 = prescribed3,
           prescribed_3 = prescribed_Dr3,
           medication_4 = prescribed4,
           prescribed_4 = prescribed_Dr4) %>%
    ## Recode drugs in medication_* columns
    mutate_at(.vars = vars(starts_with('medication_')),
              .funs = funs(case_when(
                  str_detect(., 'grand') |
                      str_detect(., 'compral') ~ 'paracetamol + aspirin',
                  str_detect(., 'adcod') |
                      str_detect(., 'acod') |
                      str_detect(., 'alcod') |
                      str_detect(., 'alcad') |
                      str_detect(., 'cend') |
                      str_detect(., 'cind') |
                      str_detect(., 'lena') |
                      str_detect(., 'sillp') |
                      str_detect(., 'silp') |
                      str_detect(., 'sinut') |
                      str_detect(., 'betap') ~ 'paracetamol + codeine',
                  str_detect(., 'anad') |
                      str_detect(., 'aspr') |
                      str_detect(., 'dispr') ~ 'aspirin',
                  str_detect(., 'bruf') |
                      str_detect(., 'IB-pro') |
                      str_detect(., 'nurof') ~ 'ibuprofen',
                  str_detect(., 'diclo') |
                      str_detect(., 'declo') |
                      str_detect(., 'volta') ~ 'diclofenac',
                  str_detect(., 'microd') |
                      str_detect(., 'miprod') |
                      str_detect(., 'mybu')
                  ~ 'paracetamol + ibuprofen + codeine',
                  str_detect(., 'pain blo') |
                      str_detect(., 'painblo') |
                      str_detect(., 'pianblo') |
                      str_detect(., 'paunblo') |
                      str_detect(., 'painam') |
                      str_detect(., 'paracet') |
                      str_detect(., 'paracent') |
                      str_detect(., 'prarcent') |
                      str_detect(., 'go pain') |
                      str_detect(., 'panado') ~ 'paracetamol',
                  str_detect(., 'tramad') ~ 'tramadol',
                  str_detect(., 'napro') ~ 'naproxen',
                  str_detect(., 'trilp') ~ 'amitriptyline',
                  str_detect(., 'killer') ~ 'not specified'
                  ))) %>%
    ## Recode prescribed_* columns to 'yes' or 'no'
    mutate_at(.vars = vars(starts_with('prescribed_')),
              .funs = funs(ifelse(. == 1,
                                  yes = 'yes',
                                  no = 'no'))) %>%
    ## Clean-up prescribed_* columns to agree with medication_* columns
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
    ## Clean-up medication_* to remove duplicates
    ### medication_2 / prescribed_2
    mutate(medication_2 = ifelse(prescribed_2 == prescribed_1 &
                                     medication_2 == medication_1,
                                 yes = NA,
                                 no = medication_2),
           prescribed_2 = ifelse(prescribed_2 == prescribed_1 &
                                     medication_2 == medication_1,
                                 yes = NA,
                                 no = prescribed_2)) %>%
    ### medication_3 / prescribed_3
    mutate(medication_3 = ifelse(prescribed_3 == prescribed_1 &
                                     medication_3 == medication_1,
                                 yes = NA,
                                 no = medication_3),
           prescribed_3 = ifelse(prescribed_3 == prescribed_1 &
                                     medication_3 == medication_1,
                                 yes = NA,
                                 no = prescribed_3)) %>%
    ### medication_4 / prescribed_4
    mutate(medication_4 = ifelse(prescribed_4 == prescribed_1 &
                                     medication_4 == medication_1,
                                 yes = NA,
                                 no = medication_4),
           prescribed_4 = ifelse(prescribed_4 == prescribed_1 &
                                     medication_4 == medication_1,
                                 yes = NA,
                                 no = prescribed_4)) %>%
    ## Clean-up pain_treatment column
    mutate(pain_treatment = ifelse(!is.na(medication_1),
                                   yes = 'yes',
                                   no = 'no')) %>%
    # Traditional medicines
    ## Rename traditional medicine columns
    rename(use_traditional_meds = traditional_meds,
           traditional_meds = traditional_meds1) %>%
    ## Remove unused columns
    ## traditional_meds2 didn't offer anything not achieved
    ## through traditional_meds
    select(-traditional_meds2, -traditional_meds3, -traditional_meds4) %>%
    ## Recode use/non-use in use_traditional_meds
    mutate(use_traditional_meds = case_when(
        .$use_traditional_meds == 1 ~ 'yes',
        .$use_traditional_meds == 0 ~ 'no'
    )) %>%
    ## Recode substance names
    mutate(traditional_meds = case_when(
        str_detect(.$traditional_meds, 'herb') |
            str_detect(.$traditional_meds, 'phila') |
            str_detect(.$traditional_meds, 'mbiza') |
            str_detect(.$traditional_meds, 'alovera') |
            str_detect(.$traditional_meds, 'aloevera') |
            str_detect(.$traditional_meds, 'izifozonke') |
            str_detect(.$traditional_meds, 'moringa') |
            str_detect(.$traditional_meds, 'mohlonyane') |
            str_detect(.$traditional_meds, 'mhlabelo') |
            str_detect(.$traditional_meds, 'supreme')  ~ 'herbal tea/drink',
        str_detect(.$traditional_meds, 'onion')  ~ 'traditional remedy',
        str_detect(.$traditional_meds, 'jinsmang') |
            str_detect(.$traditional_meds, 'drank liquid substance') |
            str_detect(.$traditional_meds, 'dieketsa') |
            str_detect(.$traditional_meds, 'forever living') ~ 'unknown'
    )) %>%
    ## Match use_tradiational_meds with traditional_meds
    mutate(use_traditional_meds = ifelse(!is.na(traditional_meds),
                                         yes = 'yes',
                                         no = 'no')) %>%
    # Other substances for pain
    ## Rename columns
    rename(use_other_substances = substances,
           substance_1 = substances1,
           substance_2 = substances2) %>%
    ## Remove unused columns
    select(-substances3, -substances4) %>%
    ### Recode use/non-use of other substances
    mutate(use_other_substances = case_when(
        .$use_other_substances == 1 ~ 'yes',
        .$use_other_substances == 0 ~ 'no'
    )) %>%
    ## Recode substances
    mutate(substance_1 = case_when(
        str_detect(.$substance_1, 'milk') ~ 'drinks milk',
        str_detect(.$substance_1, 'dagga') |
            str_detect(.$substance_1, 'marijuana') ~ 'marijuana',
        str_detect(.$substance_1, 'alcohol') |
            str_detect(.$substance_1, 'beer') ~ 'drinks alcohol',
        str_detect(.$substance_1, 'water') ~ 'drinks water',
        str_detect(.$substance_1, 'rub') |
            str_detect(.$substance_1, 'rud') ~ 'menthol rub',
        str_detect(.$substance_1, 'snuf') |
            str_detect(.$substance_1, 'ciga') ~ 'tobacco product'),
        substance_2 = case_when(
            str_detect(.$substance_2, 'dagga') ~ 'marijuana')) %>%
    ## Match use_other_subatances with substance_1
    mutate(use_other_substances = ifelse(!is.na(substance_1),
                                         yes = 'yes',
                                         no = 'no')) %>%
    # Pain relief
    ## Add column indicating whether any of prescription/traditional/other used
    mutate(used_pain_relievers = case_when(
        str_detect(pain_treatment, 'yes') |
            str_detect(use_traditional_meds, 'yes') |
            str_detect(use_other_substances, 'yes') ~ 'yes',
        TRUE ~ 'no'
    )) %>%
    ## Add a column indicating which pain relievers were used
    mutate(pain_relievers_used = case_when(
        str_detect(pain_treatment, 'yes') &
            str_detect(use_traditional_meds, 'yes') &
            str_detect(use_other_substances, 'yes') ~
            'pharmacotherapy + traditional meds + other substances',
        str_detect(pain_treatment, 'yes') &
            str_detect(use_traditional_meds, 'yes') &
            str_detect(use_other_substances, 'no') ~
            'pharmacotherapy + traditional meds',
        str_detect(pain_treatment, 'no') &
            str_detect(use_traditional_meds, 'no') &
            str_detect(use_other_substances, 'yes') ~
            'other substances',
        str_detect(pain_treatment, 'yes') &
            str_detect(use_traditional_meds, 'no') &
            str_detect(use_other_substances, 'no') ~
            'pharmacotherapy',
        str_detect(pain_treatment, 'yes') &
            str_detect(use_traditional_meds, 'no') &
            str_detect(use_other_substances, 'yes') ~
            'pharmacotherapy + other substances',
        str_detect(pain_treatment, 'no') &
            str_detect(use_traditional_meds, 'yes') &
            str_detect(use_other_substances, 'no') ~
            'traditional meds',
        str_detect(pain_treatment, 'no') &
            str_detect(use_traditional_meds, 'yes') &
            str_detect(use_other_substances, 'yes') ~
            'traditional meds + other substances',
        str_detect(pain_treatment, 'no') &
            str_detect(use_traditional_meds, 'no') &
            str_detect(use_other_substances, 'yes') ~
            'other substances'
    )) %>%
    ## Rename pain_relief column
    rename(overall_pain_relief = pain_relief) %>%
    ## Process overall_pain_relief text
    mutate(overall_pain_relief = str_replace(overall_pain_relief,
                                                      pattern = '%',
                                                      replacement = ''),
           overall_pain_relief = as.numeric(overall_pain_relief),
           overall_pain_relief = ifelse(used_pain_relievers == 'yes',
                                        yes = overall_pain_relief,
                                        no = NA)) %>%
    ## Rename best_Prelief column
    rename(best_pain_relief = best_Prelief) %>%
    ## Process best_pain_relief text
    mutate(best_pain_relief = ifelse(best_pain_relief == 1,
                                     yes = 'pharmacotherapy',
                                     no = ifelse(
                                         best_pain_relief == 2,
                                         yes = 'traditional medicine',
                                         no = ifelse(
                                                     best_pain_relief == 3,
                                                     yes = 'other substances',
                                                     no = NA))),
           best_pain_relief = ifelse(overall_pain_relief == 0,
                                     yes = 'Nothing',
                                     no = best_pain_relief)) %>%
    # Other modailities used to relieve pain
    ## Rename 'pain_relieves' columns
    select(-pain_relievesn4) %>%
    rename(use_other_modalities = pain_relieves,
           other_modality_1 = pain_relievesn1,
           other_modality_2 = pain_relievesn2,
           other_modality_3 = pain_relievesn3) %>%
    ## Recode use_other_modalities and associated columns
    mutate(use_other_modalities = ifelse(use_other_modalities == 1,
                                         yes = 'yes',
                                         no = 'no')) %>%
    mutate_at(vars(starts_with('other_modality')),
              funs(case_when(
                  str_detect(., 'sleep') ~
                      'sleep',
                  str_detect(., 'exer') |
                      str_detect(., 'exei') |
                      str_detect(., 'walk') |
                      str_detect(., 'xerc') |
                      str_detect(., 'gym') |
                      str_detect(., 'jog') ~
                      'exercise',
                  str_detect(., 'mass') |
                      str_detect(., 'rub') |
                      str_detect(., 'physio') ~
                      'massage',
                  str_detect(., 'relax') |
                      str_detect(., 'rest') |
                      str_detect(., 'rsest') |
                      str_detect(., 'read') |
                      str_detect(., 'bath') ~
                      'rest / relax',
                  str_detect(., 'not think') |
                      str_detect(., 'music') |
                      str_detect(., 'cleaning') |
                      str_detect(., 'movies') |
                      str_detect(., 'play') ~
                      'distraction',
                  str_detect(., 'stretch') |
                      str_detect(., 'strech') ~
                      'stretching',
                  str_detect(., 'smok') ~
                      'smoking',
                  str_detect(., 'position') |
                      str_detect(., 'water') |
                      str_detect(., 'green tea') |
                      str_detect(., 'hicking') |
                      str_detect(., 'water') |
                      str_detect(., 'water') |
                      str_detect(., 'soak') |
                      str_detect(., 'icebag') |
                      str_detect(., 'companion') |
                      str_detect(., 'lay flat') |
                      str_detect(., 'elevat') ~
                      'other'
              ))) %>%
    ## Fix minor issues
    ### Empty other_modality_1 column, filled other_modality_2 column
    mutate(other_modality_1 = ifelse(is.na(other_modality_1) &
                                         !is.na(other_modality_2),
                                     yes = other_modality_2,
                                     no = other_modality_1)) %>%
    ### Duplicates in other_modality_X columns
    mutate(other_modality_2 = ifelse(other_modality_2 == other_modality_1,
                                     yes = NA,
                                     no = other_modality_2),
           other_modality_3 = ifelse(other_modality_3 == other_modality_1 |
                                         other_modality_3 == other_modality_2,
                                     yes = NA,
                                     no = other_modality_3)) %>%
    # Causes of pain
    ## Remove pain_causeoX because it largely repeats the data from earlier
    select(-starts_with('pain_causeo')) %>%
    # Pain interference
    ## Rename columns
    rename(relationship_with_others = relationship,
           walking_ability = w_ability,
           normal_work = n_work,
           enjoyment_of_life = enjoyment) %>%
    ## Calculate overall interference index
    mutate(interference_index = round(rowMeans(.[59:64],
                                         na.rm = TRUE)),
           interference_index = ifelse(is.nan(interference_index),
                                       yes = NA,
                                       no = interference_index))

############################################################
#                                                          #
#                    Output data to RDS                    #
#                                                          #
############################################################
# Make directory
dir.create(path = 'data-cleaned')

# Get a list of data.frame objects
df_names <- ls()

# Place data.frames into a list using 'df_names'
df_list <- map(.x = df_names,
               ~ get(.x))

# Loop over df_list and write to RDS
walk2(.x = df_list,
      .y = df_names,
      .f = ~ write_rds(x = .x,
                       path = paste0('./data-cleaned/', .y, '.rds'),
                       compress = 'xz'))

# Loop over df_list and write to csv
walk2(.x = df_list,
      .y = df_names,
      .f = ~ readr::write_csv(x = .x,
                              path = paste0('./data-cleaned/', .y, '.csv')))

############################################################
#                                                          #
#                         Clean-up                         #
#                                                          #
############################################################
rm(list = ls())

