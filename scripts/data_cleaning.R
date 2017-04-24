############################################################
#                                                          #
#                      Load packages                       #
#                                                          #
############################################################
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
general_info %>%
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
               ifelse(employment %in% c('employed (part time/piece work) self employed',
                                        'employed (part time/piece work)- volunteer'),
                               yes = 'employed (part time)',
                               no = ifelse(employment %in% c('employed',
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
        stringr::str_detect(.$grant_type, 'c...d') ~ paste('child grant'),
        stringr::str_detect(.$grant_type, 'c..d') ~ paste('child grant'),
        stringr::str_detect(.$grant_type, 'kids') ~ paste('child grant'),
        stringr::str_detect(.$grant_type, 'pension') ~ paste('pension'),
        stringr::str_detect(.$grant_type, 'disability') ~ paste('disability grant'),
        stringr::str_detect(.$grant_type, 'medical') ~ paste('disability grant'))) %>%
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
                          stringr::str_detect(post_school_qualification, 'still'),
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
           grant, grant_type) %>%
    # Write to file
    readr::write_csv('./data/general_info.csv')

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
medical_info %>%
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
                                                 no = ifelse(is.na(alcohol_freq),
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
                                             no = ifelse(is.na(alcohol_per_sitting),
                                                         yes = NA,
                                                         no = '>4'))),
           # Convert to ordered factor
           alcohol_per_sitting = factor(forcats::fct_relevel(alcohol_per_sitting,
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
                                                     no = 'no')))) %>%
    # Write to file
    readr::write_csv('./data/medical_info.csv')

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
eq5d %>%
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
                                                  'some problems'),
                             ordered = TRUE)) %>%
    # Rename usual_activ
    rename(usual_activities = usual_activ) %>%
    # Recode usual_activities
    mutate(usual_activities =
               ifelse(usual_activities == 1,
                      yes = 'no problems',
                      no = ifelse(usual_activities == 2,
                                  yes = 'some problems',
                                  no = ifelse(is.na(usual_activities),
                                              yes = NA,
                                              no = 'unable to perform usual activities'))),
           # Convert to ordered factor
           usual_activities = factor(forcats::fct_relevel(usual_activities,
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
                      no = ifelse(pain_discomfort == 2,
                                  yes = 'moderate pain or discomfort',
                                  no = ifelse(is.na(pain_discomfort),
                                              yes = NA,
                                              no = 'extreme pain or discomfort'))),
           # Convert to ordered factor
           pain_discomfort = factor(forcats::fct_relevel(pain_discomfort,
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
                                  no = ifelse(is.na(anxiety),
                                              yes = NA,
                                              no = 'extreme anxiety or depression'))),
           # Convert to ordered factor
           anxiety = factor(forcats::fct_relevel(anxiety,
                                                 'no anxiety or depression',
                                                 'moderate anxiety or depression',
                                                 'extreme anxiety or depression'),
                                    ordered = TRUE)) %>%
    # Rename health_code
    rename(qol_vas = health_code) %>%
    readr::write_csv('./data/eq5d.csv')
