Working_directory<- setwd("C:/Users/user/Google Drive/School census/NEW CSPRO/Quality/School_Census/")

# List of required packages
packages <- c("readstata13", "tidyverse", "readxl", "scales", "shiny", "bslib",
              "shinyWidgets", "echarts4r", "sass", "shinyalert", "shinydisconnect",
              "rsconnect", "devtools", "plotly", "bsicons", "leaflet", "sf",
              "ggplot2", "haven", "dplyr", "lubridate")

# Function to check if a package is installed, and if not, install it
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  } else {
    library(p, character.only = TRUE)
  }
}

# Apply the function to each package
invisible(lapply(packages, install_if_missing))


Data_all<- read.csv("Data.csv")

users<- read.csv("users.csv")

Counties<- read.csv("Counties.csv",header = FALSE)

colnames(Counties) <- c("HH7A", "County_name")

Data_users<- merge(Data_all,users, by="HH3", all.x=TRUE)

Data_Counties<- merge(Data_users,Counties, by="HH7A", all.x=TRUE)

Data_select <- Data_Counties %>% select(USER_NAME, everything())

Data_a<-Data_select %>% select(County_name,everything())

# Assuming your data frame is named df
Dekar <- Data_a %>%
  filter(HH7A >= 101 & HH7A <= 109)

Sagire <- Data_a %>%
  filter(HH7A >= 110 & HH7A <= 117)

Punyua <- Data_a %>%
  filter(HH7A >= 118 & HH7A <= 126)

Nyawade <- Data_a %>%
  filter(HH7A >= 127 & HH7A <= 136)

Serah <- Data_a %>%
  filter(HH7A >= 137 & HH7A <= 147)

dir.create("Dekar", showWarnings = FALSE)
dir.create("Sagire", showWarnings = FALSE)
dir.create("Punyua", showWarnings = FALSE)
dir.create("Nyawade", showWarnings = FALSE)
dir.create("Serah", showWarnings = FALSE)

dir.create("Errors", showWarnings = FALSE)

dir.create("HeadCount equal Register Errors", showWarnings = FALSE)

dir.create("HeadCount more Register Errors", showWarnings = FALSE)

dir.create("Register Absent greater 5 Errors", showWarnings = FALSE)

dir.create("Frequencies", showWarnings = FALSE)

dir.create("School code errors", showWarnings = FALSE)

write.csv(Dekar, file = "Dekar/Dekar.csv", row.names = FALSE)
write.csv(Dekar, file = "Sagire/Sagire.csv", row.names = FALSE)
write.csv(Dekar, file = "Punyua/Punyua.csv", row.names = FALSE)
write.csv(Dekar, file = "Nyawade/Nyawade.csv", row.names = FALSE)
write.csv(Dekar, file = "Serah/Serah.csv", row.names = FALSE)

######Read your respective file
#Data<- read.csv("Dekar/Dekar.csv")
#Data<- read.csv("Sagire/Sagire.csv")
#Data<- read.csv("Punyua/Punyua.csv")
#Data<- read.csv("Nyawade/Nyawade.csv")
#Data<- read.csv("Serah/Serah.csv")

Data <- Punyua %>%
  rename(
    County = HH7A,
    interviewer = HH3,
    "School unique no" = HH2,
    "Sub county" = PA02,
    "Division" = PA03,
    "Location" = PA04,
    "Sub location" = PA05,
    "Constituency" = PA06,
    "Ward" = PA07,
    "Residence" = PA08,
    "Education level" = PA09,
    "Pre primary School name"=PPB12,
    "Primary School name"=PPA10,
    "Junior school name"=JSA10,
    "Secondary school name"=SSA10,
    "Prevocational school name"=PVA10,
    "PP1 Headcount"=EE02_HEAD_COUNT_TOTAL_1,
    "PP2 Headcount"=EE02_HEAD_COUNT_TOTAL_2,
    "PP1 Present"=EE02_REGISTER_PRESENT_TOTAL_1,
    "PP2 Present"=EE02_REGISTER_PRESENT_TOTAL_2,
    "PP1 Absent"=EE02_REGISTER_ABSENT_TOTAL_1,
    "PP2 Absent"=EE02_REGISTER_ABSENT_TOTAL_2,
    
    "Grade 1 Headcount"=HEAD_COUNT_TOTAL_1,
    "Grade 2 Headcount"=HEAD_COUNT_TOTAL_2,
    "Grade 3 Headcount"=HEAD_COUNT_TOTAL_3,
    "Grade 4 Headcount"=HEAD_COUNT_TOTAL_4,
    "Grade 5 Headcount"=HEAD_COUNT_TOTAL_5,
    "Grade 6 Headcount"=HEAD_COUNT_TOTAL_6,
    
    "Grade 1 Present"=REGISTER_PRESENT_TOTAL_1,
    "Grade 2 Present"=REGISTER_PRESENT_TOTAL_2,
    "Grade 3 Present"=REGISTER_PRESENT_TOTAL_3,
    "Grade 4 Present"=REGISTER_PRESENT_TOTAL_4,
    "Grade 5 Present"=REGISTER_PRESENT_TOTAL_5,
    "Grade 6 Present"=REGISTER_PRESENT_TOTAL_6,
    
    "Grade 1 Absent"=REGISTER_ABSENT_TOTAL_1,
    "Grade 2 Absent"=REGISTER_ABSENT_TOTAL_2,
    "Grade 3 Absent"=REGISTER_ABSENT_TOTAL_3,
    "Grade 4 Absent"=REGISTER_ABSENT_TOTAL_4,
    "Grade 5 Absent"=REGISTER_ABSENT_TOTAL_5,
    "Grade 6 Absent"=REGISTER_ABSENT_TOTAL_6,
    
    "Grade 7 Headcount"=JS02_HEAD_COUNT_TOTAL_1,
    "Grade 8 Headcount"=JS02_HEAD_COUNT_TOTAL_2,
    
    "Grade 7 present"=JS02_REGISTER_PRESENT_TOTAL_1,
    "Grade 8 present"=JS02_REGISTER_PRESENT_TOTAL_2,
    
    "Grade 7 Absent"=JS02_REGISTER_ABSENT_TOTAL_1,
    "Grade 8 Absent"=JS02_REGISTER_ABSENT_TOTAL_2,
    
    "Form 1 Headcount"=SE02_HEAD_COUNT_TOTAL_1,
    "Form 2 Headcount"=SE02_HEAD_COUNT_TOTAL_2,
    "Form 3 Headcount"=SE02_HEAD_COUNT_TOTAL_3,
    "Form 4 Headcount"=SE02_HEAD_COUNT_TOTAL_4,
    
    "Form 1 Present"=SE02_REGISTER_PRESENT_TOTAL_1,
    "Form 2 Present"=SE02_REGISTER_PRESENT_TOTAL_2,
    "Form 3 Present"=SE02_REGISTER_PRESENT_TOTAL_3,
    "Form 4 Present"=SE02_REGISTER_PRESENT_TOTAL_4,
    
    "Form 1 Absent"=SE02_REGISTER_ABSENT_TOTAL_1,
    "Form 2 Absent"=SE02_REGISTER_ABSENT_TOTAL_2,
    "Form 3 Absent"=SE02_REGISTER_ABSENT_TOTAL_3,
    "Form 4 Absent"=SE02_REGISTER_ABSENT_TOTAL_4,
    "MOE registration number"=PA12,
    "TSC Code"=PA14,
    "KNEC Code"=PA15)


##head count for:EE02 is pre primary, PE02 is  primary,JSE is junior secondary and SE is secondary
Enrolment<- Data%>% select(County_name:"Education level",
                           "PP1 Headcount",
                           "PP2 Headcount",
                           "PP1 Present",
                           "PP2 Present",
                           "PP1 Absent",
                           "PP2 Absent",
                           "Grade 1 Headcount",
                           "Grade 2 Headcount",
                           "Grade 3 Headcount",
                           "Grade 4 Headcount",
                           "Grade 5 Headcount",
                           "Grade 6 Headcount",
                           "Grade 1 Present",
                           "Grade 2 Present",
                           "Grade 3 Present",
                           "Grade 4 Present",
                           "Grade 5 Present",
                           "Grade 6 Present",
                           "Grade 1 Absent",
                           "Grade 2 Absent",
                           "Grade 3 Absent",
                           "Grade 4 Absent",
                           "Grade 5 Absent",
                           "Grade 6 Absent",
                           "Grade 7 Headcount",
                           "Grade 8 Headcount",
                           "Grade 7 present",
                           "Grade 8 present",
                           "Grade 7 Absent",
                           "Grade 8 Absent",
                           "Form 1 Headcount",
                           "Form 2 Headcount",
                           "Form 3 Headcount",
                           "Form 4 Headcount",
                           "Form 1 Present",
                           "Form 2 Present",
                           "Form 3 Present",
                           "Form 4 Present",
                           "Form 1 Absent",
                           "Form 2 Absent",
                           "Form 3 Absent",
                           "Form 4 Absent",
                           "Pre primary School name",
                           "Primary School name",
                           "Junior school name",
                           "Secondary school name",
                           "MOE registration number",
                           "TSC Code",
                           "KNEC Code",
                           "Prevocational school name")


##############################################################################################
# Convert empty strings to NA and convert to numeric
Enrolments <- Enrolment %>%
  mutate(
    `PP1 Headcount` = as.integer(trimws(`PP1 Headcount`)),
    `PP1 Present` = as.integer(trimws(`PP1 Present`)),
    `PP2 Present` = as.integer(trimws(`PP2 Present`)),
    `Grade 7 Absent` = as.integer(trimws(`Grade 7 Absent`)),
    `Grade 8 Absent` = as.integer(trimws(`Grade 8 Absent`)),
    `Form 1 Present` = as.integer(trimws(`Form 1 Present`)),
    `Form 3 Present` = as.integer(trimws(`Form 3 Present`)),
    `Grade 7 present` = as.integer(trimws(`Grade 7 present`)),
    `Grade 8 present` = as.integer(trimws(`Grade 8 present`))
  )


########### 2. total headcount is equal to total register (present) 
Enrolment <- Enrolments %>%
  mutate(Pre_pp = ifelse(grepl("A", `Education level`), 1, 0))

PP_Count_P <- Enrolment %>%
  filter(!is.na(`PP1 Headcount`) | !is.na(`PP2 Headcount`) | !is.na(`PP1 Present`) | !is.na(`PP2 Present`)) %>%
  rowwise() %>%
  mutate(
    head_count_total_PP = ifelse(Pre_pp == 1, sum(`PP1 Headcount`, `PP2 Headcount`), NA),
    register_present_total_PP = ifelse(Pre_pp == 1, sum(`PP1 Present`, `PP2 Present`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
PP_Count_P_error <- PP_Count_P %>%
  filter(head_count_total_PP == register_present_total_PP)

PP_Count_P_errors <- PP_Count_P_error %>% select(County_name:`Education level`,`PP1 Headcount`:`PP2 Present`,`Pre primary School name`)

write.csv(PP_Count_P_errors, file = "HeadCount equal Register Errors/Pre-p Headcount EQUALregister errors.csv", row.names = FALSE)

########### 2. total headcount is equal to total register (present) 
Enrolment <- Enrolment %>%
  mutate(Prim = ifelse(grepl("B", `Education level`), 1, 0))

P_Count_P <- Enrolment %>%
  filter(!is.na(`Grade 1 Headcount`) | !is.na(`Grade 2 Headcount`) | !is.na(`Grade 3 Headcount`) | !is.na(`Grade 4 Headcount`)| !is.na(`Grade 5 Headcount`)| !is.na(`Grade 6 Headcount`)) %>%
  rowwise() %>%
  mutate(
    head_count_total_P = ifelse(Prim == 1, sum(`Grade 1 Headcount`, `Grade 2 Headcount`,`Grade 3 Headcount`, `Grade 4 Headcount`,`Grade 5 Headcount`, `Grade 6 Headcount`), NA),
    register_present_total_P = ifelse(Prim == 1, sum(`Grade 1 Present`, `Grade 2 Present`,`Grade 3 Present`, `Grade 4 Present`,`Grade 5 Present`, `Grade 6 Present`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
P_Count_P_error <- P_Count_P %>%
  filter(head_count_total_P == register_present_total_P)

P_Count_P_errors <- P_Count_P_error %>% select(County_name:`Education level`,`Grade 1 Headcount`:`Grade 6 Present`,`Junior school name`)

write.csv(P_Count_P_errors, file = "HeadCount equal Register Errors/Primary sch Headcount EQUALregister errors.csv", row.names = FALSE)

########### 2. total headcount is equal to total register (present) 
Enrolment <- Enrolment %>%
  mutate(Jsec = ifelse(grepl("C", `Education level`), 1, 0))

JS_Count_P <- Enrolment %>%
  filter(!is.na(`Grade 7 Headcount`) | !is.na(`Grade 8 Headcount`) | !is.na(`Grade 7 present`)| !is.na(`Grade 8 present`)) %>%
  rowwise() %>%
  mutate(
    head_count_total_JS = ifelse(Jsec == 1, sum(`Grade 7 Headcount`, `Grade 8 Headcount`), NA),
    register_present_total_JS = ifelse(Jsec == 1, sum(`Grade 7 present`, `Grade 8 present`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
JS_Count_P_error <- JS_Count_P %>%
  filter(head_count_total_JS == register_present_total_JS)

JS_Count_P_errors <- JS_Count_P_error %>% select(County_name:`Education level`,`Grade 7 Headcount`:`Grade 8 Headcount`,`Grade 7 present`, `Grade 8 present`,`Junior school name`)

write.csv(JS_Count_P_errors, file = "HeadCount equal Register Errors/Junior sch Headcount EQUALregister errors.csv", row.names = FALSE)

########### 2. total headcount is equal to total register (present) 
Enrolment <- Enrolment %>%
  mutate(Sec = ifelse(grepl("D", `Education level`), 1, 0))

Sec_Count_P <- Enrolment %>%
  filter(!is.na(`Form 1 Headcount`) | !is.na(`Form 2 Headcount`) | !is.na(`Form 3 Headcount`)| !is.na(`Form 4 Headcount`)) %>%
  rowwise() %>%
  mutate(
    head_count_total_Sec = ifelse(Sec == 1, sum(`Form 1 Headcount`, `Form 2 Headcount`,`Form 3 Headcount`, `Form 4 Headcount`), NA),
    register_present_total_Sec = ifelse(Sec == 1, sum(`Form 1 Present`, `Form 2 Present`,`Form 3 Present`, `Form 4 Present`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
Sec_Count_P_error <- Sec_Count_P %>%
  filter(head_count_total_Sec == register_present_total_Sec)

Sec_Count_P_errors <- Sec_Count_P_error %>% select(County_name:`Education level`,`Form 1 Headcount`:`Form 4 Present`,`Secondary school name`)

write.csv(Sec_Count_P_errors, file = "HeadCount equal Register Errors/Secondary sch Headcount EQUALregister errors.csv", row.names = FALSE)


########### 3. total headcount is equal to total register (present) 
Enrolment <- Enrolments %>%
  mutate(Pre_pp = ifelse(grepl("A", `Education level`), 1, 0))

PP_Count_P <- Enrolment %>%
  filter(!is.na(`PP1 Headcount`) | !is.na(`PP2 Headcount`) | !is.na(`PP1 Present`) | !is.na(`PP2 Present`)) %>%
  rowwise() %>%
  mutate(
    head_count_total_PP = ifelse(Pre_pp == 1, sum(`PP1 Headcount`, `PP2 Headcount`), NA),
    register_present_total_PP = ifelse(Pre_pp == 1, sum(`PP1 Present`, `PP2 Present`,`PP1 Absent`, `PP2 Absent`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
PP_Count_P_error <- PP_Count_P %>%
  filter(head_count_total_PP > register_present_total_PP)

PP_Count_P_errors <- PP_Count_P_error %>% select(County_name:`Education level`,`PP1 Headcount`:`PP2 Present`,`Pre primary School name`)

write.csv(PP_Count_P_errors, file = "HeadCount equal Register Errors/Pre-p Headcount more register errors.csv", row.names = FALSE)

########### 2. total headcount is equal to total register (present) 
Enrolment <- Enrolment %>%
  mutate(Prim = ifelse(grepl("B", `Education level`), 1, 0))

P_Count_P <- Enrolment %>%
  filter(!is.na(`Grade 1 Headcount`) | !is.na(`Grade 2 Headcount`) | !is.na(`Grade 3 Headcount`) | !is.na(`Grade 4 Headcount`)| !is.na(`Grade 5 Headcount`)| !is.na(`Grade 6 Headcount`)) %>%
  rowwise() %>%
  mutate(
    head_count_total_P = ifelse(Prim == 1, sum(`Grade 1 Headcount`, `Grade 2 Headcount`,`Grade 3 Headcount`, `Grade 4 Headcount`,`Grade 5 Headcount`, `Grade 6 Headcount`), NA),
    register_present_total_P = ifelse(Prim == 1, sum(`Grade 1 Present`, `Grade 2 Present`,`Grade 3 Present`, `Grade 4 Present`,`Grade 5 Present`, `Grade 6 Present`,`Grade 1 Absent`, `Grade 2 Absent`,`Grade 3 Absent`, `Grade 4 Absent`,`Grade 5 Absent`, `Grade 6 Absent`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
P_Count_P_error <- P_Count_P %>%
  filter(head_count_total_P > register_present_total_P)

P_Count_P_errors <- P_Count_P_error %>% select(County_name:`Education level`,`Grade 1 Headcount`:`Grade 6 Present`,`Primary School name`)

write.csv(P_Count_P_errors, file = "HeadCount equal Register Errors/Primary sch Headcount more register errors.csv", row.names = FALSE)

########### 2. total headcount is equal to total register (present) 
Enrolment <- Enrolment %>%
  mutate(Jsec = ifelse(grepl("C", `Education level`), 1, 0))

JS_Count_P <- Enrolment %>%
  filter(!is.na(`Grade 7 Headcount`) | !is.na(`Grade 8 Headcount`) | !is.na(`Grade 7 present`)| !is.na(`Grade 8 present`)) %>%
  rowwise() %>%
  mutate(
    head_count_total_JS = ifelse(Jsec == 1, sum(`Grade 7 Headcount`, `Grade 8 Headcount`), NA),
    register_present_total_JS = ifelse(Jsec == 1, sum(`Grade 7 present`, `Grade 8 present`,`Grade 7 Absent`, `Grade 8 Absent`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
JS_Count_P_error <- JS_Count_P %>%
  filter(head_count_total_JS == register_present_total_JS)

JS_Count_P_errors <- JS_Count_P_error %>% select(County_name:`Education level`,`Grade 7 Headcount`:`Grade 8 Headcount`,`Grade 7 present`, `Grade 8 present`,`Junior school name`)

write.csv(JS_Count_P_errors, file = "HeadCount equal Register Errors/Junior sch Headcount more register errors.csv", row.names = FALSE)

########### 2. total headcount is equal to total register (present) 
Enrolment <- Enrolment %>%
  mutate(Sec = ifelse(grepl("D", `Education level`), 1, 0))

Sec_Count_P <- Enrolment %>%
  filter(!is.na(`Form 1 Headcount`) | !is.na(`Form 2 Headcount`) | !is.na(`Form 3 Headcount`)| !is.na(`Form 4 Headcount`)) %>%
  rowwise() %>%
  mutate(
    head_count_total_Sec = ifelse(Sec == 1, sum(`Form 1 Headcount`, `Form 2 Headcount`,`Form 3 Headcount`, `Form 4 Headcount`), NA),
    register_present_total_Sec = ifelse(Sec == 1, sum(`Form 1 Present`, `Form 2 Present`,`Form 3 Present`, `Form 4 Present`,`Form 1 Absent`, `Form 2 Absent`,`Form 3 Absent`, `Form 4 Absent`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
Sec_Count_P_error <- Sec_Count_P %>%
  filter(head_count_total_Sec == register_present_total_Sec)

Sec_Count_P_errors <- Sec_Count_P_error %>% select(County_name:`Education level`,`Form 1 Headcount`:`Form 4 Present`,`Secondary school name`)

write.csv(Sec_Count_P_errors, file = "HeadCount equal Register Errors/Secondary sch Headcount more register errors.csv", row.names = FALSE)


##### 4. total register (absent) is greater than 5 
Enrolment <- Enrolments %>%
  mutate(Pre_pp = ifelse(grepl("A", `Education level`), 1, 0))

PP_Count_P <- Enrolment %>%
  filter(!is.na(`PP1 Absent`) | !is.na(`PP2 Absent`)) %>%
  rowwise() %>%
  mutate(
    register_present_total_PP = ifelse(Pre_pp == 1, sum(`PP1 Absent`, `PP2 Absent`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
PP_Count_P_error <- PP_Count_P %>%
  filter(register_present_total_PP >=5)

PP_Count_P_errors <- PP_Count_P_error %>% select(County_name:`Education level`,`PP1 Absent`:`PP2 Absent`,`Pre primary School name`)

write.csv(PP_Count_P_errors, file = "HeadCount equal Register Errors/Pre-p Absent more 5 errors.csv", row.names = FALSE)

########### 4. total register (absent) is greater than 5 
Enrolment <- Enrolments %>%
  mutate(Prim = ifelse(grepl("B", `Education level`), 1, 0))

P_Count_P <- Enrolment %>%
  filter(!is.na(`Grade 1 Absent`) | !is.na(`Grade 2 Absent`)| !is.na(`Grade 3 Absent`)| !is.na(`Grade 4 Absent`)) %>%
  rowwise() %>%
  mutate(
    register_present_total_P = ifelse(Prim == 1, sum(`Grade 1 Absent`, `Grade 2 Absent`,`Grade 3 Absent`, `Grade 4 Absent`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
P_Count_P_error <- P_Count_P %>%
  filter(register_present_total_P >=5)

P_Count_P_errors <- P_Count_P_error %>% select(County_name:`Education level`,`Grade 1 Absent`:`Grade 4 Absent`,`Primary School name`)

write.csv(P_Count_P_errors, file = "HeadCount equal Register Errors/Primary sch Absent more 5 errors.csv", row.names = FALSE)


########### 4. total register (absent) is greater than 5 
Enrolment <- Enrolments %>%
  mutate(JS = ifelse(grepl("C", `Education level`), 1, 0))

JS_Count_P <- Enrolment %>%
  filter(!is.na(`Grade 7 Absent`) | !is.na(`Grade 8 Absent`)) %>%
  rowwise() %>%
  mutate(
    register_present_total_JS = ifelse(JS == 1, sum(`Grade 7 Absent`, `Grade 8 Absent`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
JS_Count_P_error <- JS_Count_P %>%
  filter(register_present_total_JS >=5)

JS_Count_P_errors <- JS_Count_P_error %>% select(County_name:`Education level`,`Grade 7 Absent`:`Grade 8 Absent`,`Junior school name`)

write.csv(JS_Count_P_errors, file = "HeadCount equal Register Errors/Junior sch Absent more 5 errors.csv", row.names = FALSE)

########### 4. total register (absent) is greater than 5 
Enrolment <- Enrolments %>%
  mutate(SS = ifelse(grepl("D", `Education level`), 1, 0))

SS_Count_P <- Enrolment %>%
  filter(!is.na(`Form 1 Absent`) | !is.na(`Form 2 Absent`)| !is.na(`Form 3 Absent`)| !is.na(`Form 4 Absent`)) %>%
  rowwise() %>%
  mutate(
    register_present_total_SS = ifelse(SS == 1, sum(`Form 1 Absent`: `Form 1 Absent`), NA)
  ) %>%
  ungroup()


##Flag them out if they are not the same
SS_Count_P_error <- SS_Count_P %>%
  filter(register_present_total_SS >=5)

SS_Count_P_errors <- SS_Count_P_error %>% select(County_name:`Education level`,`Form 1 Absent`:`Form 4 Absent`)

write.csv(SS_Count_P_errors, file = "HeadCount equal Register Errors/Secondary sch Absent more 5 errors.csv", row.names = FALSE)

##############################################################################################
# Ensure case-insensitivity and trim any extra spaces
PA12_none_freq <- Enrolment %>%
  group_by(USER_NAME) %>%
  summarise(PA12_none = sum(trimws(tolower(`MOE registration number`)) == "none", na.rm = TRUE))

write.csv(PA12_none_freq, file = "Frequencies/Freq of None in registration number.csv", row.names = FALSE)

PA14_none_freq <- Enrolment %>%
  group_by(USER_NAME) %>%
  summarise(PA14_none = sum(trimws(tolower(`TSC Code`)) == "none", na.rm = TRUE))

write.csv(PA14_none_freq, file = "Frequencies/Freq of None in TSC code.csv", row.names = FALSE)

PA15_none_freq <- Enrolment %>%
  group_by(USER_NAME) %>%
  summarise(PA15_none = sum(trimws(tolower(`KNEC Code`)) == "none", na.rm = TRUE))

write.csv(PA15_none_freq, file = "Frequencies/Freq of None in KNEC code.csv", row.names = FALSE)

School_code<- Data%>% select(County_name:"Education level",
                             "Pre primary School name",
                             "Primary School name",
                             "Junior school name",
                             "Secondary school name",
                             SAME_HEAD_PP,
                             CENSUSCODE_PP_P1,
                             CENSUSCODE,
                             CENSUSCODE_JS_P1,
                             CENSUSCODE_SS_1)

# Add an ID column to keep track of the original rows
School_Census <- Data %>%
  mutate(id = row_number())

# Add an ID column to keep track of the original rows
School_Census <- Data %>%
  mutate(id = row_number())

School_Census <- School_Census %>%
  mutate(PP = ifelse(grepl("A", `Education level`), 1, 0))

PP_Code_error <- subset(School_Census, (SAME_HEAD_PP== 1 & PP==1 & CENSUSCODE_PP_P1!=CENSUSCODE))

PP_Code_error <- PP_Code_error %>% select(County_name:`Education level`,`Pre primary School name`,`SAME_HEAD_PP`,`CENSUSCODE`,`CENSUSCODE_PP_P1`)

write.csv(PP_Code_error, file = "School code errors/Preprimary sch code errors.csv", row.names = FALSE)


########Primary school code mismatch
School_Census <- School_Census %>%
  mutate(JS = ifelse(grepl("C", `Education level`), 1, 0))

JS_Code_error <- subset(School_Census, (SAME_HEAD_PP== 1 & JS==1 & CENSUSCODE_JS_P1!=CENSUSCODE))

JS_Code_error <- JS_Code_error %>% select(County_name:`Education level`,`Junior school name`,`SAME_HEAD_PP`,`CENSUSCODE`,`CENSUSCODE_JS_P1`)

write.csv(JS_Code_error, file = "School code errors/Junior sch code errors.csv", row.names = FALSE)

##Secondary school code mismatch
School_Census <- School_Census %>%
  mutate(SS = ifelse(grepl("D", `Education level`), 1, 0))

SS_Code_error <- subset(School_Census, (SAME_HEAD_PP== 1 & SS==1 & CENSUSCODE_SS_1!=CENSUSCODE))

SS_Code_error <- SS_Code_error %>% select(County_name:`Education level`,`Secondary school name`,`SAME_HEAD_PP`,`CENSUSCODE`,`CENSUSCODE_SS_1`)

write.csv(SS_Code_error, file = "School code errors/Secondary sch code errors.csv", row.names = FALSE)


##Pre vocational code mismatch
School_Census <- School_Census %>%
  mutate(PV = ifelse(grepl("E", `Education level`), 1, 0))

PV_Code_error <- subset(School_Census, (SAME_HEAD_PP== 1 & PV==1 & CENSUSCODE_PV_P1!=CENSUSCODE))

PV_Code_error <- PV_Code_error %>% select(County_name:`Education level`,`Prevocational school name`,`SAME_HEAD_PP`,`CENSUSCODE`,`CENSUSCODE_PV_P1`)

write.csv(PV_Code_error, file = "School code errors/Pre vocational sch code errors.csv", row.names = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("School Census Data Quality System"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
