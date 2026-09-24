library(tidyverse)
library(readxl)
library(summarytools)
library(janitor)
library(dplyr)
library(stringr)

messy <- read_xlsx("data/messy_mangrove_data.xlsx")
# view(dfSummary(messy))
# dfSummary(messy)
case <- case %>%
  mutate(
    Treatment = case_when(
      str_detect(Treatment, regex("^(T1|Treatment 1)", ignore_case = TRUE)) ~ "T1",
      str_detect(Treatment, regex("^(T2|Treatment 2)", ignore_case = TRUE)) ~ "T2",
      TRUE ~ Treatment
    )
  )

# clean_names(), str_to_lower(), parse_number()



case <- messy |> 
  mutate(Location = str_to_title(Location), Treatment = str_to_title(Treatment))

lng <- case |> 
  pivot_longer(cols = "46027":"46203",
               names_to = "Date",
               values_to = "Height_cm")

lng$Date <- excel_numeric_to_date(as.numeric(lng$Date))


lng <- lng |>  #changing to match all names in treatment column
  mutate(
    Treatment = case_when(
      str_detect(Treatment, regex("^(T1|Treatment 1)", ignore_case = TRUE)) ~ "T1",
      str_detect(Treatment, regex("^(T2|Treatment 2)", ignore_case = TRUE)) ~ "T2",
      str_detect(Treatment, regex("^(CTRL|Control)", ignore_case = TRUE)) ~ "Control",
      TRUE ~ Treatment))
    
lng$Location <- str_replace_all(lng$Location, " ", "_")

lng <- lng |> 
  clean_names()

#numbers
clean <- lng |>
  mutate(
    height_cm = str_to_lower(str_trim(as.character(height_cm))), #lowercase everything
    height_cm = if_else(height_cm %in% c("n/a", "missing", "dead"), NA_character_, height_cm), #make all other non numbers a NA
    height_cm = parse_number(as.character(height_cm))) #gets rid of cm and long sig figs

hist(clean$height_cm)

get_dupes(clean)

clean <- clean |> # gets rid of all duplicate rows
  distinct()

get_dupes(clean)
