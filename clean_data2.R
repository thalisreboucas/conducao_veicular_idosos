library(tidyverse)
library(readxl)
library(DataEditR)

setwd("E:/edime/Thalis/MEU/Danielle/conducao_veicular_idosos/raw_data")


raw_df <- readxl::read_excel(file.choose(),skip =1)
#VERSAO 2 - DADOS EXCELL - ESTATÍSTICO THALIS - ABR 2023 (189 artigos) - 06-04-23.xlsx

publication <- raw_df %>%
  select("Nº artigo...1", starts_with("20") | starts_with("19")  ) %>%
  rename(id_publication = `Nº artigo...1`) %>%
  pivot_longer(-id_publication,
               values_to = "total_year",
               names_to = "year") %>%
  filter(total_year > 0) %>%
  select(-total_year)

df_publication_country <- raw_df %>%
  select("Nº artigo...30", "US", "CA", "BR", "AU", "OUTROS...35") %>%
  rename(id_publication = `Nº artigo...30`) %>%
  pivot_longer(-id_publication,
               values_to = "total_country",
               names_to = "country") %>%
  filter(total_country > 0) %>%
  mutate(country = case_when(country == "US" ~ "United States",
                             country == "BR" ~ "Brazil",
                             country == "AU" ~ "Australia",
                             country == "CA" ~ "Canada",
                             country == "OUTROS...35" ~ "Other Countries"))

df_elder_population_type <- raw_df %>%
  select("...36", "PIS", "PDA", "PDP","PDC", "OUTROS...41") %>%
  rename(id_publication = `...36`) %>%
  pivot_longer(-id_publication,
               values_to = "total_elder_population_type",
               names_to = "elder_population_type") %>%
  filter(total_elder_population_type > 0) %>%
  mutate(elder_population_type = case_when(elder_population_type == "PIS" ~ "Healthy",
                                           elder_population_type == "PDA" ~ "With Alzheimer's Disease",
                                           elder_population_type == "PDP" ~ "With Parkinson's Disease",
                                           elder_population_type == "PDC" ~ "With Mild Cognitiva Impairment",
                                           elder_population_type ==  "OUTROS...41" ~ "With Other Diseases"))

df_intervention_type <- raw_df %>%
  select("...42", "EDU", "AVP", "AVT", "AVD", "AVF") %>%
  rename(id_publication = `...42`) %>%
  pivot_longer(-id_publication,
               values_to = "total_intervention_type",
               names_to = "intervention_type") %>%
  filter(total_intervention_type > 0) %>%
  mutate(intervention_type = case_when(intervention_type == "EDU" ~ "Educational",
                                       intervention_type == "AVP" ~ "Professional Evaluation ",
                                       intervention_type == "AVT" ~ "Tests Evaliation",
                                       intervention_type == "AVD" ~ "Guideline Evaluation",
                                       intervention_type == "AVF" ~ "Family Evaluation"))

df_validator_occupation <- raw_df %>%
  select("...48", "TO...49", "PSICO...50","OUTROS...51","NHAT","NI...53" ,) %>%
  rename(id_publication = `...48`) %>%
  pivot_longer(-id_publication,
               values_to = "total_validator_occupation",
               names_to = "validator_occupation") %>%
  filter(total_validator_occupation > 0) %>%
  mutate(validator_occupation = case_when(validator_occupation == "TO...49" ~ "Occupational Therapy",
                                          validator_occupation == "PSICO...50" ~ "Psychology",
                                          validator_occupation == "NHAT" ~ "Don't Have Aplication",
                                          validator_occupation == "NI...53" ~ "Don't identified",
                                          validator_occupation == "OUTROS...51" ~ "Other Validator"))



df_author_occupation <- raw_df %>%  select("...54", "MED","PSICO...56",
                                           "TO...57" ,"BIOEST." ,"ENFER.","GERONTO","OUTROS...61","NI...62") %>%
  rename(id_publication = `...54`) %>%
  pivot_longer(-id_publication,
               values_to = "total_author_occupation",
               names_to = "author_occupation") %>%
  filter(total_author_occupation > 0) %>%
  mutate(author_occupation = case_when(author_occupation == "MED" ~ "Medicine",
                                       author_occupation == "TO...57" ~ "Occupational Therapy",
                                       author_occupation == "BIOEST." ~ "Biostatistics",
                                       author_occupation == "ENFER." ~ "Nursing",
                                       author_occupation == "PSICO...56" ~ "Psychology",
                                       author_occupation == "OUTROS...61" ~ "Others Professionals",
                                       author_occupation == "NI...62" ~ "Don't Identified",
                                       author_occupation == "GERONTO" ~ "Gerontology"))


# Terceira Base de Dados
df_valitador_occupation1 <- raw_df <- readxl::read_excel(file.choose())

df_validator_occupation <- df_valitador_occupation1 %>% 
  mutate(author_occupation = case_when(Valitador == "MED" ~ "Medicine",
                                       Valitador == "TO" ~ "Occupational Therapy",
                                       Valitador == "ENF" ~ "Nursing",
                                       Valitador == "PSICO" ~ "Psychology",
                                       Valitador == "NI" ~ "Others Professionals",
                                       Valitador == "GERONTO" ~ "Gerontology"),
  )  %>% slice(-6) %>% select(author_occupation,Total)


# Juntando tudo 

df_complete <- publication %>% 
  left_join(df_intervention_type,by = c('id_publication'='id_publication')) %>% 
  left_join(df_publication_country,by = c('id_publication'='id_publication')) %>% 
  left_join(df_elder_population_type,by = c('id_publication'='id_publication')) %>% 
  left_join(df_validator_occupation,by = c('id_publication'='id_publication')) %>% 
  left_join(df_author_occupation,by = c('id_publication'='id_publication'))

# Juntando tudo 2 

df_complete <- publication %>% 
  left_join(df_intervention_type,by = c('id_publication'='id_publication')) %>% 
  left_join(df_publication_country,by = c('id_publication'='id_publication')) %>% 
  left_join(df_elder_population_type,by = c('id_publication'='id_publication')) %>% 
  left_join(df_validator_occupation,by = c('id_publication'='id_publication')) %>% 
  left_join(df_author_occupation,by = c('id_publication'='id_publication')) 

