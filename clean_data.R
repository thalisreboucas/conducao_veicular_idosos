library(tidyverse)
library(readxl)
library(DataEditR)

setwd("E:/edime/Thalis/MEU/Danielle/conducao_veicular_idosos/raw_data")


raw_df <- readxl::read_excel(file.choose(),skip =1)
#DADOS EXCELL - BUSCA A - B (1 AO 182) - ESTATÍSTICO THALIS - 25-06-22.xlsx

publication <- raw_df %>%
  select("Nº artigo...1", starts_with("20") | starts_with("19") |starts_with("21")|starts_with("22") ) %>%
  rename(id_publication = `Nº artigo...1`) %>%
  pivot_longer(-id_publication,
               values_to = "total_year",
               names_to = "year") %>%
  filter(total_year > 0) %>%
  select(-total_year)

df_publication_country <- raw_df %>%
  select("Nº artigo...29", "US", "CA", "BR", "AU", "OUTROS...34") %>%
  rename(id_publication = `Nº artigo...29`) %>%
  pivot_longer(-id_publication,
               values_to = "total_country",
               names_to = "country") %>%
  filter(total_country > 0) %>%
  mutate(country = case_when(country == "US" ~ "United States",
                             country == "BR" ~ "Brazil",
                             country == "AU" ~ "Australia",
                             country == "CA" ~ "Canada",
                             country == "OUTROS...34" ~ "Other Countries"))

df_elder_population_type <- raw_df %>%
  select("...35", "PIS", "PDA", "PDP","PDC", "OUTROS...40") %>%
  rename(id_publication = `...35`) %>%
  pivot_longer(-id_publication,
               values_to = "total_elder_population_type",
               names_to = "elder_population_type") %>%
  filter(total_elder_population_type > 0) %>%
  mutate(elder_population_type = case_when(elder_population_type == "PIS" ~ "Healthy",
                                           elder_population_type == "PDA" ~ "With Alzheimer's Disease",
                                           elder_population_type == "PDP" ~ "With Parkinson's Disease",
                                           elder_population_type == "PDC" ~ "With Mild Cognitiva Impairment",
                                           elder_population_type == "OUTROS...40" ~ "With Other Diseases"))

df_intervention_type <- raw_df %>%
  select("...41", "EDU", "AVP", "AVT", "AVD", "AVF") %>%
  rename(id_publication = `...41`) %>%
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
  select("...47", "TO", "PSICO","NHAT","NI" ,"OUTROS...50") %>%
  rename(id_publication = `...47`) %>%
  pivot_longer(-id_publication,
               values_to = "total_validator_occupation",
               names_to = "validator_occupation") %>%
  filter(total_validator_occupation > 0) %>%
  mutate(validator_occupation = case_when(validator_occupation == "TO" ~ "Occupational Therapy",
                                          validator_occupation == "PSICO" ~ "Psychology",
                                          validator_occupation == "NHAT" ~ "Don't Have Aplication",
                                          validator_occupation == "NI" ~ "Don't identified",
                                          validator_occupation == "OUTROS...50" ~ "Other Validator"))



#Segunda Base de dados

df_author_occupation1 <- raw_df <- readxl::read_excel(file.choose(),skip = 1)

df_author_occupation <-df_author_occupation1 %>%  select("...1", "TO", "PSICO", "GERONTO","ENFER.", "BIOEST.","MED","OUTROS") %>%
                         rename(id_publication = `...1`) %>%
  pivot_longer(-id_publication,
               values_to = "total_author_occupation",
               names_to = "author_occupation") %>%
  filter(total_author_occupation > 0) %>%
  mutate(author_occupation = case_when(author_occupation == "MED" ~ "Medicine",
                                       author_occupation == "TO" ~ "Occupational Therapy",
                                       author_occupation == "BIOEST." ~ "Biostatistics",
                                       author_occupation == "ENFER." ~ "Nursing",
                                       author_occupation == "PSICO" ~ "Psychology",
                                       author_occupation == "OUTROS" ~ "Others Professionals",
                                       author_occupation == "NI" ~ "Don't Identified",
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

