d0 <- read.csv(file.path("data", "dataset_200408.csv"), header = T, sep=",")

d1 <- d0 %>%
  mutate(
    Study.Type.b = case_when(
      !Study.Type %in% c('Interventional', 'Observational') ~ 'Expanded Access',
      Study.Type == 'Observational' ~ 'Observational',
      Study.Type == 'Interventional' ~ 'Interventional'),
    Phases = case_when(
      Phases %in% c('Early Phase 1', 'Phase 1') ~ 'Phase 1',
      Phases == 'Phase 1|Phase 2' ~ 'Phase 1|2',
      Phases == 'Phase 2' ~ 'Phase 2',
      Phases == 'Phase 2|Phase 3' ~ 'Phase 2|3',
      Phases == 'Phase 3' ~ 'Phase 3',
      Phases == 'Phase 4' ~ 'Phase 4',
      Phases == 'Not Applicable' ~ 'Not Applicable'),
    Phases = fct_relevel(Phases, "Not Applicable", after = Inf),
    Funded.Bys.b = case_when(
      Funded.Bys %in% c('Industry', 'Industry|Other') ~ 'Industry',
      Funded.Bys %in% c('Other', 'Other|Industry', "Other|NIH", "NIH", "U.S. Fed") ~ 'Other')
  )

d2a <- d1 %>%
  filter(Study.Type=="Interventional" & Phases %in% c("Phase 2"," Phase 2|3"),
         Interventions %like% "Drug:")  %>%
  select(Interventions) 

d2b <- d1 %>%
  filter(Study.Type=="Interventional" & Phases=="Phase 3",
         Interventions %like% "Drug:")  %>%
  select(Interventions) 