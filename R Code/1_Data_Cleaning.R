# --------------------------------------------------------------
# 1. clear + set more off
# --------------------------------------------------------------
rm(list = ls())          # clear environment
options(warn = -1)       # suppress warnings (like set more off)

# --------------------------------------------------------------
# 2. import excel raw_data_covid.xls, sheet("Sheet1") firstrow
# --------------------------------------------------------------
library(readxl)
path <- "/Users/choochoojc/Desktop/STAT204/Homeworks/Final Project/raw_data_covid.csv"

# ---------------------------
# 1) Import
# ---------------------------
df <- read.csv(path, header = TRUE) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))   # Stata treats missing as 0 for many vars


# --------------------------------------------------------------
# 3. PANEL VARIABLES
# --------------------------------------------------------------
# tabulate panel, generate(panel)
df <- fastDummies::dummy_cols(df, select_columns = "panel", remove_first_dummy = FALSE)
df <- df %>% rename(
  Winter2018_19 = panel_2,
  Summer2019    = panel_3,
  Winter2019_20 = panel_4,
  Summer2020    = panel_5,
  Winter2020_21 = panel_6
)

# --------------------------------------------------------------
# 4. DEPENDENT VARIABLE CONSUMPTION
# --------------------------------------------------------------
# ren Stdgehört7TageHerkömmli ConsumptionRadio
df <- df %>% rename(ConsumptionRadio = Stdgehört7TageHerkömmli)
df <- df %>% rename(ConsumptionPhysical = Stdgehört7TagePhysische)
df <- df %>% rename(ConsumptionDigital = Stdgehört7TageAufIhren)
df <- df %>% rename(ConsumptionFreeStreaming = Stdgehört7TageKostenlos)
df <- df %>% rename(ConsumptionPremiumStreaming = Stdgehört7Tagekostenpfl)
df <- df %>% rename(ConsumptionOnlineRadio = Stdgehört7TageOnlineRa)
df <- df %>% rename(ConsumptionLive = Stdgehört7TageLiveMusi)

# gen Consumption_Live = ConsumptionLive
df <- df %>% mutate(Consumption_Live = ConsumptionLive)

# replace Consumption_Live = 0 if Consumption_Live ==. 
df <- df %>% mutate(Consumption_Live = replace(Consumption_Live, is.na(Consumption_Live), 0))

# gen Consumption_Rec = ConsumptionPremiumStreaming + ConsumptionPhysical + ConsumptionFreeStreaming + ConsumptionDigital + ConsumptionRadio + ConsumptionOnlineRadio
df <- df %>% mutate(
  Consumption_Rec = ConsumptionPremiumStreaming + ConsumptionPhysical +
                    ConsumptionFreeStreaming + ConsumptionDigital +
                    ConsumptionRadio + ConsumptionOnlineRadio
)

# replace Consumption_Rec = 0 if Consumption_Rec ==.
df <- df %>% mutate(Consumption_Rec = replace(Consumption_Rec, is.na(Consumption_Rec), 0))

# generate s_Consumption_Physical_R = ConsumptionPhysical/Consumption_Rec
df <- df %>% mutate(s_Consumption_Physical_R = ConsumptionPhysical / Consumption_Rec)
df <- df %>% mutate(s_Consumption_PremiumStream_R = ConsumptionPremiumStreaming / Consumption_Rec)
df <- df %>% mutate(s_Consumption_FreeStream_R = ConsumptionFreeStreaming / Consumption_Rec)
df <- df %>% mutate(s_Consumption_Digital_R = ConsumptionDigital / Consumption_Rec)
df <- df %>% mutate(s_Consumption_Radio_R = ConsumptionRadio / Consumption_Rec)
df <- df %>% mutate(s_Consumption_OnlineRadio_R = ConsumptionOnlineRadio / Consumption_Rec)

# replace s_Consumption_Physical_R = 0 if s_Consumption_Physical_R ==.
df <- df %>% mutate(across(starts_with("s_Consumption_"), ~ replace(.x, is.na(.x), 0)))

# gen s_Consumption_R = s_Consumption_PremiumStream_R + s_Consumption_Physical_R + s_Consumption_FreeStream_R + s_Consumption_Digital_R + s_Consumption_Radio_R + s_Consumption_OnlineRadio_R
df <- df %>% mutate(
  s_Consumption_R = s_Consumption_PremiumStream_R + s_Consumption_Physical_R +
                    s_Consumption_FreeStream_R + s_Consumption_Digital_R +
                    s_Consumption_Radio_R + s_Consumption_OnlineRadio_R
)

# gen s_No_Consumption_R = 0
df <- df %>% mutate(s_No_Consumption_R = 0)

# replace s_No_Consumption_R = 1 if s_Consumption_R==0
df <- df %>% mutate(s_No_Consumption_R = if_else(s_Consumption_R == 0, 1, 0))

# gen Consumption_Total = ConsumptionRadio + ConsumptionOnlineRadio + ConsumptionPhysical + ConsumptionDigital + ConsumptionFreeStreaming + ConsumptionPremiumStreaming + ConsumptionLive
df <- df %>% mutate(
  Consumption_Total = ConsumptionRadio + ConsumptionOnlineRadio +
                      ConsumptionPhysical + ConsumptionDigital +
                      ConsumptionFreeStreaming + ConsumptionPremiumStreaming +
                      ConsumptionLive
)

# gen No_Consumption = 0
df <- df %>% mutate(No_Consumption = 0)

# replace No_Consumption = 1 if Consumption_Total==0
df <- df %>% mutate(No_Consumption = if_else(Consumption_Total == 0, 1, 0))

# gen ln_Consumption_Total = ln(Consumption_Total+1)
df <- df %>% mutate(ln_Consumption_Total = log(Consumption_Total + 1))

# gen ln_Consumption_Rec = ln(Consumption_Rec+1)
df <- df %>% mutate(ln_Consumption_Rec = log(Consumption_Rec + 1))

# gen ln_Consumption_Live = ln(ConsumptionLive+1)
df <- df %>% mutate(ln_Consumption_Live = log(ConsumptionLive + 1))

# gen Outlier_Consumption = 0
df <- df %>% mutate(Outlier_Consumption = 0)

# replace Outlier_Consumption = 1 if Consumption_Total > 168
df <- df %>% mutate(Outlier_Consumption = if_else(Consumption_Total > 168, 1, Outlier_Consumption))

# replace Outlier_Consumption = 1 if Consumption_Total ==.
df <- df %>% mutate(Outlier_Consumption = if_else(is.na(Consumption_Total), 1, Outlier_Consumption))

# gen Outlier_Consumption_Panel = 0
df <- df %>% mutate(Outlier_Consumption_Panel = 0)

# bysort Pseudonym(Outlier_Consumption): replace Outlier_Consumption_Panel = 1 if Outlier_Consumption[_N] == 1
df <- df %>%
  group_by(Pseudonym) %>%
  mutate(Outlier_Consumption_Panel = if_else(max(Outlier_Consumption) == 1, 1, 0)) %>%
  ungroup()

# --------------------------------------------------------------
# 5. DEPENDENT VARIABLE SPENDING
# --------------------------------------------------------------
df <- df %>% rename(Spending_CD_Albums = CDAlbenAusgaben30Tage)
df <- df %>% rename(Spending_CD_Singles = CDSinglesAusgaben30Tage)
df <- df %>% rename(Spending_Music_DVD = MusikDVDsBluRaysAusgaben3)
df <- df %>% rename(Spending_Vinyl = VinylAlbenAusgaben30Tage)
df <- df %>% rename(Spending_DigitaleAlbums = KostenpflichtigedigitaleAlbum)
df <- df %>% rename(Spending_DigitalTracks = KostenpflichtigedigitaleTrack)
df <- df %>% rename(Spending_Stream = DigitaleMusikAboServicesPau)
df <- df %>% rename(Spending_OtherAbos = SonstigedigitaleMusikAboServ)
df <- df %>% rename(Spending_Concerts = KonzerteLiveMusikEventsAus)
df <- df %>% rename(Spending_Festivals = FestivalsLiveMusikEventsAu)
df <- df %>% rename(Spending_ClubConcerts = ClubKonzerteLiveMusikEvents)
df <- df %>% rename(Spending_DeluxeBoxes = DeluxeBoxenMusikproduktMer)

# replace Spending_CD_Albums = 0 if Spending_CD_Albums ==.
df <- df %>% mutate(across(starts_with("Spending_"), ~ replace(.x, is.na(.x), 0)))

# gen Spending_Physical = Spending_CD_Albums + Spending_CD_Singles + Spending_Music_DVD + Spending_Vinyl + Spending_DeluxeBoxes
df <- df %>% mutate(
  Spending_Physical = Spending_CD_Albums + Spending_CD_Singles +
                      Spending_Music_DVD + Spending_Vinyl + Spending_DeluxeBoxes
)

# gen Spending_Digital = Spending_DigitaleAlbums + Spending_DigitalTracks 
df <- df %>% mutate(Spending_Digital = Spending_DigitaleAlbums + Spending_DigitalTracks)

# gen Spending_Streaming = Spending_Stream + Spending_OtherAbos
df <- df %>% mutate(Spending_Streaming = Spending_Stream + Spending_OtherAbos)

# replace Spending_Physical = 0 if Spending_Physical ==. 
df <- df %>% mutate(across(c(Spending_Physical, Spending_Digital, Spending_Streaming), ~ replace(.x, is.na(.x), 0)))

# gen Spending_Rec = Spending_Streaming + Spending_Physical + Spending_Digital
df <- df %>% mutate(Spending_Rec = Spending_Streaming + Spending_Physical + Spending_Digital)

# replace Spending_Rec = 0 if Spending_Rec ==. 
df <- df %>% mutate(Spending_Rec = replace(Spending_Rec, is.na(Spending_Rec), 0))

# gen Spending_Live = Spending_Concerts + Spending_Festivals + Spending_ClubConcerts
df <- df %>% mutate(Spending_Live = Spending_Concerts + Spending_Festivals + Spending_ClubConcerts)

# replace Spending_Live = 0 if Spending_Live ==. 
df <- df %>% mutate(Spending_Live = replace(Spending_Live, is.na(Spending_Live), 0))

# gen s_Spending_Physical_R = Spending_Physical/Spending_Rec
df <- df %>% mutate(s_Spending_Physical_R = Spending_Physical / Spending_Rec)
df <- df %>% mutate(s_Spending_Digital_R = Spending_Digital / Spending_Rec)
df <- df %>% mutate(s_Spending_Streaming_R = Spending_Streaming / Spending_Rec)

# replace s_Spending_Physical_R = 0 if s_Spending_Physical_R ==. 
df <- df %>% mutate(across(starts_with("s_Spending_"), ~ replace(.x, is.na(.x), 0)))

# gen Spending_Total = ...
df <- df %>% mutate(
  Spending_Total = Spending_CD_Albums + Spending_CD_Singles + Spending_Vinyl +
                   Spending_Music_DVD + Spending_DigitaleAlbums + Spending_DigitalTracks +
                   Spending_Stream + Spending_OtherAbos + Spending_Concerts +
                   Spending_Festivals + Spending_ClubConcerts + Spending_DeluxeBoxes
)

# gen No_Spending = 0
df <- df %>% mutate(No_Spending = 0)

# replace No_Spending = 1 if Spending_Total==0
df <- df %>% mutate(No_Spending = if_else(Spending_Total == 0, 1, 0))

# gen ln_Spending_Total = ln(Spending_Total+1)
df <- df %>% mutate(ln_Spending_Total = log(Spending_Total + 1))
df <- df %>% mutate(ln_Spending_Rec = log(Spending_Rec + 1))
df <- df %>% mutate(ln_Spending_Live = log(Spending_Live + 1))

# gen s_Spending_R = s_Spending_Physical_R + s_Spending_Digital_R + s_Spending_Streaming_R
df <- df %>% mutate(s_Spending_R = s_Spending_Physical_R + s_Spending_Digital_R + s_Spending_Streaming_R)

# gen s_No_Spending_R = 0
df <- df %>% mutate(s_No_Spending_R = 0)

# replace s_No_Spending_R = 1 if s_Spending_R==0
df <- df %>% mutate(s_No_Spending_R = if_else(s_Spending_R == 0, 1, 0))

# gen Outlier_Spending = 0
df <- df %>% mutate(Outlier_Spending = 0)

# replace Outlier_Spending = 1 if Spending_Total >246
df <- df %>% mutate(Outlier_Spending = if_else(Spending_Total > 246, 1, Outlier_Spending))

# gen Outlier_Spending_Panel = 0
df <- df %>% mutate(Outlier_Spending_Panel = 0)

# bysort Pseudonym(Outlier_Spending): replace Outlier_Spending_Panel = 1 if Outlier_Spending[_N]==1
df <- df %>%
  group_by(Pseudonym) %>%
  mutate(Outlier_Spending_Panel = if_else(max(Outlier_Spending) == 1, 1, 0)) %>%
  ungroup()

# --------------------------------------------------------------
# 6. CONTROL VARIABLES
# --------------------------------------------------------------
df <- df %>% rename(gender = Geschlecht)
df <- df %>% rename(Age = Alter)
df <- df %>% rename(Children = Kinder)
df <- df %>% rename(IncomeRaw = Haushaltseinkommen)
df <- df %>% rename(NoInstrument = keins)
df <- df %>% rename(ActiveListening = WennichMusikhöremacheichni)
df <- df %>% rename(MusicAppreciation = Musikistmirimmeretwaswert)

# gen Corona = 0
df <- df %>% mutate(Corona = 0)

# replace Corona = 1 if panel == 5 | panel == 6
df <- df %>% mutate(Corona = if_else(panel %in% c(5, 6), 1, 0))

# gen Summer = 0 
df <- df %>% mutate(Summer = 0)

# replace Summer = 1 if panel == 3 | panel == 5
df <- df %>% mutate(Summer = if_else(panel %in% c(3, 5), 1, 0))

# gen Winter = 0
df <- df %>% mutate(Winter = 0)

# replace Winter = 1 if panel == 2 | panel == 4 | panel == 6
df <- df %>% mutate(Winter = if_else(panel %in% c(2, 4, 6), 1, 0))

# tabulate Warumamhäufigstengekauft, generate(Warumamhäufigstengekauft) 
df <- fastDummies::dummy_cols(df, select_columns = "Warumamhäufigstengekauft", remove_first_dummy = FALSE)

# rename Warumamhäufigstengekauft1 PurchaseReason_Atmosphere
df <- df %>% rename(PurchaseReason_Atmosphere = Warumamhäufigstengekauft_Atmosphäre)
df <- df %>% rename(PurchaseReason_Flexibility = Warumamhäufigstengekauft_Flexibilität)
df <- df %>% rename(PurchaseReason_Habit = Warumamhäufigstengekauft_Gewohnheit)
df <- df %>% rename(PurchaseReason_Mobility = Warumamhäufigstengekauft_Mobilität)
df <- df %>% rename(PurchaseReason_Other = Warumamhäufigstengekauft_Sonstiges)
df <- df %>% rename(PurchaseReason_SoundQuality = Warumamhäufigstengekauft_Soundqualität)
df <- df %>% rename(PurchaseReason_None = `Warumamhäufigstengekauft_ich kaufe keine Musik`)

# replace PurchaseReason_Atmosphere = 0 if PurchaseReason_Atmosphere ==.
df <- df %>% mutate(across(starts_with("PurchaseReason_"), ~ replace(.x, is.na(.x), 0)))

# tabulate Familienstand, generate(MaritalStatus) 
df <- fastDummies::dummy_cols(df, select_columns = "Familienstand", remove_first_dummy = FALSE)
df <- df %>% rename(MaritalStat_LivAlone = `Familienstand_allein lebend`)
df <- df %>% rename(MaritalStat_LivAlone_Partner = `Familienstand_allein lebend UND in einer festen Partnerschaft`)
df <- df %>% rename(MaritalStat_LivTogether_Partner = `Familienstand_in einer festen Partnerschaft zusammenlebend`)

# gen MaritalStatus = 0 if MaritalStat_LivAlone == 1 
df <- df %>% mutate(MaritalStatus = case_when(
  MaritalStat_LivAlone == 1 ~ 0,
  MaritalStat_LivAlone_Partner == 1 ~ 1,
  MaritalStat_LivTogether_Partner == 1 ~ 2,
  TRUE ~ NA_real_
))

# label define MaritalStatus 0 "Living alone" 1 "Living separately in a relationship" 2 "Living together in a relationship"
# label values MaritalStatus MaritalStatus
attr(df$MaritalStatus, "label") <- "0=Living alone, 1=Living separately, 2=Living together"

# tabulate Bildung, generate(education) 
df <- fastDummies::dummy_cols(df, select_columns = "Bildung", remove_first_dummy = FALSE)
df <- df %>% rename(Education_HighSchool = Bildung_Abitur)
df <- df %>% rename(Education_Bachelor = Bildung_Bachelor)
df <- df %>% rename(Education_TechnicalDiploma = Bildung_Fachhochschulreife)
df <- df %>% rename(Education_Elementary = Bildung_Hauptschulabschluss)
df <- df %>% rename(Education_Master = `Bildung_Master / Diplom / Magister / Staatsexamen`)
df <- df %>% rename(Education_MiddleSchool = `Bildung_Mittlere Reife / Realschulabschluss`)
df <- df %>% rename(Education_PhD = Bildung_Promotion)
df <- df %>% rename(Education_NoneDegree = `Bildung_kein Abschluss`)

# gen Education = 0 if Education_NoneDegree | Education_MainSchool == 1
df <- df %>% mutate(
  Education = case_when(
    Education_NoneDegree == 1 | Education_Elementary == 1 ~ 0,
    Education_MiddleSchool == 1 | Education_TechnicalDiploma == 1 | Education_HighSchool == 1 ~ 1,
    Education_Bachelor == 1 | Education_Master == 1 | Education_PhD == 1 ~ 2,
    TRUE ~ NA_real_
  )
)

# label define Education 0 "NoDegree/MainSchool" 1 "SecondarySchool/TechnicalDiploma/Abitur" 2 "Bachelor/Master/Promotion"
attr(df$Education, "label") <- "0=NoDegree/MainSchool, 1=Secondary/Technical/Abitur, 2=Bachelor/Master/PhD"

# tabulate gender, generate(gender) 
df <- fastDummies::dummy_cols(df, select_columns = "gender", remove_first_dummy = FALSE)
df <- df %>% rename(GenderOther = gender_andere)
df <- df %>% rename(GenderMale = gender_männlich)
df <- df %>% rename(GenderFemale = gender_weiblich)

# tabulate Beschäftigung, generate(Occupation) 
df <- fastDummies::dummy_cols(df, select_columns = "Beschäftigung", remove_first_dummy = FALSE)
df <- df %>% rename(Occupation_Employed = `Beschäftigung_Angestellte/r`)
df <- df %>% rename(Occupation_Apprenticeship = `Beschäftigung_Auszubildende/r`)
df <- df %>% rename(Occupation_Household = `Beschäftigung_Hausmann / Hausfrau`)
df <- df %>% rename(Occupation_School = `Beschäftigung_Schüler/in`)
df <- df %>% rename(Occupation_Selfemployed = `Beschäftigung_Selbständige/r`)
df <- df %>% rename(Occupation_Other = Beschäftigung_Sonstiges)
df <- df %>% rename(Occupation_University = `Beschäftigung_Studierende/r`)
df <- df %>% rename(Occupation_None = `Beschäftigung_ohne Beschäftigung`)

# gen Occupation = 0 if Occupation_None == 1
df <- df %>% mutate(
  Occupation = case_when(
    Occupation_None == 1 ~ 0,
    Occupation_Household == 1 ~ 1,
    Occupation_Apprenticeship == 1 | Occupation_School == 1 | Occupation_University == 1 ~ 2,
    Occupation_Employed == 1 | Occupation_Selfemployed == 1 ~ 3,
    Occupation_Other == 1 ~ 4,
    TRUE ~ NA_real_
  )
)

# label define Occupation 0 "None" 1 "Household" 2 "School/Apprenticeship/University" 3 "Employed/Selfemployed" 4 "Other"
attr(df$Occupation, "label") <- "0=None, 1=Household, 2=School/Appr/Uni, 3=Employed/Self, 4=Other"

# foreach var of varlist NoInstrument  {
#   replace `var' = "1" if `var' == "quoted"
#   replace `var' = "0" if `var' == "not quoted"
#   destring `var', replace
# }
df <- df %>% mutate(
  NoInstrument = case_when(
    NoInstrument == "quoted" ~ 1,
    NoInstrument == "not quoted" ~ 0,
    TRUE ~ as.numeric(NoInstrument)
  )
)

# foreach var of varlist ImChoraktiv PrivaterMusikunterricht Children {
#   replace `var' = "1" if `var' == "Ja" | `var' == "ja"
#   replace `var' = "0" if `var' == "Nein" | `var' == "nein"
#   destring `var', replace
# }
df <- df %>% mutate(
  across(c(ImChoraktiv, PrivaterMusikunterricht, Children),
         ~ case_when(
           . %in% c("Ja", "ja") ~ 1,
           . %in% c("Nein", "nein") ~ 0,
           TRUE ~ as.numeric(.)
         ))
)

# recode No_Mainstream_7Likert (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
df <- df %>% mutate(
  MainstreamMusic = case_when(
    No_Mainstream_7Likert == 1 ~ 7,
    No_Mainstream_7Likert == 2 ~ 6,
    No_Mainstream_7Likert == 3 ~ 5,
    No_Mainstream_7Likert == 4 ~ 4,
    No_Mainstream_7Likert == 5 ~ 3,
    No_Mainstream_7Likert == 6 ~ 2,
    No_Mainstream_7Likert == 7 ~ 1,
    TRUE ~ NA_real_
  )
)

# rename No_Mainstream_7Likert MainstreamMusic
# (already done above)

# replace MainstreamMusic = 0 if MainstreamMusic ==.
df <- df %>% mutate(MainstreamMusic = replace(MainstreamMusic, is.na(MainstreamMusic), 0))

# recode NoInstrument (1=0) (0=1) 
df <- df %>% mutate(Playinstrument = 1 - NoInstrument)

# rename NoInstrument Playinstrument
# (done above)

# gen MusicEducation = 0 if Playinstrument & PrivaterMusikunterricht == 0
df <- df %>%
  mutate(
    MusicEducation = case_when(
      Playinstrument == 1 & PrivaterMusikunterricht == 1 ~ 2,  # both 1
      Playinstrument == 1 | PrivaterMusikunterricht == 1 ~ 1,  # at least one 1
      Playinstrument == 0 & PrivaterMusikunterricht == 0 ~ 0,  # both 0
      TRUE ~ 0  # match: replace MusicEducation = 0 if missing
    )
  )

# replace MusicEducation = 0 if MusicEducation ==.
df <- df %>% mutate(MusicEducation = replace(MusicEducation, is.na(MusicEducation), 0))

# alpha Playinstrument PrivaterMusikunterricht
library(psych)
alpha_result <- alpha(df[, c("Playinstrument", "PrivaterMusikunterricht")], check.keys = TRUE)
print(alpha_result)

# gen Income = 250 if IncomeRaw == "weniger als 500 Euro" 
df <- df %>% mutate(
  Income = case_when(
    IncomeRaw == "weniger als 500 Euro" ~ 250,
    IncomeRaw == "501 Euro bis 1.000 Euro" ~ 750,
    IncomeRaw == "1.001 Euro bis 1.500 Euro" ~ 1250,
    IncomeRaw == "1.501 Euro bis 2.000 Euro" ~ 1750,
    IncomeRaw == "2.001 Euro bis 2.500 Euro" ~ 2250,
    IncomeRaw == "2.501 Euro bis 3.000 Euro" ~ 2750,
    IncomeRaw == "3.001 Euro bis 3.500 Euro" ~ 3250,
    IncomeRaw == "3.501 Euro bis 4.000 Euro" ~ 3750,
    IncomeRaw == "4.001 Euro bis 4.500 Euro" ~ 4250,
    IncomeRaw == "4.501 Euro bis 5.000 Euro" ~ 4750,
    IncomeRaw == "mehr als 5.000 Euro" ~ 5250,
    TRUE ~ NA_real_
  )
)

# egen Income_Mean = mean(Income), by(Pseudonym)
df <- df %>%
  group_by(Pseudonym) %>%
  mutate(Income_Mean = mean(Income, na.rm = TRUE)) %>%
  ungroup()

# gen IncomeImputation = cond(missing(Income), Income_Mean, Income)
df <- df %>% mutate(
  IncomeImputation = if_else(is.na(Income), Income_Mean, Income)
)

# tab IncomeImputation
table(df$IncomeImputation, useNA = "always")

# tab Income
table(df$Income, useNA = "always")

# gen ln_Income = ln(IncomeImputation) 
df <- df %>% mutate(ln_Income = log(IncomeImputation))

# egen MusicAppreciation_Mean = mean(MusicAppreciation), by(Pseudonym)
df <- df %>%
  group_by(Pseudonym) %>%
  mutate(MusicAppreciation_Mean = mean(MusicAppreciation, na.rm = TRUE)) %>%
  ungroup()

# gen MusicAppreciationImputation = cond(missing(MusicAppreciation), MusicAppreciation_Mean, MusicAppreciation)
df <- df %>% mutate(
  MusicAppreciationImputation = if_else(is.na(MusicAppreciation), MusicAppreciation_Mean, MusicAppreciation)
)

# gen Outlier_Missing = 0
df <- df %>% mutate(Outlier_Missing = 0)

# replace Outlier_Missing = 1 if IncomeImputation ==. 
df <- df %>% mutate(Outlier_Missing = if_else(is.na(IncomeImputation), 1, Outlier_Missing))

# replace Outlier_Missing = 1 if MusicAppreciationImputation ==.
df <- df %>% mutate(Outlier_Missing = if_else(is.na(MusicAppreciationImputation), 1, Outlier_Missing))

# gen Outlier_Missing_Panel = 0
df <- df %>% mutate(Outlier_Missing_Panel = 0)

# bysort Pseudonym(Outlier_Missing): replace Outlier_Missing_Panel = 1 if Outlier_Missing[_N]==1
df <- df %>%
  group_by(Pseudonym) %>%
  mutate(Outlier_Missing_Panel = if_else(max(Outlier_Missing) == 1, 1, 0)) %>%
  ungroup()

# xtset Pseudonym panel
# (No direct equivalent in R; panel structure is preserved via group_by)

# bysort Pseudonym: gen Number_Waves=[_N]
df <- df %>%
  group_by(Pseudonym) %>%
  mutate(Number_Waves = n()) %>%
  ungroup()

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================
analysis_data <- df %>%
  filter(Number_Waves == 5)

analysis_data_grok <- analysis_data
# Filter data for analysis
# analysis_data <- raw_data %>%
#   filter(Outlier_Missing_Panel == 0 & Outlier_Consumption_Panel == 0 & Number_Waves == 5)

cat("Analysis data prepared.\n")
cat("Total observations:", nrow(analysis_data), "\n")
cat("Unique individuals:", length(unique(analysis_data$Pseudonym)), "\n")

# Summary statistics
summary_vars <- c("Consumption_Total", "ConsumptionLive", "Consumption_Rec", 
                  "ConsumptionPremiumStreaming", "ConsumptionFreeStreaming", 
                  "ConsumptionDigital", "ConsumptionPhysical", "ConsumptionRadio", 
                  "ConsumptionOnlineRadio", "Spending_Total", "Spending_Live", 
                  "Spending_Rec", "Spending_Streaming", "Spending_Physical", 
                  "Spending_Digital", "MusicEducation", "MusicAppreciation", 
                  "MaritalStat_LivAlone", "MaritalStat_LivAlone_Partner", "MaritalStat_LivTogether_Partner",
                  "PurchaseReason_Atmosphere", "PurchaseReason_Flexibility", "PurchaseReason_Habit", 
                  "PurchaseReason_Mobility", "PurchaseReason_Other", "PurchaseReason_SoundQuality", "PurchaseReason_None", 
                  "Education_HighSchool", "Education_Bachelor", "Education_TechnicalDiploma", "Education_Elementary", 
                  "Education_Master", "Education_MiddleSchool", "Education_PhD", "Education_NoneDegree", 
                  "Occupation_Employed", "Occupation_Apprenticeship", "Occupation_Household", "Occupation_School", 
                  "Occupation_Selfemployed", "Occupation_Other", "Occupation_University", "Occupation_None",
                  "ActiveListening", "MainstreamMusic", "Age", "Children", "Income")

# Summary by panel
summary_by_panel_grok <- analysis_data %>%
  group_by(panel) %>%
  summarise(across(all_of(summary_vars), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        sd = ~sd(., na.rm = TRUE),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

summary_by_panel_grok_consumption <- analysis_data %>%
  filter(Outlier_Consumption_Panel == 0) %>%
  group_by(panel) %>%
  summarise(across(all_of(summary_vars), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        sd = ~sd(., na.rm = TRUE),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Export summary statistics
write.csv(summary_by_panel_grok, "/Users/choochoojc/Desktop/STAT204/Homeworks/Final Project/summarystatistics_grok.csv", row.names = FALSE)
write.csv(summary_by_panel_grok_consumption, "/Users/choochoojc/Desktop/STAT204/Homeworks/Final Project/summarystatistics_grok_consumption.csv", row.names = FALSE)
write.csv(analysis_data_grok, "/Users/choochoojc/Desktop/STAT204/Homeworks/Final Project/analysis_data_grok.csv", row.names = FALSE)

