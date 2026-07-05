# IMPORT EXTERNAL DATA AND MERGE WITH THE ORIGINAL DATASET
## Regional unemployment and unicount at 18
runemp_raw <- read_excel("External Data/regional.xlsx", sheet = "unemp") %>%
  pivot_longer(
    cols = -year,
    names_to = "region",
    values_to = "runemp_raw"
  )

runemp_devi <- read_excel("External Data/regional.xlsx", sheet = "unemp_devi") %>%
  pivot_longer(
    cols = -year,
    names_to = "region",
    values_to = "runemp_devi"
  )

rearn_raw <- read_excel("External Data/regional.xlsx", sheet = "earn") %>%
  pivot_longer(
    cols = -year,
    names_to = "region",
    values_to = "rearn_raw"
  )

rearn_devi <- read_excel("External Data/regional.xlsx", sheet = "earn_devi") %>%
  pivot_longer(
    cols = -year,
    names_to = "region",
    values_to = "rearn_devi"
  )

runemp_avg <- read_excel("External Data/regional.xlsx", sheet = "unemp_avg")
rearn_avg <- read_excel("External Data/regional.xlsx", sheet = "earn_avg")

rhome_bachfee <- read_excel("External Data/regional.xlsx", sheet = "home_bachfee") %>%
  pivot_longer(
    cols = -year,
    names_to = "region",
    values_to = "rhome_bachfee"
  )

runicount <- read_excel("External Data/regional.xlsx", sheet = "uniperpop") %>%
  pivot_longer(
    cols = -year,
    names_to = "region",
    values_to = "runicount"
  )

CPI <- read_excel("External Data/regional.xlsx", sheet = "CPI")

usoc_working <- usoc_clean %>%
  # Instrument: Raw regional unemployment at 16
  left_join(runemp_raw, by = c("year16" = "year", "region16" = "region")) %>%
  dplyr::rename(runemp16_raw = runemp_raw) %>%
  
  # Instrument: Raw regional unemployment at 18
  left_join(runemp_raw, by = c("year18" = "year", "region18" = "region")) %>%
  dplyr::rename(runemp18_raw = runemp_raw) %>%
  
  # Instrument: Raw regional unemployment at 22
  left_join(runemp_raw, by = c("year22" = "year", "region22" = "region")) %>%
  dplyr::rename(runemp22_raw = runemp_raw) %>%
  
  # Instrument: Raw regional median log earning at 16
  left_join(rearn_raw, by = c("year16" = "year", "region16" = "region")) %>%
  dplyr::rename(rearn16_raw = rearn_raw) %>%
  
  # Instrument: Raw regional median log earning at 18
  left_join(rearn_raw, by = c("year18" = "year", "region18" = "region")) %>%
  dplyr::rename(rearn18_raw = rearn_raw) %>%
  
  # Instrument: Raw regional median log earning at 22
  left_join(rearn_raw, by = c("year22" = "year", "region22" = "region")) %>%
  dplyr::rename(rearn22_raw = rearn_raw) %>%
  
  # Instrument: Deviation regional unemployment at 16
  left_join(runemp_devi, by = c("year16" = "year", "region16" = "region")) %>%
  dplyr::rename(runemp16_devi = runemp_devi) %>%
  
  # Instrument: Deviation regional unemployment at 18
  left_join(runemp_devi, by = c("year18" = "year", "region18" = "region")) %>%
  dplyr::rename(runemp18_devi = runemp_devi) %>%
  
  # Instrument: Deviation regional unemployment at 22
  left_join(runemp_devi, by = c("year22" = "year", "region22" = "region")) %>%
  dplyr::rename(runemp22_devi = runemp_devi) %>%
  
  # Instrument: Deviation regional median log earning at 16
  left_join(rearn_devi, by = c("year16" = "year", "region16" = "region")) %>%
  dplyr::rename(rearn16_devi = rearn_devi) %>%
  
  # Instrument: Deviation regional median log earning at 18
  left_join(rearn_devi, by = c("year18" = "year", "region18" = "region")) %>%
  dplyr::rename(rearn18_devi = rearn_devi) %>%
  
  # Instrument: Deviation regional median log earning at 22
  left_join(rearn_devi, by = c("year22" = "year", "region22" = "region")) %>%
  dplyr::rename(rearn22_devi = rearn_devi) %>%
  
  # Control: Average unemployment rate at region of origin (year 16)
  left_join(runemp_avg, by = c("region16" = "region")) %>%
  dplyr::rename(runemp16_avg = runemp_avg) %>%
  
  # Control: Average median earning at region of origin (year 16)
  left_join(rearn_avg, by = c("region16" = "region")) %>%
  dplyr::rename(rearn16_avg = rearn_avg) %>%
  
  # Control: Current regional unemployment rate
  left_join(runemp_raw, by = c("year", "region")) %>%
  dplyr::rename(runemp_current = runemp_raw) %>%
  
  # Control: Current regional median log earning
  left_join(rearn_raw, by = c("year", "region")) %>%
  dplyr::rename(rearn_current = rearn_raw) %>%
  
  # Instrument: Regional number of universities at 16
  left_join(runicount, by = c("year16" = "year", "region16" = "region")) %>%
  dplyr::rename(runicount16 = runicount) %>%
  
  # Instrument: Home Bachelor tuition fee at 18
  left_join(rhome_bachfee, by = c("year16" = "year", "region16" = "region")) %>%
  dplyr::rename(home_bachfee = rhome_bachfee) %>%
  
  # CPI at 18
  left_join(CPI, by = c("year18" = "year"))%>%
  dplyr::rename(CPI18 = CPI)  %>%
  
  # CPI at current time
  left_join(CPI, by = c("year"))

## Real minimum wage and CPI
mw_cpi <- read_excel("External Data/mw_cpi.xlsx", sheet = "real_converted")

get_real_mw <- function(year, age) {
  if (is.na(year) || is.na(age)) return(NA)
  if (!(year %in% mw_cpi$year)) return(NA)
  
  row <- mw_cpi[mw_cpi$year == year, ]
  
  col <- if (age < 18) {
    "Under 18"
  } else if (age >= 25) {
    "25 and over"
  } else {
    as.character(age)
  }
  
  val <- row[[col]]
  if (is.na(val)) return(NA)
  return(val)
}

# Apply to usoc_clean
usoc_working$realMW <- mapply(get_real_mw, usoc_working$year, usoc_working$age)
usoc_working$Kaitz <- mw_cpi$Kaitz[match(usoc_working$year, mw_cpi$year)]


# FINAL DATASET
usoc_working <- usoc_working %>%
  mutate(
    lwage = log(fimnlabgrs_dv/CPI*100),
    real_hbachfee = (home_bachfee/CPI18*100),
  ) 

usoc_working <- usoc_working %>%
  dplyr::select(pidp, year, age, birth_year, region, region16, region18, region22,
                year16, year18, year22, runicount16, runemp16_raw, runemp18_raw, 
                runemp22_raw, runemp16_devi, runemp18_devi, runemp22_devi, 
                runemp_current, rearn16_raw, rearn18_raw, rearn22_raw, 
                rearn16_devi, rearn18_devi, rearn22_devi, 
                rearn_current, runemp16_avg, rearn16_avg, lwage, 
                hiqual, GCSE, ALevel, VOC, Bachelor, HigherDeg, OtherDip, expyrs, 
                expyrs2, Kaitz, ability, real_hbachfee, home_bachfee, numSib, 
                PGLoan2016, race, sex, momeduc, isWorkingFT) %>%
  
  
  #England only because education policy instruments is different for these four
  #as well there's no data on higher education participation
  filter(!region %in% c("channel.islands")) %>%
  
  #Have to put the mutate here because left_join forces region to be character
  mutate(
    region = factor(region, levels = c(
      "london", "east", "east.midlands", "north.east", "north.west", 
      "south.east", "south.west", "west.midlands","yorkshire.and.the.humber",
      "scotland", "northern.ireland", "wales")),
    region16 = factor(region16, levels = c(
      "london", "east", "east.midlands", "north.east", "north.west", 
      "south.east", "south.west", "west.midlands","yorkshire.and.the.humber",
      "scotland", "northern.ireland", "wales"))
  )

usoc_working <- usoc_working %>%
  ungroup() %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  filter(min(age) <= 23, isWorkingFT == 1, lwage > 0) %>%
  ungroup()