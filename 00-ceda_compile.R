## Compiling CA election Data Archive (CEDA) data from all years into one file
## Justin de Benedictis-Kessner

## ------------ ##
#### Preamble ####
## ------------ ##
library(tidyverse)
library(readr)
library(janitor)
library(glue)
library(readxl)

## -------------- ##
#### Read files ####
## -------------- ##

# ## version using compiled file from Valory Messier @ CSU Sacramento (1995-2015)
# files <- list.files(".",pattern="CEDA\\d{4}Data")
# years <- str_extract(files,"\\d{4}") %>% as.numeric()
# files <- files[years > 2015] # only newer years
# 
# ceda_all <- NULL
# 
# ## read compiled file 1995-2015:
# ceda_all <- read_excel("All Years Candidates 1995-2015.xlsx",guess_max = 30000) %>% # have to set it for this to avoid creating NAs in numeric cols
#   clean_names()
# 
# ## Fixing errors:
# ceda_all <- filter(ceda_all, YEAR != "YEAR") # remove errant header row
# 
# # CSD/PLACE columns switched in 2011, fixing:
# table(ceda_all$YEAR[which(ceda_all$CSD !="0" & ceda_all$CSD !="1")]) 
# ceda_all <- ceda_all %>%
#   mutate(CSD_2 = ifelse(YEAR==2011,PLACE,CSD),
#          PLACE_2 = ifelse(YEAR==2011,CSD,PLACE),
#          CSD = CSD_2,
#          PLACE = PLACE_2
#   ) 
# table(ceda_all$CSD) # all 0s and 1s now
# 
# ceda_all <- ceda_all %>%
#   mutate(RECODE_OFFNAME = toupper(RECODE_OFFNAME))
# 
# ## some office names need better grouping:
# ceda_all$RECODE_OFFNAME[ceda_all$RECODE_OFFNAME == "DIRECTOR, CSD"] <- "CSD/CSA DIRECTOR"
# ceda_all$RECODE_OFFNAME[ceda_all$RECODE_OFFNAME == "OTHER CITY"] <- "OTHER CITY OFFICE"
# ceda_all$RECODE_OFFNAME[ceda_all$RECODE_OFFNAME == "OTHER CITY OFFICES"] <- "OTHER CITY OFFICE"
# ceda_all$RECODE_OFFNAME[ceda_all$RECODE_OFFNAME == "OTHER COUNTY"] <- "OTHER COUNTY OFFICE"
# ceda_all$RECODE_OFFNAME[ceda_all$RECODE_OFFNAME == "OTHER COUNTY OFFICES"] <- "OTHER COUNTY OFFICE"
# ceda_all$RECODE_OFFNAME[ceda_all$RECODE_OFFNAME == "SCHOOL BOARD MEMBER"] <- "SCHOOL BOARD"
# table(ceda_all$RECODE_OFFNAME) # better now
# 
# # fixing elected, whcih should only be 1/2/3
# table(ceda_all$elected)
# table(ceda_all$YEAR[which(ceda_all$elected !="1" & ceda_all$elected !="2" & ceda_all$elected !="3")]) # just 2011 and 2012
# View(ceda_all %>% filter(YEAR==2011)) # looks like elected, runoff, checkrunoff shifted left into multi_raceid column in 2011
# View(ceda_all %>% filter(YEAR==2012)) # looks like elected and RVOTES columns switched in 2012
# 
# ## This is a mess - better to just compile myself

## -------------------------------- ##
#### Compiling all years by hand: ####
## -------------------------------- ##

files <- list.files(".",pattern="CEDA\\d{4}Data") # files downloaded from SacState website: https://csus-dspace.calstate.edu/handle/10211.3/210187

ceda_all <- NULL
for(i in 1:length(files)){
  # figure out individual year's appropriate sheet name:
  sheet_names <- excel_sheets(files[i])
  sheet_to_extract <- str_which(sheet_names,regex(pattern = "candidate",ignore_case=T))
  # read individual year's file:
  temp.file <- read_excel(files[i],sheet = sheet_to_extract,na = "") %>%
    clean_names()

  if(sum(str_count(names(temp.file),"^area$"))>0){
    temp.file <-  temp.file %>%
      mutate(area = as.character(area))
  }
  if(sum(str_count(names(temp.file),"^elected$"))>0){
    temp.file <-  temp.file %>%
      mutate(elected = as.character(elected))
  }
  if(sum(str_count(names(temp.file),"^writein$"))>0){
    temp.file <-  temp.file %>%
      mutate(writein = as.numeric(writein))
  }
  if(sum(str_count(names(temp.file),"^totvotes$"))>0){
    temp.file <-  temp.file %>%
      mutate(totvotes = as.numeric(totvotes))
  }
  if(sum(str_count(names(temp.file),"^totalwritein_votes$"))>0){
    temp.file <-  temp.file %>%
      mutate(totalwritein_votes = as.numeric(totalwritein_votes))
  }
  if(sum(str_count(names(temp.file),"^year$"))==0){ # add year if not there (only 1995)
    temp.file <- temp.file %>%
      mutate(year = lubridate::year(date))
  }
  
  # check for date/year errors (only 2020)
  # file_year <- gsub(".*?(\\d+).*","\\1",files[i])
  # wrong_years <- dim(temp.file %>% filter(year != file_year))[1]
  #   if(wrong_years>0){
  #     print(paste0("Stopping - ", wrong_years, " observations w/ incorrect years in ", file_year, " file."))
  #     break
  #   }
  
  if(grepl(2020,files[i])){ # fix 2020 issues
    temp.file$date[temp.file$raceid==44] <- "2020-11-03"
    temp.file$date[temp.file$raceid==1171] <- "2020-11-03"
    temp.file$date[temp.file$raceid==427] <- "2020-11-03"
    temp.file$date[temp.file$raceid==840] <- "2020-11-03"
    temp.file$year <- 2020
  }
  # append this year to all other years:
  ceda_all <- bind_rows(ceda_all,temp.file)
}

### Data cleaning:
skimr::skim(ceda_all) # to check data

# some columns differ in some years - combining them
ceda_all <- ceda_all %>%
  mutate(first = coalesce(firstname,first),
         last = coalesce(lastname,last),
         incumb = coalesce(incumb,inc),
         co = coalesce(co,co_number),
         # raceid = coalesce(raceid,race_id) # not clear if these are actually the same
  )

## recoding incumbency var:
ceda_all$incumb[ceda_all$incumb == "Yes"] <- "Y"
ceda_all$incumb[ceda_all$incumb == "No"] <- "N"

## recoding term variable:
ceda_all$term[ceda_all$term == "FUll"] <- "Full"


# year missing in all 1995 elecs:
# ceda_all %>% filter(is.na(year)) %>% View()
# summary(ceda_all$date[is.na(ceda_all$year)])
# ceda_all$year[is.na(ceda_all$year)] <- 1995

## add month variable:
ceda_all <- ceda_all %>%
  mutate(month = lubridate::month(date))
tabyl(ceda_all$month) # worked for all of them
ceda_all$date <- lubridate::ymd_hms(ceda_all$date)
ceda_all$date <- lubridate::date(ceda_all$date) # to take out hms from some places
summary(ceda_all$date) # from 1995 to 2021

# 1995 also missing recoded office name and office num:
ceda_all$recode_offname[ceda_all$year==1995] <- ceda_all$office[ceda_all$year==1995]


## some office names need better grouping:
ceda_all <- ceda_all %>%
  mutate(recode_offname = toupper(recode_offname))
tabyl(ceda_all,recode_offname)
ceda_all$recode_offname[ceda_all$recode_offname == "DIRECTOR, CSD"] <- "CSD/CSA DIRECTOR"
ceda_all$recode_offname[ceda_all$recode_offname == "DIRECTOR"] <- "CSD/CSA DIRECTOR"
ceda_all$recode_offname[ceda_all$recode_offname == "OTHER CITY"] <- "OTHER CITY OFFICE"
ceda_all$recode_offname[ceda_all$recode_offname == "OTHER CITY OFFICES"] <- "OTHER CITY OFFICE"
ceda_all$recode_offname[ceda_all$recode_offname == "OTHER COUNTY"] <- "OTHER COUNTY OFFICE"
ceda_all$recode_offname[ceda_all$recode_offname == "OTHER COUNTY OFFICES"] <- "OTHER COUNTY OFFICE"
ceda_all$recode_offname[ceda_all$recode_offname == "SCHOOL BOARD MEMBER"] <- "SCHOOL BOARD"
ceda_all$recode_offname[grep(x = ceda_all$office, pattern="MAYOR",ignore.case = T)] <- "MAYOR"
tabyl(ceda_all,recode_offname) # better now

ceda_all <- ceda_all %>%
  mutate(recode_office = case_when(
    recode_offname == "COUNTY SUPERVISOR" ~ 1 ,
    recode_offname == "CITY COUNCIL" ~ 2,
    recode_offname == "SCHOOL BOARD" ~ 3 ,
    recode_offname == "CSD/CSA DIRECTOR" ~ 4,
    recode_offname == "OTHER COUNTY OFFICE" ~ 5,
    recode_offname == "SUPERIOR JUDGE" ~ 5,
    recode_offname == "OTHER CITY OFFICE" ~ 6,
    recode_offname == "CITY ATTORNEY" ~ 6,
    recode_offname == "CITY CLERK" ~ 6,
    recode_offname == "CITY CLERK-ASSESSOR" ~ 6,
    recode_offname == "CITY TREASURER" ~ 6,
    recode_offname == "MAYOR" ~ 6,
    recode_offname == "OTHER SCHOOL DISTRICT OFFICE" ~ 7 ))
tabyl(ceda_all,recode_offname,recode_office) # better now


## areas missing for many city council races:
tabyl(filter(ceda_all, recode_offname=="CITY COUNCIL"), area) # missing in 25331 cases
table(ceda_all$office[ceda_all$recode_offname=="CITY COUNCIL" & is.na(ceda_all$area)])


# create new raceid vars for grouping
ceda_all <- ceda_all %>%
  mutate(new_raceid = paste(cntyname,place,office,area,term,date,sep="_"))

# take out old columns
# ceda_all <- ceda_all %>%
#   select(-firstname,-lastname,-inc,-co_number,-race_id)

# rearrange into order of contemporary codebook
ceda_all <- ceda_all %>%
  select(record_id,raceid,race_id,new_raceid,co,jur,cntyname,year,month,date,
         place,csd,office,recode_office,recode_offname,area,
         term,vote_number,last,first,baldesig,incumb,num_inc,cand_number,
         votes,writein,sumvotes,totvotes,percent,elected,rvotes,runoff,checkrunoff,
         multi_race_id,multi_cand_id,multi_co,
         indivtotal_votes,total_writein,multitotal_votes,newtotvotes,rindivto,newelected)
  

#### Output data ####
# write_csv(ceda_all,"ceda_allcandidates_1995-2020.csv")
write_rds(ceda_all,"ceda_allcandidates_1995-2024.rds",compress = "gz")
