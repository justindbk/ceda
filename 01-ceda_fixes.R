## This script fixes errors in the CEDA dataset as downloaded from the yearly 
##    datasets available on the CSU Sacramento/CA SOS website:
##    https://csu-csus.esploro.exlibrisgroup.com/esploro/outputs/dataset/California-Elections-Data-Archive-CEDA/99257830890201671?institution=01CALS_USL

## Authors: Rachel Bernhard and Justin de Benedictis-Kessner
## To add further fixes or to comment on existing fixes, 
##    please contact Justin at jdbk@hks.harvard.edu

## Preamble --------------------------------------------------------------------
library(tidyverse)


## Read data -------------------------------------------------------------------

ceda <- read_csv("ceda_allcandidates_1995-2020.csv",guess_max = 200000)
# merge two raceid vars:
ceda <- ceda %>%
  mutate(race_id = coalesce(race_id,raceid))

ceda$date <- lubridate::ymd_hms(ceda$date)
ceda$date <- lubridate::date(ceda$date) # to take out hms from some places
summary(ceda$date) # from 1995 to 2020

## Fixes ----------------------------------------------------------------------
# Kamala Harris's 2003 SF DA race:
# ceda %>% filter(race_id=="200300039") %>% View()
ceda$votes[which(ceda$record_id=="200300151")] <- 105617
ceda$votes[which(ceda$record_id=="200300152")] <- 137111
ceda$elected[which(ceda$record_id=="200300151")] <- "2"
ceda$elected[which(ceda$record_id=="200300152")] <- "1"
ceda$rvotes[which(ceda$record_id=="200300151")] <- 2
ceda$rvotes[which(ceda$record_id=="200300152")] <- 1

# Greg Ficke, Aliso Viejo City Council 2012 (https://www.ocregister.com/2012/12/07/aliso-viejo-says-goodbye-to-garcia-and-ficke/)
ceda$elected[which(ceda$record_id==201201721)] <- 2 # he actually lost

# Marty Graves, Kings River Union School Board 2014 (https://tularecoelections.org/elections/registrar-of-voters/historical-information/prior-election-summary-reports/november-4-2014-general-election/)
ceda$elected[which(ceda$record_id==201405673)] <- 2 # he actually lost

# Clyde Fuller Jr, San Diego Unified 2002 missing district (correct in pdf report)
ceda$area[which(ceda$record_id==200205014)] <- "C" # one cand was previously missing a district

# San Clemente City Council 2012 multiple candidates wrong
# correct results per https://ocvote.gov/data/election-results-archives and https://www.ocregister.com/2012/11/08/san-clemente-council-election-results-stir-joy-worry/
# View(ceda %>% filter(race_id==201200589))
# Robert Baker in results twice -- going to consolidate his entries
# ceda[which(ceda$record_id==201201774),] # this is the one to remove
ceda$votes[which(ceda$record_id==201201775)] <- ceda$votes[which(ceda$record_id==201201775)] + ceda$votes[which(ceda$record_id==201201774)] # adding votes from both his lines
ceda$rvotes[which(ceda$record_id==201201775)] <- 1
# Chris Hamm actually won
ceda$rvotes[which(ceda$record_id==201201885)] <- 2
ceda$elected[which(ceda$record_id==201201885)] <- 1
# other candidates' order
ceda$rvotes[which(ceda$record_id==201201886)] <- 3
ceda$rvotes[which(ceda$record_id==201201887)] <- 4
ceda$rvotes[which(ceda$record_id==201201888)] <- 5
# all cands need vote total and cand number updated:
ceda$totvotes[which(ceda$race_id==201200589)] <- 46998
ceda$cand_number[which(ceda$race_id==201200589)] <- 5
# remove extra entry for Baker
ceda <- ceda %>% filter(is.na(record_id) | record_id!=201201774)

# Don DuBain, Solano County District Attorney 2014 (https://www.thereporter.com/2014/06/11/solano-county-da-race-all-but-settled/)
# View(ceda %>% filter(race_id==20140541))
ceda$elected[which(ceda$record_id==201401156)] <- 2 # incumbent DuBain actually lost to Abrams

# Turlock Joint Elementary in 1995: two separate races for separate districts via CA SOS report
# View(ceda %>% filter(co==50 & year==1995 & place=="Turlock Joint Elementary"))
ceda$area[which(ceda$co==50 & ceda$year==1995 & ceda$place=="Turlock Joint Elementary" & ceda$totvotes==13135)] <- "at-large A" # creating at-large district w/ fake name
ceda$area[which(ceda$co==50 & ceda$year==1995 & ceda$place=="Turlock Joint Elementary" & ceda$totvotes==11602)] <- "at-large B" # creating at-large district w/ fake name

# Las Virgenes Unified was actually a multi-county race across Ventura and LA Counties
# View(ceda %>% filter(place=="Las Virgenes Unified" & year==2013) %>% arrange(last))
ceda$multi_co[which(ceda$place=="Las Virgenes Unified" & ceda$year==1995)] <- 1
ceda$multi_co[which(ceda$place=="Las Virgenes Unified" & ceda$year==2013)] <- 1

# Escondido City Council 2014, 3 losers incorrectly marked winners
# View(ceda %>% filter(race_id==20141041) %>% arrange(votes))
ceda$elected[which(ceda$record_id==201402859)] <- 2 # actually lost
ceda$elected[which(ceda$record_id==201402858)] <- 2 # actually lost
ceda$elected[which(ceda$record_id==201402861)] <- 2 # actually lost

# Solano County Supervisor Dist 4 - 3 losers incorrectly marked winners, totvotes wrong (https://www.thereporter.com/2014/06/04/vasquez-cruises-on-to-victory/)
# View(ceda %>% filter(race_id==20140540))
ceda$elected[which(ceda$record_id==201401152)] <- 2 # actually lost
ceda$elected[which(ceda$record_id==201401153)] <- 2 # actually lost
ceda$elected[which(ceda$record_id==201401154)] <- 2 # actually lost
ceda$totvotes[which(ceda$race_id==20140540)] <- sum(ceda$votes[which(ceda$race_id==20140540)])

# Carpinteria USD 2014 Dist 1 - 1 loser incorrectly marked winner, totvotes wrong
# View(ceda %>% filter(race_id==20141699))
ceda$elected[which(ceda$record_id==201405249)] <- 2 # actually lost
ceda$totvotes[which(ceda$race_id==20141699)] <- 6965 # per http://sbcvote.com/Elect/Resources/Results11_2014/results-1.htm

# Compton USD in 1995 has non-top-vote getting marked winning
# View(ceda %>% filter(place=="Compton Unified" & year==1995) %>% arrange(last))
# Patillo actually won per next election she's incumbent (https://elections.cdn.sos.ca.gov/county-city-school-district-election-results/school_district_report_1999.pdf)
ceda$elected[which(ceda$place=="Compton Unified" & ceda$year==1995 & ceda$last=="Patillo")] <- 1
ceda$elected[which(ceda$place=="Compton Unified" & ceda$year==1995 & ceda$last=="Mays")] <- 2 # lost

# Antelope Valley Community College in 1995 was actually a multi-county race across Kern and LA Counties
# View(ceda %>% filter(place=="Antelope Valley Community College" & year==1995) %>% arrange(last))
ceda$multi_co[which(ceda$place=="Antelope Valley Community College" & ceda$year==1995)] <- 1

# Winters Joint Unified in 1995: actually multi-county in Yolo and Solano Counties
ceda$multi_co[which(ceda$place=="Winters Joint Unified" & ceda$year==1995)] <- 1

# Klamath-Trinity Joint Unified in 1995: actually multi-county in Humboldt and Trinity Counties
ceda$multi_co[which(ceda$place=="Klamath-Trinity Joint Unified" & ceda$year==1995)] <- 1

# Kings River Union Elementary in 2014 has loser cand winning (https://tularecoelections.org/elections/registrar-of-voters/historical-information/prior-election-summary-reports/november-4-2014-general-election/)
# View(ceda %>% filter(race_id==20141816))
ceda$elected[which(ceda$record_id==201405673)] <- 2 # actually lost

# Snowline Joint Unified in 1995 actually multi-county in LA and San Bernardino Counties
# View(ceda %>% filter(place=="Snowline Joint Unified" & year==1995) %>% arrange(last))
ceda$multi_co[which(ceda$place=="Snowline Joint Unified" & ceda$year==1995)] <- 1

# Coalinga-Huron Unified in 1995 actually multi-county in Fresno and Monterey Counties
# View(ceda %>% filter(place=="Coalinga-Huron Unified" & year==1995) %>% arrange(last))
ceda$multi_co[which(ceda$place=="Coalinga-Huron Unified" & ceda$year==1995)] <- 1

# Monson-Sultana Joint Union in 1995 actually multi-county in Fresno and Tulare Counties
# View(ceda %>% filter(place=="Monson-Sultana Joint Union" & year==1995) %>% arrange(last))
ceda$multi_co[which(ceda$place=="Monson-Sultana Joint Union" & ceda$year==1995)] <- 1

# San Bernardino Community College, 1995 actually multi-county between San Bernardino and Riverside Counties
# View(ceda %>% filter(place=="San Bernardino Community College" & year==1995) %>% arrange(last))
ceda$multi_co[which(ceda$place=="San Bernardino Community College" & ceda$year==1995)] <- 1

# El Camino Community College in 2018 missing district for one candidate
# View(ceda %>% filter(race_id==1644)) # interestingly, this race_id also a diff race in 2020 - race_id not a unique value across years!
ceda$area[which(ceda$record_id==201804679)] <- 2

# Los Angeles City Council Dist 12 in 2011 missing winner (Mitchell Englander had wrong race_id but correct area var)
# View(ceda %>% filter(race_id==20110291)) 
ceda$race_id[which(ceda$record_id==201100305)] <- 20110291

# San Diego Fallbrook CPA in 2018 - different spelling of office for this election across cands in same race
# View(ceda %>% filter(race_id==405 & year==2018)) 
ceda$office[which(ceda$race_id==405 & ceda$co==37 & ceda$year==2018)] <- "MEMBER, Fallbrook CPA"

# Victor Valley Community College in 1995: actually multi-county across LA and San Bernardino Counties
# View(ceda %>% filter(place=="Victor Valley Community College" & year==1995) %>% arrange(last))
ceda$multi_co[which(ceda$place=="Victor Valley Community College" & ceda$year==1995)] <- 1

# San Diego Community College Dist A in 2002 - two cands missing district
# View(ceda %>% filter(race_id==200201593)) 
ceda$area[which(ceda$record_id==200205009)] <- "A"
ceda$area[which(ceda$record_id==200205012)] <- "A"

# Yucaipa-Calimesa Joint Unified in 1995: actually multi-county across Riverside and San Bernardino Counties
# View(ceda %>% filter(place=="Yucaipa-Calimesa Joint Unified" & year==1995) %>% arrange(last))
ceda$multi_co[which(ceda$place=="Yucaipa-Calimesa Joint Unified" & ceda$year==1995)] <- 1

# King City Joint Union High in 1995: actually multi-county across Monterey and San Benito Counties
# View(ceda %>% filter(place=="King City Joint Union High" & year==1995) %>% arrange(last))
ceda$multi_co[which(ceda$place=="King City Joint Union High" & ceda$year==1995)] <- 1

# EARLIMART ELEMENTARY-R in 1997 - all three candidates got elected (per later 1998 and 2004 results showing they're incumbents), vote_number and elected wrong for two
# View(ceda %>% filter(place=="EARLIMART ELEMENTARY-R" & year==1997) %>% arrange(area,votes))
ceda$elected[which(ceda$record_id==199702332)] <- 1 # actually won
ceda$elected[which(ceda$record_id==199702333)] <- 1 # actually won
ceda$vote_number[which(ceda$record_id==199702332)] <- 1 # one winner
ceda$vote_number[which(ceda$record_id==199702333)] <- 1 # one winner

# INGLEWOOD City Council in 1995: some candidates in April elec missing district, term; and runoff for District 4 not coded as such
# View(ceda %>% filter(place=="INGLEWOOD" & year==1995 & office=="CITY COUNCIL") %>% arrange(area,votes))
ceda$elected[which(ceda$place=="INGLEWOOD" & ceda$date=="1995-04-04" & ceda$office=="CITY COUNCIL" & ceda$last=="Hardeman")] <- 3
ceda$elected[which(ceda$place=="INGLEWOOD" & ceda$date=="1995-04-04" & ceda$office=="CITY COUNCIL" & ceda$last=="Thomas" & ceda$first=="Ervin \"Tony\"")] <- 3
ceda$area[which(ceda$place=="INGLEWOOD" & ceda$date=="1995-04-04" & ceda$office=="CITY COUNCIL" & ceda$last=="Thomas")] <- 4
ceda$area[which(ceda$place=="INGLEWOOD" & ceda$date=="1995-04-04" & ceda$office=="CITY COUNCIL" & ceda$last=="Benson")] <- 4
ceda$area[which(ceda$place=="INGLEWOOD" & ceda$date=="1995-04-04" & ceda$office=="CITY COUNCIL" & ceda$last=="Stevens")] <- 3
ceda$area[which(ceda$place=="INGLEWOOD" & ceda$date=="1995-04-04" & ceda$office=="CITY COUNCIL" & ceda$last=="Bueno-Flores")] <- 3
ceda$term[which(ceda$place=="INGLEWOOD" & ceda$date=="1995-04-04" & ceda$office=="CITY COUNCIL")] <- "Full"

# Lincoln City Treasurer in 1996 - only one candidate, who apparently wasn't elected?
#   no data online - called Placer County elections office on 4/29/22
#   Placer County Elections office (Sarah) said no candidates filed, but there were write-in candidates (Spencer Short)
#   Elections code says that in nonpartisan offices, no candidate will be elected unless they get 50% of vote+1, so no cand won
#   If no one has been nominated, city can just appoint any elector.
# City Clerk's office in Lincoln (Dia) did some digging around.
#   Resolution says that Spencer Short WAS elected per the next meeting minutes
# View(ceda %>% filter(race_id==199600579))
ceda$elected[which(ceda$record_id==199601800)] <- 1

# Plumas County, Director Grizzly Rank CSD, 2016 - all actually got elected per August minutes (https://static1.squarespace.com/static/57fe6fc620099e7176946fb9/t/57ffbcc7b8a79bec49c336f5/1476377800119/Minutes+Board+Meeting+Grizzly+Ranch+August+1++2016.pdf)
# View(ceda %>% filter(place=="Plumas" & office=="DIRECTOR, Grizzly Ranch CSD" & year==2016)) 
ceda$elected[which(ceda$place=="Plumas" & ceda$office=="DIRECTOR, Grizzly Ranch CSD" & ceda$year==2016)] <- 1

# Monterey Superior Court Judge 2008: incorrect candidates listed as going to runoff
# View(ceda2 %>% filter(race_id==200800156))
ceda$elected[which(ceda$record_id==200800404)] <- 3 # actually went to runoff
ceda$elected[which(ceda$record_id==200800401)] <- 3 # actually went to runoff
ceda$elected[which(ceda$record_id==200800402)] <- 2 # actually lost

# AMERICAN CANYON City Council 2006 - runoff incorrectly listed
ceda$elected[which(ceda$record_id==200602092)] <- 2 # actually lost

# BURBANK UNIFIED 2003, Burbank City Council 1999, 2001, 2005, 2013, 2017 all have races that go to a runoff but not for top candidate!

# Humboldt County Supervisor Dist 2 in 2008: no runoff, 2nd and 3rd cands actually lost
ceda$elected[which(ceda$record_id==200800134)] <- 2
ceda$elected[which(ceda$record_id==200800135)] <- 2

## 30 fixes from UC Davis RAs:
# South Lake Tahoe City Council in 2014: vote totals wrong for several candidates (https://edcgov.us/elections/election/099/099.pdf), therefore elected ind wrong for two
ceda$votes[which(ceda$record_id==201401600)] <- 1218
ceda$votes[which(ceda$record_id==201401599)] <- 1383
ceda$votes[which(ceda$record_id==201401601)] <- 1126
ceda$elected[which(ceda$record_id==201401599)] <- 1
ceda$elected[which(ceda$record_id==201401597)] <- 2

# El Dorado County Supervisor dist 4: vote total wrong for Howard Penn (https://edcgov.us/elections/election/099/099.pdf)
ceda$votes[which(ceda$record_id==201400197)] <- 6230

# Alameda County Superior Judge 2016 has winning and losing candidate vote totals/rank switched (https://www.acvote.org/acvote-assets/pdf/elections/2016/11082016/results/november-8-2016-summary-results-report.pdf)
# View(ceda %>% filter(race_id==7 & year==2016))
ceda$votes[which(ceda$record_id==201600011)] <- 256107
ceda$votes[which(ceda$record_id==201600012)] <- 283396
ceda$elected[which(ceda$record_id==201600011)] <- 2
ceda$elected[which(ceda$record_id==201600012)] <- 1


# Merced County Supervisor D1 in 2016: vote total wrong (https://www.yourcentralvalley.com/news/merced-county-election-results-for-supervisor-races/)
ceda$votes[which(ceda$record_id==201600334)] <- 1253

# Fresno County Supervisor D1 in 2014: 2nd cand votes wrong (https://www.co.fresno.ca.us/departments/county-clerk-registrar-of-voters/election-information/election-results/results-of-november-4-2014-statewide-general-election)
ceda$votes[which(ceda$record_id==201400237)] <- 9866

# Parlier Mayor in 2014: 2nd cand votes wrong (https://www.co.fresno.ca.us/departments/county-clerk-registrar-of-voters/election-information/election-results/results-of-november-4-2014-statewide-general-election)
ceda$votes[which(ceda$record_id==201401684)] <- 462

# El Dorado Union High Scholl Board in 2014: 4th cand votes wrong (https://edcgov.us/elections/election/099/099.pdf)
ceda$votes[which(ceda$record_id==201403821)] <- 14927

# Golden Plains Unified in 2014: 2nd place cand vote total wrong (https://www.co.fresno.ca.us/departments/county-clerk-registrar-of-voters/election-information/election-results/results-of-november-4-2014-statewide-general-election)
ceda$votes[which(ceda$record_id==201403871)] <- 272

# Kings Canyon Joint Unified in 2014: winner votes wrong (https://www.co.fresno.ca.us/departments/county-clerk-registrar-of-voters/election-information/election-results/results-of-november-4-2014-statewide-general-election)
ceda$votes[which(ceda$record_id==201403878)] <- 538

# Humboldt County Supervisor in 1998: actually, no error here - SmartVoter has wrong votes b/c Kirk won and was inc in 2002 (https://www.northcoastjournal.com/103102/cover1031.html)
# View(ceda %>% filter(race_id==199800122))

# Humboldt County Supervisor D1 in 2000: vote totals wrong (https://humboldtgov.org/DocumentCenter/View/59722/Final-Canvass-of-Votes---Consolidated-Presidential-Election-November-7-2000?bidId=)
# View(ceda %>% filter(race_id==200000058))
ceda$votes[which(ceda$record_id==200000162)] <- 5085
ceda$votes[which(ceda$record_id==200000163)] <- 6040

# SF County Supervisor D5 in 2000: winner votes wrong (https://sfelections.sfgov.org/november-7-2000-consolidated-presidential-general-election-0)
# View(ceda %>% filter(race_id==200000210))
ceda$votes[which(ceda$record_id==200000587)] <- 12743

# SF County Supervisor D8 in 2000: 2nd cand votes wrong (https://sfelections.sfgov.org/november-7-2000-consolidated-presidential-general-election-0)
# View(ceda %>% filter(race_id==200000213))
ceda$votes[which(ceda$record_id==200000619)] <- 11531

# SF County Supervisor D11 in 2000: winner votes wrong (https://sfelections.sfgov.org/november-7-2000-consolidated-presidential-general-election-0)
# View(ceda %>% filter(race_id==200000216))
ceda$votes[which(ceda$record_id==200000647)] <- 6290

# Round Valley Unified 2001: 2rd cand votes wrong
# View(ceda %>% filter(race_id==200100366))
ceda$votes[which(ceda$record_id==200101653)] <- 167

# Arcata City Council : not sure smartvoter is right (http://www.smartvoter.org/2004/11/02/ca/hm/race/01/)
# View(ceda %>% filter(race_id==200400401))

# Consumnes River CSD in 2005: not sure if SmartVoter correct (http://www.smartvoter.org/2005/11/08/ca/ed/ballot.html)
# View(ceda %>% filter(race_id==200500003))

# McKinleyville CSD in 2005: actually all correct (https://humboldtgov.org/DocumentCenter/View/4006/November-8-2005-Special-Statewide-Consolidated-Election---Final-Canvass-Summary-Report-PDF?bidId=)
# View(ceda %>% filter(race_id==200500009))

# Calexico Unified School Dist in 2006: not clear if SOS results or county results wrong (https://elections.imperialcounty.org/regvoters/ElectionResults/OtherDoctos/AElectionNovember72006.pdf)
# View(ceda %>% filter(race_id==200601286))

# Monterey Superior Judge #2 in 2008: this is actually correct! (https://www.montereycountyelections.us/files/mced/Election_Info/past_results/SOV_2008-11-04.pdf)
# View(ceda %>% filter(race_id==200800157))

# SF County Supervisor D8 in 2010: unclear how to code due to RCV, but winner and just-loser stay same (https://sfelections.org/results/20101102/data/d8.html)
# View(ceda %>% filter(race_id==201000427))
ceda$votes[which(ceda$record_id==201000917)] <- 18239
ceda$votes[which(ceda$record_id==201000915)] <- 14687

# Jurupa Valley City Council in 2012: many votes wrong (https://www.voteinfo.net/eresults/69/Election%20Result.htm)
# View(ceda %>% filter(race_id==201200641))
ceda$votes[which(ceda$record_id==201202088)] <- 5644
ceda$votes[which(ceda$record_id==201202089)] <- 5335
ceda$votes[which(ceda$record_id==201202090)] <- 3193
ceda$votes[which(ceda$record_id==201202027)] <- 7083
ceda$votes[which(ceda$record_id==201202028)] <- 5791

# Perris City Council 2012: all votes wrong (https://www.voteinfo.net/eresults/69/Election%20Result.htm)
# View(ceda %>% filter(race_id==201200646))
ceda$votes[which(ceda$record_id==201202039)] <- 5642
ceda$votes[which(ceda$record_id==201202106)] <- 3926
ceda$votes[which(ceda$record_id==201202040)] <- 4612
ceda$votes[which(ceda$record_id==201202107)] <- 3659

# Wildomar City Council in 2012: all votes wrong (https://www.voteinfo.net/eresults/69/Election%20Result.htm)
# View(ceda %>% filter(race_id==201200649))
ceda$votes[which(ceda$record_id==201202117)] <- 3808
ceda$votes[which(ceda$record_id==201202118)] <- 1334
ceda$votes[which(ceda$record_id==201202046)] <- 4874
ceda$votes[which(ceda$record_id==201202047)] <- 4039

# Coachella City Treasurer in 2012: all votes wrong (https://www.voteinfo.net/eresults/69/Election%20Result.htm)
# View(ceda %>% filter(race_id==201200656))
ceda$votes[which(ceda$record_id==201202136)] <- 2780
ceda$votes[which(ceda$record_id==201202142)] <- 2356
ceda$votes[which(ceda$record_id==201202143)] <- 284

# Cathedral City Mayor in 2012: votes all wrong (https://www.voteinfo.net/eresults/69/Election%20Result.htm), and winner wrong b/c 13-vote gap (https://kesq.com/news/2012/12/12/cathedral-city-mayor-kathy-derosa-sworn-in/)
# View(ceda %>% filter(race_id==201200663))
ceda$votes[which(ceda$record_id==201202146)] <- 5716
ceda$votes[which(ceda$record_id==201202153)] <- 858
ceda$votes[which(ceda$record_id==201202152)] <- 5729
ceda$elected[which(ceda$record_id==201202152)] <- 1
ceda$elected[which(ceda$record_id==201202146)] <- 2

# Riverside Community College Board D1 in 2012: votes all wrong (https://www.voteinfo.net/eresults/69/Election%20Result.htm)
# View(ceda %>% filter(race_id==201201261))
ceda$votes[which(ceda$record_id==201204156)] <- 22955
ceda$votes[which(ceda$record_id==201204201)] <- 15876

## SF Board of Supervisors: vote_number wrong for all and elected inds wrong for 2nd and 3rd cands in 2008
# View(ceda %>% filter(race_id==200800216) %>% arrange(desc(votes)))
ceda$vote_number[which(ceda$place=="SAN FRANCISCO" & ceda$year==2008 & ceda$office=="COUNTY SUPERVISOR")] <- 1
ceda$elected[which(ceda$record_id==200800552)] <- 2
ceda$elected[which(ceda$record_id==200800554)] <- 2
ceda$elected[which(ceda$record_id==200800587)] <- 2
ceda$elected[which(ceda$record_id==200800585)] <- 2
ceda$elected[which(ceda$record_id==200800555)] <- 2
ceda$elected[which(ceda$record_id==200800561)] <- 2
ceda$elected[which(ceda$record_id==200800565)] <- 2
ceda$elected[which(ceda$record_id==200800566)] <- 2
ceda$elected[which(ceda$record_id==200800569)] <- 2
ceda$elected[which(ceda$record_id==200800567)] <- 2
ceda$elected[which(ceda$record_id==200800571)] <- 2
ceda$elected[which(ceda$record_id==200800572)] <- 2
ceda$elected[which(ceda$record_id==200800577)] <- 2
ceda$elected[which(ceda$record_id==200800575)] <- 2



# Truckee Town Council in 2006: this is correct (https://www.townoftruckee.com/government/town-council/history-of-town-council-members)
# ceda$elected[which(ceda$record_id==200602136)]

# Twin Ridges Elem School District in 2006: this is correct (https://www.gpelections.org/races/deonne-noel-runs-for-school-board-2006/)
# ceda$elected[which(ceda$record_id==200604221)]

# Edgemont CSD in 2001: vote_number wrong for all, elected ind wrong for 2nd and 3rd cands (https://www.voteinfo.net/eresults/04/ElectionResult.htm)
# View(ceda %>% filter(race_id==200100022))
ceda$vote_number[which(ceda$race_id==200100022)] <- 3
ceda$elected[which(ceda$record_id==200100082)] <- 1
ceda$elected[which(ceda$record_id==200100084)] <- 1


# "incumbent" indicator incorrect in many easily detectable places:
# View(ceda_w %>% filter(incumb=="N" & (str_detect(baldesig,"incumbent") | str_detect(baldesig,"Incumbent") | str_detect(baldesig,"Current") | str_detect(baldesig,"current"))) %>% arrange(date)) # 642 of these listed wrong
ceda_w$incumb[which(ceda_w$baldesig=="Incumbent")] <- "Y"
ceda_w$incumb[which(ceda_w$baldesig=="Appointed incumbent")] <- "Y"
ceda_w$incumb[which(ceda_w$baldesig=="Appointed Incumbent")] <- "Y"
# View(ceda_w[which(str_detect(ceda_w$baldesig,"Current")),]) # could probably change these too, but not 100% clear whether always true


## Output data ----------------------
# write_csv(ceda,"ceda_allcandidates_1995-2020_fixed.csv")
write_rds(ceda,"ceda_allcandidates_1995-2020_fixed.rds",compress = "gz")
