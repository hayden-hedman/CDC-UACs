## MEGAZORD - FT. BLISS DATA SUMMARY REPORTING
###########################################################################################################################################################################################
## LOAD PACKAGES
pacman::p_load(dplyr,tidyr, tidyverse, zoo, scales, ggpubr, readxl,data.table,curl,XLConnect)
##########################################################################################################################################################################################
## GLOBAL VARIABLES USED THROUGHOUT
sys_date = format(Sys.Date(), "%m-%d-%Y")
## COVID DATA TODAY'S DATE - 1
covid_date = format(Sys.Date() -1, "%m-%d-%Y") ## USED FOR PRINTING (NO SLASHES)
## USED FOR MATCHING
covid_date2 = format(Sys.Date() -1,)
## UCM BED DATE
bed_census_date = format(Sys.Date()-1, "%m/%d/%Y")
## UCM CURRENT DATE
bed_census_date2 = format(Sys.Date(), "%m/%d/%Y")
## USED FOR MATCHING
## TOTAL FACILITY OCCUPANCY (TFC)
TFC = 4000
##########################################################################################################################################################################################
## (1) PPT DAILY SLIDE: SUMMARIZE UCM
## SET WORKING DIRECTORY
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_INPUT_DATA")
## UCM COVID TEST FILENAME
ucm_covid_test_name<- paste("BorderResults_FortBlissTX_UAC_",covid_date2,".xlsx", sep = "")
## LOAD UCM COVID TEST DATA
ucm <- read_excel(ucm_covid_test_name)
#ucm <- read_excel("BorderResults_FortBlissTX_UAC_20210925_000507_Andrea.xlsx")
## CONVERT TO DATA.FRAME
ucm <- data.frame(ucm)
## CLEAN UP VARIABLES
colnames(ucm)[1]<-"a_number"
## CREATE VARIABLES FOR COVID-19 TEST TYPE AND RESULT OUTCOME 
## BINAX POSITIVE
ucm$binaxnow_pos =0
ucm$binaxnow_pos[which(ucm$TestType=="BinaxNow" & ucm$Result=="Positive")]<-"1"
## BINAX NEGATIVE
ucm$binaxnow_neg =0
ucm$binaxnow_neg[which(ucm$TestType=="BinaxNow" & ucm$Result=="Negative")]<-"1"
## ACULA POSITIVE
ucm$acula_pos =0
ucm$acula_pos[which(ucm$TestType=="Accula" & ucm$Result=="Positive")]<-"1"
## ACULA NEGATIVE
ucm$acula_neg =0
ucm$acula_neg[which(ucm$TestType=="Accula" & ucm$Result=="Negative")]<-"1"



###############################################################################
## SET WORKING DIRECTORY OF TRIPWIRE DATABASE
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_INPUT_DATA")
## AUTOMATE TRIPWIRE DATASET FILE NAME
tripwire_name<- paste("tripwire_",covid_date2,".csv", sep = "")
## LOAD IN TRIPWIRE DATABASE
tripwire_curr <- read.csv(tripwire_name, header=T)
colnames(tripwire_curr)[3]<- "a_number"
## SUBSET TRIPWIRE OF DAYS PRIOR TO TODAY'S DATE
tripwire_cutoff = Sys.Date() -1 

## FORMAT TRIPWIRE DATE
tripwire_curr$date <- tripwire_curr$Enroll.Date
## CUT H:M:S FROM DATE 
tripwire_curr$date <- substr(tripwire_curr$date, 1, nchar(tripwire_curr$date)-9)
## CONVERT DATE TO DATE IN Y-m-d format
tripwire_curr$date <- as.Date(tripwire_curr$date, format="%m/%d/%Y")
## SUBSET TRIPWIRE DF BY ALL DATA BEFORE CUTOFF DATE
tripwire <- subset(tripwire_curr, date < tripwire_cutoff) 
##################################################################################
## CREATE FILTER VARIABLE FOR DUPLICATE A#'s
ucm$duplicate=0
ucm$duplicate[which(duplicated(ucm$a_number))]<-1
## FILTER OUT INTAKE BY UNIQUE UCM A#
intake_sub <- subset(ucm , !(a_number %in% tripwire$a_number) & duplicate=="0");dim(intake_sub)
## INTAKE VARIABLES OF IMPORTANCE TO PULL OUT
intake_sub$intake=1
intake_sub<-intake_sub[,c("a_number","intake")]
## FILTER OUT SERIAL BY UNIQUE UCM A#
serial_sub <- subset(ucm, (a_number %in% tripwire$a_number) & duplicate=="0");dim(serial_sub)
## SERIAL VARIABLES OF IMPORTANCE TO PULL OUT
serial_sub$serial=1
serial_sub<-serial_sub[,c("a_number","serial")]
## MERGE INTAKE FILTER WITH UNIQUE DF
ucm2 <- merge(x = ucm, y = intake_sub, by = c("a_number"),all.x=T)
## REPLACE NA IN INTAKE WITH 0 (0=SERIAL; 1=INTAKE)
ucm2$intake[is.na(ucm2$intake)]<-0
## MERGE INTAKE FILTER WITH UNIQUE DF
ucm3 <- merge(x = ucm2, y = serial_sub, by = c("a_number"),all.x=T)
## REPLACE NA IN SERIAL WITH 0 (1=SERIAL; 0=INTAKE)
ucm3$serial[is.na(ucm3$serial)]<-0
########################################################################################
## CREATE FILTERS BASED ON TEST FREQUENCY (SERIAL & INTAKE)
## SERIAL TEST RESULT (0 = NO (NEGATIVE OR NON-SERIAL); 1 = POSITIVE SERIAL)
ucm3$serial_pos = 0
ucm3$serial_pos[which(ucm3$Result=="Positive" & ucm3$serial == "1")] <- "1"
## INTAKE TESTING (1= YES, 0= NO)
ucm3$intake_pos = 0
ucm3$intake_pos[which(ucm3$Result=="Positive" & ucm3$intake == "1")] <- "1"
######################################################################################
## SUBSET ONLY UNIQUE A# BY DUPLICATE FILTER (1=DUP, 0=NON-DUP)
ucm_unique <- subset(ucm3, duplicate=="0")

## SUMMARIZE GRAND TOTALS FOR MORING PPT SLIDE
ucm_slide_summ <- data.frame(group_by(ucm_unique, ) %>% 
                                    summarize(ucm_intake_total_count = sum(intake=="1"),
                                              ucm_intake_positive_count = sum(intake_pos=="1"),
                                              ucm_serial_total_count = sum(serial=="1"),
                                              ucm_serial_positive_count = sum(serial_pos=="1"),
                                              ucm_total_count = sum(ucm_intake_total_count,ucm_serial_total_count),
                                              ucm_total_positive_count = sum(ucm_intake_positive_count,ucm_serial_positive_count)))
## ---------------------------------------------------------------------------------
## ADD IN TOTAL INTAKE COUNT (CORRECTION: TRIPWIRE IS DELAYED IN REGISTERING INTAKES)
## SET WORKING DIRECTORY TO INTAKE LINE LIST
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/Ft. Bliss Line Lists")
intake_line_list <- read.csv("UAC_Intake_Line_List.csv",header=T)
## SUBSET CURRENT UAC INTAKE
curr_intake <- subset(intake_line_list, date==bed_census_date)
## MODIFY TOTAL INTAKE FOR SUMMARY SLIDE
ucm_slide_summ$ucm_intake_total_count <- curr_intake$total_intake
## MODIFY SERIAL COUNT
ucm_slide_summ$ucm_serial_total_count = ucm_slide_summ$ucm_total_count - ucm_slide_summ$ucm_intake_total_count
## ---------------------------------------------------------------------------------
ucm_slide_long <- ucm_slide_summ %>% gather(ucm_slide_summ, value = Value)
colnames(ucm_slide_long)[1]<- "Attribute"
Attribute = c("UCs COVID-19 Tested from INTAKE","UCs COVID-19 Positive from INTAKE","UCs Tested from SERIAL TESTING","UCs COVID-19 Positive from SERIAL TESTING","Total UCs Tested for COVID-19 (INTAKE + SERIAL TEST)","Total UCs Positive for COVID-19 (INTAKE + SERIAL TEST)")

ucm_slide_long$Attribute = Attribute
##################################################################################################
## SUBSET UCM POSITIVE COVID-19 FOR LINE LIST
ucm_pos <- subset(ucm4, Result=="Positive" & duplicate == "0")
## CREATE COMBINED LAST NAME VARIABLE
tripwire_curr$full_last_name <- with(tripwire_curr, paste0(Last.Name, "-",X2nd.Last.Name))
##------------------------------------------------------------------------------------
## GARBAGE PLACE HOLDER VARIABLES
tripwire_curr$Manifest.Date = tripwire_curr$date
tripwire_curr$Manifest.Covid.Pos = ""
tripwire_curr$CDCID = ""
tripwire_curr$IncidentID = ""
tripwire_curr$Date.Diagnosis = tripwire_curr$date
tripwire_curr$Date.arrival = tripwire_curr$date
tripwire_curr$Date.intake.test = tripwire_curr$date
tripwire_curr$Date.Discharge = tripwire_curr$date
tripwire_curr$Date.Diagnosis = tripwire_curr$date
tripwire_curr$Last.Name = tripwire_curr$f
tripwire_curr$First.Name = tripwire_curr$First.Name
tripwire_curr$Tent.Origin = tripwire_curr$Dorm
tripwire_curr$Bed = tripwire_curr$Bunk
tripwire_curr$Diagnosis = "COVID"
##------------------------------------------------------------------------------------
ucm_pos_trip <- merge(x=ucm_pos, y=tripwire_curr, by=c("a_number"),all.x=T)
ucm_pos_trip$Manifest.CBP.Site = ucm_pos_trip$bus_origin
## PULL OUT TRIPWIRE VARIABLES OF IMPORTANCE
ucm_pos_line_list <- ucm_pos_trip[,c("Manifest.CBP.Site","Manifest.Date","Manifest.Covid.Pos","CDCID","IncidentID","Date.Diagnosis","Date.arrival","Date.intake.test","Date.Diagnosis","Last.Name","First.Name","a_number","Tent.Origin","Bed","Pod","Diagnosis")]
## SET WORKING DIRECTORY TO OUTPUT FOLDER 
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_OUTPUT_DATA/Daily UC COVID Line List Submissions")
## CREATE FILE NAME
ucm_LL_file_name<- paste("UCM_COVID19_LINE_LIST_",covid_date2,".csv", sep = "")
## SAVE FILE AS CSV 
write.csv(ucm_pos_line_list, ucm_LL_file_name, row.names=F)
##############################################################################################
## CREATE COUNT VARIABLE
ucm_pos_line_list$count=1
## SUMMARIZE UAC POSITIVE TESTS BY TENT OCCUPANCY 
tent_pos_summ <- data.frame(group_by(ucm_pos_line_list,Tent.Origin) %>% 
                                 summarize(tent_count= sum(count=="1")))
## FORMAT DATA.FRAME NAMES TO COMBINE (rbind)
colnames(tent_pos_summ)[1]<- "Attribute"
colnames(tent_pos_summ)[2]<- "Value"
##############################################################################################
## SET WORKING DIRECTORY TO HOT BUS LINE LIST
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/Ft. Bliss Line Lists")
## LOAD HOT BUS LINE LIST
ucm_bus <- read.csv("UCM_Manifest_Bus_Line_List.csv",header=T) 
ucm_bus_curr_date <- subset(ucm_bus, as.character(date) == covid_date2)
ucm4 <- merge(x=ucm3, y=ucm_bus_curr_date,by=c("a_number"),all.x=T)
ucm4$bus[is.na(ucm4$bus)]<-"cold"
ucm4$bus_origin[is.na(ucm4$bus_origin)]<-"Bliss"
ucm4$cbp_result[is.na(ucm4$cbp_result)]<-"Negative"
##############################################################################################
## (2) PPT DAILY SLIDE: SUMMARIZE STAFF
## SET WORKING DIRECTORY TO STAFF FOLDER
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_INPUT_DATA")
## -------------------------------------------------------------------------------
## AUTOMATE STAFF COVID FILE NAME
staff_covid_test_name<- paste("BorderResults_FortBlissTX_Staff_",covid_date2,".xlsx", sep = "")
## LOAD STAFF DATA
staff <- read_excel(staff_covid_test_name)
staff <- data.frame(staff)
## -------------------------------------------------------------------------------
## CREATE TEMP ID FOR FILTERING DUPLICATE STAFF TESTS
staff$temp_id <- with(staff, paste0(First.Name, Last.Name, Date.of.Birth))
## FILTER DUPLCIATE STAFF TESTS & PCR (ACULA)
## NOTE: ONLY REPORTING POSITIVE PCR POSITIVE TESTS FOR STAFF
unique_staff <- subset(staff, !duplicated(temp_id))
## SUMMARIZE STAFF COVID-19 TEST RESULT DATA
staff_slide_summ <- data.frame(group_by(staff,) %>% 
                                    summarize(staff_pos        = sum(Result=="Positive"),
                                              staff_neg        = sum(Result=="Negative"),
                                              staff_total_test = sum(staff_pos,staff_neg)))
## -------------------------------------------------------------------------------
## ACULA PCR POSITIVE ONLY
unique_staff_acula_count <- nrow(subset(staff, Test.Type=="Accula" & Result=="Positive"))
unique_staff_acula <- subset(staff, Test.Type=="Accula" & Result=="Positive")

## USE ONLY POSITIVE PCR ACULA FOR STAFF CASE REPORTING
staff_slide_summ$staff_pos = unique_staff_acula_count
######################################################################################################################################
## TRANSFORM STAFF DATA FROM WIDE TO LONG
staff_slide_long <- staff_slide_summ %>% gather(staff_slide_summ, value = Value)
colnames(staff_slide_long)[1]<- "Attribute"
staff_slide_long = subset(staff_slide_long, Attribute!="staff_neg")
Attribute = c("Total Staff Positive for COVID-19","Staff Tested for COVID-19")
staff_slide_long$Attribute <- Attribute
## ADD EXTRA STAFF VARIABLES
unique_staff_acula$date_of_diagnosis = unique_staff_acula$Test.Date
## ADD STAFF FULL NAME 
unique_staff_acula$full_name <- toupper(with(unique_staff_acula, paste0(First.Name, " ",Last.Name)))
unique_staff_acula$date_of_diagnosis = unique_staff_acula$Test.Date
unique_staff_acula$area_of_work=""
unique_staff_acula$area_frequented=""
## FILTER OUT DUPLICATE TESTS
unique_staff_acula <- subset(unique_staff_acula, !duplicated(temp_id))
## FORMAT STAFF TEST DATE
unique_staff_acula$date <- format(as.Date(unique_staff_acula$date, "%b %d %Y"), "%Y-%m-%d")
# FORMAT STAFF LINE LIST OUTPUT
staff_covid_line_list <- unique_staff_acula[,c("date","full_name","area_of_work","area_frequented")]
## FORMAT DATE VARIABLE
staff_covid_line_list$date_of_diagnosis <- format(staff_covid_line_list$date_of_diagnosis, format="%m/%d/%Y") 
## SET WD TO STAFF COVID POSITIVE FOLDER
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_OUTPUT_DATA/Staff Confirmed Positive Daily Tests")
## SAVE AS .CSV  
staff_file_name<- paste("STAFF_Positive_Line_List_",covid_date2,".csv", sep = "")
write.csv(staff_covid_line_list, staff_file_name, row.names=F)
##############################################################################################
## (3) PPT DAILY SLIDE: SUMMARIZE BED OCCUPANCY DATA
## LOAD DIRECTORY OF CENSUS FOLDER 
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/Ft. Bliss Line Lists")
## LOAD UAC CENSUS DATA
uac_bed <- read.csv("Bliss_UAC_COVID_Census_Line_List.csv",header=T)
## EXTRACT DATE THAT IS EQUAL TO TODAY'S DATE -1 DAY
uac_bed <- subset(uac_bed, date==bed_census_date)
## CURRENT FACILITY POPULATION
CFP = uac_bed$current_facility_population
## COVID-19 BED-USE (E28)
CBU = uac_bed$covid19_bed_use
## PROPORTION OF COVID BEDS
PCB = signif(CBU/CFP*100,digits=3)
## COMPILE BED VARIABELS FOR TABLE
Value = c(CFP,CBU,PCB)
Attribute = c("Total UC population","Total UCs in Isolation beds","Proportion (%) of UC population in isolation")
bed_slide <- data.frame(Attribute,Value)
##############################################################################################
## (4) PPT DAILY SLIDE: SUMMARIZE OTHER DISEASES
# LOAD VACCINE FIELD SURVEY DIRECTORY
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/Ft. Bliss Line Lists/Site Field Surveys")
## LOAD DATA 
surv <- read_excel("FT_Bliss_SitRep_Survey_Data_9Nov2021.xlsx")
## CONVERT TO DATA.FRAME
surv <- as.data.frame(surv)
## FORMAT SURVEY DATE
surv$Date <- format(surv$Date, "%Y-%m-%d")
## SUBSET SURVEY DATA = TODAY'S DATE
surv = data.frame(subset(surv, Date==covid_date2)) 
## QUANTIFY CASE DATA FROM SURVEY
Lice       = surv$Lice
Scabies    = surv$Scabies
Flu        = surv$Flu     ## TOTAL FLU TYPES
Varicella  = surv$Varicella
Strep      = surv$Strep
## COMPILE CASE DATA FOR AM SLIDE
Value = c(Lice,Scabies,Flu,Varicella,Strep)
Attribute = c("Lice","Scabies","Flu","Varicella","Strep")
other_dis_slide <- data.frame(Attribute, Value)
###########################################################################
## (5) PPT DAILY SLIDE: COMPILE TABLES FOR PPT REPORT
comp_ppt_report <- rbind(ucm_slide_long,staff_slide_long,bed_slide,other_dis_slide)
## SET WORKING DIRECTORY FOR THE OUTPUT FOLDER
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_OUTPUT_DATA/Summarized Metrics for Daily Slide")
## PREPARE FILE NAME
slide_file_name<- paste("PPT_Slide_Summary_Report_",sys_date,".csv", sep = "")
## SAVE AM SLIDE TO .CSV
write.csv(comp_ppt_report, slide_file_name, row.names=F)
##########################################################################################
## SITREP REPORT
Q1_text = "1. Number of new arrivals to the facility (last 24 hrs)"
## TRANSLATION: N KIDS FROM AM PPT SLIDE
Q1 = comp_ppt_report[1,2]
## ----------------------------------------------------------------------------
Q2_text =	"2. Current number of children at facility"  
## TRANSLATION: Total UC Population from Census dashboard
Q2 = CFP
## ----------------------------------------------------------------------------
Q3_text =	"3. Current number of COVID-19 isolation beds occupied" 
## TRANSLATION: Total UC Population from Census dashboard
Q3 = CBU
## ----------------------------------------------------------------------------
Q4_text = "4.	Number of new arrivals (last 24 hrs) that tested positive" 
## from CBP (Customs and Border Protection) 
## TRANSLATION: HOT BUS SUM 
## HOT BUS INTAKE SUM
Q4 = sum(ucm4$bus=="hot" & ucm4$duplicate=="0")
## ----------------------------------------------------------------------------
Q5_text = "5.	DOES YOUR FACILITY ROUTINELY RETEST UCs IDENTIFIED AS POSITIVE from CBP BY PCR/CONFIRMATORY TEST?" 
## TRANSLATION: YES BASED ON BLISS EIS PROTOCOL
Q5 = "YES"
## ----------------------------------------------------------------------------
Q5.1_text = "5.1 Of those who were identified as positive from CBP, how many were either confirmed by PCR OR classified as  COVID-19 cases due to the presence of symptoms by your facility?" 
## TRANSLATION: This number is hot bus UCs confirmed by PCR; use R code bus total 
## and check w/ border results for those that had just a PCR test that was positive (no binaxnow)
## The number of "c+" UCs on manifest who were confirmed with a positive Accula

## SUMMARIZE UC'S BY FREQUENCY OF EACH TEST TYPE
uc_test_type_summ <- data.frame(group_by(ucm4, a_number) %>% 
                                    summarize(count_binax_pos = sum(binaxnow_pos=="1"),
                                              count_binax_neg = sum(binaxnow_neg=="1"),
                                              count_acula_pos = sum(acula_pos=="1"),
                                              count_acula_neg = sum(acula_neg=="1")))

acula_pos_sum = subset(uc_test_type_summ, count_binax_pos==0 & count_binax_neg == 0 & count_acula_pos >1)
## REMOVE DUPLICATE A#'S
acula_pos_sum <- subset(acula_pos_sum, !duplicated(a_number))
## REMOVE DUPLICATE A#'S
Q5.1 =  nrow(acula_pos_sum)
## ----------------------------------------------------------------------------
Q5.2_text = "5.2 Of those who were identified as positive from CBP, how many were" 
## confirmed NOT to be COVID-19 cases by your facility? 
## TRANSLATION: 5.2 = Q4 value minus Q5.1 value
## This number is hot bus UCs with PCR negative results (no binaxnow performed)
## The number of "c+" UCs on manifest but with negative Accula at fort bliss
Q5.2 = Q4 - Q5.1
Q6_text = "6.	Is serial COVID-19 testing occurring at your facility (Yes/No)?" 
## TRANSLATION: EIS PROTOCOL -- ALWAYS "YES"
Q6 = "YES"
Q6.1_text = "6.1 Number of UCs tested on arrival"
## TRANSLATION: total intake from pivot - CBP +
## Subtract Q1-Q4
## This is the number of cold bus kids on the manifest (those without c+ or pc+)
Q6.1 = as.numeric(Q1) - as.numeric(Q4)
## ----------------------------------------------------------------------------
Q6.2_text = "6.2 Number of UCs who tested positive among those tested on arrival"
## TRANSLATION:  COLD BUS UC'S THAT WERE DETECTED COVID POSITIVE AT BLISS ON ARRIVAL 
Q6.2 = ucm_slide_long[2,2]
##subQ6.2 = subset(ucm4, intake=="1" & bus=="cold" & Result=="Positive")
Q7_text = "7. Number of UCs tested during routine testing (q3)"
## TRANSLATION = TOTAL UCS TESTED FOR SERIAL YESTERDAY
Q7 = comp_ppt_report[3,2]
## ----------------------------------------------------------------------------
Q8_text = "8. Number of UCs who tested positive among those tested during routine testing (q3)"
## TRANSLATION: .	= Q7 - negatives
## Number of serial-tested UCs with a positive Accula result
Q8 = nrow(subset(ucm4, serial=="1" & acula_pos=="1"))
## --------------------------------------------------------------------------------
Q9_text = ("9. Number of UCs tested for other reasons")
## Number of serial-tested UCs with a PCR only test (no binax result is listed)
##  look up these UCs in UC Portal to find out why they got a PCR test
## Zero unless otherwise mentioned
Q9 = nrow(subset(ucm4, serial=="1" & TestType!="BinaxNow"))
Q9_sub = subset(ucm4, serial=="1" & TestType!="BinaxNow")


## SUMMARIZE UC'S BY FREQUENCY OF EACH TEST TYPE
serial_only <- subset(ucm4, serial=="1")
serial_only_summ <- data.frame(group_by(serial_only, a_number) %>% 
                                  summarize(count_binax_pos = sum(binaxnow_pos=="1"),
                                            count_binax_neg = sum(binaxnow_neg=="1"),
                                            count_acula_pos = sum(acula_pos=="1"),
                                            count_acula_neg = sum(acula_neg=="1")))

serial_no_binax <- subset(serial_only_summ, count_binax_neg==0 & count_binax_pos==0)
##################################################################################################
## SET WORKING DIRECTORY TO Q9 FOR FOLLOW UP IN UC PORTAL OR ETRUENORTH
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_OUTPUT_DATA/Q9_Serial_No_Binax")
## SAVE FILE TO LATER LOOK UP WHY ONLY RECEIVED A PCR TEST
SitRep_Q9_file_name<- paste("UCM_SITREP_Q9_",covid_date2,".csv", sep = "")
write.csv(serial_no_binax,SitRep_Q9_file_name, row.names=F) 
## -----------------------------------------------------------------------------
Q10_text = "10. Number of UCs who tested positive among those tested for other reasons"
## TRANSLATION: Of the UCs listed in Q9, how many had a positive PCR test?
## Zero unless otherwise mentioned.
Q10 <- nrow(subset(serial_no_binax, count_acula_pos>0))
## -----------------------------------------------------------------------------
Q11_text = "11. Number of COVID-19 cases in fully vaccinated  UCs"  
##   TRANSLATION: LEAVE BLANK 
Q11 = "NA - NOT REPORTED AT BLISS"
## -----------------------------------------------------------------------------
Q12_text = "12. TOTAL number of UCs tested at any point in this reporting period" 
## TRANSLATION: Q6.1 + Q7 + Q9
Q12 = as.numeric(Q6.1) + as.numeric(Q7) + as.numeric(Q9)
## -----------------------------------------------------------------------------
Q13_text = "13. TOTAL number of UCs who tested positive at any point in this reporting period (by PCR/confirmatory test only)"
## TRANSLATION: Cold bus positives + Serial test (Q6.2 + Q8 + Q10)
Q13 = Q6.2 + Q8 + Q10
## -----------------------------------------------------------------------------
Q14_text = "14. Number of staff tested in this reporting period" 
## TRANSLATION: Should match AM slide  
Q14 = comp_ppt_report[8,2]
## -----------------------------------------------------------------------------
Q15_text = "15. Number of staff who tested positive in this reporting period"
Q15 = comp_ppt_report[7,2]
## -----------------------------------------------------------------------------
Q16_text = "16. Current number of COVID-19 positive UCs at your facility"
## TOTAL COVID BEDS OCCUPIED (SAME AS AM SLIDE)
Q16 = CBU
####################################################################################
## -----------------------------------------------------------------------------
Q17_text = "17.	Cumulative number of COVID-19 positive UCs in this facility to date (Should include ALL cases, both from testing and those who were identified as positive from CBP)"
## TRANSLATION: (Yesterday's report's value for Q17) + Q5.1 + Q13
## LOAD CUMULATIVE COVID UC DATABASE
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_OUTPUT_DATA/Daily UC COVID Cumulative Report")## LOAD CUMULATIVE UC DATASET
## LOAD CUMULATIVE UC DATA
uc_cum <- read.csv("ft_bliss_uc_daily_cumulative_covid.csv",header=T)
## EXTRACT COUNT FROM PREVIOUS DAY
prev_uc_cum = subset(uc_cum, date==bed_census_date)  
Q17 = prev_uc_cum$cumulative_covid_uc_sum + Q5.1 + Q13
cumulative_covid_uc_sum = Q17 
temp_entry = data.frame(bed_census_date2,cumulative_covid_uc_sum)
## CHANGE COLUMN NAME IN TEMP ENTRY
colnames(temp_entry)[1]<-"date"
## CONVERT DATE TO CHARACTER FOR COMPILING
temp_entry$date <- as.character(temp_entry$date)
## COMPILE NEW AND OLD CUMULATIVE COVID DATASET
um_comp_uc_covid <- rbind(uc_cum,temp_entry)
## REMOVE DUPLICATES IN CASE CODE IS RE-RUN
um_comp_uc_covid <- subset(um_comp_uc_covid, !duplicated(date))
##  ADD TIME STAMP
ucm_cum_file_name<- paste("ft_bliss_uc_daily_cumulative_covid",".csv", sep = "")
## SAVE NEW VERSION OF THE CUMULATIVE COVID LINE LIST
write.csv(um_comp_uc_covid, ucm_cum_file_name,row.names=F)
## -----------------------------------------------------------------------------------
## ADDITIONAL VACCINE SURVEY DATA SITREP QUESTIONS
## -----------------------------------------------------------------------------------
Q18_text = "Q18. Is vaccination occurring?  (yes/no)"
Q18 = "YES"
## -----------------------------------------------------------------------------------
Q19_text = "19. Number of COVID-19 vaccinations given"
Q19 = surv$Total.number.of.COVID.19.vaccinations.given...
## -----------------------------------------------------------------------------------
Q19.1_text = "19.1 Number of first dose COVID-19 vaccinations given"
Q19.1 = surv$Number.first.COVID.19.doses.given
## -----------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------
Q19.2_text = "19.2 Number of second dose COVID-19 vaccinations given"
Q19.2 = surv$Number.second.COVID.19.doses.given
## -----------------------------------------------------------------------------------
Q20_text = "20. Number of COVID-19 declinations"
Q20 = surv$Number.declined.COVID.19.vaccine
## -----------------------------------------------------------------------------------
Q21_text = "21.  ineligible due to COVID + status:"
Q21 = surv$ineligible.due.to.COVID...status
## -----------------------------------------------------------------------------------
Q22_text = "22. # ineligible for other reasons:"
Q22 = surv$COVID.19.ineligible.for.other.reasons
## -----------------------------------------------------------------------------------
Q23_text = "23. Number of varicella vaccinations given (last 24 hrs):"
Q23 = surv$Number.of.varicella.vaccinations.given
# -----------------------------------------------------------------------------------
Q23.1_text = "23.1 # First dose varicella given:"
Q23.1 = surv$Number.of.varicella.vaccinations.given
## -----------------------------------------------------------------------------------
Q23.2_text = "23.2 # varicella Second dose given:"
Q23.2 = surv$Number.of.second.dose.varicella
# -----------------------------------------------------------------------------------
Q24_text = "24. Number of MMR vaccinations given (last 24 hrs):"
Q24 = surv$Number.of.MMR.vaccinations.given
# -----------------------------------------------------------------------------------
Q24.1_text = "24.1 MMR # First dose given:"
Q24.1 = surv$Number.of.first.dose.MMR
## -----------------------------------------------------------------------------------
Q24.2_text = "24.2 MMR # Second dose given:"
Q24.2 = surv$Number.of.second.dose.MMR
## -----------------------------------------------------------------------------------
Q25_text = "25. Number of influenza vaccinations given (last 24 hrs):"
Q25 = surv$Number.of.influenza.vaccinations.given
## -----------------------------------------------------------------------------------4
Q26_text = "26. Number of UCs who received other vaccinations (last 24 hrs):"
Q26 = surv$Number.of.UCs.who.received.other.vaccinations
## -----------------------------------------------------------------------------------
Q27_text = "27. Suspected Influenza:"
Q27 = surv$Number.Suspected.Influenza
## -----------------------------------------------------------------------------------
Q28_text = "28. Confirmed Influenza:"
Q28 = surv$Number.Confirmed.Influenza
## -----------------------------------------------------------------------------------
Q29_text = "29. Lice, last 24 hours:"
Q29 = surv$Lice
## -----------------------------------------------------------------------------------
Q30_text = "30. Confirmed Varicella, last 24 hours:"
Q30 = surv$Varicella
## -----------------------------------------------------------------------------------
Q30.1_text = "30.1. Confirmed Varicella, last 24 hours:"
Q30.1 = surv$Confirmed.Varicella
## -----------------------------------------------------------------------------------
Q31_text = "31. Scabies, last 24 hours:"
Q31 = surv$Scabies
## -----------------------------------------------------------------------------------
Q32_text = "32. Suspected Strep:"
Q32 = surv$Strep
## -----------------------------------------------------------------------------------
Q33_text = "33. Confirmed Strep, last 24 hours:"
Q33 = surv$Strep
## -----------------------------------------------------------------------------------
Q34_text = "34. Suspected viral hepatitis:"
Q34 = surv$Confirmed.viral.hepatitis
## -----------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------
Q35_text = "35. Confirmed viral hepatitis, last 24 hours:"
Q35 = surv$Confirmed.viral.hepatitis
## -----------------------------------------------------------------------------------
Q36_text = "36. Suspected tuberculosis:"
Q36 = surv$Suspected.tuberculosis 
## -----------------------------------------------------------------------------------
Q37_text = "37. Confirmed tuberculosis, last 24 hours:s"
Q37 = surv$Confirmed.tuberculosis.. 
## -----------------------------------------------------------------------------------
Q38_text = "38. Other, last 24 hours:"
Q38 = surv$Other.disease 
## -----------------------------------------------------------------------------------
Q39_text = "39. Number of UCs admitted to the hospital for COVID-19, last 24 hours:"
Q39 = surv$Number.of.UCs.admitted.to.the.hospital.for.COVID.19
## -----------------------------------------------------------------------------------
Q40_text = "40. Number of UCs admitted to the hospital for other conditions, last 24 hours"
Q40 = surv$Number.of.UCs.admitted.to.the.hospital.for.other.conditions
## -----------------------------------------------------------------------------------
Q41_text = "41. Number of UCs admitted to the hospital for unknown reasons, last 24 hours:"
Q41 = surv$Number.of.UCs.admitted.to.the.hospital.for.unknown.reasons
## -----------------------------------------------------------------------------------
Q42_text = "42. NOTES"
Q42 = surv$Notes
## -----------------------------------------------------------------------------------
## MASTER FORGE THE SITREP REPORT
## -----------------------------------------------------------------------------------
comp_text = c(Q1_text,Q2_text,Q3_text,Q4_text,Q5_text,Q5.1_text,Q5.2_text,Q6_text, Q6.1_text, Q6.2_text, Q7_text,Q8_text,Q9_text,Q10_text,Q11_text,Q12_text,Q13_text,Q14_text,Q15_text,Q16_text,Q17_text,Q18_text,Q19_text,Q19.1_text,Q19.2_text,Q20_text,Q21_text,Q22_text,Q23_text,Q23.1_text, Q23.2_text,Q24_text,Q24.1_text,Q24.2_text,Q25_text,Q26_text,Q27_text,Q28_text,Q29_text,Q30_text,Q30.1_text,Q31_text,Q32_text,Q33_text,Q34_text,Q35_text,Q36_text,Q37_text,Q38_text,Q39_text,Q40_text,Q41_text,Q42_text)
comp_responses = c(Q1,Q2,Q3,Q4,Q5,Q5.1,Q5.2,Q6,Q6.1,Q6.2,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q19.1,Q19.2,Q20,Q21,Q22,Q23,Q23.1,Q23.2,Q24,Q24.1,Q24.2,Q25,Q26,Q27,Q28,Q29,Q30,Q30.1,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42)
## MEGAZORD SITREP RESPONSES!
comp_sitrep <- data.frame(comp_text,comp_responses)
## CONVERT ALL NA TO 0
comp_sitrep[is.na(comp_sitrep)] <-0
## CLEAN UP TABLE VARIABLE NAMES
colnames(comp_sitrep)[1]<-"QUESTION"
colnames(comp_sitrep)[2]<-"RESPONSE"
## SET WORKING DIRECTORY TO COMPILE SITREP FOLDER
setwd("C:/Users/rhv8/Response Force One/Fort Bliss EIS Medical - Documents/General/SWBMTF_DATA_AUTOMATION/DAILY_OUTPUT_DATA/SitRep_R_Compiled_Output")
## CREATE SITREP FILE NAME
sitrep_file_name<- paste("FtBliss_SitRep_",covid_date2,".csv", sep = "")
## SAVE FILE 
write.csv(comp_sitrep, sitrep_file_name, row.names = F)
## ----------------------------------------------------------------------------------------------

