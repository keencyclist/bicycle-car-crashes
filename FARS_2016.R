# use the tidyverse of packages
install.packages("tidyverse")
library(tidyverse)

# read bike ped crashes
pbtype <- read.csv('pbtype.csv')
# keep only the bicycle and other cycle (tricycle) crashes 
bicyclists <- pbtype %>%
  filter(PBPTYPE %in% c(6,7))

# join with PERSON file
person <- read.csv('person.csv')
bikecrash <- bicyclists %>%
  inner_join(person,by=c("ST_CASE","PER_NO", "VEH_NO"))
  
# keep only fatalities - 
bikecrash <- bikecrash %>%
  filter(INJ_SEV == 4)

# join with accident file
accident <- read.csv('accident.csv')
bikecrash <- bikecrash %>%
  inner_join(accident,by="ST_CASE")

# join with Safety Equipment file
safetyeq <- read.csv('SafetyEq.csv')
safetyeq <- safetyeq %>%
  group_by(ST_CASE, PER_NO, VEH_NO) %>%
  summarise(Helmet = ifelse(any(MSAFEQMT == 2), 1, 0),
            Reflective = ifelse(any(MSAFEQMT == 3), 1, 0),
            Lighting = ifelse(any(MSAFEQMT == 5), 1, 0))

# get a unique list of fatally injured bicyclists
bicyclist_list <- bikecrash %>%
  group_by(ST_CASE, PER_NO, VEH_NO) %>%
  summarise(count=n())

safety_fields <- safetyeq %>%
  semi_join(bicyclist_list,by=c("ST_CASE", "PER_NO"))

bikecrash <- bikecrash %>%
  left_join(safety_fields, by=c("ST_CASE", "PER_NO"))


# get a unique list of bike crash case numbers
bikecases <- bikecrash %>%
  group_by(ST_CASE) %>%
  summarise(MVcount = n())

# create a separate file of drivers involved in bike crashes
drivers <- person %>%
  semi_join(bikecases,by = "ST_CASE") %>%
  filter(PER_TYP == 1)

# merge Drivers with Vehicle file
vehicle <- read.csv('vehicle.csv')
drivers <- drivers %>%
  left_join(vehicle,by = c("ST_CASE","VEH_NO")) 

# recode unknowns
drivers <- drivers %>%
  mutate(VSPD_LIM = ifelse(VSPD_LIM>=98,NA,VSPD_LIM),
         TRAV_SP = ifelse(TRAV_SP>=997,NA,TRAV_SP))  

# recode impact area
drivers <- drivers %>%
  mutate(ImpactArea = ifelse(IMPACT1.x %in% c(11,12,1),"Front",
                             ifelse(IMPACT1.x %in% c(5,6,7),"Back",
                                    ifelse(IMPACT1.x %in% c(8,9,10,61,62,63), "Left", 
                                           ifelse(IMPACT1.x %in% c(2,3,4,81,82,83), "Right",
                                                  ifelse(IMPACT1.x == 14, "Underneath",
                                                         ifelse(IMPACT1.x == 0, "Non-collision","Unknown")))))))

# add flag for driver impairment other than drugs or alcohol
drimpair <- read.csv('drimpair.csv')
drivers <- drivers %>%
  left_join(drimpair,by = c("ST_CASE","VEH_NO")) 

# create variable for impairments other than drugs and alcohol
 drivers <- drivers %>%
      mutate(Other_Impairment = ifelse(DRIMPAIR %in% c(1,2,3,4,5,6,7,10,96),1,0))
 
 # check for any driver distractions
distract <- read.csv('distract.csv')
drivers <- drivers %>%
   left_join(distract,by = c("ST_CASE", "VEH_NO")) 

 # create variable for driver distracted
 # excludes 'Looked but did not see' but includes all other distractions
drivers <- drivers %>%
   mutate(Driver_Distracted = ifelse(between(MDRDSTRD,3,98) & MDRDSTRD != 96,1,0))

vision <- read.csv('Vision.csv')
drivers <- drivers %>%
  left_join(vision,by = c("ST_CASE", "VEH_NO"))  %>%
  mutate(Vision_Obscured = ifelse(between(MVISOBSC,1,98),1,0),
         Glare = ifelse(MVISOBSC == 2, 1, 0), 
         Dirty_Windshield = ifelse(MVISOBSC == 13, 1, 0))

violations <- read.csv('Violatn.csv')

violations <- violations %>%
  semi_join(bikecases,by = "ST_CASE") %>%
  filter(MVIOLATN != 0) # eliminate cases where no violation was charged

violations <- violations %>%
  group_by(ST_CASE) %>% # count the number of violations per incident, even where there are multiple drivers
  mutate(homicide = ifelse(any(MVIOLATN %in% c(1,9)),1,0),
         careless = ifelse(any(MVIOLATN %in% c(2,3,4)),1,0),
         hit_run_charge = ifelse(any(MVIOLATN %in% c(5,7,8)),1,0),
         phone = ifelse(any(MVIOLATN == 10),1,0),
         DWI_charge = ifelse(any(between(MVIOLATN,11,19)),1,0),
         speeding = ifelse(any(between(MVIOLATN,21,29)),1,0),
         moving_violation = ifelse(any(between(MVIOLATN,31,69)) | any(MVIOLATN %in% c(93,98)),1,0),
         non_moving_violation = ifelse(any(between(MVIOLATN,70,89)) | any(MVIOLATN == 92),1,0),
         unknown_violation = ifelse(any(MVIOLATN == 99),1,0))

drivers <- drivers %>%
  left_join(violations,by = c("ST_CASE", "VEH_NO")) 



# Create file with the driver info, 1 record per bike crash, to merge with bikecrash df.
driver_fields <- drivers %>%
  group_by(ST_CASE) %>%
  summarise(PSL = max(VSPD_LIM),
            Travel_Speed = max(TRAV_SP), 
            ImpactArea = first(ImpactArea),
            TrafficControl = ifelse(any(VTRAFCON %in% c(97,98,99)), min(VTRAFCON), max(VTRAFCON)),
            TrafficwayType = ifelse(any(VTRAFWAY %in% c(8,9)), min(VTRAFWAY), max(VTRAFWAY)),
            Lanes = ifelse(any(VNUM_LAN %in% c(8,9)), min(VNUM_LAN), max(VNUM_LAN)), 
            Previous_Crashes = ifelse(max(PREV_ACC <98), max(PREV_ACC),min(PREV_ACC)),
            Previous_Suspensions = ifelse(max(PREV_SUS <98), max(PREV_SUS),min(PREV_SUS)),
            Previous_DWI = ifelse(max(PREV_DWI <98), max(PREV_DWI),min(PREV_DWI)),
            Previous_Speeding = ifelse(max(PREV_SPD <98), max(PREV_SPD),min(PREV_SPD)),
            Previous_Violations = ifelse(max(PREV_OTH <98), max(PREV_OTH),min(PREV_OTH)),
            Other_Factors = ifelse(any(DR_SF1 > 0), max(DR_SF1),0),
            Vision_Obscured = ifelse(any(Vision_Obscured>0),1,0),
            Glare = ifelse(any(Glare>0),1,0),
            Dirty_Windshield = ifelse(any(Dirty_Windshield>0),1,0),            
            Driver_Drinking = ifelse(any(DR_DRINK == 1), 1, 0),
            Driver_Drugged = ifelse(any(DRUGS == 1), 1, 0),
            Driver_Distracted = ifelse(any(Driver_Distracted == 1), 1, 0),
            Male_Driver = ifelse(any(SEX == 1), 1, 0),
            Over69_Driver = ifelse(any(AGE > 69 & AGE < 998),1, 0),
            Teen_Driver = ifelse(any(AGE < 20),1, 0),
            Hit_And_Run = ifelse(any(HIT_RUN == 1), 1, 0),
            Driver_Speeding = ifelse(any(SPEEDREL %in% c(2,3,4,5)),1,0),
            Truck_Bus = ifelse(any(between(BODY_TYP.x,39,80)),1,0),
            Other_Impairment = ifelse(any(Other_Impairment == 1),1,0),
            homicide = max(homicide),
            careless = max(careless),
            hit_run_charge = max(hit_run_charge),
            phone = max(phone),
            DWI_charge = max(DWI_charge),
            speeding = max(speeding),
            moving_violation = max(moving_violation),
            non_moving_violation = max(non_moving_violation),
            unknown_violation = max(unknown_violation))

# join driver fields to bikecrash
bikecrash <- bikecrash %>%
  left_join(driver_fields,by="ST_CASE")
# Recodes
bikecrash <- bikecrash %>%
  replace_na(list(PSL = 99))

write.csv(bikecrash, file='FARS2016.csv')


# combine 2014 and 2015 FARS with 2016
bikecrash2015 <- read.csv(file = 'bikecrashFARS_2015.csv')
bikecrash2015 <- bikecrash2015 %>%
  select(-PEDSNR,-RAIL)

bikecrash <- bikecrash %>%
  select(-PEDSNR, -RAIL)

bikecrash_all <- bind_rows(bikecrash, bikecrash2015)

bikecrash2014 <- read.csv(file = 'bikecrashFARS_2014.csv')
bikecrash2014 <- bikecrash2014 %>%
  select(-PEDSNR,-RAIL)
bikecrash_all <- bind_rows(bikecrash_all, bikecrash2014)


bikecrash <- bikecrash %>%
  mutate(daytime = ifelse(LGT_COND == 1, 1, 0),
         dark_notlighted = ifelse(LGT_COND %in% c(2,6), 1, 0),
         dark_lighted = ifelse(LGT_COND == 3, 1, 0),
         dusk = ifelse(LGT_COND == 5, 1, 0),
         dawn = ifelse(LGT_COND == 4, 1, 0),
         sidewalk_present = ifelse(PBSWALK == 1, 1, 0),
         overtake = ifelse(BIKECGP == 230, 1, 0),
         motorist_didnt_yield = ifelse(BIKECTYPE %in% c(141,143,151,217,218,321,322,323,328,329), 1, 0), # includes stop sign, driveway, RTOR
         bicyclist_didnt_yield = ifelse(BIKECTYPE %in% c(142,144,147,148,311,312,313), 1, 0), #includes stop sign, driveway
         bicyclist_red_signal = ifelse(BIKECTYPE %in% c(153,155), 1, 0),
         motorist_red_signal = ifelse(BIKECTYPE %in% c(152,154), 1, 0),
         motorist_left = ifelse(BIKECTYPE %in% c(111,211,212), 1, 0), # includes improper left turn
         motorist_right = ifelse(BIKECTYPE %in% c(112,213,214,217,218), 1, 0), # includes improper right turn
         bicyclist_midblock = ifelse(BIKECTYPE %in% c(225,318,319,357), 1, 0),
         bicyclist_left = ifelse(BIKECTYPE %in% c(114,221,222), 1, 0),
         bicyclist_right = ifelse(BIKECTYPE %in% c(115,223,224), 1, 0),
         wrong_way = ifelse(BIKECTYPE == 250, 1,0 ),
         other_crossing = ifelse(BIKECTYPE %in% c(158,160,180,380), 1, 0),
         other_parallel = ifelse(BIKECTYPE == 280, 1, 0),
         using_shoulder = ifelse(BIKEPOS == 2, 1, 0),
         using_sidewalk = ifelse(BIKEPOS %in% c(3,4),1, 0),
         off_trafficway = ifelse(BIKEPOS %in% c(5,6),1, 0),
         facing_traffic = ifelse(BIKEDIR == 2,1, 0),
         Bicyclist_Drinking = ifelse(DRINKING == 1, 1, 0),
         Bicyclist_Drugged = ifelse(DRUGS == 1, 1, 0),
         Male_Bicyclist = ifelse(SEX == 1, 1, 0),
         Over69_Bicyclist = ifelse(AGE > 69 & AGE < 998,1, 0),
         Child_Bicyclist = ifelse(AGE < 16,1, 0),
         Rural = ifelse(RUR_URB.x == 1, 1, 0),
         Rain = ifelse(WEATHER == 2, 1, 0),
         Snow = ifelse(WEATHER %in% c(3,4,11,12), 1, 0),
         PSL_recode = ifelse(PSL < 25, "20 or less",
                             ifelse(PSL < 35, "25 to 30",
                                    ifelse(PSL < 50, "35 to 45", 
                                           ifelse(PSL !=99, "50 or more","unknown")))),
         PSL20 = ifelse(PSL < 25, 1, 0),
         PSL25 = ifelse(PSL == 25, 1, 0),
         PSL30 = ifelse(PSL == 30, 1, 0),
         PSL35 = ifelse(PSL == 35, 1, 0),
         PSL40 = ifelse(PSL == 40, 1, 0),
         PSL45 = ifelse(PSL == 45, 1, 0),
         PSL50 = ifelse(PSL > 50 & PSL != 99, 1, 0),
         PSL_LT35 = ifelse(PSL < 35, 1, 0))

# create crash type recode
bikecrash <- bikecrash %>% 
  mutate(crash_type = 
           ifelse(BIKECGP == 230, "Motorist Overtaking",
                  ifelse(BIKECGP == 240, "Bicyclist Overtaking",       
                  ifelse(BIKECTYPE  %in% c(158,160,180,380), "Crossing Paths, No Details",
                  ifelse(BIKECTYPE == 250, "Head On, Wrong Way Bicyclist",
                                ifelse(BIKECTYPE == 255, "Head On, Wrong Way Motorist",
                                ifelse(BIKECTYPE  %in% c(121,122,123,124,129), "Bicyclist Lost Control",
                                ifelse(BIKECTYPE  %in% c(131,132,133,134,139), "Motorist Lost Control",
                                ifelse(BIKECTYPE %in% c(114,221,222), "Bicyclist Left Turn",
                                       ifelse(BIKECTYPE %in% c(115,223,224), "Bicyclist Right Turn",     
                                ifelse(BIKECTYPE == 280, "Parallel Paths, No Details",
                         ifelse(BIKECTYPE %in% c(141,143,151,217,218,321,322,323,328,329), "Motorist Didn't Yield", 
                                ifelse(BIKECTYPE %in% c(142,144,147,148,311,312,313), "Bicyclist Didn't Yield", 
                                       ifelse(BIKECTYPE %in% c(153,155), "Bicyclist Red Signal",
                                              ifelse(BIKECTYPE %in% c(152,154), "Motorist Red Signal",
                                                     ifelse(BIKECTYPE %in% c(111,211,212), "Motorist Left Turn", 
                                                            ifelse(BIKECTYPE %in% c(112,213,214,217,218), "Motorist Right Turn", 
                                                                   ifelse( BIKECTYPE %in% c(225,318,319,357), "Bicyclist Midblock",
                                   ifelse( BIKECTYPE %in% c(970,980), "Unknown",
                                           "Other")))))))))))))))))))
                                           
write.csv(bikecrash_all, file='FARS2014_2016.csv')                                                                                                                                                            

write.csv(ls(bikecrash_all), file='fatal_vars.csv')

bikecrash_all <- bikecrash_all %>% 
  mutate(crash_error =
          ifelse(BIKECGP == 230, "Motorist Overtaking",
          ifelse(BIKECTYPE %in% c(131,132,133,134,139,255,141,143,151,217,218,321,322,323,328,329,152,154,111,211,212,112,213,214,217,218),"Other Motorist Error",
          ifelse(BIKECTYPE %in% c(121,122,123,124,129,241,242,243,244,249,250,114,221,222,115,223,224,142,144,147,148,311,312,313,153,155,225,318,319,357),"Bicyclist Error",
                 "Other and Unknown"))))

bikecrash_all <- bikecrash_all %>% 
  mutate(bicyclist_position =
           ifelse(facing_traffic==1 & using_sidewalk==1,"Crosswalk, Facing Traffic", 
                  ifelse(facing_traffic==0 & using_sidewalk==1, "Crosswalk, With Traffic",
                  ifelse(facing_traffic==1 & using_sidewalk==0, "Road, Facing Traffic",
                  "Road, With Traffic"))))

bikecrash_all <- bikecrash_all %>% 
  mutate(driver_error =
           ifelse(Driver_Drinking==1 | Driver_Drugged==1 | Driver_Distracted==1 | Driver_Speeding==1 | Hit_And_Run ==1 | Vision_Obscured == 1 | 
                    Other_Impairment==1, "Yes", "No"))

model_fatals <- bikecrash %>%
select(AGE, bicyclist_didnt_yield, Bicyclist_Drinking, Bicyclist_Drugged, bicyclist_left, bicyclist_midblock, bicyclist_red_signal, bicyclist_right, BIKECGP, 
       BIKECTYPE, BIKEDIR, BIKELOC, BIKEPOS, BODY_TYP, Child_Bicyclist, crash_type, dark_lighted, dark_notlighted, dawn, daytime, Driver_Distracted, 
       Driver_Drinking, Driver_Drugged, Driver_Speeding, dusk, facing_traffic, Helmet, Hit_And_Run, INJ_SEV, Lanes, LGT_COND, Lighting, LOCATION, Male_Bicyclist, 
       Male_Driver, motorist_didnt_yield, motorist_left, motorist_red_signal, motorist_right, off_trafficway, 
       other_crossing, Other_Impairment, Over69_Bicyclist, Over69_Driver, overtake, PBAGE, PSL, PSL_LT35, PSL20, PSL25, PSL30, PSL35, PSL40, 
       PSL45, PSL50, Rain, Teen_Driver, TrafficControl, TrafficwayType, Travel_Speed, Truck_Bus, using_shoulder, using_sidewalk, wrong_way, YEAR, Snow)

model_fatals <- model_fatals %>%
mutate(WEIGHT = 1)

write.csv(model_fatals,file = 'model_fatals.csv')

library(janitor)
crosstab1 <- bikecrash_all %>%
    crosstab(crash_error, bicyclist_position, percent = "none") 
write.csv(crosstab1, file='errorbyposition.csv')
crosstab2 <- bikecrash_all %>%
   crosstab(crash_error, driver_error, percent = "none") 
write.csv(crosstab2, file='driver_error.csv')

crosstab3 <- bikecrash_all %>%
  filter(daytime==1) %>%
  crosstab(crash_error, using_shoulder, percent = "none") 


install.packages("gmodels")
library(gmodels)

crosstab1 <- bikecrash_all %>%
  CrossTable(bikecrash_all$crash_error,bikecrash_all$daytime, digits = 0, format = "SPSS", fisher = TRUE, expected = TRUE, chisq = TRUE,  prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE)

bikeroadcrash <- bikecrash_all %>%
  filter(using_sidewalk == 0)




#  logistic regression models

run_logit <- function(dependent) {
             model_output <- glm(bikecrash_all[[dependent]] ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural + 
             Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
             Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
             using_shoulder +  Male_Bicyclist + Child_Bicyclist  + using_sidewalk + facing_traffic,
             data = bikecrash_all, family = "binomial")
             return(model_output)
             }

model1 <- run_logit("overtake")
model2 <- run_logit("motorist_didnt_yield")
summary(model2)

model_name <- paste("model", as.character(i), sep = "") 
model_results <- run_logit(get(model_name)) # run model i 
all_results <- model_results

dependent_list <- c("overtake", "motorist_didnt_yield", "bicyclist_didnt_yield",
                    "motorist_right", "motorist_left", "bicyclist_midblock", "bicyclist_left", "other_parallel",
                    "other_crossing", "bicyclist_left")
i <- c(1:length(dependent_list)) # initialize counter
repeat {
  model_name <- paste("model", as.character(i), sep = "") 
  get(model_name) <- run_logit(get(model_name)) # run model i 
  all_results <- bind_cols(model_results, all_results)
  i = i + 1
  if (! i < 11) {break}
}



model1 <- glm(overtake ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural + 
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + Other_Impairment +
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder +  Male_Bicyclist + Child_Bicyclist + Bicyclist_Drinking  + using_sidewalk + facing_traffic,
                data = bikecrash_all, family = "binomial")

summary(model1)
output_model11 <- as.data.frame(summary.glm(model1)$coefficients) %>%
  mutate(OddsRatio = exp(Estimate)) %>%
  select(1,5,4)
write.csv(output_model1,file="FARS_OVertake.csv")

model2 <- glm(motorist_didnt_yield ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted +
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                data = bikecrash_all, family = "binomial")

model3 <- glm(bicyclist_didnt_yield ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                data = bikecrash_all, family = "binomial")

model4 <- glm(motorist_right ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                data = bikecrash_all, family = "binomial")

model5 <- glm(motorist_left ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural + 
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                data = bikecrash_all, family = "binomial")

model6 <- glm(bicyclist_midblock ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                data = bikecrash_all, family = "binomial")

model7 <- glm(bicyclist_left ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                data = bikecrash_all, family = "binomial")

model8 <- glm(other_parallel ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                data = bikecrash_all, family = "binomial")

model9 <- glm(other_crossing ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                 Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                 Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                 using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                 data = bikecrash_all, family = "binomial")

model10 <- glm(bicyclist_right ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                 Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                 Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                 using_shoulder + Male_Bicyclist + Child_Bicyclist + using_sidewalk + facing_traffic,
                 data = bikecrash_all, family = "binomial")

model11 <- glm(wrong_way ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                using_shoulder + Male_Bicyclist + Child_Bicyclist,
                data = bikecrash_all, family = "binomial")

model12 <- glm(motorist_red_signal ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                 Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                 Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                 using_shoulder + Male_Bicyclist + Child_Bicyclist,
               data = bikecrash_all, family = "binomial")

model13 <- glm(bicyclist_red_signal ~ daytime + PSL_LT35 + PSL50 + sidewalk_present + Rural +
                 Truck_Bus + Driver_Drinking + Driver_Drugged + Driver_Distracted + 
                 Driver_Speeding + Hit_And_Run + Male_Driver + Teen_Driver + Over69_Driver +
                 using_shoulder + Male_Bicyclist + Child_Bicyclist,
               data = bikecrash_all, family = "binomial")


output_model11 <- as.data.frame(summary.glm(model11)$coefficients) %>%
  mutate(OddsRatio = exp(Estimate)) %>%
  select(5,4)


put_in_export_form <- function(model_name) {
    model_results <- as.data.frame(summary.glm(model_name)$coefficients) %>%
    mutate(OddsRatio = exp(Estimate)) %>%
    select(5,4)
    return(model_results)
}
i <- 1 # initialize counter
model_name <- paste("model", as.character(i), sep = "") 
model_results <- put_in_export_form(get(model_name)) # get results for model 1
all_results <- model_results
variable_names <- all.vars(formula.model1)

repeat {
  i = i + 1
  model_name <- paste("model", as.character(i), sep = "") 
  model_results <- put_in_export_form(get(model_name))
  all_results <- bind_cols(model_results, all_results)
  if (! i < 11) {break}
}
  
write.csv(all_results, file='FARS_logit_results.csv') # note: order is last added on left to first added on right
write.csv(summary.glm(model11)$coefficients,file="model11.csv")  # output separately because of different indep. vars


write.csv(summary.glm(model10)$coefficients,file="model10.csv")  # output separately because of different indep. vars


freq1 <- bikecrash_all %>%
  group_by(crash_type) %>%
  summarise(count=n())

freq1 <- bikecrash_all %>%
  group_by(LGT_COND) %>%
  summarise(count=n())

freq2 <- bikecrash_all %>%
  group_by(BIKECTYPE) %>%
  summarise(count=n())

write.csv(freq2, file="BikeFatals_2014_16.csv")


