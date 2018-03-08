library(tidyverse)
library(xlsx)
library(lubridate)
library(eeptools)
library(reshape2)


ce <- read.table("20180308_concurrent", sep = "\t", header = TRUE)

sum(is.na(ce$HS_GRAD_YEAR))
#60

ce <- as.tibble(ce)

table(ce$TERM_CODE)

ce <- ce %>% mutate(ENROLLED_AFTER_HS_GRAD_01 = ifelse(ENROLLED_AFTER_HS_GRAD == "Y", 1, 0))
ce <- ce %>% mutate(EVER_ENROLLED_NON_CONCURRENT_01 = ifelse(EVER_ENROLLED_NON_CONCURRENT == "Y", 1, 0))
ce <- ce %>% mutate(ENROLLED_WITHIN_ONE_YR_OF_HS_01 = ifelse(ENROLLED_WITHIN_ONE_YR_OF_HS == "Y", 1, 0))
ce$ENROLLED_AFTER_HS_GRAD_01 <- as.integer(ce$ENROLLED_AFTER_HS_GRAD_01)
ce$EVER_ENROLLED_NON_CONCURRENT_01 <- as.integer(ce$EVER_ENROLLED_NON_CONCURRENT_01)
ce$ENROLLED_WITHIN_ONE_YR_OF_HS_01 <- as.integer(ce$ENROLLED_WITHIN_ONE_YR_OF_HS_01)

ce %>% group_by(PIDM, HS_GRAD_YEAR) %>%
  slice(1) %>%
  group_by(HS_GRAD_YEAR) %>%
  filter(HS_GRAD_YEAR > 2012, HS_GRAD_YEAR < 2018) %>%
  summarise(n = n(), ever = sum(EVER_ENROLLED_NON_CONCURRENT_01), enrollafter = sum(ENROLLED_AFTER_HS_GRAD_01), within1 = sum(ENROLLED_WITHIN_ONE_YR_OF_HS_01))
  
ce %>% filter(EVER_ENROLLED_NON_CONCURRENT == "Y", ENROLLED_AFTER_HS_GRAD == "N") %>%
  summarise(null = sum(is.na(HS_GRAD_YEAR)), n = n())

######clearinghouse#######
#identify unique students to send to clearinghouse
#need first, last, middle, birthdate

clearinghouse <- ce %>% dplyr::distinct(PIDM, FIRST_NAME, MIDDLE_NAME, LAST_NAME, BIRTH_DATE)
write.table(clearinghouse, "clearinghouse.txt", sep = ",")
ce %>% summarize(minterm = max(TERM_CODE))
#min = 200920
#max = 201720

enr_aft <- ce %>% group_by(PIDM) %>%
  summarize(BIRTH_DATE = max(BIRTH_DATE), maxterm = max(TERM_CODE), ENROLLED_AFTER_HS_GRAD = max(ENROLLED_AFTER_HS_GRAD),
            numcourses = n())

###########do CE students enroll at SLCC?##########
###Figure out student's age at last enrollment
table(enr_aft$maxterm)
# 201220-201720
# END DATES
# 201220 = May 31, 2012
# 201230 = August 31, 2012
# 201240 = December 31, 2012

# create date variable for term_code
#this is the last day of the semester
enr_aft <- enr_aft %>% mutate(term_end_date = ifelse(maxterm == 201220, "2012-06-31",
                                                     ifelse(maxterm == 201230, "2012-08-31",
                                                            ifelse(maxterm == 201240, "2012-12-31",
                                                                   ifelse(maxterm == 201320, "2013-06-31",
                                                                          ifelse(maxterm == 201330, "2013-08-31",
                                                                                 ifelse(maxterm == 201340, "2013-12-31",
                                                                                        ifelse(maxterm == 201420, "2014-06-31",
                                                                                               ifelse(maxterm == 201430, "2014-08-31",
                                                                                                      ifelse(maxterm == 201440, "2014-12-31",
                                                                                                             ifelse(maxterm == 201520, "2015-06-31",
                                                                                                                    ifelse(maxterm == 201530, "2015-08-31",
                                                                                                                           ifelse(maxterm == 201540, "2015-12-31",
                                                                                                                                  ifelse(maxterm == 201620, "2016-06-31",
                                                                                                                                         ifelse(maxterm == 201630, "2016-08-31",
                                                                                                                                                ifelse(maxterm == 201640, "2016-12-31",
                                                                                                                                                       ifelse(maxterm == 201720, "2017-06-31",
                                                                                                                                                              NA)))))))))))))))))

#create age variable from birthdate + term_end_date
#convert BIRTH_DATE to date
enr_aft$BIRTH_DATE <- as.Date(enr_aft$BIRTH_DATE, "%d-%b-%Y")
enr_aft$term_end_date <- as.Date(enr_aft$term_end_date, "%Y-%M-%d")

#calculate age at the end of their max concurrent enrollment term
enr_aft <- enr_aft %>% mutate(age = as.period(interval(start = BIRTH_DATE, end = term_end_date))) %>%
  .$age %>%
  .@year %>%
  mutate(enr_aft, age = .)

#maybe I just want age at the time the data were pulled. They could have taken a concurrent their junior year
enr_aft <- enr_aft %>% mutate(date = Sys.Date())

enr_aft <- enr_aft %>% mutate(age_current = as.period(interval(start = BIRTH_DATE, end = date))) %>%
  .$age_current %>%
  .@year %>%
  mutate(enr_aft, age_current = .)

table(enr_aft$age_current)

#filter out students who are under 18; assume everyone 18 and over graduated from high school and everyone under 18 did not graduate
enr_aft_18 <- enr_aft %>%
  filter(age >= 18)

enr_aft_18_current <- enr_aft %>%
  filter(age_current >= 18)

enr_aft %>% filter(age >= 18) %>%
  ggplot(aes(slcc_after_hs)) +
  geom_bar(stat = "identity")

#U & N = did not attend SLCC; Y = did attend SLCC
enr_aft <- enr_aft %>% mutate(slcc_after_hs = ifelse(ENROLLED_AFTER_HS_GRAD == "Y", "Y", "N"))

#visualize it!
#using age at last term
enr_aft %>% filter(age >= 18) %>%
  ggplot(aes(slcc_after_hs)) +
  geom_bar(fill = "#00A8E1") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1, size = 3) +
  labs(title = "Concurrent students who enrolled at SLCC \nafter graduating high school", 
       x = "Attended SLCC after HS graduation?", y = "Count")

chart_enrollcount <- enr_aft %>% filter(age >= 18) %>%
  ggplot(aes(slcc_after_hs)) +
  geom_bar(fill = "#00A8E1") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1, size = 3) +
  labs(title = "Concurrent students who enrolled at SLCC \nafter graduating high school", 
       x = "Attended SLCC after HS graduation?", y = "Count")

enr_aft_18 <- enr_aft_18 %>% mutate(slcc_after_hs = ifelse(ENROLLED_AFTER_HS_GRAD == "Y", "Y", "N"))

pc_enr_aft_18 <- enr_aft_18 %>%
  group_by(slcc_after_hs)%>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) 

chart_enrollpercent <- pc_enr_aft_18 %>%
  ggplot(aes(slcc_after_hs, freq)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Percent", 
       subtitle = "30% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3) +
  ylim(0, .8)

chart_enroll <- pc_enr_aft_18 %>%
  ggplot(aes(slcc_after_hs, n)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Percent", 
       subtitle = "30% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3)

#using age as of data pull
enr_aft %>% filter(age_current >= 18) %>%
  ggplot(aes(slcc_after_hs)) +
  geom_bar(fill = "#00A8E1") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1, size = 3) +
  labs(title = "Concurrent students who enrolled at SLCC \nafter graduating high school", 
       x = "Attended SLCC after HS graduation?", y = "Count")

pc_enr_aft_18_current <- enr_aft_18_current %>%
  group_by(slcc_after_hs)%>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) 

chart_enrollpc_current <- pc_enr_aft_18_current %>%
  ggplot(aes(slcc_after_hs, freq)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Percent", 
       subtitle = "28% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3) +
  ylim(0, .8)

chart_enroll_current <- pc_enr_aft_18_current %>%
  ggplot(aes(slcc_after_hs, n)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Percent", 
       subtitle = "28% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3)

######How many take concurrent, overall and by semester?#######
ce <- ce %>% mutate(course_year = ifelse(TERM_CODE == 201220, 2012, ifelse(TERM_CODE == 201230, 2012, ifelse(TERM_CODE == 201240, 2012,
                                                                                                             ifelse(TERM_CODE == 201320, 2013, ifelse(TERM_CODE == 201330, 2013, ifelse(TERM_CODE == 201340, 2013,
                                                                                                                                                                                        ifelse(TERM_CODE == 201420, 2014, ifelse(TERM_CODE == 201430, 2014, ifelse(TERM_CODE == 201440, 2014,
                                                                                                                                                                                                                                                                   ifelse(TERM_CODE == 201520, 2015, ifelse(TERM_CODE == 201530, 2015, ifelse(TERM_CODE == 201540, 2015,
                                                                                                                                                                                                                                                                                                                                              ifelse(TERM_CODE == 201620, 2016, ifelse(TERM_CODE == 201630, 2016, ifelse(TERM_CODE == 201640, 2016,
                                                                                                                                                                                                                                                                                                                                                                                                                         2017))))))))))))))))

ce <- ce %>% mutate(course_sem = ifelse(TERM_CODE == 201220, "Spring", ifelse(TERM_CODE == 201230, "Summer", ifelse(TERM_CODE == 201240, "Fall",
                                                                                                                    ifelse(TERM_CODE == 201320, "Spring", ifelse(TERM_CODE == 201330, "Summer", ifelse(TERM_CODE == 201340, "Fall",
                                                                                                                                                                                                       ifelse(TERM_CODE == 201420, "Spring", ifelse(TERM_CODE == 201430, "Summer", ifelse(TERM_CODE == 201440, "Fall",
                                                                                                                                                                                                                                                                                          ifelse(TERM_CODE == 201520, "Spring", ifelse(TERM_CODE == 201530, "Summer", ifelse(TERM_CODE == 201540, "Fall",
                                                                                                                                                                                                                                                                                                                                                                             ifelse(TERM_CODE == 201620, "Spring", ifelse(TERM_CODE == 201630, "Summer", ifelse(TERM_CODE == 201640, "Fall",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Spring"))))))))))))))))

#number of courses taken
chart_count <- ce %>% group_by(TERM_CODE) %>%
  arrange(TERM_CODE) %>%
  filter(TERM_CODE > 201140) %>%
  summarize(n = n(), course_year = max(course_year), course_sem = max(course_sem)) %>%
  ggplot(aes(x = course_year, y = n)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  facet_wrap(~course_sem) +
  geom_text(aes(label = n, vjust = -1)) +
  ylim(0, 11000) +
  labs(title = "Number of concurrent enrollments by semester",
       x = "Year", y = "Count",
       subtitle = "Concurrent enrollments appear to be declining, \nand are least common during summer semester")


#number of unique students enrolled
distinct_by_term <- ce %>% group_by(TERM_CODE) %>%
  filter(TERM_CODE > 201140) %>%
  dplyr::distinct(PIDM) 

distinct_by_term <- distinct_by_term %>% mutate(course_year = ifelse(TERM_CODE == 201220, 2012, ifelse(TERM_CODE == 201230, 2012, ifelse(TERM_CODE == 201240, 2012,
                                                                                                                                         ifelse(TERM_CODE == 201320, 2013, ifelse(TERM_CODE == 201330, 2013, ifelse(TERM_CODE == 201340, 2013,
                                                                                                                                                                                                                    ifelse(TERM_CODE == 201420, 2014, ifelse(TERM_CODE == 201430, 2014, ifelse(TERM_CODE == 201440, 2014,
                                                                                                                                                                                                                                                                                               ifelse(TERM_CODE == 201520, 2015, ifelse(TERM_CODE == 201530, 2015, ifelse(TERM_CODE == 201540, 2015,
                                                                                                                                                                                                                                                                                                                                                                          ifelse(TERM_CODE == 201620, 2016, ifelse(TERM_CODE == 201630, 2016, ifelse(TERM_CODE == 201640, 2016,
                                                                                                                                                                                                                                                                                                                                                                                                                                                     2017))))))))))))))))

distinct_by_term <- distinct_by_term %>% mutate(course_sem = ifelse(TERM_CODE == 201220, "Spring", ifelse(TERM_CODE == 201230, "Summer", ifelse(TERM_CODE == 201240, "Fall",
                                                                                                                                                ifelse(TERM_CODE == 201320, "Spring", ifelse(TERM_CODE == 201330, "Summer", ifelse(TERM_CODE == 201340, "Fall",
                                                                                                                                                                                                                                   ifelse(TERM_CODE == 201420, "Spring", ifelse(TERM_CODE == 201430, "Summer", ifelse(TERM_CODE == 201440, "Fall",
                                                                                                                                                                                                                                                                                                                      ifelse(TERM_CODE == 201520, "Spring", ifelse(TERM_CODE == 201530, "Summer", ifelse(TERM_CODE == 201540, "Fall",
                                                                                                                                                                                                                                                                                                                                                                                                         ifelse(TERM_CODE == 201620, "Spring", ifelse(TERM_CODE == 201630, "Summer", ifelse(TERM_CODE == 201640, "Fall",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Spring"))))))))))))))))


chart_count_unique <- distinct_by_term %>% group_by(TERM_CODE) %>%
  summarize(n = n(), course_year = max(course_year), course_sem = max(course_sem)) %>%
  ggplot(aes(x = course_year, y = n)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  facet_wrap(~course_sem) +
  geom_text(aes(label = n), vjust = -1, size = 3) +
  ylim(0, 7500) +
  labs(title = "Number of concurrent students enrolled by semester",
       x = "Year", y = "Count")

####Average course load per semester####
chart_courseload <- ce %>% 
  group_by(TERM_CODE, course_year, course_sem, PIDM) %>%
  summarize(n = n()) %>%
  group_by(TERM_CODE, course_year, course_sem) %>%
  filter(TERM_CODE > 201140) %>%
  summarize(mean_courses = mean(n)) %>%
  ggplot(aes(x = course_year, y = round(mean_courses, 1))) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  facet_wrap(~course_sem) +
  geom_text(aes(label = round(mean_courses, 1), vjust = -1)) +
  ylim(0, 2) +
  labs(title = "Average concurrent courseload by semester",
       x = "Year", y = "Courseload",
       subtitle = "Concurrent students typically take between 1 and 2 courses, \nand summer semester students take more.")

####Average overall courseload########
chaart_overall <- ce %>% group_by(PIDM) %>%
  summarize(num_courses = n()) %>%
  ungroup() %>%
  group_by(num_courses) %>%
  summarise(n = n()) %>%
  ggplot(aes(num_courses, n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Total number of courses taken by concurrent students",
       x = "Number of courses taken",
       y = "Count",
       subtitle = "The majority of students took 1 or 2 courses over their concurrent enrollment career, \nthough some took many more.") +
  geom_text(aes(label = n), vjust = -1, size = 3)

#####relationship between coursetaking and enrolling######
enr_aft_18 <- enr_aft_18 %>%
  mutate(slcc_after_dummy = ifelse(slcc_after_hs == "Y", 1, 0))

enr_aft_18_current <- enr_aft_18_current %>%
  mutate(slcc_after_dummy = ifelse(slcc_after_hs == "Y", 1, 0))

#simple logistic regression with current age
mod_current <- glm(slcc_after_dummy ~ numcourses, data = enr_aft_18_current, family = binomial(link = "logit"))
summary(mod_current)

#Call:
#  glm(formula = slcc_after_dummy ~ numcourses, family = binomial(link = "logit"), 
#      data = enr_aft_18_current)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.2859  -0.7978  -0.7520   1.4580   1.6741  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.187045   0.018339  -64.73   <2e-16 ***
#  numcourses   0.068504   0.004119   16.63   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 39493  on 33543  degrees of freedom
#Residual deviance: 39223  on 33542  degrees of freedom
#AIC: 39227

#Number of Fisher Scoring iterations: 4

#simple logistic regression with age at max term
mod_maxterm <- glm(slcc_after_dummy ~ numcourses, data = enr_aft_18, family = binomial(link = "logit"))
summary(mod_maxterm)

#Call:
#  glm(formula = slcc_after_dummy ~ numcourses, family = binomial(link = "logit"), 
#      data = enr_aft_18)

#Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
#-1.1745  -0.8408  -0.8059   1.4889   1.6243  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.058223   0.023216  -45.58   <2e-16 ***
#  numcourses   0.050064   0.004578   10.94   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 27058  on 22268  degrees of freedom
#Residual deviance: 26941  on 22267  degrees of freedom
#AIC: 26945

#Number of Fisher Scoring iterations: 4

chart_count
chart_courseload
chart_enrollpercent

######clearinghouse data######
nsc <- read.csv("20171211_clearinghouse.csv")

#create term_code variable for nsc dataset
table(nsc$Enrollment.Begin)
#20090824 - 20171128

nsc <- nsc %>% mutate(TERM_CODE = ifelse(`Enrollment.Begin` %in% 20090101:20090430, 200920,
                                         ifelse(`Enrollment.Begin` %in% 20090501:20090731, 200930,
                                                ifelse(`Enrollment.Begin` %in% 20090801:20091231, 200940,
                                                       ifelse(`Enrollment.Begin` %in% 20100101:20100430, 201020,
                                                              ifelse(`Enrollment.Begin` %in% 20100501:20100731, 201030,
                                                                     ifelse(`Enrollment.Begin` %in% 20100801:20101231, 201040,
                                                                            ifelse(`Enrollment.Begin` %in% 20110101:20110430, 201120,
                                                                                   ifelse(`Enrollment.Begin` %in% 20110501:20110731, 201130,
                                                                                          ifelse(`Enrollment.Begin` %in% 20110801:20111231, 201140,
                                                                                                 ifelse(`Enrollment.Begin` %in% 20120101:20120430, 201220,
                                                                                                        ifelse(`Enrollment.Begin` %in% 20120501:20120731, 201230,
                                                                                                               ifelse(`Enrollment.Begin` %in% 20120801:20121231, 201240,
                                                                                                                      ifelse(`Enrollment.Begin` %in% 20130101:20130430, 201320,
                                                                                                                             ifelse(`Enrollment.Begin` %in% 20130501:20130731, 201330,
                                                                                                                                    ifelse(`Enrollment.Begin` %in% 20130801:20131231, 201340,
                                                                                                                                           ifelse(`Enrollment.Begin` %in% 20140101:20140430, 201420,
                                                                                                                                                  ifelse(`Enrollment.Begin` %in% 20140501:20140731, 201430,
                                                                                                                                                         ifelse(`Enrollment.Begin` %in% 20140801:20141231, 201440,
                                                                                                                                                                ifelse(`Enrollment.Begin` %in% 20150101:20150430, 201520,
                                                                                                                                                                       ifelse(`Enrollment.Begin` %in% 20150501:20150731, 201530,
                                                                                                                                                                              ifelse(`Enrollment.Begin` %in% 20150801:20151231, 201540,
                                                                                                                                                                                     ifelse(`Enrollment.Begin` %in% 20160101:20160430, 201620,
                                                                                                                                                                                            ifelse(`Enrollment.Begin` %in% 20160501:20160731, 201630,
                                                                                                                                                                                                   ifelse(`Enrollment.Begin` %in% 20160801:20161231, 201640,
                                                                                                                                                                                                          ifelse(`Enrollment.Begin` %in% 20170101:20170430, 201720,
                                                                                                                                                                                                                 ifelse(`Enrollment.Begin` %in% 20170501:20170731, 201730,
                                                                                                                                                                                                                        ifelse(`Enrollment.Begin` %in% 20170801:20171231, 201740,
                                                                                                                                                                                                                               NA))))))))))))))))))))))))))))

slcc_cech <- cech %>% filter(`College Name` == "SALT LAKE COMMUNITY COLLEGE")
slcc_cech_begindate <- slcc_cech %>% group_by(`Enrollment Begin`) %>% summarize(n = n() )
slcc_cech <- slcc_cech %>% mutate(term_code = ifelse(`Enrollment Begin` > 20120501 & `Enrollment Begin` < 20120731, "201230", 0))

nsc <- nsc %>% mutate(PIDM = Requester.Return.Field)
nsc <- nsc %>% mutate(source = "nsc")
ce <- ce %>% mutate(source = "banner")

#concatenate PIDM & TERM_CODE to create new variable to join on
nsc <- nsc %>% mutate(PIDMterm = paste0(PIDM, TERM_CODE))
ce <- ce %>% mutate(PIDMterm = paste0(PIDM, TERM_CODE))

#anti join ce and nsc to filter out concurrent enrollment
nsc_noce <- anti_join(ce, nsc, by = c("PIDM", "TERM_CODE"))
ce_noce <- anti_join(nsc, ce, by = c("PIDM", "TERM_CODE"))

ce_noce$College.Name[ce_noce$College.Name=="NA"] <- "No college"
is.na(ce_noce$College.Name)
sum(is.na(ce_noce$College.Name))
ce_noce$College.Name[is.na(ce_noce$College.Name)] <- "No college"
ce_noce %>% replace_na(College.Name = "No college")

ce_nsc <- dplyr::bind_rows(ce, nsc)
#too many NAs
rm(ce_nsc)

ce_tojoin <- ce %>% 
  group_by(PIDM) %>%
  summarise(num_ce = n(), max_ce_term = max(TERM_CODE), ce_gpa = mean(grade_num), ENROLLED_AFTER_HS_GRAD = max(ENROLLED_AFTER_HS_GRAD),
            GENDER = max(GENDER), ETHNICITY = max(ETHNICITY), BIRTH_DATE = max(BIRTH_DATE))

#left join on PIDMterm
cech <- left_join(ce, nsc, by = "PIDMterm")
#this just gives concurrent, but I also want post-high school

#create numeric grade variable
ce <- ce %>% mutate(grade_num = ifelse(FINAL_GRADE == "A", 4, ifelse(FINAL_GRADE == "A-", 3.7, ifelse(FINAL_GRADE == "B+", 3.3, 
                                                                                                      ifelse(FINAL_GRADE == "B", 3, ifelse(FINAL_GRADE == "B-", 2.7, ifelse(FINAL_GRADE == "C+", 2.3,
                                                                                                                                                                            ifelse(FINAL_GRADE == "C", 2, ifelse(FINAL_GRADE == "C-", 1.7, ifelse(FINAL_GRADE == "D+", 1.3,
                                                                                                                                                                                                                                                  ifelse(FINAL_GRADE == "D", 1, ifelse(FINAL_GRADE == "D-", .7, ifelse(FINAL_GRADE == "E", 0,
                                                                                                                                                                                                                                                                                                                       ifelse(FINAL_GRADE == "W", 0, ifelse(FINAL_GRADE == "I", 0, NA)))))))))))))))



cech <- left_join(nsc, ce_tojoin, by = "PIDM")

#filter out concurrent enrollment semesters
cech_no_ce <- cech %>%
  filter(TERM_CODE > max_ce_term)

####which colleges after HS?########
#first college attended after ce
#locate first class by term_code
first_coll <- cech_no_ce %>%
  group_by(PIDM) %>% 
  arrange(TERM_CODE) %>%
  slice(1) %>%
  ungroup
anti_join()
first_coll_sum <- first_coll %>% 
  group_by(`College.Name`) %>% 
  summarize(count = n()) %>% 
  mutate(percent = count/sum(count)*100) %>% 
  arrange(desc(count)) %>%
  slice(1:10)

#reorder so most frequent colleges appear first
first_coll_sum$`College.Name` <- factor(first_coll_sum$`College.Name`, level = first_coll_sum$`College.Name`[order(first_coll_sum$count)])

#plot of first institution attended after HS graduation
chart_firstcoll <- first_coll_sum %>%
  ggplot(aes(`College.Name`, percent)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  labs(title = "First institution attended after high school graduation",
       subtitle = "For 35% of concurrent students, SLCC was the first college they attended after high school.",
       x = "Institution", y = "Percent",
       caption = "*The figure next to each bar represents the count of students who attended that institution") +
  geom_text(aes(label = count), hjust = -0.25) +
  ylim(0, 100)

#ever attended slcc after graduation
college_ever <- cech_no_ce %>%
  group_by(PIDM, `College.Name`) %>%
  summarize(count = 1) %>%
  group_by(`College.Name`) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  mutate(percent = count/25999*100) %>%
  slice(1:10)

#reorder so most frequent colleges appear first
college_ever$`College.Name` <- factor(college_ever$`College.Name`, level = college_ever$`College.Name`[order(college_ever$count)])

#plot
chart_college_ever <- college_ever %>%
  ggplot(aes(`College.Name`, percent))+
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Institutions ever attended after high school graduation",
       subtitle = "43% of concurrent students attended SLCC at some point after graduating from high school.",
       x = "Institution", y = "Percent",
       caption = "*The figure next to each bar represents the count of students who attended that institution") +
  geom_text(aes(label = count), hjust = -0.25) +
  ylim(0, 100)

#########num courses -> enrolling at SLCC?########
#need data viz of % of students who went to slcc at each number of courses
#Count
enr_aft_18_current %>% group_by(slcc_after_hs, numcourses) %>%
  summarize(count = n()) %>%
  ggplot(aes(slcc_after_hs, count)) +
  geom_col(fill = "#00A8E1") +
  facet_wrap(~numcourses) +
  theme_minimal() +
  labs(x = "Enrolled at SLCC after taking concurrent?", y = "Count")

#Percent
percent_table_courses <- enr_aft_18_current %>%
  group_by(numcourses, slcc_after_hs)%>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) 

percent_table_courses %>%
  ggplot(aes(slcc_after_hs, freq)) +
  geom_col(fill = "#00A8E1") +
  facet_wrap(~numcourses) +
  theme_minimal() +
  labs(x = "Enrolled at SLCC after taking concurrent?", y = "Percent") +
  geom_text(aes(label = n), vjust = -1)

#collapse num_courses >15
enr_aft_18_current <- enr_aft_18_current %>% 
  mutate(numcourses_coll = ifelse(numcourses > 15, 16, numcourses))

percent_courses_coll <- enr_aft_18_current %>%
  group_by(numcourses_coll, slcc_after_hs)%>%
  summarize(n = n(), numcourses = min(numcourses)) %>%
  mutate(freq = n/sum(n)) 

percent_courses_coll$numcourses <- as.integer(percent_courses_coll$numcourses)

chart_pccourses <- percent_courses_coll %>% 
  arrange(numcourses) %>%
  ggplot(aes(slcc_after_hs, freq)) +
  geom_col(fill = "#00A8E1") +
  facet_wrap(~numcourses_coll) +
  theme_minimal() +
  labs(x = "Enrolled at SLCC after taking concurrent?", y = "Proportion", 
       title = "Proportion of concurrent students who enrolled at SLCC,\nby total number of concurrent courses taken",
       subtitle = "Number of courses taken was not associated with likelihood of enrolling, \nthough those who took more than 12 courses were less likely to enroll. \nThis may be because they had completed an Associate's degree by high school graduation.",
       caption = "*The figure above each bar represents the count of students in that category") +
  geom_text(aes(label = n), vjust = -.5) +
  ylim(0, 1)

chart_numcourses_enr_smooth <- percent_courses_coll %>% 
  filter(slcc_after_hs == "Y") %>%
  ggplot(aes(numcourses, freq)) +
  geom_smooth(method = "loess") +
  theme_minimal() +
  ylim(0,1) +
  labs(x = "Number of concurrent courses taken", 
       y = "% enrolling at SLCC after HS",
       title = "Relationship between number of courses taken \nand likelihood of enrolling at SLCC",
       subtitle = "Students who take very few or very many concurrent courses \nare less likely to enroll at SLCC, \nbut those who take 8-12 are more likely.")

####what ce courses do students take?####
#create class variable = subject + number (e.g., ENGL 1010)
ce <- ce %>% mutate(class = paste(COURSE_SUBJECT, COURSE_NUMBER))

chart_commoncourses <- ce %>% group_by(class) %>%
  summarize(count=n(), average_grade = mean(grade_num, na.rm=TRUE)) %>%
  arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(class, count), count)) +
  geom_col(fill = "#00A8E1") +
  coord_flip() +
  labs(title = "Most frequent concurrent courses taken",
       x = "Course",
       y = "Count") +
  ylim(0, 10500) +
  geom_text(aes(label = count), hjust = -.25) +
  theme_minimal()

#FIN 1050 = Personal Finance
#HUMA 1100 = Intro to Humanities
#ART 1020 = Intro to Drawing
#FHS 2400 = Marriage and family relations
#MA 1100 = Medical Terminology


first_coll_sum$`College Name` <- factor(first_coll_sum$`College Name`, level = first_coll_sum$`College Name`[order(first_coll_sum$count)])

#students who retake courses: 371 of them
ce %>% group_by(PIDM, class) %>%
  summarise(count = n(),
            mean_grade = mean(grade_num, na.rm = TRUE), 
            min_grade = min(grade_num, na.rm = TRUE), 
            max_grade = max(grade_num, na.rm = TRUE),
            firstterm = min(TERM_CODE),
            lastterm = max(TERM_CODE)) %>%
  filter(count > 1) %>%
  arrange(desc(count))
#108724 concurrent enrollments; 187 retook a concurrent course
# this is 0.17% of enrollments. I'm not going to worry about them.

#most common subject?
chart_commonsubject <- ce %>% group_by(COURSE_SUBJECT) %>%
  summarize(count=n()) %>%
  arrange(desc(count, na.rm = TRUE)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(COURSE_SUBJECT, count), count)) +
  geom_col(fill = "#00A8E1") +
  coord_flip() +
  labs(title = "Most frequent course subject courses taken",
       x = "Course subject",
       y = "Count",
       subtitle = "Math, art, and English are the most popular concurrent subjects") +
  geom_text(aes(label = count), hjust = -.25) +
  ylim(0,25000) +
  theme_minimal()

#####course-taking -> enrolling at SLCC?#####
#do students who take different classes differ in their likelihood of enrolling at SLCC?
#filter by subject, then re-examine charts
ce_math <- ce %>% filter(COURSE_SUBJECT == "MATH")

ce %>% 
  filter(COURSE_SUBJECT == "MATH", unique(PIDM)) %>%
  ggplot(aes(ENROLLED_AFTER_HS_GRAD)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal()

ce_math <- ce_math %>% mutate(date = Sys.Date())

ce_math$BIRTH_DATE <- as.Date(ce_math$BIRTH_DATE, "%d-%b-%Y")

ce_math$age <- mutate(floor(age_calc(ce_math$BIRTH_DATE, units = "years")))

table(ce_math$age)
#15   16   17   18   19   20   21   22   23   24   25   28   29 
#1   26  250 1804 2124 3634 4756 4239 3595 1130    5    1    1 

#filter out students who are under 18; assume everyone 18 and over graduated from high school and everyone under 18 did not graduate
ce_math_18 <- ce_math %>%
  filter(age >= 18)

#U & N = did not attend SLCC; Y = did attend SLCC
ce_math <- ce_math %>% mutate(slcc_after_hs = ifelse(ENROLLED_AFTER_HS_GRAD == "Y", "Y", "N"))

#visualize it!
#students who took a math class: did they enroll at SLCC?
chart_math <- ce_math %>% filter(age >= 18, !duplicated(PIDM)) %>%
  ggplot(aes(slcc_after_hs)) +
  geom_bar(fill = "#00A8E1") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1, size = 3) +
  labs(title = "Concurrent students who enrolled at SLCC \nafter graduating high school", 
       x = "Attended SLCC after HS graduation?", y = "Count") 

########LEFT OFF HERE 20170104##########
chart_math <- ce_math %>% filter(age >= 18, !duplicated(PIDM)) %>%
  ggplot(aes(slcc_after_hs)) +
  geom_col(fill = "#00A8E1") +
  geom_text(aes(label = count(slcc_after_hs)/sum(slcc_after_hs))) +
  theme_minimal() +
  labs(title = "Concurrent students who enrolled at SLCC \nafter graduating high school", 
       x = "Attended SLCC after HS graduation?", y = "Count") 

#each math class taken: did they enroll at SLCC?
chart_math_detail <- ce_math %>% filter(age >= 18, !duplicated(PIDM)) %>%
  ggplot(aes(slcc_after_hs)) +
  geom_bar(fill = "#00A8E1") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1, size = 3) +
  labs(title = "Concurrent students who enrolled at SLCC \nafter graduating high school", 
       x = "Attended SLCC after HS graduation?", y = "Count") +
  facet_wrap(~COURSE_NUMBER)

pc_enr_aft_18 <- enr_aft_18 %>%
  group_by(slcc_after_hs)%>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) 

chart_enrollpercent <- pc_enr_aft_18 %>%
  ggplot(aes(slcc_after_hs, freq)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Percent", 
       subtitle = "30% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3) +
  ylim(0, .8)

chart_enroll <- pc_enr_aft_18 %>%
  ggplot(aes(slcc_after_hs, n)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Count") +
  #subtitle = "30% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3)

chart_enroll_current <- pc_enr_aft_18_current %>%
  ggplot(aes(slcc_after_hs, n)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Count", 
       subtitle = "30% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3)

#using age as of data pull
enr_aft %>% filter(age_current >= 18) %>%
  ggplot(aes(slcc_after_hs)) +
  geom_bar(fill = "#00A8E1") +
  theme_minimal() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1, size = 3) +
  labs(title = "Concurrent students who enrolled at SLCC \nafter graduating high school", 
       x = "Attended SLCC after HS graduation?", y = "Count")

pc_enr_aft_18_current <- enr_aft_18_current %>%
  group_by(slcc_after_hs)%>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) 

chart_enrollpc_current <- pc_enr_aft_18_current %>%
  ggplot(aes(slcc_after_hs, freq)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Percent", 
       subtitle = "28% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3) +
  ylim(0, .8)

chart_enroll_current <- pc_enr_aft_18_current %>%
  ggplot(aes(slcc_after_hs, n)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Attending SLCC after concurrent enrollment", 
       x = "Attended SLCC after HS graduation?", y = "Percent", 
       subtitle = "28% of concurrent students transition to become SLCC students \nafter completing high school") +
  geom_text(aes(label = paste0(round(freq*100, 0), "%")), vjust = -1, size = 3)

#######Awards###########
cech %>% group_by(Degree.Title) %>% summarise(n = n())
#115 unique award titles

########DATA VIZ############
chart_count #num concurrent enrollments by semester                      
chart_count_unique #num concurrent students by semester
chart_courseload #average concurrent courseload by semester
chart_overall #total number of courses taken by concurrent students
chart_firstcoll #first college attended after HS (top 10)
chart_firstcoll_nocoll #first college attended after HS (top 10), including "No college"
chart_college_ever #colleges ever attended after HS (top 10)
chart_enroll #% enrolled at SLCC after HS
chart_numcourses_enr_smooth #smoothed line graph of relationship between number of courses taken and likelihood of enrolling
chart_commoncourses #concurrent courses most frequently taken
chart_commonsubject #most popular concurrent subjects

table(cech$College.Name)
is.na(cech$College.Name)
is.na(cech_no_ce$College.Name)

####Create new dataset with just concurrent students who did not attend college
#filter by age >= 18 (as of date of data pull)
#filter out students who are not in NSC after max_ce_term
#resulting students are (presumably) high school grads who didn't attend college

#new variable: today's date
ce <- ce %>% mutate(date = Sys.Date())
#tell R BIRTH_DATE is a date
ce$BIRTH_DATE <- as.Date(ce$BIRTH_DATE, "%d-%b-%Y")
#create age variable
ce <- ce %>% mutate(age_current = as.period(interval(start = BIRTH_DATE, end = date))) %>%
  .$age_current %>%
  .@year %>%
  mutate(ce, age_current = .)

table(ce$age_current)

#create base table to add nsc to
ce_no_college <- ce %>% 
  group_by(PIDM) %>%
  summarise(num_ce = n(), max_ce_term = max(TERM_CODE), ce_gpa = mean(grade_num), ENROLLED_AFTER_HS_GRAD = max(ENROLLED_AFTER_HS_GRAD),
            GENDER = max(GENDER), ETHNICITY = max(ETHNICITY), age_current = max(age_current)) %>%
  filter(age_current >= 18)

#join nsc
ce_no_college <- left_join(ce_no_college, nsc, by = "PIDM")
#sort by pidm, term code, slice top 1
ce_no_college <- ce_no_college %>% arrange(desc(TERM_CODE.y)) %>%
  group_by(PIDM) %>%
  slice(1)
#now I have one entry for each student who was enrolled in concurrent

#create dataset of completions
completions <- nsc %>% filter(Graduated. == "Y")
#remove completions from main nsc dataset
nsc_no_completions <- nsc %>% filter(Graduated. == "N")

#####START OVER: how many people did not go to college?#####
#start with cech
#add age
cech <- cech %>% mutate(date = Sys.Date())
cech$BIRTH_DATE <- as.Date(cech$BIRTH_DATE, "%d-%b-%Y")

cech <- cech %>% mutate(age_current = as.period(interval(start = BIRTH_DATE, end = date))) %>%
  .$age_current %>%
  .@year %>%
  mutate(cech, age_current = .)

#remove completions
cech_no_completions <- cech %>% filter(Graduated. == "N")

#new dataset: all students who went to SLCC
cech_SLCC <- cech_no_completions %>%
  filter(College.Name == "SALT LAKE COMMUNITY COLLEGE")

#filter out non-concurrent students using max ce term and term code,
#1 row per student
cech_SLCC <- cech_SLCC %>%
  filter(TERM_CODE <= max_ce_term) %>%
  group_by(PIDM) %>%
  slice(1)

#take concurrent students out of nsc dataset, then add them back in?
#no, I only want the students who took concurrent, but then didn't go to college afterwards.
#max term in nsc dataset, compare to max ce term?

#anti join cech_slcc to cech, removes concurrent students
cech_no_concurrent <- anti_join(cech, cech_SLCC, by = "PIDMterm")
cech_no_concurrent <- cech %>% filter(TERM_CODE > max_ce_term)

###this didn't work###
#try again
#start with cech
#add age
cech <- cech %>% mutate(date = Sys.Date())
cech$BIRTH_DATE <- as.Date(cech$BIRTH_DATE, "%d-%b-%Y")

cech <- cech %>% mutate(age_current = as.period(interval(start = BIRTH_DATE, end = date))) %>%
  .$age_current %>%
  .@year %>%
  mutate(cech, age_current = .)

#remove completions
cech_no_completions <- cech %>% filter(Graduated. == "N")

#new variable: max term
max_term <- nsc %>% group_by(PIDM) %>%
  summarise(max_term = max(TERM_CODE, na.rm = TRUE))
sum(is.na(max_term$max_term))

#join max_term to nsc
nsc <- left_join(nsc, max_term, by = "PIDM")

#remove those under 18

#Find Alejandro Aburto
ce %>% filter(LAST_NAME == "Aalona", FIRST_NAME == "Kiana") %>%
  select(class, COURSE_SECTION, TERM_CODE, ENROLLED_AFTER_HS_GRAD, age_current, BANNER_ID)

nsc %>% filter(First.Name == "AALONA", Last.Name == "KIANA") %>%
  select(TERM_CODE)

#create new variable: CE term
ce <- ce %>% mutate(ce_term = "Y")
table(cech$source)

#all students with NO RECORD FOUND by Clearinghouse
#create csv, send to Debbie
no_nsc_record <- nsc %>% filter(Record.Found.Y.N == "N")

#add banner id to no_nsc_record
bannerid <- ce %>% group_by(PIDM) %>%
  summarize(BANNER_ID = max(BANNER_ID))

#join to no_nsc_record
no_nsc_record <- left_join(no_nsc_record, bannerid, by = "PIDM")

write.table(no_nsc_record, "H:/My Documents/Secondary Data Research/Concurrent Enrollment/no_nsc_record", sep = "\t")

table(max_term$max_term)

nsc %>% group_by(PIDM) %>%
  summarise(bannerID = max(Requester.Return.Field)) %>%
  select(College.Name)

#create new variable: if College.Name field is blank, it is a concurrent semester;
#otherwise it's a college semester
nsc <- nsc %>% mutate(con_college = ifelse(College.Name == "", "concurrent",
                                           "college"))

cech <- cech %>% mutate(con_college = ifelse(College.Name == "", 
                                             "concurrent",
                                             "college"))

cech_no_completions <- cech_no_completions %>% mutate(con_college = ifelse(College.Name == "", 
                                                                           "concurrent",
                                                                           "college"))
cech_no_completions <- cech_no_completions %>% mutate(College = College.Name)

toString(cech_no_completions$College)
is.character(cech_no_completions$College)
cech_no_completions$College <- as.character(cech_no_completions$College)

cech_no_completions$'College Name' <- NULL
cech_no_completions <- cech_no_completions %>% 
  mutate(College = ifelse(College == "", "NO COLLEGE", College))

cech_no_completions_firstcollege <- cech_no_completions %>% filter(age_current >= 18) %>%
  filter(TERM_CODE > max_ce_term) %>%
  group_by(PIDM) %>%
  arrange(TERM_CODE) %>%
  slice(1) 
#this filters out everyone who did not go to college, must add them back in
cech_no_completions_nocollege <- cech_no_completions %>% filter(age_current >= 18) %>%
  filter(College == "NO COLLEGE") %>%
  group_by(PIDM)

cech_no_completions_firstcollege <- bind_rows(cech_no_completions_firstcollege, cech_no_completions_nocollege)
table(cech_no_completions_bindall$College)

cech_no_completions_firstcollege %>%
  filter(College == "NO COLLEGE")

cech %>% filter(PIDM == 381431) %>%
  select(TERM_CODE)

cech_no_completions %>% filter(age_current >= 18) %>%
  filter(TERM_CODE > max_ce_term) %>%
  group_by(PIDM) %>%
  arrange(TERM_CODE) %>%
  slice(1) %>%
  select(College, TERM_CODE, GENDER, ETHNICITY, num_ce) %>%
  group_by(College) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice(1:10) %>%
  arrange(desc(Count)) %>%
  ggplot(aes(College, Count)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = Count, hjust = -.1)) +
  ylim(0, 15000)

#data viz: where did students go their first semester of college (including no college)
#must figure out how to arrange it. Why isn't arrange(desc(Count)) working?
#use reorder after ggplot call
#try to add percentages?
chart_firstcoll_nocoll <- cech_no_completions_bindall %>% 
  group_by(College) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(College, Count), Count)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = Count, hjust = -.1)) +
  ylim(0, 15000) +
  labs(title = "First college attended after high school")+
  xlab("College")

########Create profile for SLCC vs. No college students##########
#demographics:
#Age, Race, Gender, Income, Zip code, High school, cohort(max_ce_term?), enrollment status, GPA
profiles <- cech_no_completions_bindall %>%
  group_by(College) %>%
  summarize(count = n(),
            #            age = round(mean(age_current), digits = 2),
            #            pcLT18 = round((sum(age_current < 18)/count)*100, digits = 2),
            pc18 = round((sum(age_current == 18)/count)*100, digits = 2),
            pc19 = round((sum(age_current == 19)/count)*100, digits = 2),
            pc20 = round((sum(age_current == 20)/count)*100, digits = 2),
            pcGT20 = round((sum(age_current > 20)/count)*100, digits = 2),
            pcmaxce201220 = round((sum(max_ce_term == 201220)/count)*100, digits = 2),
            pcmaxce201240 = round((sum(max_ce_term == 201240)/count)*100, digits = 2),
            pcmaxce201320 = round((sum(max_ce_term == 201320)/count)*100, digits = 2),
            pcmaxce201340 = round((sum(max_ce_term == 201340)/count)*100, digits = 2),
            pcmaxce201420 = round((sum(max_ce_term == 201420)/count)*100, digits = 2),
            pcmaxce201440 = round((sum(max_ce_term == 201440)/count)*100, digits = 2),
            pcmaxce201520 = round((sum(max_ce_term == 201520)/count)*100, digits = 2),
            pcmaxce201540 = round((sum(max_ce_term == 201540)/count)*100, digits = 2),
            pcmaxce201620 = round((sum(max_ce_term == 201620)/count)*100, digits = 2),
            pcmaxce201640 = round((sum(max_ce_term == 201640)/count)*100, digits = 2),
            pcmaxce201720 = round((sum(max_ce_term == 201720)/count)*100, digits = 2),
            pcfemale = round((sum(GENDER == "Female")/count)*100, digits = 2),
            pcmale = round((sum(GENDER == "Male")/count)*100, digits = 2),
            pcwhite = round((sum(ETHNICITY == "White")/count)*100, digits = 2),
            pcHispanic = round((sum(ETHNICITY == "Hispanic")/count)*100, digits = 2),
            #            pcOther = round((sum(ETHNICTY != "White" | ETHNICITY != "Hispanic")/count), digits = 2),
            #            cohort = round(mean(max_ce_term), digits = 2),
            num_ce = round(mean(num_ce, na.rm = T), digits = 2),
            ce_GPA = round(mean(ce_gpa, na.rm = T), digits = 2)) %>%
  arrange(desc(count))

table(cech_no_completions_bindall$max_ce_term)
#average age
profiles %>% arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(College, age), age)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = age, hjust = -.1)) +
  ylim(0, 30) +
  labs(title = "Average age of students") +  
  xlab("Average age")

#race
#percent White
profiles %>% arrange(desc(count)) %>%
  #    slice(1:10) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%  
  ggplot(aes(reorder(College, pcwhite), pcwhite)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = pcwhite, hjust = -.1)) +
  ylim(0, 100) +
  labs(title = "Percent identifying as white") +
  xlab("College") +
  ylab("Percent white")  

profiles %>% arrange(desc(count)) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  ggplot(aes(reorder(College, count), pcwhite)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = pcwhite, hjust = -.1)) +
  ylim(0, 100) +
  labs(title = "Percent identifying as white") +
  xlab("College") +
  ylab("Percent white")  

#percent Hispanic
profiles %>% arrange(desc(count)) %>%
  #  slice(1:10) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  ggplot(aes(reorder(College, pcHispanic), pcHispanic)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = pcHispanic, hjust = -.1)) +
  ylim(0, 50) +
  labs(title = "Percent identifying as Hispanic/Latino/a") +
  xlab("College") +
  ylab("Percent Hispanic")  

profiles %>% arrange(desc(count)) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  ggplot(aes(reorder(College, count), pcHispanic)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = pcHispanic, hjust = -.1)) +
  ylim(0, 100) +
  labs(title = "Percent identifying as Hispanic/Latino/a") +
  xlab("College") +
  ylab("Percent Hispanic")  

#percent female
profiles %>% arrange(desc(count)) %>%
  #  slice(1:10) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  ggplot(aes(reorder(College, pcfemale), pcfemale)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = (pcfemale), hjust = -.1)) +
  ylim(0, 100) +
  labs(title = "Percent identifying as female") +
  xlab("College") +
  ylab("Percent female")

profiles %>% arrange(desc(count)) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  ggplot(aes(reorder(College, count), pcfemale)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = pcfemale, hjust = -.1)) +
  ylim(0, 100) +
  labs(title = "Percent identifying as female") +
  xlab("College") +
  ylab("Percent female") 

#GPA
profiles %>% arrange(desc(count)) %>%
  #  slice(1:10) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  ggplot(aes(reorder(College, ce_GPA), ce_GPA)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = (ce_GPA), hjust = -.1)) +
  ylim(0, 4) +
  labs(title = "Average concurrent GPA") +
  xlab("College") +
  ylab("Average GPA")

profiles %>% arrange(desc(count)) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  ggplot(aes(reorder(College, count), ce_GPA)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = ce_GPA, hjust = -.1)) +
  ylim(0, 4) +
  labs(title = "Average GPA") +
  xlab("College") +
  ylab("GPA") 

#num CE courses
profiles %>% arrange(desc(count)) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  #  slice(1:10) %>%
  ggplot(aes(reorder(College, num_ce), num_ce)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = (num_ce), hjust = -.1)) +
  ylim(0, 6) +
  labs(title = "Average number of concurrent courses taken") +
  xlab("College") +
  ylab("Average concurrent courses")

profiles %>% arrange(desc(count)) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" | 
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" |
           College == "UTAH STATE UNIVERSITY" |
           College == "WEBER STATE UNIVERSITY") %>%
  ggplot(aes(reorder(College, count), num_ce)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = num_ce, hjust = -.1)) +
  ylim(0, 5) +
  labs(title = "Average number of concurrent enrollment courses taken") +
  xlab("College") +
  ylab("Number of courses") 

#Cohort -- must figure this one out
profiles %>% arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(College, pcmaxce201720), pcmaxce201720)) +
  geom_col(fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = (pcmaxce201720), hjust = -.1)) +
  ylim(0, 6) +
  labs(title = "Percent of ") +
  xlab("College") +
  ylab("Average concurrent courses")

#########SLCC vs. No college: statistically significant differences?########
#% female, % white, % hispanic, num ce courses, mean gpa
#boxplot of difference in avg GPA between SLCC and no college
cech_SLCC <- cech_no_completions_bindall %>% filter(College == "SALT LAKE COMMUNITY COLLEGE")
cech_nocoll <- cech_no_completions_bindall %>% filter(College == "NO COLLEGE")
cech_uu <- cech_no_completions_bindall %>% filter(College == "UNIVERSITY OF UTAH")
cech_uvu <- cech_no_completions_bindall %>% filter(College == "UTAH VALLEY UNIVERSITY")
boxplot(cech_SLCC$ce_gpa, cech_nocoll$ce_GPA)

#boxplot of GPA
boxplot_GPA <- cech_no_completions_bindall %>% filter(College == "SALT LAKE COMMUNITY COLLEGE" | College == "NO COLLEGE") %>%
  ggplot(aes(College, ce_gpa)) +
  geom_boxplot(fill = "#00A8E1") +
  theme_minimal() +
  ylab("GPA")

t.test_GPA <- t.test(cech_nocoll$ce_gpa, cech_SLCC$ce_gpa, alternative = "two.sided", var.equal = F)

#Welch Two Sample t-test

#data:  cech_nocoll$ce_gpa and cech_SLCC$ce_gpa
#t = -11.224, df = 3407.2, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.3173058 -0.2229344
#sample estimates:
#  mean of x mean of y 
#2.74425   3.01437 
#boxplot of number of courses

boxplot_numcourse <- cech_no_completions_bindall %>% filter(College == "SALT LAKE COMMUNITY COLLEGE" | College == "NO COLLEGE") %>%
  ggplot(aes(College, num_ce)) +
  geom_boxplot(fill = "#00A8E1") +
  theme_minimal() +
  ylab("Number of courses")

t.test_numcourse <- t.test(cech_nocoll$num_ce, cech_SLCC$num_ce, alternative = "two.sided", var.equal = T)

#Welch Two Sample t-test

#data:  cech_nocoll$num_ce and cech_SLCC$num_ce
#t = -39.64, df = 5683.5, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.438046 -1.302512
#sample estimates:
#  mean of x mean of y 
#2.026146  3.396425 

ce$zip5 <- as.double(substr(ce$ADDRESS_ZIP, 1,5))

#isolate zip codes
zips <- ce %>% group_by(PIDM) %>%
  slice(1) %>%
  select(zip5)

#join with clearinghouse data
cech <- left_join(cech, zips, by = "PIDM")
cech_no_ce <- left_join(cech_no_ce, zips, by = "PIDM")
cech_no_completions <- left_join(cech_no_completions, zips, by = "PIDM")
cech_no_completions_bindall <- left_join(cech_no_completions_bindall, zips, by = "PIDM")
cech_no_completions_firstcollege <- left_join(cech_no_completions_firstcollege, zips, by = "PIDM")
cech_no_completions_nocollege <- left_join(cech_no_completions_nocollege, zips, by = "PIDM")
cech_no_concurrent <- left_join(cech_no_concurrent, zips, by = "PIDM")
cech_nocoll <- left_join(cech_nocoll, zips, by = "PIDM")
cech_SLCC <- left_join(cech_SLCC, zips, by = "PIDM")

ce %>% group_by(PIDM) %>%
  slice(1) %>%
  group_by(zip5) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  slice(1:10) %>%
  ggplot(aes(reorder(zip5,-n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Where CE students live, by zip code") +
  xlab("Zip code") +
  ylab("Count") +
  geom_text(aes(label = n), vjust = -1, size = 3)

# A tibble: 10 x 2
#zip5     n
#<dbl> <int>
#1 84095  2573 South Jordan
#2 84020  2166 Draper
#3 84081  2073 West Jordan, WVC, Oquirrh
#4 84065  2063 Riverton, Bluffdale, Draper
#5 84096  1694 Riverton, Herriman, Lark
#6 84088  1303 West Jordan
#7 84092  1288 Sandy, Alta, Draper, Granite
#8 84120  1226 WVC, Salt Lake, Alpine Meadows
#9 84123  1173 Murray, Taylorsville, WVC, West Jordan, Millcreek
#10 84094 1017 Sandy, White City, Wilshire Park

#SLCC students: where do they live?
cech_SLCC %>% group_by(PIDM) %>%
  slice(1) %>%
  group_by(zip5) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  slice(1:10) %>%
  ggplot(aes(reorder(zip5,-n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Where CE students who go to SLCC live, by zip code") +
  xlab("Zip code") +
  ylab("Count") +
  geom_text(aes(label = n), vjust = -1, size = 3)

# A tibble: 10 x 2
#zip5     n
#<dbl> <int>
#1 84095  2010 South Jordan
#2 84081  1530 West Jordan, WVC, Oquirrh
#3 84065  1498 Riverton, Bluffdale, Draper
#4 84020  1397 Draper
#5 84096  1143 Riverton, Herriman, Lark
#6 84088   991 West Jordan
#7 84120   955 WVC, SLC, Alpine Meadows
#8 84123   952 Murray, Taylorsville, WVC, West Jordan, Millcreek
#9 84092   929 Sandy, Alta, Draper, Granite
#10 84094   739 Sandy, White City, Wilshire Park

#No college students: where do they live?
cech_nocoll %>% group_by(PIDM) %>%
  slice(1) %>%
  group_by(zip5) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  slice(1:10) %>%
  ggplot(aes(reorder(zip5,-n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Where CE students who don't go to college live, by zip code") +
  xlab("Zip code") +
  ylab("Count") +
  geom_text(aes(label = n), vjust = -1, size = 3)

# A tibble: 10 x 2
#zip5     n
#<dbl> <int>
#1 84065   189 Riverton, Bluffdale, Draper
#2 84020   174 Draper
#3 84081   174 West Jordan, WVC, Oquirrh
#4 84096   171 Riverton,Herriman, Lark
#5 84095   128 South Jordan
#6 84092   113 Sandy, Alta, Draper, Granite
#7 84120   107 WVC, SLC, Alpine Meadows
#8 84094    95 Sandy, White City, Wilshire Park
#9 84088    89 West Jordan
#10 84119   81 WVC, SLC, Taylorsville, South Salt Lake

cech_uu %>% group_by(PIDM) %>%
  slice(1) %>%
  group_by(zip5) %>%
  summarise(n = n()) %>%
  arrange(desc(n, na.rm = T)) %>%
  na.omit() %>%
  slice(1:10) %>%
  ggplot(aes(reorder(zip5,-n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Where CE students who go to the U live, by zip code") +
  xlab("Zip code") +
  ylab("Count") +
  geom_text(aes(label = n), vjust = -1, size = 3)

# A tibble: 10 x 2
#zip5     n
#<dbl> <int>
#1 84020   224 Draper
#2 84095   213 South Jordan
#3 84092   188 WVC, SLC, Alpine Meadows
#4 84081   176 West Jordan, WVC, Oquirrh
#5 84121   165 Cottonwood, Alta, Holladay, Murray, Brighton, Solitude
#6 84120   147 WVC, SLC, Alpine Meadows
#7 84117   139 Holladay, Millcreek, Murray
#8 84096   130 Riverton,Herriman, Lark
#9 84094   125 Sandy, White City, Wilshire Park
#10 84088  123 West Jordan

cech_uvu %>% group_by(PIDM) %>%
  slice(1) %>%
  group_by(zip5) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  slice(1:10) %>%
  ggplot(aes(reorder(zip5,-n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Where CE students who go to UVU live, by zip code") +
  xlab("Zip code") +
  ylab("Count") +
  geom_text(aes(label = n), vjust = -1, size = 3)

# A tibble: 11 x 2
#zip5     n
#<dbl> <int>
#1 84020   307 Draper
#2 84095   236 South Jordan
#3 84065   196 Riverton, Bluffdale, Draper
#4 84096   171 Riverton,Herriman, Lark
#5 84092   135 Sandy, Alta, Draper, Granite
#6 84081   117 West Jordan, WVC, Oquirrh
#7 84094    86 Sandy, White City, Wilshire Park
#8 84088    75 West Jordan
#9 84070    62 Sandy, Midvale
#10 84660   58 Spanish Fork, Lake Shore, Birdseye, Benjamin, Palmyra

cech_no_completions_bindall %>% group_by(PIDM) %>%
  slice(1) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" |
           College == "UNIVERSITY OF UTAH" | 
           College == "UTAH VALLEY UNIVERSITY" | 
           College == "UTAH STATE UNIVERSITY" | 
           College == "WEBER STATE UNIVERSITY") %>%
  group_by(College, zip5) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  slice(1:5) %>%
  ggplot(aes(reorder(zip5,-n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  labs(title = "Where CE students live, by zip code") +
  xlab("Zip code") +
  ylab("Count") +
  geom_text(aes(label = n), vjust = -1, size = 3) +
  facet_wrap(~College)

#maybe make a map? http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
#map of Utah, SL, Davis counties, with a dot for each student's zip code, colored by the college they went to; 
#would have to jitter and/or divide by 100 so there aren't so many dots; could size the dots according to how many?

######profiles: course-taking######
cecourses <- ce %>% select(PIDM, PIDMterm, TERM_CODE, HIGH_SCHOOL, FINAL_GRADE, CRN, COURSE_SUBJECT, COURSE_NUMBER, COURSE_SECTION,
                           PRIMARY_INSTRUCTOR_PIDM, class, zip5)
cech_firstcoll_cecourses <- left_join(cech_no_completions_firstcollege, cecourses, by = "PIDM")

cech_firstcoll_cecourses <- cech_firstcoll_cecourses %>% mutate(gradenum = ifelse(FINAL_GRADE == "A", 4,
                                                                                  ifelse(FINAL_GRADE == "A-", 3.7,
                                                                                         ifelse(FINAL_GRADE == "B+", 3.3,
                                                                                                ifelse(FINAL_GRADE == "B", 3,
                                                                                                       ifelse(FINAL_GRADE == "B-", 2.7,
                                                                                                              ifelse(FINAL_GRADE == "C+", 2.3,
                                                                                                                     ifelse(FINAL_GRADE == "C", 2,
                                                                                                                            ifelse(FINAL_GRADE == "C-", 1.7,
                                                                                                                                   ifelse(FINAL_GRADE == "D+", 1.3,
                                                                                                                                          ifelse(FINAL_GRADE == "D", 1,
                                                                                                                                                 ifelse(FINAL_GRADE == "D-", .7,
                                                                                                                                                        0))))))))))))

cech_firstcoll_cecourses %>% group_by(College, class) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" |
           College == "UNIVERSITY OF UTAH") %>%
  summarise(n = n(), avg_grade = mean(gradenum)) %>%
  group_by(College) %>%
  mutate(pc_course = round(((n/sum(n))*100),1)) %>%
  arrange(College, desc(pc_course)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(class, pc_course), pc_course)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~College) +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label = pc_course), hjust = -.25, size = 3) +
  #  geom_text(aes(label = round(avg_grade, 1)), hjust = -.25, size = 3) +
  #  ylim(0,100) +
  xlab("Course") +
  ylab("Percent")


cech_firstcoll_cecourses %>% group_by(College, class) %>%
  filter(College == "SALT LAKE COMMUNITY COLLEGE" | 
           College == "NO COLLEGE" |
           College == "UNIVERSITY OF UTAH") %>%
  summarise(n = n(), avg_grade = mean(gradenum)) %>%
  group_by(College) %>%
  mutate(pc_course = round(((n/sum(n))*100),1)) %>%
  arrange(College, desc(pc_course)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(class, pc_course), pc_course)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~College) +
  coord_flip() +
  theme_minimal() +
  #  geom_text(aes(label = pc_course), hjust = -.25, size = 3) +
  geom_text(aes(label = round(avg_grade, 1)), hjust = -.25, size = 3) +
  #  ylim(0,100) +
  xlab("Course") +
  ylab("Percent") +
  labs(caption = "Figures represent the average GPA for students who took the course")

cech_no_completions_firstcollege %>% group_by(College) %>%
  summarise(n = n()) %>%
  mutate(pc_college = round(n/sum(n)*100, 1)) %>%
  arrange(desc(pc_college)) %>%
  slice(1:11) %>%
  ggplot(aes(reorder(College, n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label = pc_college), hjust = -.25, size = 3) +
  xlab("College")+
  ylab("Count") +
  labs(caption = "*Figures to the right of bars represent the percent of all CE students who attended each institution") +
  ylim(0,10000)

cech_no_completions_firstcollege <- cech_no_completions_firstcollege %>% 
  mutate(attendSLCC = ifelse(College == "SALT LAKE COMMUNITY COLLEGE", "Y", "N"))

table(cech_no_completions_firstcollege$attendSLCC)

cech_no_completions_firstcollege %>% 
  group_by(attendSLCC) %>%
  summarize(n = n()) %>%
  mutate(pc_attend = round(n/sum(n)*100,1)) %>%
  ggplot(aes(attendSLCC,n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  #  coord_flip() +
  theme_minimal() +
  geom_text(aes(label = pc_attend), vjust = -1, size = 3) +
  xlab("Attended SLCC after high school?")+
  ylab("Count") +
  ylim(0,20000) +
  labs(capton = "*Figures above bars represent the percent of CE students")

cech_no_completions_firstcollege %>% filter(attendSLCC == "N") %>%
  group_by(College) %>%
  summarise(n = n()) %>%
  mutate(pc_college = round(n/sum(n)*100, 1)) %>%
  arrange(desc(pc_college)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(College, n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label = pc_college), hjust = -.25, size = 3) +
  xlab("College")+
  ylab("Count") +
  labs(caption = "*Figures to the right of bars represent the percent of CE students who attend each institution,\nof students who did not attend SLCC") +
  ylim(0,7500)

cech_no_completions_firstcollege %>% mutate(maxceterm_match_term = ifelse(max_ce_term == TERM_CODE, T, F)) %>%
  group_by(maxceterm_match_term) %>%
  summarize(n = n())

ce %>% group_by(AGE_ON_FIRST_DAY) %>%
  summarise(n = n()) %>%
  mutate(pc = round(n/sum(n)*100, 1)) %>%
  slice(7:12) %>%
  ggplot(aes(AGE_ON_FIRST_DAY,n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  geom_text(aes(label = pc), vjust = -1, size = 3) +
  theme_minimal() +
  labs(caption = "*Figures above the bars represent the percent of CE students at that age") +
  xlab("Age") +
  ylab("Count") 

#####HIGH SCHOOL PIPELINE######
####what is the concurrent yield by district and high school?####
#enrollments
ce %>% group_by(TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  filter(TERM_CODE > 201140) %>%
  summarize(count = n()) 

#headcount
ce %>% group_by(TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  filter(TERM_CODE > 201140, 
         TERM_CODE != 201230, 
         TERM_CODE != 201330,
         TERM_CODE != 201430,
         TERM_CODE != 201530,
         TERM_CODE != 201630) %>%
  slice(1) %>%
  group_by(TERM_CODE, HIGH_SCHOOL) %>%
  summarize(count = n()) %>%
  #  filter(!is.na(HIGH_SCHOOL)) %>%
  arrange(TERM_CODE, desc(count)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(HIGH_SCHOOL, count), count)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Count")

ce %>% group_by(TERM_CODE)

chart_CEbyhighschool <- ce %>% select(DISTRICT, TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  group_by(TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  filter(TERM_CODE > 201520, 
         TERM_CODE != 201230, 
         TERM_CODE != 201330,
         TERM_CODE != 201430,
         TERM_CODE != 201530,
         TERM_CODE != 201630) %>%
  slice(1) %>%
  select(DISTRICT, TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  group_by(TERM_CODE, HIGH_SCHOOL) %>%
  summarize(count = n(), DISTRICT = first(DISTRICT)) %>%
  filter(!is.na(HIGH_SCHOOL)) %>%
  arrange(TERM_CODE, desc(count)) %>%
  mutate(pc = round(count/sum(count)*100, 1)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(HIGH_SCHOOL, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = count), hjust = -.2, size = 3) +
  ylim(0,15) +
  scale_fill_manual("Districts", values = c("#CC8A00","#FFEFBD", "#FFCD00", "#00A8E1", 
                                            "#003865","#833921"))

chart_CEbyhighschool_2015 <- ce %>% select(DISTRICT, TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  group_by(TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  filter(TERM_CODE == 201540 | TERM_CODE == 201620) %>%
  slice(1) %>%
  select(DISTRICT, TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  group_by(TERM_CODE, HIGH_SCHOOL) %>%
  summarize(count = n(), DISTRICT = first(DISTRICT)) %>%
  filter(!is.na(HIGH_SCHOOL)) %>%
  arrange(TERM_CODE, desc(count)) %>%
  mutate(pc = round(count/sum(count)*100, 1)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(HIGH_SCHOOL, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = count), hjust = -.2, size = 3) +
  ylim(0,15) +
  scale_fill_manual("Districts", values = c("Canyons" = "#CC8A00","Charter" = "#FFEFBD", "Granite" = "#FFCD00",
                                            "Jordan" = "#00A8E1", "Murray" = "#003865", "Salt Lake" = "#833921"))

chart_CEbyhighschool_2016 <- ce %>% select(DISTRICT, TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  group_by(TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  filter(TERM_CODE == 201640 | TERM_CODE == 201720) %>%
  slice(1) %>%
  select(DISTRICT, TERM_CODE, HIGH_SCHOOL, PIDM) %>%
  group_by(TERM_CODE, HIGH_SCHOOL) %>%
  summarize(count = n(), DISTRICT = first(DISTRICT)) %>%
  filter(!is.na(HIGH_SCHOOL)) %>%
  arrange(TERM_CODE, desc(count)) %>%
  mutate(pc = round(count/sum(count)*100, 1)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(HIGH_SCHOOL, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = count), hjust = -.2, size = 3) +
  ylim(0,15) +
  scale_fill_manual("Districts", values = c("Canyons" = "#CC8A00","Charter" = "#FFEFBD", "Granite" = "#FFCD00",
                                            "Jordan" = "#00A8E1", "Murray" = "#003865", "Salt Lake" = "#833921"))

high_schools <- ce %>% dplyr::distinct(HIGH_SCHOOL)
high_schools <- high_schools %>% filter(HIGH_SCHOOL != "zz Do Not Use")

high_schools <- sub("High School", "", high_schools$HIGH_SCHOOL, ignore.case = TRUE)

HIGH_SCHOOL = c("Alta High School",
                "Brighton High School",
                "Corner Canyon High School",
                "Hillcrest High School",
                "Jordan High School",
                "Academy For Math Engineering and Science (AMES)",
                "American International School Of Utah",
                "City Academy",
                "East Hollywood High School",
                "Paradigm High School",
                "Salt Lake Center For Science Education",
                "Salt Lake School For The Performing Arts",
                "Summit Academy High School",
                "Utah International Charter School",
                "Cottonwood High School",
                "Cyprus High School",
                "Granger High School",
                "Granite Connection High School",
                "Hunter High School",
                "Kearns High School",
                "Olympus High School",
                "Skyline High School",
                "Taylorsville High School",
                "Bingham High School",
                "Copper Hills High School",
                "Herriman High School",
                "Itineris Early College HS",
                "Riverton High School",
                "Valley High School School",
                "West Jordan High School",
                "Murray High School",
                "East High School",
                "Highland High School",
                "Horizonte Instr and Trn Ctr",
                "West High School",
                "Innovations High School")

DISTRICT = c("Canyons",
             "Canyons",
             "Canyons",
             "Canyons",
             "Canyons",
             "Charter",
             "Charter",
             "Charter",
             "Charter",
             "Charter",
             "Charter",
             "Charter",
             "Charter",
             "Charter",
             "Granite",
             "Granite",
             "Granite",
             "Granite",
             "Granite",
             "Granite",
             "Granite",
             "Granite",
             "Granite",
             "Jordan",
             "Jordan",
             "Jordan",
             "Jordan",
             "Jordan",
             "Jordan",
             "Jordan",
             "Murray",
             "Salt Lake",
             "Salt Lake",
             "Salt Lake",
             "Salt Lake",
             "Charter")

district_id = data.frame(DISTRICT, HIGH_SCHOOL)

ce <- left_join(ce, district_id, by = "HIGH_SCHOOL")

chart_CEbydistrict <- ce %>% group_by(TERM_CODE, DISTRICT, PIDM) %>%
  filter(TERM_CODE > 201520, 
         TERM_CODE != 201230, 
         TERM_CODE != 201330,
         TERM_CODE != 201430,
         TERM_CODE != 201530,
         TERM_CODE != 201630) %>%
  slice(1) %>%
  group_by(TERM_CODE, DISTRICT) %>%
  summarize(n = n()) %>%
  filter(!is.na(DISTRICT)) %>%
  arrange(TERM_CODE, desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1)) %>%
  ggplot(aes(reorder(DISTRICT, pc), pc)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="District", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = n), hjust = -.2, size = 3)+
  ylim(0,55)

#######CE DEMOGRAPHICS#######
#Gender
ce %>% filter(TERM_CODE > 201520) %>%
  group_by(course_year, PIDM) %>%
  summarise(gender = max(GENDER)) %>%
  mutate(male = ifelse(gender == "Male", 1, 0)) %>%
  mutate(female = ifelse(gender == "Female", 1, 0)) %>%
  group_by(course_year) %>%
  summarise(n = n(), male = sum(male), female = sum(female)) %>%
  mutate(pcfemale = female/n) %>%
  ggplot(aes(course_year, pcfemale)) +
  geom_bar(stat = "identity", fill = "#00A8E1")

ce %>% filter(TERM_CODE > 201520) %>%
  group_by(TERM_CODE, PIDM) %>%
  summarise(gender = max(GENDER)) %>%
  mutate(male = ifelse(gender == "Male", 1, 0)) %>%
  mutate(female = ifelse(gender == "Female", 1, 0)) %>%
  group_by(TERM_CODE) %>%
  summarise(n = n(), male = sum(male), female = sum(female)) %>%
  mutate(pcfemale = female/n) %>%
  ggplot(aes(as.factor(TERM_CODE), pcfemale)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  labs(x = "Semester", y = "Percent female") +
  geom_text(aes(label = round(pcfemale, digits = 2), vjust = -1)) +
  ylim(0,.65) +
  theme_minimal()

#num ce by gender
ce %>% group_by(PIDM) %>%
  summarise(n = n(), gender = max(GENDER)) %>%
  group_by(gender) %>%
  filter(gender != "Unknown") %>%
  summarise(count = n(), avg_courses = mean(n), sd = sd(n)) %>%
  ggplot(aes(gender, avg_courses))  +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  labs(x = "Gender", y = "Average number of CE courses taken") +
  geom_text(aes(label = round(avg_courses, digits = 2), vjust = -1)) +
  ylim(0,3.3)
#t-test: p = 0.0018

#race
ce %>% filter(TERM_CODE > 201520) %>%
  group_by(TERM_CODE, PIDM) %>%
  summarise(race = max(ETHNICITY))  %>%
  #  mutate(white = ifelse(race == "White", 1, 0)) %>%
  #  mutate(hispanic = ifelse(race == "Hispanic", 1, 0)) %>%
  #  mutate(otherURM = ifelse(race != "White" & race != "Hispanic",1, 0)) %>%
  #  group_by(TERM_CODE) %>%
  #  summarise(n = n(), white = sum(white), hispanic = sum(hispanic), otherURM = sum(otherURM)) %>%
  #  mutate(pcwhite = white/n) %>%
  #  mutate(pchispanic = hispanic/n) %>%
  #  mutate(pcother = otherURM/n) %>%
  ggplot(aes(race, count(race))) +
  geom_bar(position = "dodge") +
  labs(x = "Semester", y = "Count") +
  #  geom_text(aes(label = round(pcfemale, digits = 2), vjust = -1)) +
  theme_minimal() +
  facet_wrap(~TERM_CODE)

ce %>% filter(TERM_CODE > 201520) %>%
  group_by(PIDM) %>%
  summarise(race = max(ETHNICITY))  %>%
  group_by(race) %>%
  summarise(n =n()) %>%
  mutate(Percent = n/sum(n)) %>%
  filter(race != "Unknown") %>%
  ggplot(aes(reorder(race, Percent), Percent)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  labs(x = "Race/Ethnicity", y = "Percent") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = round(Percent, digits = 2), hjust = -.2)) +
  ylim(0,.8)

ce %>% 
  #filter(TERM_CODE > 201520) %>%
  group_by(PIDM) %>%
  summarise(n = n(), race = max(ETHNICITY))  %>%
  group_by(race) %>%
  summarise(count = n(), avg_courses = mean(n), sd = sd(n)) %>%
  filter(race != "Unknown") %>%
  ggplot(aes(reorder(race, avg_courses), avg_courses)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  labs(x = "Race/Ethnicity", y = "Average number of courses") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = round(avg_courses, digits = 2), hjust = -.2)) +
  ylim(0,4.5) +
  geom_hline(yintercept = 3.016, color = "#FFCD00", size = 1, linetype = "dashed") +
  geom_text(aes(0, 3.016, label = "Average", vjust = -3, hjust = -.1))

ce %>% filter(TERM_CODE > 201520) %>%
  group_by(PIDM) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  summarize(avg_courses = mean(count))

#race by district
ce_new %>% group_by(DISTRICT, ETHNICITY) %>%
  #  filter(ETHNICITY == "Asian") %>%
  summarise(n = n()) %>%
  mutate(pc = n/sum(n)) %>%
  ggplot(aes(DISTRICT, n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~ETHNICITY) +
  theme_minimal() +
  coord_flip()

ce_new %>% group_by(ETHNICITY, DISTRICT) %>%
  filter(ETHNICITY == "Asian") %>%
  summarise(n = n()) %>%
  mutate(pc = n/sum(n)) %>%
  ggplot(aes(reorder(DISTRICT, pc), pc)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~ETHNICITY) +
  theme_minimal() +
  
  coord_flip() +
  labs(x = "District", y = "Percent")

ce_new %>% group_by(ETHNICITY, DISTRICT) %>%
  filter(ETHNICITY != "Unknown" & ETHNICITY != "Non-Resident Alien") %>%
  summarise(n = n()) %>%
  mutate(pc = n/sum(n)) %>%
  filter(DISTRICT != "South Summit" & DISTRICT != "Cache" & DISTRICT != "Alpine" & DISTRICT != "Nebo" &
           DISTRICT != "Private" & DISTRICT != "Weber" & DISTRICT != "Park City") %>%
  ggplot(aes(reorder(DISTRICT,n), pc, fill = ETHNICITY)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  coord_flip() +
  labs(x = "District", y = "Percent") +
  scale_fill_manual("Ethnicity", values = c("#BCDDF4", "#00A8E1", "#003865", "#833921","#CC8A00", "#FFCD00","#FFEFBD")) 

#SL County region
ce %>% filter(TERM_CODE > 201520) %>%
  group_by(PIDM) %>%
  summarise(n = n(), zip = max(zip5)) %>%
  group_by(zip) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(zip != "NA") %>%
  slice(1:10) %>%
  ggplot(aes(reorder(zip, -n), n)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  labs(x = "Zip code", y = "Count") +
  theme_minimal() +
  geom_text(aes(label = n, vjust = -1)) +
  ylim(0,1050)


CITY = c("Copperton",
         "Draper",
         "Magna",
         "Midvale",
         "Riverton",
         "Sandy",
         "West Jordan",
         "West Jordan",
         "West Jordan",
         "Sandy",
         "Sandy",
         "Sandy",
         "Sandy",
         "Sandy",
         "South Jordan",
         "Herriman",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "South Salt Lake",
         "Murray",
         "Emigration Canyon",
         "Millcreek",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "South Salt Lake",
         "Salt Lake",
         "Holladay",
         "Kearns",
         "West Valley City",
         "West Valley City",
         "Cottonwood Heights",
         "Cottonwood Heights",
         "Taylorsville",
         "Millcreek",
         "West Valley City",
         "West Valley City",
         "West Valley City",
         "West Valley City",
         "Taylorsville",
         "West Valley City",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "Salt Lake",
         "Murray",
         "Salt Lake",
         "South Salt Lake",
         "West Valley City",
         "Cottonwood Heights",
         "South Jordan",
         "Tooele")

REGION = c("S",
           "S",
           "W",
           "E",
           "S",
           "E",
           "W",
           "W",
           "W",
           "E",
           "E",
           "E",
           "E",
           "E",
           "S",
           "S",
           "N",
           "N",
           "N",
           "N",
           "N",
           "N",
           "E",
           "N",
           "E",
           "N",
           "N",
           "N",
           "N",
           "N",
           "N",
           "N",
           "E",
           "W",
           "W",
           "W",
           "E",
           "E",
           "W",
           "E",
           "W",
           "W",
           "W",
           "W",
           "W",
           "W",
           "N",
           "N",
           "N",
           "N",
           "N",
           "N",
           "E",
           "N",
           "N",
           "W",
           "E",
           "S",
           "W"
)

zip5 = c(84006,
         84020,
         84044,
         84047,
         84065,
         84070,
         84081,
         84084,
         84088,
         84090,
         84091,
         84092,
         84093,
         84094,
         84095,
         84096,
         84101,
         84102,
         84103,
         84104,
         84105,
         84106,
         84107,
         84108,
         84109,
         84110,
         84111,
         84112,
         84113,
         84114,
         84115,
         84116,
         84117,
         84118,
         84119,
         84120,
         84121,
         84122,
         84123,
         84124,
         84125,
         84126,
         84127,
         84128,
         84129,
         84130,
         84132,
         84134,
         84142,
         84150,
         84151,
         84152,
         84157,
         84158,
         84165,
         84170,
         84171,
         84009,
         84074)

ce$CITY <- NULL

region_id = data.frame(CITY, REGION, zip5)

ce <- left_join(ce, region_id, by = "zip5")

ce %>% filter(TERM_CODE > 201520) %>%
  group_by(PIDM) %>%
  summarise(n = n(), zip = max(zip5), region = first(REGION), city = first(CITY)) %>%
  group_by(zip) %>%
  summarise(n = n(), region = first(region), city = first(city)) %>%
  arrange(desc(n)) %>%
  filter(zip != "NA") %>%
  slice(1:15) %>%
  ggplot(aes(reorder(zip, n), n, fill = region)) +
  #ggplot(aes(reorder(HIGH_SCHOOL, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  labs(x = "Zip code", y = "Count") +
  theme_minimal() +
  geom_text(aes(label = n, hjust = -.1)) +
  ylim(0,1050) +
  scale_fill_manual("Region", values = c("S" = "#00A8E1", "W" = "#FFCD00", "E" = "#833921")) +
  coord_flip()

ce %>% filter(TERM_CODE > 201520) %>%
  group_by(PIDM) %>%
  summarise(n = n(), zip = max(zip5), region = first(REGION), city = first(CITY)) %>%
  group_by(zip) %>%
  summarise(n = n(), region = first(region), city = first(city)) %>%
  mutate(pczip = n/sum(n)) %>%
  arrange(desc(n)) %>%
  filter(zip != "NA") %>%
  slice(1:15) %>%
  ggplot(aes(reorder(zip, pczip), pczip, fill = region)) +
  #ggplot(aes(reorder(HIGH_SCHOOL, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  labs(x = "Zip code", y = "Count", caption = "*Figure to the right of bar represents count of students") +
  theme_minimal() +
  geom_text(aes(label = n, hjust = -.1)) +
  ylim(0,.08) +
  scale_fill_manual("Region", values = c("S" = "#00A8E1", "W" = "#FFCD00", "E" = "#833921", "N" = "#003865")) +
  coord_flip()


ce %>% filter(TERM_CODE == 201540 | TERM_CODE == 201620) %>%
  group_by(TERM_CODE, PIDM) %>%
  summarise(n = n(), zip = max(zip5), region = first(REGION), city = first(CITY)) %>%
  group_by(TERM_CODE, zip) %>%
  summarise(n = n(), region = first(region), city = first(city)) %>%
  mutate(pczip = n/sum(n)) %>%
  arrange(desc(n)) %>%
  filter(zip != "NA") %>%
  slice(1:15) %>%
  ggplot(aes(reorder(zip, pczip), pczip, fill = region)) +
  #ggplot(aes(reorder(HIGH_SCHOOL, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  labs(x = "Zip code", y = "Count", caption = "*Figure to the right of bar represents count of students") +
  theme_minimal() +
  geom_text(aes(label = n, hjust = -.1)) +
  ylim(0,.09) +
  scale_fill_manual("Region", values = c("S" = "#00A8E1", "W" = "#FFCD00", "E" = "#833921", "N" = "#003865")) +
  coord_flip() +
  facet_wrap(~TERM_CODE)

ce %>% filter(TERM_CODE == 201640 | TERM_CODE == 201720) %>%
  group_by(TERM_CODE, PIDM) %>%
  summarise(n = n(), zip = max(zip5), region = first(REGION), city = first(CITY)) %>%
  group_by(TERM_CODE, zip) %>%
  summarise(n = n(), region = first(region), city = first(city)) %>%
  mutate(pczip = n/sum(n)) %>%
  arrange(desc(n)) %>%
  filter(zip != "NA") %>%
  slice(1:15) %>%
  ggplot(aes(reorder(zip, pczip), pczip, fill = region)) +
  #ggplot(aes(reorder(HIGH_SCHOOL, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  labs(x = "Zip code", y = "Count", caption = "*Figure to the right of bar represents count of students") +
  theme_minimal() +
  geom_text(aes(label = n, hjust = -.1)) +
  ylim(0,.085) +
  scale_fill_manual("Region", values = c("S" = "#00A8E1", "W" = "#FFCD00", "E" = "#833921", "N" = "#003865")) +
  coord_flip() +
  facet_wrap(~TERM_CODE)

#Pell eligible
ce_new %>% group_by(EVER_PELL_ELIGIBLE_IND) %>%
  summarize(n = n())

#first gen
ce_new %>% group_by(FIRST_GENERATION_IND) %>%
  summarize(n = n())


######cohort based: within 1 year/5 years#######
enroll_cohorts2 <- cech_no_completions_bindall %>% filter(TERM_CODE > max_ce_term | College == "NO COLLEGE") %>%
  group_by(PIDM) %>%
  arrange(TERM_CODE) %>%
  slice(1) %>%
  select(PIDM, TERM_CODE, max_ce_term, College) %>%
  mutate(enroll_gap = TERM_CODE - max_ce_term) %>%
  mutate(enroll_1_yr = ifelse(enroll_gap <= 120, 1,0)) %>%
  mutate(enroll_5_yr = ifelse(enroll_gap <= 520, 1, 0))

enroll_cohorts2 <- cech_no_completions_bindall %>% filter(TERM_CODE > max_ce_term | College == "NO COLLEGE") %>%
  group_by(PIDM) %>%
  arrange(TERM_CODE) %>%
  slice(1) %>%
  select(PIDM, TERM_CODE, max_ce_term, College) %>%
  mutate(enroll_gap = TERM_CODE - max_ce_term) %>%
  mutate(enroll_1_yr = ifelse(enroll_gap <= 100, 1,0)) %>%
  mutate(enroll_5_yr = ifelse(enroll_gap <= 500, 1, 0))

enroll_cohorts$enroll_1_yr[is.na(enroll_cohorts$enroll_1_yr)] <- 0
enroll_cohorts$enroll_5_yr[is.na(enroll_cohorts$enroll_5_yr)] <- 0

enroll_cohorts2$enroll_1_yr[is.na(enroll_cohorts2$enroll_1_yr)] <- 0
enroll_cohorts2$enroll_5_yr[is.na(enroll_cohorts2$enroll_5_yr)] <- 0

enroll_cohorts %>% ungroup() %>%
  filter(max_ce_term < 201640, 
         max_ce_term != 201230, 
         max_ce_term != 201330, 
         max_ce_term != 201430, 
         max_ce_term != 201530, 
         max_ce_term != 201630) %>%
  summarize(n = n(), sum_1_yr = sum(enroll_1_yr), sum_5_yr = sum(enroll_5_yr), pc_enroll = sum_1_yr/n)
## A tibble: 1 x 4
#n sum_1_yr sum_5_yr pc_enroll
#<int>    <dbl>    <dbl>     <dbl>
# 23803    17635    22062  0.740873

enroll_cohorts2 %>% ungroup() %>%
  filter(max_ce_term < 201640, 
         max_ce_term != 201230, 
         max_ce_term != 201330, 
         max_ce_term != 201430, 
         max_ce_term != 201530, 
         max_ce_term != 201630) %>%
  summarize(n = n(), sum_1_yr = sum(enroll_1_yr), sum_5_yr = sum(enroll_5_yr), pc_enroll = sum_1_yr/n)

# A tibble: 1 x 4
#n sum_1_yr sum_5_yr pc_enroll
#<int>    <dbl>    <dbl>     <dbl>
# 23803    16006    22044 0.6724362

#percent attending college within ONE (inclusive = 120) year of max CE 
enroll_cohorts %>% group_by(max_ce_term) %>%
  summarize(n = n(), enroll_1_yr = sum(enroll_1_yr), enroll_5_yr = sum(enroll_5_yr),
            pc_1_year = enroll_1_yr/n, pc_5_yr = enroll_5_yr/n) %>%
  filter(max_ce_term != 201230, max_ce_term != 201330, max_ce_term != 201430, max_ce_term != 201530, max_ce_term != 201630,
         max_ce_term < 201640) %>%
  mutate(semester = ifelse(max_ce_term == 201220, "Spring", 
                           ifelse(max_ce_term == 201240, "Fall",
                                  ifelse(max_ce_term == 201320, "Spring",
                                         ifelse(max_ce_term == 201340, "Fall", 
                                                ifelse(max_ce_term == 201420, "Spring",
                                                       ifelse(max_ce_term == 201440, "Fall",
                                                              ifelse(max_ce_term == 201520, "Spring",
                                                                     ifelse(max_ce_term == 201540, "Fall",
                                                                            ifelse(max_ce_term == 201620, "Spring",
                                                                                   ifelse(max_ce_term == 201640, "Fall",
                                                                                          ifelse(max_ce_term == 201720, "Spring", 0)))))))))))) %>%
  ggplot(aes(as.character(max_ce_term), pc_1_year, fill = semester)) +
  geom_bar(stat = "identity") +
  labs(x = "Last CE term", y = "Percent attending college within one year of CE", caption = "*Figure above bar represents number of students") +
  geom_text(aes(label = enroll_1_yr), vjust = -.2) +
  scale_fill_manual("Semester", values = c("Fall" = "#FFCD00", "Spring" = "#00A8E1")) +
  theme_minimal() +
  #  coord_flip() +
  ylim(0,1)

#percent attending college within ONE (exclusive = 100) year of max CE 
enroll_cohorts2 %>% group_by(max_ce_term) %>%
  summarize(n = n(), enroll_1_yr = sum(enroll_1_yr), enroll_5_yr = sum(enroll_5_yr),
            pc_1_year = enroll_1_yr/n, pc_5_yr = enroll_5_yr/n) %>%
  filter(max_ce_term != 201230, max_ce_term != 201330, max_ce_term != 201430, max_ce_term != 201530, max_ce_term != 201630,
         max_ce_term < 201640) %>%
  mutate(semester = ifelse(max_ce_term == 201220, "Spring", 
                           ifelse(max_ce_term == 201240, "Fall",
                                  ifelse(max_ce_term == 201320, "Spring",
                                         ifelse(max_ce_term == 201340, "Fall", 
                                                ifelse(max_ce_term == 201420, "Spring",
                                                       ifelse(max_ce_term == 201440, "Fall",
                                                              ifelse(max_ce_term == 201520, "Spring",
                                                                     ifelse(max_ce_term == 201540, "Fall",
                                                                            ifelse(max_ce_term == 201620, "Spring",
                                                                                   ifelse(max_ce_term == 201640, "Fall",
                                                                                          ifelse(max_ce_term == 201720, "Spring", 0)))))))))))) %>%
  ggplot(aes(as.character(max_ce_term), pc_1_year, fill = semester)) +
  geom_bar(stat = "identity") +
  labs(x = "Last CE term", y = "Percent attending college within one year of CE", caption = "*Figure above bar represents number of students") +
  geom_text(aes(label = enroll_1_yr), vjust = -.2) +
  scale_fill_manual("Semester", values = c("Fall" = "#FFCD00", "Spring" = "#00A8E1")) +
  theme_minimal() +
  #  coord_flip() +
  ylim(0,1) +
  geom_hline(yintercept = .67, color = "#833921", size = 1, linetype = "dashed") +
  geom_text(aes(0, .67, label = "Average", vjust = 1, hjust = -.1))

enroll_cohorts2 %>% group_by(max_ce_term) %>%
  summarize(n = n(), enroll_1_yr = sum(enroll_1_yr), enroll_5_yr = sum(enroll_5_yr),
            pc_1_year = enroll_1_yr/n, pc_5_yr = enroll_5_yr/n) %>%
  filter(max_ce_term == 201220 | max_ce_term == 201320 | max_ce_term == 201420 | max_ce_term == 201520 | max_ce_term == 201620) %>%
  ungroup() %>%
  summarize(avg = mean(pc_1_year))
enroll_cohorts2 %>% group_by(max_ce_term) %>%
  summarize(n = n(), enroll_1_yr = sum(enroll_1_yr), enroll_5_yr = sum(enroll_5_yr),
            pc_1_year = enroll_1_yr/n, pc_5_yr = enroll_5_yr/n) %>%
  filter(max_ce_term == 201220 | max_ce_term == 201320 | max_ce_term == 201420 | max_ce_term == 201520 | max_ce_term == 201620) %>%
  ggplot(aes(as.character(max_ce_term), pc_1_year)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  labs(x = "Last CE term", y = "Percent attending college within one year of CE", caption = "*Figure above bar represents number of students") +
  #  geom_text(aes(label = enroll_1_yr), vjust = -.2) +
  theme_minimal() +
  ylim(0,1) +
  geom_hline(yintercept = .717, color = "#FFCD00", size = 1, linetype = "dashed") +
  geom_text(aes(0, .717, label = "Average", vjust = 1, hjust = -.1))

enroll_cohorts2 %>% group_by(max_ce_term) %>%
  summarize(n = n(), enroll_1_yr = sum(enroll_1_yr), enroll_5_yr = sum(enroll_5_yr),
            pc_1_year = enroll_1_yr/n, pc_5_yr = enroll_5_yr/n) %>%
  filter(max_ce_term == 201240 | max_ce_term == 201340 | max_ce_term == 201440 | max_ce_term == 201540) %>%
  ungroup() %>%
  summarize(avg = mean(pc_1_year))

####where are the missing students?####
rm(enroll_cohorts)
ce %>% filter(TERM_CODE > 201220) %>%
  group_by(TERM_CODE) %>%
  summarize(n = n(), SCH = n*3, hs_na = sum(is.na(HIGH_SCHOOL)), hs_na_SCH = hs_na*3)

# A tibble: 15 x 4
#  TERM_CODE     n   SCH hs_na
#       <int> <int> <dbl> <int>
# 1    201230   581  1743   241
# 2    201240 10326 30978  5361
# 3    201320  8058 24174  4083
# 4    201330   528  1584   242
# 5    201340 10622 31866  6048
# 6    201420  6768 20304  3841
# 7    201430   631  1893   337
# 8    201440 10560 31680  6650
# 9    201520  6719 20157  4216
#10    201530   612  1836   443
#11    201540  9253 27759  6832
#12    201620  6109 18327  4572
#13    201630   269   807   246
#14    201640 10290 30870  9140
#15    201720  6374 19122  5612

########NEW high school data##########
ce_new <- ce_new %>% group_by(PIDM) %>%
  mutate(max_ce_term = max(TERM_CODE))

highschools <- ce_new %>% group_by(CAMPUS)%>%
  summarize(n = n()) %>%
  arrange(CAMPUS)

DISTRICT <- c("Canyons",
              "Charter",
              "Jordan",
              "Davis",
              "Canyons",
              "Canyons",
              "Charter",
              "Charter",
              "Davis",
              "Charter",
              "Jordan",
              "Canyons",
              "Granite",
              "Granite",
              "Davis",
              "Salt Lake",
              "Charter",
              "Weber",
              "Granite",
              "Granite",
              "Granite",
              "Tooele",
              "Jordan",
              "Salt Lake",
              "Canyons",
              "Salt Lake",
              "Granite",
              "Charter",
              "Charter",
              "Jordan",
              "SLCC",
              "SLCC",
              "Jordan",
              "Jordan",
              "Private",
              "Granite",
              "Davis",
              "Private",
              "SLCC",
              "SLCC",
              "SLCC",
              "Cache",
              "Alpine",
              "Murray",
              "South Summit",
              "Davis",
              "Charter",
              "Granite",
              "Charter",
              "Park City",
              "Charter",
              "Jordan",
              "Charter",
              "Salt Lake",
              "Granite",
              "SLCC",
              "SLCC",
              "SLCC",
              "Nebo",
              "Tooele",
              "Charter",
              "Davis",
              "SLCC",
              "Granite",
              "SLCC",
              "Alpine",
              "Tooele",
              "Tooele",
              "Charter",
              "Davis",
              "Weber",
              "Salt Lake",
              "Jordan",
              "SLCC",
              "SLCC",
              "Davis"
)

CAMPUS <- c("Alta High School",
            "American International H.S.",
            "Bingham High School",
            "Bountiful High School",
            "Brighton High School",
            "Canyons Tech Ed Cntr-Sandy",
            "Charter School-No District",
            "City Academy",
            "Clearfield High School",
            "Concordia Preparatory School",
            "Copper Hills High School",
            "Corner Canyon High School",
            "Cottonwood High School",
            "Cyprus High School",
            "Davis High School",
            "East High School",
            "East Hollywood High School",
            "Fremont High School",
            "Granger High School",
            "Granite District Center",
            "Granite Technical Institute",
            "Grantsville High School",
            "Herriman High School",
            "Highland High School",
            "Hillcrest High School",
            "Horizonte High School",
            "Hunter High School",
            "Innovations HIgh School",
            "Itineris Early College H.S.",
            "Jordan Applied Tech Ctr-South",
            "Jordan Campus",
            "Jordan Campus Concurrent Enrl.",
            "Jordan High School",
            "Jordn Applied Tech Ctr - North",
            "Juan Diego High School",
            "Kearns High School",
            "Layton High School",
            "Liahona Academy",
            "Library Square Concurrent Enrl",
            "Meadowbrook Campus Concurrent",
            "Miller Campus Concurrent Enrl.",
            "Mountain Crest High School",
            "Mountain View High School",
            "Murray High School",
            "North Summit High School",
            "Northridge High School",
            "NUAMES Northern Utah Academy",
            "Olympus High School",
            "Paradigm High School",
            "Park City High School",
            "Providence Hall Charter School",
            "Riverton High School",
            "Salt Lake Cntr for Science Ed.",
            "Salt Lake Tech. Center",
            "Skyline High School",
            "SLCC Online",
            "SLCC Online Concurrent Enrl.",
            "South City Campus Concurrent",
            "Spanish Fork High School",
            "Stansbury High School",
            "Summit Academy High School",
            "Syracuse High School",
            "Taylorsville Campus Concurrent",
            "Taylorsville High School",
            "Taylorsville Redwood Campus",
            "Timpanogos High School",
            "Tooele Comunity Learning Cntr",
            "Tooele High School",
            "Vanguard Academy",
            "Viewmont High School",
            "Weber High School",
            "West High School",
            "West Jordan High School",
            "West Valley Ctr Concurrent Enr",
            "Westpointe Cntr Concurrent Enr",
            "Woods Cross High School"
)

district_xwalk <- data.frame(DISTRICT, CAMPUS)
ce_new <- left_join(ce_new, district_xwalk, by = "CAMPUS")

####REVISIT: what is the concurrent yield by district and high school?####
#enrollments
ce_new %>% group_by(TERM_CODE, CAMPUS, PIDM) %>%
  filter(TERM_CODE > 201140) %>%
  summarize(count = n()) 

#headcount
ce_new %>% group_by(TERM_CODE, CAMPUS, PIDM) %>%
  filter(TERM_CODE > 201140, 
         TERM_CODE != 201230, 
         TERM_CODE != 201330,
         TERM_CODE != 201430,
         TERM_CODE != 201530,
         TERM_CODE != 201630) %>%
  slice(1) %>%
  group_by(TERM_CODE, CAMPUS) %>%
  summarize(count = n()) %>%
  #  filter(!is.na(HIGH_SCHOOL)) %>%
  arrange(TERM_CODE, desc(count)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(CAMPUS, count), count)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Count")

chart_CEbyhighschool <- ce_new %>% select(DISTRICT, TERM_CODE, CAMPUS, PIDM) %>%
  group_by(TERM_CODE, CAMPUS, PIDM) %>%
  filter(TERM_CODE > 201520, 
         TERM_CODE != 201230, 
         TERM_CODE != 201330,
         TERM_CODE != 201430,
         TERM_CODE != 201530,
         TERM_CODE != 201630) %>%
  slice(1) %>%
  select(DISTRICT, TERM_CODE, CAMPUS, PIDM) %>%
  group_by(TERM_CODE, CAMPUS) %>%
  summarize(count = n(), DISTRICT = first(DISTRICT)) %>%
  arrange(TERM_CODE, desc(count)) %>%
  mutate(pc = round(count/sum(count)*100, 1)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(CAMPUS, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = count), hjust = -.2, size = 3) +
  ylim(0,20) +
  scale_fill_manual("District", values = c("#BCDDF4", "#00A8E1", "#003865", "#833921","#CC8A00", "#FFCD00","#FFEFBD"))

ce_new %>% select(DISTRICT, TERM_CODE, CAMPUS, PIDM) %>%
  group_by(DISTRICT) %>%
  summarize(count = n()) %>%
  mutate(pc = round(count/sum(count)*100, 1)) %>%
  arrange(desc(count)) %>%
  ggplot(aes(reorder(DISTRICT, pc), pc)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  labs(x="District", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = count), hjust = -.2, size = 3)

ce_new %>% select(DISTRICT, TERM_CODE, CAMPUS, PIDM) %>%
  group_by(CAMPUS) %>%
  summarize(count = n(), DISTRICT = first(DISTRICT)) %>%
  mutate(pc = round(count/sum(count)*100, 1)) %>%
  arrange(desc(count)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(CAMPUS, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = count), hjust = -.2, size = 3) +
  ylim(0,10) +
  #  scale_fill_brewer(palette = "YlGnBu")+
  scale_fill_manual("District", values = c("#BCDDF4", "#00A8E1", "#003865", "#833921","#CC8A00", "#FFCD00","#FFEFBD"))

chart_CEbyhighschool_2015 <- ce_new %>% select(DISTRICT, TERM_CODE, CAMPUS, PIDM) %>%
  group_by(TERM_CODE, CAMPUS, PIDM) %>%
  filter(TERM_CODE == 201540 | TERM_CODE == 201620) %>%
  slice(1) %>%
  select(DISTRICT, TERM_CODE, CAMPUS, PIDM) %>%
  group_by(TERM_CODE, CAMPUS) %>%
  summarize(count = n(), DISTRICT = first(DISTRICT)) %>%
  filter(!is.na(CAMPUS)) %>%
  arrange(TERM_CODE, desc(count)) %>%
  mutate(pc = round(count/sum(count)*100, 1)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(CAMPUS, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = count), hjust = -.2, size = 3) +
  ylim(0,10) +
  #  scale_fill_brewer(palette = "YlGnBu")+
  scale_fill_manual("District", values = c("#BCDDF4", "#00A8E1", "#003865", "#833921","#CC8A00", "#FFCD00","#FFEFBD"))

chart_CEbyhighschool_2016 <- ce_new %>% select(DISTRICT, TERM_CODE, CAMPUS, PIDM) %>%
  group_by(TERM_CODE, CAMPUS, PIDM) %>%
  filter(TERM_CODE == 201640 | TERM_CODE == 201720) %>%
  slice(1) %>%
  select(DISTRICT, TERM_CODE, CAMPUS, PIDM) %>%
  group_by(TERM_CODE, CAMPUS) %>%
  summarize(count = n(), DISTRICT = first(DISTRICT)) %>%
  filter(!is.na(CAMPUS)) %>%
  arrange(TERM_CODE, desc(count)) %>%
  mutate(pc = round(count/sum(count)*100, 0)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(CAMPUS, pc), pc, fill = DISTRICT)) +
  geom_bar(stat = "identity") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="High school", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = count), hjust = -.2, size = 3) +
  ylim(0,10) +
  #  scale_fill_brewer(palette = "YlGnBu")
  scale_fill_manual("District", values = c("#BCDDF4", "#00A8E1", "#003865", "#833921","#CC8A00", "#FFCD00","#FFEFBD"))

chart_CEbydistrict <- ce_new %>% group_by(TERM_CODE, DISTRICT, PIDM) %>%
  filter(TERM_CODE > 201520, 
         TERM_CODE != 201230, 
         TERM_CODE != 201330,
         TERM_CODE != 201430,
         TERM_CODE != 201530,
         TERM_CODE != 201630) %>%
  slice(1) %>%
  group_by(TERM_CODE, DISTRICT) %>%
  summarize(n = n()) %>%
  filter(!is.na(DISTRICT)) %>%
  arrange(TERM_CODE, desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1)) %>%
  ggplot(aes(reorder(DISTRICT, pc), pc)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~TERM_CODE) +
  theme_minimal() +
  coord_flip() +
  labs(x="District", y = "Percent", caption = "*Figure to the right of bar represents count of students") +
  geom_text(aes(label = n), hjust = -.2, size = 3)+
  ylim(0,40)

campus_join <- ce_new %>% group_by(PIDM) %>%
  summarize(CAMPUS = first(CAMPUS))

enroll_cohorts2 <- left_join(enroll_cohorts2, campus_join, by = "PIDM")

#High school: students who didn't go to college within 1 year
enroll_cohorts2 %>% filter(max_ce_term != 201240, max_ce_term != 201340, max_ce_term != 201440, max_ce_term != 201540,  
                           enroll_1_yr == 0) %>%
  group_by(CAMPUS) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

enroll_cohorts2 %>% filter(enroll_1_yr == 0) %>%
  group_by(DISTRICT) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

enroll_cohorts2 %>% filter(max_ce_term != 201240, max_ce_term != 201340, max_ce_term != 201440, max_ce_term != 201540, enroll_1_yr == 0) %>%
  group_by(DISTRICT) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

#U Students
enroll_cohorts2 %>% filter(enroll_1_yr == 1, College == "UNIVERSITY OF UTAH") %>%
  group_by(CAMPUS) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

enroll_cohorts2 %>% filter(enroll_1_yr == 1, College == "UNIVERSITY OF UTAH") %>%
  group_by(DISTRICT) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

#UVU students
enroll_cohorts2 %>% filter(enroll_1_yr == 1, College == "UTAH VALLEY UNIVERSITY") %>%
  group_by(CAMPUS) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

enroll_cohorts2 %>% filter(enroll_1_yr == 1, College == "UTAH VALLEY UNIVERSITY") %>%
  group_by(DISTRICT) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

#SLCC students
enroll_cohorts2 %>% filter(enroll_1_yr == 1, College == "SALT LAKE COMMUNITY COLLEGE") %>%
  group_by(CAMPUS) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

enroll_cohorts2 %>% filter(enroll_1_yr == 1, College == "SALT LAKE COMMUNITY COLLEGE") %>%
  group_by(DISTRICT) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pc = round(n/sum(n)*100, 1))

enroll_cohorts2 <- left_join(enroll_cohorts2, district_xwalk, by = "CAMPUS")

#where do former CE students go to college? Within 1 year
enroll_cohorts2 %>% filter(enroll_1_yr == 1) %>%
  filter(max_ce_term > 201240, max_ce_term != 201330, max_ce_term != 201430, max_ce_term != 201530, max_ce_term != 201630) %>%
  group_by(max_ce_term, College) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:5) %>%
  ggplot(aes(reorder(College, n), n)) +
  geom_col(stat = "identity", fill = "#00A8E1") +
  facet_wrap(~max_ce_term) +
  coord_flip() +
  theme_minimal()

#COUNT
enroll_cohorts2 %>% filter(enroll_1_yr == 1) %>%
  filter(max_ce_term == 201220 | max_ce_term == 201320 | max_ce_term == 201420 | 
           max_ce_term == 201520 | max_ce_term == 201620 | max_ce_term == 201720) %>%
  group_by(max_ce_term, College) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice(1:5) %>%
  ggplot(aes(x = as.character(max_ce_term), y = n, group = College)) +
  geom_line(aes(color = College), size = .7) +
  theme_minimal() +
  labs(x = "Last CE term", y = "Count") +
  scale_color_manual(values = c("#BCDDF4", "#00A8E1", "#003865", "#833921","#CC8A00", "#FFCD00"))

#PERCENT
enroll_cohorts2 %>% filter(enroll_1_yr == 1) %>%
  filter(max_ce_term == 201220 | max_ce_term == 201320 | max_ce_term == 201420 | 
           max_ce_term == 201520 | max_ce_term == 201620 | max_ce_term == 201720) %>%
  group_by(max_ce_term, College) %>%
  summarise(n = n()) %>%
  mutate(pc = n/sum(n)) %>%
  arrange(desc(pc)) %>%
  slice(1:5) %>%
  ggplot(aes(x = as.character(max_ce_term), y = pc, group = College)) +
  geom_line(aes(color = College), size = .7) +
  theme_minimal() +
  labs(x = "Last CE term", y = "Percent") +
  scale_color_manual(values = c("#BCDDF4", "#00A8E1", "#003865", "#833921","#CC8A00", "#FFCD00"))

enroll_cohorts2 %>% filter(enroll_1_yr == 1) %>%
  group_by(College) %>%
  summarise(n = n()) %>%
  mutate(pc = n/sum(n)) %>%
  arrange(desc(pc)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(College, pc), pc)) +
  geom_bar(stat = "identity", fill = "#00A8E1") +
  theme_minimal() +
  coord_flip() +
  labs(x = "College", y = "Percent") 
