library(tidyverse)
library(infer)
library(coin)
library(broom)
library(modelr)
library(ggmosaic)

## Introduction 
# To investigate the question of whether university education is worth the investment,
# we've decided to explore the general health of individuals in comparison with their education.
# Specifically, we've broken down our analysis into 3 categories; Physical Health, Mental Health, and Substance Addictions.
# Students may build unhealthy habits such as stress eating or poor diet during their time in school
# in order to adapt to their environment whether due to the stress or the lack of time to prepare healthy meals.
# We'd like to explore whether there are consequences towards their physical well-being and if so, how severe are the consequences.

# Furthermore, statistically speaking, more than half of Canadian university students self-diagnose themselves to feeling extreme anxiety due.
# Although most of these individuals tend to recover from these stressful events, being exposed to frequent periods of anxiety may
# increase the chances for other mental disorders to develop. We'd like to explore whether spending too much time in school may be a
# risk factor for mental disorders.

# Lastly, some students may resort to substances such that alcohol or cannabis to help themselves cope with the stressful
# environment of university. Depending on the substance and the frequency of use, the consequences of substance abuse may
# stay with the user far past after they've finished their education. We've like to explore whether schooling has a correlation
# with the chances for addiction or usage in individuals.

## Data 
# The "Health Promotion Survey" data on CHASS will be used to explore physical health relations to education.
# The data is collected by observing adults all across Canada and surveying them about factors which may affect their physical well being.
# The data is collected in the '90s, thus a lot of cultural norms was changed since. Thus when analyzing this data,
# we need to consider the fact that these trends may not project the current trends education may have on physical health but
# can provide us with a general idea. We plan on using the BMI to categorize individuals into weight groups such as;
# "Underweight, Healthy, Overweight, Obese", and check if there's a trend in physical well-being within different education groups.

## Analysis 
phy_health = read_csv('data/physical_health_canada.csv')

#### Data Exploration
# Rename variables
phy_health = phy_health %>% rename("sex" = "l1", "height" = "a3cm", "weight" = "a4kg",
                                   "educ" = "r2", "age" = "dvage")
# Filter invalid data
phy_health = phy_health %>% filter(educ != 10 & educ != 99 & educ != 1) %>% 
    filter(weight != 999) %>% filter(height != 999) %>%
    filter(age != 1 & age != 2) %>% 
    filter(bmi >= 12) %>% filter(bmi <= 45)

# Group data by their respective groups and apply bootstrap sampling
phy_male_data = phy_health %>% filter(sex == 2)
phy_male_ele = phy_male_data %>% filter(educ == 2 | educ == 3) %>% sample_n(5000, replace = TRUE)
phy_male_sec = phy_male_data %>% filter(educ == 4 | educ == 5) %>% sample_n(5000, replace = TRUE)
phy_male_col = phy_male_data %>% filter(educ == 6 | educ == 7) %>% sample_n(5000, replace = TRUE)
phy_male_uni = phy_male_data %>% filter(educ == 8 | educ == 9) %>% sample_n(5000, replace = TRUE)
phy_female_data = phy_health %>%  filter(sex == 1)
phy_female_ele = phy_female_data %>% filter(educ == 2| educ == 3) %>% sample_n(5000, replace = TRUE)
phy_female_sec = phy_female_data %>% filter(educ == 4| educ == 5) %>% sample_n(5000, replace = TRUE)
phy_female_col = phy_female_data %>% filter(educ == 6| educ == 7) %>% sample_n(5000, replace = TRUE)
phy_female_uni = phy_female_data %>% filter(educ == 8| educ == 9) %>% sample_n(5000, replace = TRUE)

# Create a combined density plot with all the grouped data sets
ggplot() + geom_density(data = phy_male_ele, aes(x=bmi, color='#navyblue'), kernel = "gaussian", bw=0.65) +
    geom_density(data = phy_male_sec, aes(x=bmi, color='blue'), kernel = "gaussian", bw=0.65) + 
    geom_density(data = phy_male_col, aes(x=bmi, color='dodgerblue'), kernel = "gaussian", bw=0.65) +
    geom_density(data = phy_male_uni, aes(x=bmi, color='turquoise4'), kernel = "gaussian", bw=0.65) + 
    geom_density(data = phy_female_ele, aes(x=bmi, color='darksalmon'), kernel = "gaussian", bw=0.65) + 
    geom_density(data = phy_female_sec, aes(x=bmi, color='red'), kernel = "gaussian", bw=0.65) + 
    geom_density(data = phy_female_col, aes(x=bmi, color='darkorange4'), kernel = "gaussian", bw=0.65) +
    geom_density(data = phy_female_uni, aes(x=bmi, color='chocolate2'), kernel = "gaussian", bw=0.65) +
    xlab("BMI") + ylab("Density") + ggtitle("BMI in Comparison to Education Levels") +
    geom_vline(xintercept = 18.5) + geom_vline(xintercept = 25) + geom_vline(xintercept = 30) + geom_vline(xintercept = 40) +
    scale_color_discrete(name="Categories", labels=c('M Elementary','M Secondary', 'M College', 'M University', 'F Elementary', 'F Secondary', 'F College', 'F University'))

# Calculate the ratios of individuals in each category seperated by their education
m_e_count = phy_male_ele %>% count()
male_ele_underweight = phy_male_ele %>% filter(bmi <= 18.5) %>% count()/ m_e_count
male_ele_healthy = phy_male_ele %>% filter(bmi > 18.5 & bmi <= 25) %>% count()/ m_e_count
male_ele_overweight = phy_male_ele %>% filter(bmi > 25 & bmi <= 30) %>% count()/ m_e_count
male_ele_obese = phy_male_ele %>% filter(bmi > 30) %>% count()/ m_e_count
m_s_count = phy_male_sec %>% count()
male_sec_underweight = phy_male_sec %>% filter(bmi <= 18.5) %>% count()/ m_s_count
male_sec_healthy = phy_male_sec %>% filter(bmi > 18.5 & bmi <= 25) %>% count()/ m_s_count
male_sec_overweight = phy_male_sec %>% filter(bmi > 25 & bmi <= 30) %>% count()/ m_s_count
male_sec_obese = phy_male_sec %>% filter(bmi > 30) %>% count()/m_s_count
m_c_count = phy_male_col %>% count()
male_col_underweight = phy_male_col %>% filter(bmi <= 18.5) %>% count()/ m_c_count
male_col_healthy = phy_male_col %>% filter(bmi > 18.5 & bmi <= 25) %>% count()/ m_c_count
male_col_overweight = phy_male_col %>% filter(bmi > 25 & bmi <= 30) %>% count()/ m_c_count
male_col_obese = phy_male_col %>% filter(bmi > 30) %>% count()/ m_c_count
m_u_count = phy_male_uni %>% count()
male_uni_underweight = phy_male_uni %>% filter(bmi <= 18.5) %>% count()/ m_u_count
male_uni_healthy = phy_male_uni %>% filter(bmi > 18.5 & bmi <= 25) %>% count()/ m_u_count
male_uni_overweight = phy_male_uni %>% filter(bmi > 25 & bmi <= 30) %>% count()/ m_u_count
male_uni_obese = phy_male_uni %>% filter(bmi > 30) %>% count()/ m_u_count
f_e_count = phy_female_ele %>% count()
female_ele_underweight = phy_female_ele %>% filter(bmi <= 18.5) %>% count()/ f_e_count
female_ele_healthy = phy_female_ele %>% filter(bmi > 18.5 & bmi <= 25) %>% count()/ f_e_count
female_ele_overweight = phy_female_ele %>% filter(bmi > 25 & bmi <= 30) %>% count()/ f_e_count
female_ele_obese = phy_female_ele %>% filter(bmi > 30) %>% count()/ f_e_count
f_s_count = phy_female_sec %>% count()
female_sec_underweight = phy_female_sec %>% filter(bmi <= 18.5) %>% count()/ f_s_count
female_sec_healthy = phy_female_sec %>% filter(bmi > 18.5 & bmi <= 25) %>% count()/ f_s_count
female_sec_overweight = phy_female_sec %>% filter(bmi > 25 & bmi <= 30) %>% count()/ f_s_count
female_sec_obese = phy_female_sec %>% filter(bmi > 30) %>% count()/ f_s_count
f_c_count = phy_female_col %>% count()
female_col_underweight = phy_female_col %>% filter(bmi <= 18.5) %>% count()/ f_c_count
female_col_healthy = phy_female_col %>% filter(bmi > 18.5 & bmi <= 25) %>% count()/ f_c_count
female_col_overweight = phy_female_col %>% filter(bmi > 25 & bmi <= 30) %>% count()/ f_c_count
female_col_obese = phy_female_col %>% filter(bmi > 30) %>% count()/ f_c_count
f_u_count = phy_female_uni %>% count()
female_uni_underweight = phy_female_uni %>% filter(bmi <= 18.5) %>% count()/ f_u_count
female_uni_healthy = phy_female_uni %>% filter(bmi > 18.5 & bmi <= 25) %>% count()/ f_u_count
female_uni_overweight = phy_female_uni %>% filter(bmi > 25 & bmi <= 30) %>% count()/ f_u_count
female_uni_obese = phy_female_uni %>% filter(bmi > 30) %>% count()/ f_u_count

# Create table displaying the ratios
ratio = matrix(c(male_ele_underweight$n, male_ele_healthy$n, male_ele_overweight$n, male_ele_obese$n,
                 male_sec_underweight$n, male_sec_healthy$n, male_sec_overweight$n, male_sec_obese$n,
                 male_col_underweight$n, male_col_healthy$n, male_col_overweight$n, male_col_obese$n,
                 male_uni_underweight$n, male_uni_healthy$n, male_uni_overweight$n, male_uni_obese$n,
                 female_ele_underweight$n, female_ele_healthy$n, female_ele_overweight$n, female_ele_obese$n,
                 female_sec_underweight$n, female_sec_healthy$n, female_sec_overweight$n, female_sec_obese$n,
                 female_col_underweight$n, female_col_healthy$n, female_col_overweight$n, female_col_obese$n,
                 female_uni_underweight$n, female_uni_healthy$n, female_uni_overweight$n, female_uni_obese$n),
               ncol = 4, byrow = TRUE)
colnames(ratio) = c("Underweight", "Healthy", "Overweight", "Obese")
rownames(ratio) = c("Male Elementary", "Male Secondary", "Male College", "Male University",
                    "Female Elementary", "Female Secondary", "Female College", "Female University")
table = as.table(ratio)
table %>% knitr::kable()

# From exploring the data, we can is that although the density plot does not show a clear correlation between
# education and physical well-being, calculating the ratios of individuals in each category and
# plotting the results as a barplot reveals a slight tren. We noticed that as the groups education increases,
# the less amount of non-healthy BMI scores exist. Thus at the university level of education,
# we see the most amount of healthy individuals in both gender categories.

#### Data Modeling/ Inference
# Create new data frame with the ratio calculated
educ = c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
sex = c(1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2)
ratios = c(male_ele_underweight$n, male_sec_underweight$n, male_col_underweight$n, male_uni_underweight$n, 
           female_ele_underweight$n, female_sec_underweight$n, female_col_underweight$n, female_uni_underweight$n,
           male_ele_healthy$n, male_sec_healthy$n, male_col_healthy$n, male_uni_healthy$n,
           female_ele_healthy$n, female_sec_healthy$n, female_col_healthy$n, female_uni_healthy$n,
           male_ele_overweight$n, male_sec_overweight$n, male_col_overweight$n, male_uni_overweight$n,
           female_ele_overweight$n, female_sec_overweight$n, female_col_overweight$n, female_uni_overweight$n, 
           male_ele_obese$n, male_sec_obese$n, male_col_obese$n, male_uni_obese$n, 
           female_ele_obese$n, female_sec_obese$n, female_col_obese$n, female_uni_obese$n)

ratio_frame = data.frame(educ, sex, c("Underweight", "Underweight", "Underweight", "Underweight",
                                      "Underweight", "Underweight", "Underweight", "Underweight",
                                      "Healthy", "Healthy", "Healthy", "Healthy", 
                                      "Healthy", "Healthy", "Healthy", "Healthy",
                                      "Overweight", "Overweight", "Overweight", "Overweight",
                                      "Overweight", "Overweight", "Overweight", "Overweight",
                                      "Obese", "Obese", "Obese", "Obese", 
                                      "Obese", "Obese", "Obese", "Obese"), ratios)

colnames(ratio_frame) = (c('Educ', 'sex', 'type', 'Ratio'))
ratio_frame = mutate(ratio_frame, Educ = factor(Educ, levels = c(1,2,3,4), labels = c("Elementary", "Secondary", "College", "University")))

# User data frame to create a dotted graph and apply a linear regreesion method into it
ratio_frame %>% ggplot(aes(x=Educ, y=Ratio, col=type, fill=type)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Education") + ggtitle("Weight Distribution among Education Levels")
phy_pred = ratio_frame %>% mutate(numberic_educ = c(8,12,14,16,8,12,14,16,8,12,14,16,8,12,14,16,8,12,14,16,8,12,14,16,8,12,14,16,8,12,14,16)) %>% 
  glm( Ratio ~ numberic_educ * type, data=.) %>% tidy()
phy_pred %>%  knitr::kable()

pred_matrix = matrix(c(0.1929143 - 0.1709571 + (0.0276528 - 0.0268214) * 18, 0.1929143 + 0.0276528 * 18,
                       0.1929143 + 0.3151143 + (0.0276528 - 0.0423571) * 18, 0.1929143 + 0.0841857 + (0.0276528 - 0.0413329) * 18,
                       0.1929143 - 0.1709571 + (0.0276528 - 0.0268214) * 24, 0.1929143 + 0.0276528 * 24,
                       0.1929143 + 0.3151143 + (0.0276528 - 0.0423571) * 24, 0.1929143 + 0.0841857 + (0.0276528 - 0.0413329) * 24),
                     ncol=4, byrow=TRUE)
colnames(pred_matrix) = c("Underweight", "Healthy", "Overweight", "Obese")
rownames(pred_matrix) = c("Masters", "PhD")
ptable = as.table(pred_matrix)
ptable %>% knitr::kable()

# Since we assume for there to be a relationship between education and BMI score,
# we've attempted to fit a model for each weight catergory and used it to predict the ratios of
# individual who may have Masters and PhD level of education. Since education is a categorical value,
# we defined education as the amount of years spent in schooling to more accurately compute a proper model.
# Our predictions alined with our earlier hypothesis, however our predicitons is prone to yield negative values
# and values over 1 as education increases as our data closely resembles a linear model. 

## Summary 
# For each education level, we found that the frequency of substance use varies.
# We did have to take into account that the majority of our data is from secondary and post secondary education levels.
# Our point estimates tell us that those with no education had a higher frequency rate of using drugs/alcohol several
# times a day, while those with post secondary had a lower frequency rate. And,
# those who only have secondary education have a higher frequency rate of taking drugs/alcohol
# several times in a month while those who have no education have a lower frequency rate. When we looked at proportions,
# we could see an increase in the percentage of those who used drugs with an increase in education level. 
# Thus we conclude that those in higher education are more subject to substance abuse.  
# Taking a look at education versus physical health, we noted that those in a higher education level
# led to an increase in physical health, so we conclude that there is a correlation between education and overall physical well-being.  
# From the hypothesis tests, it concludes that no relationship between people's self-perceived mental
# health level and education level. However, it shows a correlation between education level and
# three different mental disorders. People who have higher education have higher proportions in terms of
# having mental disorders. In addition, people who have achieved at least some post secondary education
# tend to have higher life stress level. In conclusion, there exists a correlation  between people's mental health and education. 

## Appendix 
### References
# + Attitudes and behaviours regarding the non-medical use of drugs
# + http://sda.chass.utoronto.ca/sdaweb/nac/g0000036/doc/g036.htm 
# 
# + Canadian alcohol and drug use monitoring survey
# + http://sda.chass.utoronto.ca/sdaweb/dli2/cadums/cadums08/doc/cadu.htm
# 
# + Canadian Community Health Survey 2012
# + http://sda.chass.utoronto.ca/cgi-bin/sda/hsda?harcsda3+cchs2012mh
# 
# + Health Promotion Survey
# + http://sda.chass.utoronto.ca/cgi-bin/sda/hsda?harcsda+hps90
