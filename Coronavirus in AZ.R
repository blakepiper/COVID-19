### Looking at Coronavirus Cases in Arizona ###
### Data pulled at 6pm on April 12, 2020 ###

## Findings ##

## ~95.6% of the variance in cases is explained by population alone, I threw this out
## ~27.9% of the variance in cases can be explained by percent of the population that has completed college
## Model with all predictors can explain ~97.3% of the variance in cases
## Whiteness explains 34% of the variance in poverty, such that the whiter the county the less impoverished it is

library(PerformanceAnalytics)

county_name = c("Mohave", "Coconino", "Navajo", "Apache", "Yavapai", "Gila", "Maricopa", "Pinal", "Graham",
                "Greenlee", "La Paz", "Yuma", "Pima", "Cochise", "Santa Cruz")
county_cases = c(30, 243, 335, 72, 63, 3, 1960, 163, 2, 2, 4, 17, 622, 15, 8)
county_population = c(209550, 142854, 110445, 71818, 231993, 53889, 4410824, 447138, 38072,
                      9483, 21098, 212128, 1039073, 126770, 46511)
percent_in_poverty = c(16.8, 15.9, 28.5, 37.3, 13.2, 20.4, 12.3, 12.9, 20.2, 10.5, 23.7,
                       19.5, 16.2, 15.1, 24.4)
median_household_income = c(45129, 60187, 40650, 33652, 48758, 43585, 65234, 58848, 50157,
                            65818, 37350, 44116, 53395, 49164, 40545)
percent_completed_college = c(12.9, 35.6, 15.8, 11.7, 25.2, 18.5, 32.0, 19.0, 15.3, 11.9, 12.4,
                              14.8, 31.9, 23.0, 21.0)
percent_white = c(77.6, 54.1, 41.5, 18.3, 80.7, 62.4, 55.7, 57.1, 50.9, 47.1, 57.8, 31.4, 52.1, 55.2, 14.9)

total_cases = sum(county_cases)
cases_pop = frame$county_cases/frame$county_population
cases_pop_frame = data.frame(county_name, cases_pop)
frame = data.frame(county_name, county_cases, county_population, percent_in_poverty, 
                   median_household_income, percent_completed_college, percent_white)
frame_no_name = data.frame(county_cases, county_population, percent_in_poverty, 
                           median_household_income, percent_completed_college, percent_white)

chart.Correlation(frame_no_name)

## Fake Regression Assumption Check
random = rchisq(nrow(frame_no_name), 7)
fake = lm(random~., data=frame_no_name)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

## Linearity
qqnorm(standardized)
abline(0,1, col = "red")

## Normality
hist(standardized)

## Homogeneity
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

## First Step, Regressing Cases on Population
first_step = lm(county_cases ~ county_population, data = frame)
summary(first_step)

## Second Step, Regressing Cases on Population + College Completion
second_step = lm(county_cases ~ county_population + percent_completed_college, data = frame)
summary(second_step)
# ~96% of the variance in cases is explained by population alone, so I'll rerun the analysis
# with college completion on the first step

## First Step, Regressing Cases on College Completion
first_step_2 = lm(county_cases ~ percent_completed_college, data = frame)
summary(first_step_2)

## Second Step, Regression Cases on College Completion + Median Household Income
second_step_2 = lm(county_cases ~ percent_completed_college + median_household_income)
summary(second_step_2)

## Full Multiple Predictor Model against all best practice
full = lm(county_cases ~ county_population + percent_in_poverty + median_household_income +
            percent_completed_college + percent_white, data = frame)
summary(full)

## Poverty and Race
race = lm(percent_in_poverty ~ percent_white, data = frame)
summary(race)

## Experimenting with a Poverty to Income Ratio
p_i = frame$percent_in_poverty/frame$median_household_income
frame_pi = frame
frame_pi$poverty_income = p_i

p_i_mod = lm(county_cases ~ p_i, data = frame_pi)
summary(p_i_mod)

## Experimenting with Cases/Population Ratio as the DV

frame_pi$cases_pop_ratio = cases_pop

frame_pi_no_names = data.frame(county_cases, county_population, percent_in_poverty, 
                               median_household_income, percent_completed_college, percent_white, cases_pop, p_i)

chart.Correlation(frame_pi_no_names)

