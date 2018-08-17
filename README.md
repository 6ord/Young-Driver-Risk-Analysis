Summer 2018 Capstone Project: Risk Analysis of Young Drivers

# Context:

Young people and driving notoriously don’t go together in the eyes of risk managers. Males under 25 years old pay significantly more in car insurance premiums, and most car rental companies don’t even allow them to rent their vehicles. This is because risk managers at the insurance companies and car rental companies view them as risky business. That is, the odds of having to pay an insurance claim, or getting a damaged car back from the renter was shown to be higher than other demographics.

# Problem:

I’d like to validate these perceptions by analyzing a dataset that captures each individual who was involved in a car accident in Canada from 1994-2014. We’ll see if there’s any statistical evidence supporting a narrative that young people aren’t as risky as perceived to be behind the wheel.

# Data:

The structured csv dataset from Transport Canada via Kaggle contains under 5.8 million records, each representing a person recorded in the car accident. The 22 attributes include year, number of cars involved, road condition, and the age of the person and injuries. (Full data dictionary attached with submission.) What it does lack, is a grouping by incident, date and location of the accident.
https://www.kaggle.com/tbsteal/canadian-car-accidents-19942014

Conceptual use of this data can be found here (https://prezi.com/view/LquWzGw3HZMw1s6x849h/) - Light analysis was done between R and Excel Power Pivot. This project will extend the depth of the analysis, with a focus on using open source tools.

# Getting this R Project Started:
1. Install arules and ggplot2 packages
2. Set working directory
3. Run lines 34-37 in accidents.r

# About the R Scripts:

accid_base.r
- Imports original dataset
- Builds user defined attributes
- Addresses missing values
- Build vehicle driver age attribute

accid_range.r
- Band age into age groups
- Band collision Hour into times of day, weekday/weekend
- Map/categorize collision configuration
- Map/categorize road configuration
- Map/categorize traffic signal type

accid_reduceToOccur.r
- Creates the 'per occurrence' dataset from the 'per person' level

accidents.r
- Imports 3rd party libraries
- Build data summary within R
- Frequency plots
- Association Rules
- Chi Square tests
