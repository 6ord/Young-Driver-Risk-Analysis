Summer 2018 Capstone Project: Risk Analysis of Young Drivers

# Context:

Young people and driving notoriously don’t go together in the eyes of risk managers. Males under 25 years old pay significantly more in car insurance premiums, and most car rental companies don’t even allow them to rent their vehicles. This is because risk managers at the insurance companies and car rental companies view them as risky business. That is, the odds of having to pay an insurance claim, or getting a damaged car back from the renter was shown to be higher than other demographics.

# Problem:

I’d like to validate these perceptions by analyzing a dataset that captures each individual who was involved in a car accident in Canada from 1994-2014. We’ll see if there’s any statistical evidence supporting a narrative that young people aren’t as risky as perceived to be behind the wheel.

# Data:

The structured csv dataset from Transport Canada via Kaggle contains under 5.8 million records, each representing a person recorded in the car accident. The 22 attributes include year, number of cars involved, road condition, and the age of the person and injuries. (Full data dictionary attached with submission.) What it does lack, is a grouping by incident, date and location of the accident.
https://www.kaggle.com/tbsteal/canadian-car-accidents-19942014

# Technique & Tools:

The sheer size of this dataset will require the use of a Hadoop cluster (Hive, Pig, Spark) to conduct the data discovery phase efficiently. After that, I’d like to attempt using Bayesian Inference to investigate whether there’s a causal relationship between gender, age and fatality. There’s also an opportunity to look at yearly trends, and/or validate common reasoning for high insurance prices for young drivers.

