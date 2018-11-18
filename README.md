# AmExpert-2018-Machine-Learning-Hackathon-
https://datahack.analyticsvidhya.com/contest/american-express-amexpert-2018/
Problem Statement
Recent years have witnessed a surge in the number of internet savvy users. Companies in the financial services domain leverage this huge internet traffic arriving at their interface by strategically placing ads/promotions for cross selling of various financial products on a plethora of web pages. The digital analytics unit of Best Cards Company uses cutting edge data science and machine learning for successful promotion of its valuable card products. They believe that a predictive model that forecasts whether a session involves a click on the ad/promotion would help them extract the maximum out of the huge clickstream data that they have collected. You are hired as a consultant to build an efficient model to predict whether a user will click on an ad or not, given the following features:
Clickstream data/train data for duration: (2nd July 2017 – 7th July 2017)
Test data for duration: (8th July 2017 – 9th July 2017)
User features (demographics, user behaviour/activity, buying power etc.)
Historical transactional data of the previous month with timestamp info (28th May 2017– 1st July 2017). This data contains actions (views/interest registered) taken by the user historically on the product page via an ad or other sources
Ad features (product category, webpage, campaign for ad etc.)
Date time features (exact timestamp of the user session)
 
Data
Train Data
Variable	Definition
session_id	Unique ID for a session
DateTime	Timestamp
user_id	Unique ID for user
product	Product ID
campaign_id	Unique ID for ad campaign
webpage_id	Webpage ID at which the ad is displayed
product_category_1	Product category 1 (Ordered)
product_category_2	Product category 2
user_group_id	Customer segmentation ID
gender	Gender of the user
age_level	Age level of the user
user_depth	Interaction level of user with the web platform (1 - low, 2 - medium, 3 - High)
city_development_index	Scaled development index of the residence city
var_1	Anonymised session feature
is_click	0 - no click, 1 - click  
Historical User logs
Variable	Definition
DateTime	Timestamp
user_id	Unique ID for the user
product	Product ID
action	view/interest (view - viewed the product page, interest - registered interest for the product)
Evaluation Metric
The evaluation metric for this competition is AUC-ROC score.
