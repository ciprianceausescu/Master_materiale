"""
    starter code for exploring the Enron dataset (emails + finances)
    loads up the dataset (pickled dict of dicts)
    the dataset has the form
    enron_data["LASTNAME FIRSTNAME MIDDLEINITIAL"] = { features_dict }
    {features_dict} is a dictionary of features associated with that person
    you should explore features_dict as part of the mini-project,
    but here's an example to get you started:
    enron_data["SKILLING JEFFREY K"]["bonus"] = 5600000

"""

import pickle

enron_data = pickle.load(open("../final_project/final_project_dataset.pkl", "rb"))

#####################################################################################
##### How many data points (people) are in the dataset?
#### cf. https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3004808717/m-3005648570
print("Length of dataset: " + str(len(enron_data)))  # 146

##### For each person, how many features are available?
#### cf. https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3035498566/m-3010518798
print("Features: " + str(len(enron_data["SKILLING JEFFREY K"].keys())))  # 21

##### The “poi” feature records whether the person is a person of interest, according to our definition. How many POIs are there in the E+F dataset?
#### cf. https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3021858558/m-3011778732
print("POT E+F: " + str(len([1 for person in enron_data if enron_data[person]['poi']])))  # 18

##### What is the total value of the stock belonging to James Prentice?
#### cf. https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3025828584/m-3010568605

[s for s in enron_data.keys() if "PRENTICE" in s]  # ['PRENTICE JAMES']
print("Stock of James Prentice: " + str(enron_data['PRENTICE JAMES'].keys()))
print("Stock of James Prentice: " + str(enron_data['PRENTICE JAMES']['total_stock_value']))  # 1095040

##### How many email messages do we have from Wesley Colwell to persons of interest?
#### cf. https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3018078550/m-3014558607
[s for s in enron_data.keys() if "WESLEY" in s]  # ['COLWELL WESLEY']
print("Emails from Wesley: " + str(enron_data['COLWELL WESLEY']['from_this_person_to_poi']))  # 11

##### What’s the value of stock options exercised by Jeffrey Skilling?
#### cf. https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3013308702/m-3007818802
[s for s in enron_data.keys() if "SKILLING" in s]  # ['SKILLING JEFFREY K']
print("Stock exercised by Skilling: " + str(enron_data['SKILLING JEFFREY K']['exercised_stock_options']))  # 19250000

##### Of these three individuals (Lay, Skilling and Fastow), who took home the most money (largest value of “total_payments” feature)?
##### How much money did that person get?
#### cf. https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3017538609/m-3005068624
execs = [s for s in enron_data.keys() if ("SKILLING" in s) or ("LAY" in s) or ("FASTOW" in s)]
print(max([(enron_data[person]['total_payments'], person) for person in execs]))  # (103559793, 'LAY KENNETH L')

##### How is an unfilled feature denoted?
print(enron_data['FASTOW ANDREW S']['deferral_payments'])

##### How many folks in this dataset have a quantified salary? What about a known email address?
#### cf. https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3003098655/m-3003048586
print("Count qualified salary: " + str(len([enron_data[person]['salary'] for person in enron_data if enron_data[person]['salary'] != 'NaN'])))  # 95
print("Count qualified email: " + str(len([enron_data[person]['email_address'] for person in enron_data if enron_data[person]['email_address'] != 'NaN'])))  # 111

##### How many people in the E+F dataset (as it currently exists) have “NaN” for their total payments? What percentage of people in the dataset as a whole is this?
#### https://www.udacity.com/course/viewer#!/c-ud120/l-2291728537/e-3033088677/m-3013038717

print("Percentage from people that have NaN for total payments: " + str(float(len([enron_data[person]['total_payments'] for person in enron_data if
           enron_data[person]['total_payments'] == 'NaN'])) / float(len(enron_data.keys())) * 100.)) # 14.383561643835616

# How many POIs in the E+F dataset have “NaN” for their total payments?
# What percentage of POI’s as a whole is this?
count_NaN_tp = 0
for key in enron_data.keys():
    if enron_data[key]['total_payments'] == 'NaN' and enron_data[key]['poi'] == True :
        count_NaN_tp+=1
print (count_NaN_tp)
print (float(count_NaN_tp)/len(enron_data.keys()))