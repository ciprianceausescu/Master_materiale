#!/usr/bin/python

import pickle
import sys
import matplotlib.pyplot
from pprint import pprint

sys.path.append("../tools/")
from feature_format import featureFormat, targetFeatureSplit


### read in data dictionary, convert to numpy array
data_dict = pickle.load( open("../final_project/final_project_dataset.pkl", "rb") )
features = ["salary", "bonus"]
data_dict.pop("TOTAL", 0)
data = featureFormat(data_dict, features)

### your code below
for point in data:
    salary = point[0]
    bonus = point[1]
    matplotlib.pyplot.scatter(salary, bonus)

matplotlib.pyplot.xlabel("salary")
matplotlib.pyplot.ylabel("bonus")
matplotlib.pyplot.show()

print(data.max())

for key, value in data_dict.items():
    if value['bonus'] == data.max():
        print(key)

outliers = []
for key in data_dict:
    val = data_dict[key]["salary"]
    if val == 'NaN':
        continue
    outliers.append((key, int(val)))

pprint(sorted(outliers, key=lambda x:x[1], reverse=True)[:2])