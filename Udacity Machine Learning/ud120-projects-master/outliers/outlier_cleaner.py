#!/usr/bin/python
import numpy as np

def outlierCleaner(predictions, ages, net_worths):
    """
        Clean away the 10% of points that have the largest
        residual errors (difference between the prediction
        and the actual net worth).

        Return a list of tuples named cleaned_data where 
        each tuple is of the form (age, net_worth, error).
    """
    cleaned_data = []
    differences = []
    differences_final = []

    ### your code goes here
    # Calculate all differences between prediction and actual net worth
    for i in range(0, len(predictions)):
        differences.append(predictions[i]-net_worths[i])

    # Sort the list of differences and take the first 81 of them
    list.sort(differences)
    for i in range(0, 81):
        differences_final.append(differences[i])

    for i in range(0, len(predictions)):
        if(predictions[i]-net_worths[i] in differences_final):
            cleaned_data.append([ages[i], net_worths[i], predictions[i]-net_worths[i]])


    return cleaned_data

