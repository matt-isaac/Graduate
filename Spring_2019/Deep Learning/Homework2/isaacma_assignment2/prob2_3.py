# -*- coding: utf-8 -*-
"""
Deep Learning - Homework 2
prob2_3.py

@author: misaa
"""


from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
import pandas as pd
import numpy as np

filename = 'data_seed.dat'
dat = np.fromfile(filename, sep = '\t')
dat = np.reshape(dat, (210,8))

df = pd.DataFrame(dat)


def cv_5fold(data, method):
    """
    Performs 5fold cross validation for either logistic regression or random forests.
    """
    np.random.shuffle(data) # shuffle data
    l = data.shape[0]
    # split into 5 parts
    i1, i2, i3, i4= int(l * 0.2), int(l * 0.4), int(l * 0.6), int(l * 0.8)
    d1, d2, d3, d4, d5 = data[: i1], data[i1 + 1 : i2], data[i2 + 1 : i3], data[i3 + 1 : i4], data[i4 + 1 :]
    
    data_list = [d1, d2, d3, d4, d5]
    pa = []
    pa_train = []
    counter = 0
    for ds in data_list: # iterate over 5 data partitions. 
        
        # format training data
        train_list = [x for i,x in enumerate(data_list) if i!=counter] 
        train_data = np.vstack(train_list)
        train_data = pd.DataFrame(train_data)
        test_data = pd.DataFrame(test_data)
        train_features = train_data.iloc[:,0:6] # training features
        train_y = train_data.iloc[:,7] # training response
        
        # format test data
        test_data = ds
        test_features = test_data.iloc[:,0:6] # test features
        test_y = test_data.iloc[:,7] # test response
        
        # initialize respective classifiers
        if method == 'rf':    
            clf = RandomForestClassifier()
            
        elif method == 'lr':
            clf = LogisticRegression()
            
        clf.fit(train_features, train_y) # train classifier
        
        pred = clf.predict(test_features) # predict on test set
        pred = pd.DataFrame(pred)
        
        pred_train = clf.predict(train_features) # preict on training set
        pred_train = pd.DataFrame(pred_train)
        
        correct = pred[0] == test_y
        accuracy = np.mean(correct) # accuracy for test set calcualated
        
        correct_train = pred_train[0] == train_y
        accuracy_train = np.mean(correct_train) # accuracy for training set calculated
        
        pa.append(accuracy)
        pa_train.append(accuracy_train)
        
    #average accuracy taken. 
    avg_pa = np.round(np.mean(pa), decimals = 3)
    avg_pa_train = np.round(np.mean(pa_train), decimals = 3)
    print(str(avg_pa_train) +  ", " + str(avg_pa))
    return(avg_pa_train, avg_pa)    




"""
realized I didn't actually need to code up LOOCV...
"""
#def cv_loo(data, method):
#    
#    pa = []
#    pa_train = []
#    counter = 0
#    
#    for index, row in data.iterrows():
#        data2 = data
#        
#        # format training data
#        train_data = data2.drop(data.index[counter])
#        train_features = train_data.iloc[:,0:6]
#        train_y = train_data.iloc[:,7]
#        
#        test_features = row.iloc[0:6]
#        test_features = test_features.values.reshape(1, -1)
#        
#        test_y = (row.iloc[7])
#
#        if method == 'rf':    
#            clf = RandomForestClassifier()
#        
#        elif method == 'lr':
#            clf = LogisticRegression()
#        
#        clf.fit(train_features, train_y)
#        
#        
#        
#        pred = clf.predict(test_features)
#        pred_train = clf.predict(train_features)
#        
#        pred_train = pd.DataFrame(pred_train)
#        correct_train = pred[0] == test_y
#        accuracy_train = np.mean(correct_train)
#        
#        pa_train.append(accuracy_train)
#        
#        if pred == test_y:
#            pa.append(1)
#        else:
#            pa.append(0)
#        counter = counter + 1
#    avg_pa = np.round(np.mean(pa), decimals = 3)
#    avg_pa_train = np.round(np.mean(pa_train), decimals = 3)
#    print(str(avg_pa_train) +  ", " + str(avg_pa))
#    return(avg_pa_train, avg_pa) 
#    
    

    
#cv_loo(df, method = 'rf') # 0.862, 0.862

#cv_loo(df, method = 'lr') # 0.91, 0.91

cv_5fold(dat, method = 'rf') # 0.989, 0.971
    
cv_5fold(dat, method = 'lr') # 0.909, 0.884
    


