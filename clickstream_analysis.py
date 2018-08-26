# -*- coding: utf-8 -*-
"""
Created on Sun Aug 26 16:58:53 2018

@author: Daniel
"""

from os import getcwd, chdir
getcwd()
chdir('C:/Users/Daniel/click_stream_analysis')

import numpy as np
import pandas as pd

pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

# scikit-learn commonly used classes
from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import RandomizedSearchCV
from sklearn.metrics import classification_report 
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import StandardScaler
from sklearn.feature_selection import SelectFromModel
from sklearn.pipeline import Pipeline

# scikit learn popular classifier
from sklearn.ensemble import RandomForestClassifier

# XGBoost
from xgboost import XGBClassifier


log = pd.read_csv('./data/ML_data_clickStreams.csv', encoding='cp949')
log.head()
log.info()
log.describe()


# CT_xxx :: 웹사이트 카테고리 별 체류시간 비율, 즉, 총 17개 카테고리 중에 특정 카테고리에 얼마나 머물렀는가를 비율로 계산한 값
# COVERAGE :: 서로 다른 웹 사이트에 얼마나 다양하게 접속했는지에 대한 비률('서로 다은 카테고리 수/17'로 계산)
# DWELLTIME :: 총 체류시간
# PAGEVIEWS :: 총 페이지뷰
# HF_xxx :: 시간대별(0-5시, 6-11시, 12-17시, 18-23시) 체류시간 비율
# DF_xxx :: 요일별 체류시간 비율
# VISITES :: 접속한 서로 다른 웹사이트의 수
# SITECOV :: 웹사이트 카케고리 별 체류시간 변동계수(카테고리별 체류시간의 '표준편차/평균' 값)
# VDAYS :: 총 접속일수
# DAYTIME :: 하루 평균 체류시간
# DAYCOV :: 일별 변동계수(일일 체류시간의 '표준편차/평균' 값)
# SCH_KEYWORDS :: 네이버에서 검색한 검색량
# SCH_TOPKEYWORD :: 네이버에서 가장 많이 검색한 검색어
# GENDER :: 고객성별(남자/여자). 예측하고자 하는 값

# Encode categorical values using one-hot encoding
encoded_log = pd.get_dummies(log, columns=['SCH_TOPKEYWORD'])
encoded_log.head()
encoded_log.shape

# Split data
dfX = encoded_log.drop(['CUS_ID', 'GENDER'], axis=1)
dfy = encoded_log['GENDER']
X_train, X_test, y_train, y_test = train_test_split(dfX, dfy, test_size=.2, 
                                                    random_state=0)

X_train.shape
X_train.head()
y_train.shape

# tune models
# set hyper_parameters

rf_params = {
        'rf__max_features' : np.arange(5, 10), ## feature의 수
        'rf__n_estimators' : [100,300,500] ## tree의 수
        }

xgb_params = {
        'xgb__subsample' : np.arange(.5, 1.0, .2),
        'xgb__max_depth' : np.arange(3,10,3),
        }

# make pipeline
rf_pipeline = Pipeline([
        ('scaler', StandardScaler()),
        ('rf', RandomForestClassifier(random_state=0, n_jobs=-1))
        ])

xgb_pipeline = Pipeline([
        ('scaler', StandardScaler()),
        ('xgb', XGBClassifier(random_state=0, n_jobs=-1))
        ])


models = [
        ('rf', rf_pipeline, rf_params),
        ('xgb', xgb_pipeline, xgb_params)
        ]

# Run grid search & CV
tuned_models = {}
best_score = -1
best_model = None
for name, model, param in models :
    grid_search = GridSearchCV(model, param, cv=5).fit(X_train, y_train)
    score = grid_search.score(X_test, y_test)
    print('{} ==> {} {}'.format(name, score, grid_search.best_params_))
    
    tuned_models[name] = grid_search
    if score > best_score :
        best_score = score
        best_model = name
        
#############################################################################
# rf ==> 0.65 {'rf__max_features': 7, 'rf__n_estimators': 300}
# xgb ==> 0.6825 {'xgb__max_depth': 3, 'xgb__subsample': 0.7}
#############################################################################
        
# Encode categorical values using label encoding
encoded_log = log.copy()
encoded_log['SCH_TOPKEYWORD'] = encoded_log['SCH_TOPKEYWORD'].astype('category')
encoded_log['SCH_TOPKEYWORD'] = encoded_log['SCH_TOPKEYWORD'].cat.codes
encoded_log['SCH_TOPKEYWORD'].value_counts()

encoded_log.head()

# Split data
dfX = encoded_log.drop(['CUS_ID', 'GENDER'], axis=1)
dfy = encoded_log['GENDER']
X_train, X_test, y_train, y_test = train_test_split(dfX, dfy, test_size=.2, 
                                                    random_state=0)

# tune models
# set hyper_parameters

rf_params = {
        'rf__max_features' : np.arange(5, 10), ## feature의 수
        'rf__n_estimators' : [100,300,500] ## tree의 수
        }

xgb_params = {
        'xgb__subsample' : np.arange(.5, 1.0, .2),
        'xgb__max_depth' : np.arange(3,10,3),
        }

# make pipeline
rf_pipeline = Pipeline([
        ('scaler', StandardScaler()),
        ('rf', RandomForestClassifier(random_state=0, n_jobs=-1))
        ])

xgb_pipeline = Pipeline([
        ('scaler', StandardScaler()),
        ('xgb', XGBClassifier(random_state=0, n_jobs=-1))
        ])

models = [
        ('rf', rf_pipeline, rf_params),
        ('xgb', xgb_pipeline, xgb_params)
        ]

tuned_models = {}
best_score = -1
best_model = None
for name, model, param in models :
    grid_search = RandomizedSearchCV(model, param, cv=5, n_iter=10).fit(X_train, y_train)
    score = grid_search.score(X_test, y_test)
    print ('{} ==> {} {}'.format(name, score, grid_search.best_params_))
    tuned_models[name] = grid_search
    if score > best_score :
        best_score = score
        best_model = name



















