import time
start_time = time.time()

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
#from sklearn import pipeline, model_selection
from sklearn import pipeline, grid_search
#from sklearn.feature_extraction import DictVectorizer
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.pipeline import FeatureUnion
from sklearn.decomposition import TruncatedSVD
#from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics import mean_squared_error, make_scorer
#from nltk.metrics import edit_distance
# from nltk.stem.porter import *
# stemmer = PorterStemmer()
#from nltk.stem.snowball import SnowballStemmer #0.003 improvement but takes twice as long as PorterStemmer
#stemmer = SnowballStemmer('english')
import re
#import enchant
import random
random.seed(2016)
print ("data preparation")
num_train=74067
df_train = pd.read_csv("../input/x_train.csv", encoding="ISO-8859-1")
df_test=pd.read_csv("../input/x_test.csv", encoding="ISO-8859-1")
coswithst=pd.read_csv("../data/coswithst_m3.csv", encoding="ISO-8859-1",header=None)
coswithst.columns = ['distance']
cws_trn=coswithst.iloc[:num_train]
cws_tst=coswithst.iloc[num_train:]

id_test = df_test['id']
id_train = df_train['id']
y_train = pd.read_csv("../input/y_train.csv", encoding="ISO-8859-1",header=None)

x_train =df_train[:]
x_test = df_test[:]
x_train= pd.concat([x_train, cws_trn], axis=1)
x_test = pd.concat([x_test.reset_index(drop=True),cws_tst.reset_index(drop=True)], axis=1)

print ("data preparation done!")
print (x_train.head())
def seg_words(str1, str2):
    str2 = str2.lower()
    str2 = re.sub("[^a-z0-9./]"," ", str2)
    str2 = [z for z in set(str2.split()) if len(z)>2]
    words = str1.lower().split(" ")
    s = []
    for word in words:
        if len(word)>3:
            s1 = []
            s1 += segmentit(word,str2,True)
            if len(s)>1:
                s += [z for z in s1 if z not in ['er','ing','s','less'] and len(z)>1]
            else:
                s.append(word)
        else:
            s.append(word)
    return (" ".join(s))

def segmentit(s, txt_arr, t):
    st = s
    r = []
    for j in range(len(s)):
        for word in txt_arr:
            if word == s[:-j]:
                r.append(s[:-j])
                #print(s[:-j],s[len(s)-j:])
                s=s[len(s)-j:]
                r += segmentit(s, txt_arr, False)
    if t:
        i = len(("").join(r))
        if not i==len(st):
            r.append(st[i:])
    return r

def str_common_word(str1, str2):
    words, cnt = str1.split(), 0
    for word in words:
        if str2.find(word)>=0:
            cnt+=1
    return cnt

def str_whole_word(str1, str2, i_):
    cnt = 0
    while i_ < len(str2):
        i_ = str2.find(str1, i_)
        if i_ == -1:
            return cnt
        else:
            cnt += 1
            i_ += len(str1)
    return cnt

def fmean_squared_error(ground_truth, predictions):
    fmean_squared_error_ = mean_squared_error(ground_truth, predictions)**0.5
    return fmean_squared_error_

RMSE  = make_scorer(fmean_squared_error, greater_is_better=False)

class cust_regression_vals(BaseEstimator, TransformerMixin):
    def fit(self, x, y=None):
        return self
    def transform(self, hd_searches):
        d_col_drops=['id','search_term','product_title','product_description','brand']
        hd_searches = hd_searches.drop(d_col_drops,axis=1).values
        return hd_searches

class cust_txt_col(BaseEstimator, TransformerMixin):
    def __init__(self, key):
        self.key = key
    def fit(self, x, y=None):
        return self
    def transform(self, data_dict):
        return data_dict[self.key].apply(str)

print ("model preparation")
rfr = RandomForestRegressor(n_estimators = 500, n_jobs = -1, random_state = 2016, verbose = 1)
tfidf = TfidfVectorizer(ngram_range=(1, 1), stop_words='english')
tsvd = TruncatedSVD(n_components=10, random_state = 2016)
clf = pipeline.Pipeline([('union', FeatureUnion(transformer_list = [('cst',  cust_regression_vals()),
                                                                    ('txt1', pipeline.Pipeline([('s1', cust_txt_col(key='search_term')), ('tfidf1', tfidf), ('tsvd1', tsvd)])),
                                                                    ('txt2', pipeline.Pipeline([('s2', cust_txt_col(key='product_title')), ('tfidf2', tfidf), ('tsvd2', tsvd)])),
                                                                    ('txt3', pipeline.Pipeline([('s3', cust_txt_col(key='product_description')), ('tfidf3', tfidf), ('tsvd3', tsvd)])),
                                                                    ('txt4', pipeline.Pipeline([('s4', cust_txt_col(key='brand')), ('tfidf4', tfidf), ('tsvd4', tsvd)]))
                                                                    ],
                                                transformer_weights = {'cst': 1.0,
                                                'txt1': 0.5,
                                                'txt2': 0.25,
                                                'txt3': 0.25,
                                                'txt4': 0.5
                                                },
                                                n_jobs = -1
                                                )),('rfr', rfr)])
param_grid = {'rfr__max_features':[9,10,11,12], 'rfr__max_depth': [15,20,25,30]}
model = grid_search.GridSearchCV(estimator = clf, param_grid = param_grid, n_jobs = -1, cv = 5, verbose = 20, scoring=RMSE)
model.fit(x_train, y_train)

print("Best parameters found by grid search:")
print(model.best_params_)
print("Best CV score:")
print(model.best_score_)
print(model.best_score_ + 0.47003199274)

# refit the model to get the prediction value
y_pred1= model.predict(x_train)
pd.DataFrame({"id": id_train, "relevance": y_pred1}).to_csv('prediction.csv',index=False)
print("--- Training Fit: %s minutes ---" % round(((time.time() - start_time)/60),2))

# get the submission data
y_pred = model.predict(x_test)
pd.DataFrame({"id": id_test, "relevance": y_pred}).to_csv('submission.csv',index=False)
print("--- Training & Testing: %s minutes ---" % round(((time.time() - start_time)/60),2))
# Best parameters found by grid search:
# {'rfr__max_depth': 25, 'rfr__max_features': 10}
# Best CV score:
# -0.464172592186
# 0.00585940055376

# Best parameters found by grid search:
# {'rfr__max_features': 12, 'rfr__max_depth': 30}
# Best CV score:
# -0.463630093205
# 0.00640189953506