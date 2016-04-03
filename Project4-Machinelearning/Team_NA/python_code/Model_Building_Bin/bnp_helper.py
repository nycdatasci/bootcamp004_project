import pandas as pd
import scipy as sp
import xgboost as xgb
import pprint as pp
import common_helper
from sklearn import cross_validation, metrics   #Additional scklearn functions
import matplotlib.pylab as plt
from matplotlib.pylab import rcParams
import csv

rcParams['figure.figsize'] = 12, 4

def save_result(ids, predprob_test, result_file_name):    
    # Save results
    predictions_file = open(result_file_name, "w")
    open_file_object = csv.writer(predictions_file)
    open_file_object.writerow(["ID", "PredictedProb"])
    open_file_object.writerows(zip(ids, predprob_test))
    predictions_file.close()

def xgboost_model_fit(alg, features, label, metric=["logloss"], useTrainCV=True, cv_folds=5, early_stopping_rounds=50):
    
    if useTrainCV:
        xgb_param = alg.get_xgb_params()
        xgtrain = xgb.DMatrix(features, label=label)
        cvresult = xgb.cv(xgb_param, xgtrain, num_boost_round=alg.get_params()['n_estimators'], nfold=cv_folds, metrics=metric, early_stopping_rounds=early_stopping_rounds, show_progress=False)
        alg.set_params(n_estimators=cvresult.shape[0])
    
    #Fit the algorithm on the data
    alg_fit = alg.fit(features, label, eval_metric=metric)
        
    print alg_fit
    
    #Predict training set:
    dtrain_predictions = alg.predict(features)
    dtrain_predprob = alg.predict_proba(features)
        
    #Print model report:
    print "\nModel Report"
    print "Accuracy : %.4g" % metrics.accuracy_score(label, dtrain_predictions)
    print "AUC Score (Train): %f" % metrics.roc_auc_score(label, dtrain_predprob[:,1])    
    print "Log Loss (Train): %f" % metrics.log_loss(label, dtrain_predprob)
    
    #feat_imp = pd.Series(alg.booster().get_fscore()).sort_values(ascending=False)
    
    #feat_imp.plot(kind='bar', title='Feature Importances')
    #plt.ylabel('Feature Importance Score')
    
def feature_importance_plot(feature_scores, top = 500):    
    feat_imp = pd.Series(feature_scores).sort_values(ascending=False)[:top]
    
    feat_imp.plot(kind='bar', title='Feature Importances')
    plt.ylabel('Feature Importance Score')
    

def clean(df, drop_collinearity = True, inplace = False):
    '''
    @summary: clean the data by converting some numeric variables to categorical variables;
    reducing high collinearity. 
    '''
    
    # convert numeric to string
    #variables = ['v38', 'v62', 'v72', 'v129']
    #df = common_helper.numeric_to_categorical(df, variables)
    
    # drop column v22
    if('v22' in df.columns):
        if(inplace):
            df.drop('v22', axis=1, inplace = inplace)
        else:
            df = df.drop('v22', axis=1, inplace = inplace)
    
    # Drop highly correlated columns
    if(drop_collinearity):
        df = drop_highly_correlated_variables(df, True)
        
    return df
    
    
def drop_highly_correlated_variables(df, inplace = False):
    '''
    @summary: Check highly correlated column pairs. Drop the one in a pair with higher missingness.
    If one varialbe is marked to be dropped, other pairs contain it will be skipped for checking.
    '''
    corr = common_helper.get_corr(df, 0.9)
    vars_to_drop = []

    for var_corr in corr:
        if(var_corr['_var1'] not in vars_to_drop and var_corr['var2_na'] not in vars_to_drop):
            var_to_drop = var_corr['_var1'] if var_corr['var1_na'] > var_corr['var2_na'] else var_corr['_var2']
            vars_to_drop.append(var_to_drop)

            
    if(inplace):
        df.drop(vars_to_drop, axis=1, inplace = inplace)
    else:
        df = df.drop(vars_to_drop, axis=1, inplace = inplace)
        
    return df
    

def get_categorical_variables(df):
    filter_categorical = df.dtypes == 'object'
    vars_cate = df.columns[filter_categorical]
    
    return vars_cate


def get_numeric_variables(df):
    filter_categorical = df.dtypes == 'object'
    vars_numeric = df.columns[-filter_categorical]
    
    return vars_numeric
        
def impute_cate_with_na_numeric_with_interpolate_linear(df):
    df_predictors = df.iloc[:,2:]
    df_target = df.iloc[:,:2]
    
    # Find categorical variables
    df_cate = df_predictors[get_categorical_variables(df_predictors)]
    
    # Impute categorical data, fill in with 'NA'
    df_cate = df_cate.fillna('NA')
    
    # Find umeric variables
    df_numeric = df_predictors[get_numeric_variables(df_predictors)]
    
    # Impute numeric variables with interpolate linear
    df_numeric = df_numeric.interpolate(method='linear')

    return pd.concat([df_target, df_cate, df_numeric], axis = 1)


def impute_cate_with_na_numeric_with_outlier(df):    
    df_predictors = df.iloc[:,2:]
    df_target = df.iloc[:,:2]
    
    # Find categorical variables
    vars_cate = get_categorical_variables(df_predictors)
    df_cate = df_predictors[vars_cate]
    
    # Impute categorical data, fill in with 'NA'
    df_cate = df_cate.fillna('NA')
    
    # Find umeric variables
    df_numeric = df_predictors[get_numeric_variables(df_predictors)]
    
    # Impute numeric variables with interpolate linear
    df_numeric = df_numeric.fillna(-999)

    return pd.concat([df_target, df_cate, df_numeric], axis = 1)

