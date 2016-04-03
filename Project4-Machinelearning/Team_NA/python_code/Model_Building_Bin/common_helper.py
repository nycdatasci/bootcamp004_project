import pandas as pd
import pprint as pp
import numpy as np

def numeric_to_categorical(df, variables):
    '''
    @summary: converts numeric variables in a dataframe to categorical type (as str).
    '''
    for var in variables: 
        df[var] = df[var].astype('str')  
    
    return df

def dummify(df, cate_variables, inplace = False):
    '''
    @Summary: convert the categorical variables to numeric variables by using dummies (binary).
    Old categorical variables will be dropped.
    @param df: target dataframe
    @param cate_variables: target variables to be converted to binary dummies
    @param inplace: indicate if operation should be performed on original dataframe or a copy of dataframe. 
    Original dataframe is used when inplace = True, otherwise, a copy of the dataframe.
    @return: A dataframe with new converted numeric variables. 
    '''
    # make a copy before creating dummies
    if(inplace):
        df_new = df.copy()
    else:
        df_new = df
    
    # convert text-based columns to dummies (except v22)
    for var_name in cate_variables:
        dummies = pd.get_dummies(df[var_name], prefix=var_name)
        
        # Drop the current variable, concat/append the dummy dataframe to the dataframe.
        df_new = pd.concat([df_new.drop(var_name, 1), dummies.iloc[:,1:]], axis = 1)
    
    return df_new



def get_corr(df, corr_threshold): 
    '''
    @summary: Get variable pairs with their correlation based on the passed threshold.
    @param df: target dataframe
    @param corr_threshold: cut-off correlation (abs(corrleation) > corr_threshold will be returned)
    @
    '''
    
    corr_var = df.corr()
    high_correlation = []

    for index, row in corr_var.iterrows():
        row_index_num = corr_var.index.get_loc(index)

        var1_na_count = sum(df.iloc[:,row_index_num].isnull())

        for colname in corr_var.columns.values:
            col_index = corr_var.columns.get_loc(colname)
            if(col_index < row_index_num and abs(row[colname]) > corr_threshold and row.name != colname):
            #if(col_index < row_index_num and row[colname] > correlation_threshold and row.name != colname):
                var2_na_count = sum(df[colname].isnull())
                high_correlation.append({'_var1': row.name, '_var2': colname, \
                                         '_var_corr': round(row[colname],3), \
                                         'var1_na': var1_na_count, 'var2_na': var2_na_count})

    return high_correlation

def get_feature_importance(df, model):    
    feature_imprtance = zip(df.columns[1:], model.feature_importances_)
    dtype = [('feature', 'S10'), ('importance', 'float')]
    feature_imprtance = np.array(feature_imprtance, dtype = dtype)
    feature_sort = np.sort(feature_imprtance, order='importance')[::-1]
    return feature_sort