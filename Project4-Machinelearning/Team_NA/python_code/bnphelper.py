import dataiku
from dataiku import pandasutils as pdu
import pandas as pd
import pprint

def change_numeric_to_categorical(df, cardinality):
    '''
    @summary: converts numeric variables in a dataframe to categorical based on the cardinality threshold.
    '''
    audit = pdu.audit(df)

    numeric_low_cardinality = audit[((audit._b_data_type == 'int64') | (audit._b_data_type == 'float64')) & (audit._c_cardinality < cardinality)]

    variables = numeric_low_cardinality._a_variable

    for var in variables: 
        df[var] = df[var].astype('str')   


def convert_to_numeric_dataframe(df, cate_variables):
    '''
    @Summary: convert the categorical variables to numeric variables by using dummies (binary).
    Old categorical variables will be dropped.
    @return: A copy of the old dataframe with new converted numeric variables. 
    '''
    # make a copy before creating dummies
    df_numeric = df.copy()
    
    #audit = pdu.audit(df)

    # convert text-based columns to dummies (except v22)
    for var_name in cate_variables:
        if(var_name != 'v22'):        
            dummies = pd.get_dummies(df[var_name], prefix=var_name)
            #pp.pprint(dummies.head())

            # Drop the current variable, concat/append the dummy dataframe to the dataframe.
            df_numeric = pd.concat([df_numeric.drop(var_name, 1), dummies.iloc[:,1:]], axis = 1)

    #train_df_numeric.head()
    
    return df_numeric