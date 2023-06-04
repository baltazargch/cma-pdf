import pandas as pd
import numpy as np
from functools import partial
import re

def str_squish(string):
    # Replace any sequence of whitespace characters with a single space
    return re.sub(r'(?<=\S)\s{2,}(?=\S)', ' ', string.strip())

def make_italics(data, name):
    if data == 'NA':
        return np.nan
    else:
        data = str_squish(data)
        for x in name:
            data = re.sub(x, r'  \\textit{' + str_squish(x.replace('\\', '')) + r'} ', data)
            # data = re.sub(x, r'  \\textit{' + x.replace('\\', '') + r'} ', data)
    
        return str_squish(data).replace(' ( ',' (').replace(')} .',')}.').replace(r'(?<![a-zA-Z])', '');
  
def atomic_names(names):
    # Split each name into individual words
    out = np.array([])
    for x in names:
        out = np.concatenate([out, re.split(r'\s+', x)])
    
    # Remove empty strings and duplicates
    out = np.unique(out[out != ''])
    
    # Create a DataFrame to count the length of each word
    out_df = pd.DataFrame({'out': out})
    out_df['count'] = out_df['out'].apply(lambda x: len(x))
    
    # Filter for words with length greater than 1, and sort by count in descending order
    out_df = out_df[out_df['count'] > 1].sort_values(by='count', ascending=False)
    
    # Return the list of words
    return list(out_df['out']);
  
def make_pad(string):
    # Create an empty list to store the padded strings
    out = []
    
    # Iterate over each string in the input list
    for s in string:
        # Add the cases
        out.append(' ' + s + ' ')
        out.append(s + ',')
        out.append(s + '\.')
        out.append(r'(?<![a-zA-Z])' + s + ' ')
        out.append('\(' + s)
        out.append(s + '\)')
    
    # Return the list of padded strings
    return out

def make_pad_uniletter(string):
    # Create an empty list to store the padded strings
    out = []
    
    # Iterate over each string in the input list
    for s in string:
        # Add the cases
        # out.append(' ' + s + '\. ')
        out.append(r'(?<![a-zA-Z])' + s + '\. ')
        out.append('\(' + s + '\. ')
    
    # Return the list of padded strings
    return out  
  

def apply_italics_to_cols(df, names):
    str_cols = df.select_dtypes(include=[object]).columns
    
    for col in str_cols:
        df[col] = np.where(df[col].apply(lambda x: isinstance(x, str)), df[col].apply(lambda x: make_italics(x, names)), df[col])
    
    return df
