import pandas as pd
import numpy as np
from functools import partial
import re

def str_squish(string):
    # Replace any sequence of whitespace characters with a single space
    return re.sub(r'\s+', ' ', string.strip())

def make_italics(data, name):
    data = str_squish(data)
    for x in name:
      # re.sub(r'\s+', '', 
      data = re.sub(x, r'  \\textit{' + str_squish(x.replace('\\', '')) + r'} ', data)
      # data = str_squish(data)
    
    return str_squish(data).replace('( ','(').replace(') ',')');
  
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
        out.append(s + ' ')
        out.append('\(' + s)
        out.append(s + '\)')
    
    # Return the list of padded strings
    return out

def apply_italics_to_cols(df, col_names, name_list):
    for col in col_names:
        if df[col].dtype == 'O': # Check if column is of object type (i.e., string)
            df[col] = df[col].apply(lambda x: make_italics(x, name_list))
            df[col] = str_squish(df[col])
    return df;
