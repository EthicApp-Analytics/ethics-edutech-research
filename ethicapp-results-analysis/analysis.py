import pandas as pd
import numpy as np

data = pd.read_excel(io="20190405-results.xlsx", sheet_name="Chat")
groups = data['team'].unique()

lst = range(len(groups))

for group in groups:
    print(data['team'].unique())

