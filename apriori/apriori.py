#################################
#
# Example adapted from http://intelligentonlinetools.com/blog/2018/02/10/how-to-create-data-visualization-for-association-rules-in-data-mining/
#
#################################


# Import necessary python libraries
import pandas as pd
import csv
from mlxtend.preprocessing import TransactionEncoder
from mlxtend.frequent_patterns import apriori, association_rules

# Read in CSV file into an array of arrays
# Make sure that your data is structured like the data given in tutorial
dataset = []
with open('apriori_data.csv') as f:
	reader = csv.reader(f)
	for row in reader:
		dataset.append(row)
for row in dataset: 
	print(row)

# Transform your data for the apriori algorithm
oht = TransactionEncoder()
oht_ary = oht.fit(dataset).transform(dataset)
df = pd.DataFrame(oht_ary, columns=oht.columns_)
print(df)

frequent_itemsets = apriori(df, min_support=0.6, use_colnames=True)
print(frequent_itemsets)

rules = association_rules(frequent_itemsets, metric="confidence", min_threshold=0.7)
print(rules[['antecedents', 'consequents', 'support', 'confidence']])