import re
import pandas as pd
import numpy as np
import nltk
from nltk.corpus import stopwords
from nltk.tag.perceptron import PerceptronTagger
from compiler.ast import flatten



###load and merge data#####
trn_og = pd.read_csv("train.csv",encoding="ISO-8859-1")
dsrp_og= pd.read_csv("product_descriptions.csv",encoding="ISO-8859-1");
tst_og= pd.read_csv("test.csv",encoding="ISO-8859-1");
df_attr= pd.read_csv('attributes.csv', encoding="ISO-8859-1")
train = pd.merge(left=trn_og,right=dsrp_og, how='left', left_on='product_uid', right_on='product_uid')
test = pd.merge(left=tst_og,right=dsrp_og, how='left', left_on='product_uid', right_on='product_uid')

df_brand = df_attr[df_attr.name == "MFG Brand Name"][["product_uid", "value"]].rename(columns={"value": "brand"})

train = pd.merge(train, df_brand, how='left', on='product_uid')
test = pd.merge(test, df_brand, how='left', on='product_uid')

train = train.replace(np.nan,'none', regex=True)
test = test.replace(np.nan,'none', regex=True)

#read in the manually built dictionaries as dataframes and then convert to dictionaries
dicts=pd.read_csv('dict.csv')

joined_df=pd.concat([dicts['joined'],dicts['1st']],axis=1)
spells_df=pd.concat([dicts['spell'],dicts['2nd']],axis=1)
syns_df=pd.concat([dicts['syn'],dicts['3rd']],axis=1)

joined_dict=joined_df.set_index('joined').to_dict()['1st']
spells_dict=spells_df.set_index('spell').to_dict()['2nd']
syns_dict=syns_df.set_index('syn').to_dict()['3rd']


###do subsetting just to speed things up, can turn on/off as needed
#train=train[1:50]
#test=test[1:50]


###force lower case#######
train['product_description']=train['product_description'].map(lambda x: x.lower())
train['product_title']=train['product_title'].map(lambda x: x.lower())
train['search_term']=train['search_term'].map(lambda x: x.lower())
train['brand']=train['brand'].map(lambda x: x.lower())

test['product_description']=test['product_description'].map(lambda x: x.lower())
test['product_title']=test['product_title'].map(lambda x: x.lower())
test['search_term']=test['search_term'].map(lambda x: x.lower())
test['brand']=test['brand'].map(lambda x: x.lower())


###################################################
####remove symbols#### but put spaces in there original place, and aditionaly remove # and . (but not %)
###original symbols to replace: re.sub( '[:\',\-!;"()?]'

train['product_description']=train['product_description'].map(lambda x:re.sub( '[:\',\-!.;/*()?#]', " ", x))
train['product_title']=train['product_title'].map(lambda x:re.sub( '[:\',\-!.;/*()?#]', " ", x))
train['search_term']=train['search_term'].map(lambda x:re.sub( '[:\',\-!.;/*()?#]', " ", x))
train['brand']=train['brand'].map(lambda x:re.sub( '[:\',\-!.;/*()?#]', " ", x))

test['product_description']=test['product_description'].map(lambda x:re.sub( '[:\',\-!.;/*()?#]', " ", x))
test['product_title']=test['product_title'].map(lambda x:re.sub( '[:\',\-!.;/*()?#]', " ", x))
test['search_term']=test['search_term'].map(lambda x:re.sub( '[:\',\-!.;/*()?#]', " ", x))
test['brand']=test['brand'].map(lambda x:re.sub( '[:\',\-!.;/*()?#]', " ", x))
####################
####split the string columns into lists, giving us a nested list
train['product_description']=train['product_description'].map(lambda x: x.split(" "))
train['product_title']=train['product_title'].map(lambda x: x.split(" "))
train['search_term']=train['search_term'].map(lambda x: x.split(" "))
train['brand']=train['brand'].map(lambda x: x.split(" "))

test['product_description']=test['product_description'].map(lambda x: x.split(" "))
test['product_title']=test['product_title'].map(lambda x: x.split(" "))
test['search_term']=test['search_term'].map(lambda x: x.split(" "))
test['brand']=test['brand'].map(lambda x: x.split(" "))
############################
###map number words to numbers
number_dict={'one':'1', 'two':'2', 'three':'3','four':'4','five':'5','six':'6','seven':'7','eight':'8','nine':'9','ten':'10'}
train['product_description']=train['product_description'].map(lambda x:[number_dict.get(word,word)for word in x])
train['product_title']=train['product_title'].map(lambda x:[number_dict.get(word,word)for word in x])
train['search_term']=train['search_term'].map(lambda x:[number_dict.get(word,word)for word in x])
train['brand']=train['brand'].map(lambda x:[number_dict.get(word,word)for word in x])

test['product_description']=test['product_description'].map(lambda x:[number_dict.get(word,word)for word in x])
test['product_title']=test['product_title'].map(lambda x:[number_dict.get(word,word)for word in x])
test['search_term']=test['search_term'].map(lambda x:[number_dict.get(word,word)for word in x])
test['brand']=test['brand'].map(lambda x:[number_dict.get(word,word)for word in x])

print 'numbers remapped'
############################
#rejoin the nested lists so i can do the regular expressions neccesary for measurements
train['product_description']=train['product_description'].map(lambda x:' '.join(x))
train['product_title']=train['product_title'].map(lambda x:' '.join(x))
train['search_term']=train['search_term'].map(lambda x:' '.join(x))
train['brand']=train['brand'].map(lambda x:' '.join(x))

test['product_description']=test['product_description'].map(lambda x:' '.join(x))
test['product_title']=test['product_title'].map(lambda x:' '.join(x))
test['search_term']=test['search_term'].map(lambda x:' '.join(x))
test['brand']=test['brand'].map(lambda x:' '.join(x))
##############################
###normalize all the measurment dimensions to one format, starting with 3dimensional measurements

train['product_description']=train['product_description'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5x\9 ',x))
train['product_title']=train['product_title'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5x\9 ',x))
train['search_term']=train['search_term'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5x\9 ',x))
train['brand']=train['brand'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5x\9 ',x))

test['product_description']=test['product_description'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5x\9 ',x))
test['product_title']=test['product_title'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5x\9 ',x))
test['search_term']=test['search_term'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5x\9 ',x))
test['brand']=test['brand'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5x\9 ',x))

##############################
###normalize all the measurment dimensions to one format, now doing the two dimensional measurements

train['product_description']=train['product_description'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5 ',x))
train['product_title']=train['product_title'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5 ',x))
train['search_term']=train['search_term'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})',r'\1x\5 ',x))
train['brand']=train['brand'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?',r'\1x\5',x))

test['product_description']=test['product_description'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?',r'\1x\5',x))
test['product_title']=test['product_title'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?',r'\1x\5',x))
test['search_term']=test['search_term'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?',r'\1x\5',x))
test['brand']=test['brand'].map(lambda x:re.sub('(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?\s{0,2}(x|by|[*])\s{0,2}(\d{1,10})\s{0,2}(feet|foot|ft|inches|inches|in)?\s{0,2}(l|w|d|h)?',r'\1x\5',x))

#############################
####split the string columns into lists, giving us a nested list
train['product_description']=train['product_description'].map(lambda x: x.split(" "))
train['product_title']=train['product_title'].map(lambda x: x.split(" "))
train['search_term']=train['search_term'].map(lambda x: x.split(" "))
train['brand']=train['brand'].map(lambda x: x.split(" "))

test['product_description']=test['product_description'].map(lambda x: x.split(" "))
test['product_title']=test['product_title'].map(lambda x: x.split(" "))
test['search_term']=test['search_term'].map(lambda x: x.split(" "))
test['brand']=test['brand'].map(lambda x: x.split(" "))
############################
#check how many unique words at this stage
#print 'starting unique words in train and test'
#checkframe=pd.concat([train, test], ignore_index=True).drop(['id','relevance','product_uid'],1)
#print len(set(flatten(checkframe.values.tolist())))
#wordlist=sorted(set(flatten(checkframe.values.tolist())))
#############################
##remove stop words
#cachedStopWords = stopwords.words("english")
#train['product_description']=train['product_description'].map(lambda x:[word for word in x if word not in cachedStopWords])
#train['product_title']=train['product_title'].map(lambda x:[word for word in x if word not in cachedStopWords])
#train['search_term']=train['search_term'].map(lambda x:[word for word in x if word not in cachedStopWords])
#train['brand']=train['brand'].map(lambda x:[word for word in x if word not in cachedStopWords])
#
#test['product_description']=test['product_description'].map(lambda x:[word for word in x if word not in cachedStopWords])
#test['product_title']=test['product_title'].map(lambda x:[word for word in x if word not in cachedStopWords])
#test['search_term']=test['search_term'].map(lambda x:[word for word in x if word not in cachedStopWords])
#test['brand']=test['brand'].map(lambda x:[word for word in x if word not in cachedStopWords])
#
#cachedStopWords=None#free up some of the cache

##check how many unique words at this stage
#print 'unique words in train and test after stop word removal'
#checkframe=pd.concat([train, test], ignore_index=True).drop(['id','relevance','product_uid'],1)
#print len(set(flatten(checkframe.values.tolist())))
##############################
###remove empty strings
train['product_description']=train['product_description'].map(lambda x:filter(bool,x))
train['product_title']=train['product_title'].map(lambda x:filter(bool,x))
train['search_term']=train['search_term'].map(lambda x:filter(bool,x))
train['brand']=train['brand'].map(lambda x:filter(bool,x))

test['product_description']=test['product_description'].map(lambda x:filter(bool,x))
test['product_title']=test['product_title'].map(lambda x:filter(bool,x))
test['search_term']=test['search_term'].map(lambda x:filter(bool,x))
test['brand']=test['brand'].map(lambda x:filter(bool,x))

print 'removed empty strings'
#####################################
###do the POS tagging
#tagger = PerceptronTagger()
tagger = nltk.UnigramTagger(nltk.corpus.brown.tagged_sents())#this tagger is vastly faster for some reason
tagset='universal'
train['product_description']=train['product_description'].map(lambda x:nltk.tag._pos_tag(x,tagset, tagger))
train['product_title']=train['product_title'].map(lambda x:nltk.tag._pos_tag(x,tagset, tagger))
train['search_term']=train['search_term'].map(lambda x:nltk.tag._pos_tag(x,tagset, tagger))
train['brand']=train['brand'].map(lambda x:nltk.tag._pos_tag(x,tagset, tagger))

test['product_description']=test['product_description'].map(lambda x:nltk.tag._pos_tag(x,tagset, tagger))
test['product_title']=test['product_title'].map(lambda x:nltk.tag._pos_tag(x,tagset, tagger))
test['search_term']=test['search_term'].map(lambda x:nltk.tag._pos_tag(x,tagset, tagger))
test['brand']=test['brand'].map(lambda x:nltk.tag._pos_tag(x,tagset, tagger))

tagger=None#free up some of the cache

print 'POS tagging done'
#####################################
###map the POS tag for the lematizer
mapping={'NOUN':'n', 'VERB':'v', 'ADJ':'a','ADV':'r','NUM':'n','X':'n','.':'n','ADP':'n','CONJ':'n','PRT':'n','DET':'n','PRON':'n'}#POS types that don't help lemmatization will be set to nouns
train['product_description']=train['product_description'].map(lambda x:[ [word[0], mapping[word[1]]] for word in x] )
train['product_title']=train['product_title'].map(lambda x:[ [word[0], mapping[word[1]]] for word in x] )
train['search_term']=train['search_term'].map(lambda x:[ [word[0], mapping[word[1]]] for word in x] )
train['brand']=train['brand'].map(lambda x:[ [word[0], mapping[word[1]]] for word in x] )

test['product_description']=test['product_description'].map(lambda x:[ [word[0], mapping[word[1]]] for word in x] )
test['product_title']=test['product_title'].map(lambda x:[ [word[0], mapping[word[1]]] for word in x] )
test['search_term']=test['search_term'].map(lambda x:[ [word[0], mapping[word[1]]] for word in x] )
test['brand']=test['brand'].map(lambda x:[ [word[0], mapping[word[1]]] for word in x] )

print 'POS tags remapped'
#####################################
###do the lematization, this will also remove those POS tags that were put on previosly(they were only really included to improve the lematization)
lmtizer=nltk.stem.WordNetLemmatizer()
train['product_description']=train['product_description'].map(lambda x:[lmtizer.lemmatize(word[0]) for word in x])
train['product_title']=train['product_title'].map(lambda x:[lmtizer.lemmatize(word[0]) for word in x])
train['search_term']=train['search_term'].map(lambda x:[lmtizer.lemmatize(word[0]) for word in x])
train['brand']=train['brand'].map(lambda x:[lmtizer.lemmatize(word[0]) for word in x])

test['product_description']=test['product_description'].map(lambda x:[lmtizer.lemmatize(word[0]) for word in x])
test['product_title']=test['product_title'].map(lambda x:[lmtizer.lemmatize(word[0]) for word in x])
test['search_term']=test['search_term'].map(lambda x:[lmtizer.lemmatize(word[0]) for word in x])
test['brand']=test['brand'].map(lambda x:[lmtizer.lemmatize(word[0]) for word in x])

lmtizer=None#free up some of the cache

#check how many unique words at this stage
#print 'unique words in train and test after lemmatization'
#checkframe=pd.concat([train, test], ignore_index=True).drop(['id','relevance','product_uid'],1)
#print len(set(flatten(checkframe.values.tolist())))
#####################################
######apply porter stemmer
#stemmer = nltk.stem.PorterStemmer() # Create our stemmer 
#train['product_description']=train['product_description'].map(lambda x:[stemmer.stem(word) for word in x])
#train['product_title']=train['product_title'].map(lambda x:[stemmer.stem(word) for word in x])
#train['search_term']=train['search_term'].map(lambda x:[stemmer.stem(word) for word in x])
#train['brand']=train['brand'].map(lambda x:[stemmer.stem(word) for word in x])
#
#
#test['product_description']=test['product_description'].map(lambda x:[stemmer.stem(word) for word in x])
#test['product_title']=test['product_title'].map(lambda x:[stemmer.stem(word) for word in x])
#test['search_term']=test['search_term'].map(lambda x:[stemmer.stem(word) for word in x])
#test['brand']=test['brand'].map(lambda x:[stemmer.stem(word) for word in x])
#
#stemmer=None #free up some of the cache
#
###check how many unique words at this stage
#print 'unique words in train and test after stemming'
#checkframe=pd.concat([train, test], ignore_index=True).drop(['id','relevance','product_uid'],1)
#print len(set(flatten(checkframe.values.tolist())))
#####################################
############################
###split joined words specified in dictionary

train['product_description']=train['product_description'].map(lambda x:[joined_dict.get(word,word)for word in x])
train['product_title']=train['product_title'].map(lambda x:[joined_dict.get(word,word)for word in x])
train['search_term']=train['search_term'].map(lambda x:[joined_dict.get(word,word)for word in x])
train['brand']=train['brand'].map(lambda x:[joined_dict.get(word,word)for word in x])

test['product_description']=test['product_description'].map(lambda x:[joined_dict.get(word,word)for word in x])
test['product_title']=test['product_title'].map(lambda x:[joined_dict.get(word,word)for word in x])
test['search_term']=test['search_term'].map(lambda x:[joined_dict.get(word,word)for word in x])
test['brand']=test['brand'].map(lambda x:[joined_dict.get(word,word)for word in x])

print 'split joined words'
############################
#rejoin just so i can split again, so my unjoined words will be their own elements
train['product_description']=train['product_description'].map(lambda x:' '.join(x))
train['product_title']=train['product_title'].map(lambda x:' '.join(x))
train['search_term']=train['search_term'].map(lambda x:' '.join(x))
train['brand']=train['brand'].map(lambda x:' '.join(x))

test['product_description']=test['product_description'].map(lambda x:' '.join(x))
test['product_title']=test['product_title'].map(lambda x:' '.join(x))
test['search_term']=test['search_term'].map(lambda x:' '.join(x))
test['brand']=test['brand'].map(lambda x:' '.join(x))
######################################
####split the string columns into lists, giving us a nested list
train['product_description']=train['product_description'].map(lambda x: x.split(" "))
train['product_title']=train['product_title'].map(lambda x: x.split(" "))
train['search_term']=train['search_term'].map(lambda x: x.split(" "))
train['brand']=train['brand'].map(lambda x: x.split(" "))

test['product_description']=test['product_description'].map(lambda x: x.split(" "))
test['product_title']=test['product_title'].map(lambda x: x.split(" "))
test['search_term']=test['search_term'].map(lambda x: x.split(" "))
test['brand']=test['brand'].map(lambda x: x.split(" "))
############################
##fix a small set of common spelling errors, i can add this list later
train['product_description']=train['product_description'].map(lambda x:[spells_dict.get(word,word)for word in x])
train['product_title']=train['product_title'].map(lambda x:[spells_dict.get(word,word)for word in x])
train['search_term']=train['search_term'].map(lambda x:[spells_dict.get(word,word)for word in x])
train['brand']=train['brand'].map(lambda x:[joined_dict.get(word,word)for word in x])

test['product_description']=test['product_description'].map(lambda x:[spells_dict.get(word,word)for word in x])
test['product_title']=test['product_title'].map(lambda x:[spells_dict.get(word,word)for word in x])
test['search_term']=test['search_term'].map(lambda x:[spells_dict.get(word,word)for word in x])
test['brand']=test['brand'].map(lambda x:[spells_dict.get(word,word)for word in x])

print 'corrected some spelling'
############################
##replace synonyms
train['product_description']=train['product_description'].map(lambda x:[syns_dict.get(word,word)for word in x])
train['product_title']=train['product_title'].map(lambda x:[syns_dict.get(word,word)for word in x])
train['search_term']=train['search_term'].map(lambda x:[syns_dict.get(word,word)for word in x])
train['brand']=train['brand'].map(lambda x:[syns_dict.get(word,word)for word in x])

test['product_description']=test['product_description'].map(lambda x:[syns_dict.get(word,word)for word in x])
test['product_title']=test['product_title'].map(lambda x:[syns_dict.get(word,word)for word in x])
test['search_term']=test['search_term'].map(lambda x:[syns_dict.get(word,word)for word in x])
test['brand']=test['brand'].map(lambda x:[syns_dict.get(word,word)for word in x])

print 'generated synonym list'
############################
###############################
###remove empty strings from synonym columns
train['product_description']=train['product_description'].map(lambda x:filter(bool,x))
train['product_title']=train['product_title'].map(lambda x:filter(bool,x))
#train['search_term_syn ']=train['search_term_syn'].map(lambda x:filter(bool,x))
train['brand']=train['brand'].map(lambda x:filter(bool,x))

test['product_description']=test['product_description'].map(lambda x:filter(bool,x))
test['product_title']=test['product_title'].map(lambda x:filter(bool,x))
#test['search_term_syn ']=test['search_term_syn'].map(lambda x:filter(bool,x))
test['brand']=test['brand'].map(lambda x:filter(bool,x))

print 'removed empty string from synonym lists'
#####################################
######apply porter stemmer
#stemmer = nltk.stem.PorterStemmer() # Create our stemmer 
#train['product_description']=train['product_description'].map(lambda x:[stemmer.stem(word) for word in x])
#train['product_title']=train['product_title'].map(lambda x:[stemmer.stem(word) for word in x])
#train['search_term']=train['search_term'].map(lambda x:[stemmer.stem(word) for word in x])
#train['brand']=train['brand'].map(lambda x:[stemmer.stem(word) for word in x])
#
#
#test['product_description']=test['product_description'].map(lambda x:[stemmer.stem(word) for word in x])
#test['product_title']=test['product_title'].map(lambda x:[stemmer.stem(word) for word in x])
#test['search_term']=test['search_term'].map(lambda x:[stemmer.stem(word) for word in x])
#test['brand']=test['brand'].map(lambda x:[stemmer.stem(word) for word in x])
#
#stemmer=None #free up some of the cache
#
###check how many unique words at this stage
#print 'unique words in train and test after stemming'
#checkframe=pd.concat([train, test], ignore_index=True).drop(['id','relevance','product_uid'],1)
#print len(set(flatten(checkframe.values.tolist())))

###########################
# undo the splitting I did, so that it resembles the original data before I save it back to csv
train['product_description']=train['product_description'].map(lambda x:' '.join(x))
train['product_title']=train['product_title'].map(lambda x:' '.join(x))
train['search_term']=train['search_term'].map(lambda x:' '.join(x))
train['brand']=train['brand'].map(lambda x:' '.join(x))

test['product_description']=test['product_description'].map(lambda x:' '.join(x))
test['product_title']=test['product_title'].map(lambda x:' '.join(x))
test['search_term']=test['search_term'].map(lambda x:' '.join(x))
test['brand']=test['brand'].map(lambda x:' '.join(x))
#######################
##clean some 


######################################
#fill any na's we maybe have generated, so that it gets fed to the next part of code smoothly
train=train.replace('','__')
test=test.replace('','__')
train=train.replace('  ','__')
test=test.replace('  ','__')
train=train.replace(' ','__')
test=test.replace(' ','__')
train=train.replace('   ','__')
test=test.replace('   ','__')
train=train.replace('    ','__')
test=test.replace('    ','__')
train=train.fillna('__')
test=test.fillna('__')
#export to csv
train.to_csv('train7.csv',encoding="ISO-8859-1", index=False)
test.to_csv('test7.csv',encoding="ISO-8859-1", index=False)
