my_dict={}
page=1
a_str='&sortBy=recent'
text = requests.get(address).text
text = BeautifulSoup(text, 'html.parser')
my_dict[page] = text
#The following finds the paragraph contains the address to the next page
tmp = text.find_all('li', class_="a-last")
my_str= str(tmp[0])
my_str = re.findall('href="(.*)">Next',my_str)[0]#This extracts the address for the next page
address="http://www.amazon.com"+ my_str +a_str 
page+=1

while True:
    #print page
    #print address
    text = requests.get(address).text
    text = BeautifulSoup(text, 'html.parser')
    my_dict[page]= text
    
    #The following finds the paragraph contains the address to the net page
    tmp = text.find_all('li', class_="a-last")
    try:
        my_str= str(tmp[0])
    except IndexError:
        #print "Oh~~"
        continue
    try:
        my_str = re.findall('href="(.*)">Next',my_str)[0]
    except IndexError:
        print "finish downloading"
        break
    address="http://www.amazon.com"+ my_str +a_str
    page +=1
    t = np.random.rand()
    time.sleep(t)

    
def get_review_data_from_fix_page(review_page, row):
    index =0
    while True:
        try:
            review_df.loc[row]= get_review_data_from_fix_review( review_page[index] )
            row+=1
            #print row
            index+=1
        except IndexError:
            break
    
    return row

def get_review_data_from_fix_review( Tag ):
    #get the review ID
    review_txt =unicodedata.normalize('NFKD', Tag.prettify()).encode('ascii','ignore')
    review_ID = re.findall('id="(.*)">', review_txt)[0]
    
    #Reviewer account and profile address
    author_tag = Tag.find_all(class_='a-size-base a-link-normal author')[0]
    author_uni = author_tag.prettify()
    author_txt = unicodedata.normalize('NFKD', author_uni).encode('ascii','ignore')
    author = re.findall(' (.*)n</a>', author_txt)[0]
    author_address = author_tag.get('href')
    author_address_txt = unicodedata.normalize('NFKD', author_address).encode('ascii','ignore')
    author_address = "http://www.amazon.com"+author_address_txt
    
    #About the review text itself:
    date_tag = Tag.find_all('span', class_="a-size-base a-color-secondary review-date")[0]
    date_uni = date_tag.get_text()
    date_txt = unicodedata.normalize('NFKD', date_uni).encode('ascii','ignore')
    date_str = re.findall('on (.*)', date_txt)[0]
    date_tmp = time.strptime(date_str, "%B %d, %Y")
    date = dt.datetime(date_tmp[0], date_tmp[1], date_tmp[2])
    
    verify_tag=Tag.find_all('span', class_="a-size-mini a-color-state a-text-bold")
    if len(verify_tag)>0:
        verify_uni=verify_tag[0].get_text()
        verify_txt=unicodedata.normalize('NFKD', verify_uni).encode('ascii','ignore')
    else:
        verify_txt = None
    verify = (verify_txt=='Verified Purchase')
    
    
    title_tag= Tag.find_all('a', class_="a-size-base a-link-normal review-title
 a-color-base a-text-bold")[0]
    title_uni= title_tag.get_text()
    title_txt= unicodedata.normalize('NFKD', title_uni).encode('ascii','ignore')
    
    star_tag = Tag.find_all('i', class_="a-icon-star")[0]
    star_uni = star_tag.get_text()
    star_txt= unicodedata.normalize('NFKD', star_uni).encode('ascii','ignore')
    star = int(star_txt)
    
    review_tag= Tag.find_all('span', class_="a-size-base review-text")[0]
    review_uni= review_tag.get_text()
    review_txt= unicodedata.normalize('NFKD', review_uni).encode('ascii', 'ignore')
    
    return [review_ID, author, author_address, date, verify, title_txt, 
            star, review_txt]
review_df = pd.DataFrame(columns=['review_ID', 'author', 'author_adress', 
                                  'date', 'verify', 'title', 'star_given', 
                                  'review_txt'])
row  =0
for item in my_dict:
    review_page = my_dict





.find_all('div', class_="a-section review")
    row = get_review_data_from_fix_page(review_page, row)
review_sort = review_df.sort(columns=['star_given'])

chars_to_remove = set(string.punctuation)

def rm_target(raw_text, exclude):
    return ''.join(ch for ch in raw_text if ch not in exclude)

def filter_stop_word(word_list):
    return [word for word in word_list if word not in stopwords.words('english')]

def stem(word):
    for suffix in ['ing', 'ly', 'ed', 'ious', 'ies', 'ive', 'es', 's', 'ment']:
        if word.endswith(suffix):
            return word[:-len(suffix)]
    return word



###This part makes a dict of words showing up and their freq 
len_r =len(review_sort)
bag_of_word=[]
branch_freq_dict={}
for i in range(len_r):
    text= review_sort['review_txt'].iloc[i] #get a particular review
    text= text.lower()      # chage into lower case
    text= rm_target(text, chars_to_remove) #remove punctuation
    text= text.split()       #string to list
    text= list(set(text)) #Same words in one review account for 1 freq
    bag_of_word += text

#remove the stop next (this can apply to list, not string)
filtered_word = filter_stop_word( bag_of_word )

#obtain the branch_freq dict
for item in filtered_word:
    if item in branch_freq_dict.keys():
        branch_freq_dict






def matching(sentence, stem_set):
    word_bag = sentence.split()
    if len(set(word_bag) & stem_set)==0:
        return
    else:
        return sentence

def separate(paragraph):
    sentences = re.split('(?<!w.w.)(?<![A-Z][a-z].)(?<=.|?)s', paragraph)
    return sentences
    
def addlst(x, y):
    return x+y

print('The level of rated to be investigated:nEnter 5 for 5 starsnEnter 4 for 4 starsnEnter 3 for LESS THAN 3 starsn')
star = input("Enter:")
if 5>= star & star>=4:
    select_df = review_df[ review_df['star_given']==star ]
elif star ==3:
    select_df = review_df[ review_df['star_given']<=star ]
else:
    print "Invalid input, %d rated doen't exist" % star
    raise
    
txt_lst= list(select_df['review_txt'])
sentence_lst = map(separate, txt_lst)
word= raw_input('nThe word you want to investigate? ')
sentence_lst = reduce(addlst, sentence_lst)
a = np.repeat(set(stem_branch_dict[word]),len(sentence_lst) , axis=0)
a= list(a)
result_lst =map(matching, sentence_lst,a)
i=1
print "n"
for term in result_lst:
    if term!= None:
        print str(i)+". "+term+'n'
        i+=1