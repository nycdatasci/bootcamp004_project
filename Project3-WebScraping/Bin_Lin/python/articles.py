class Article(object):
    def __init__(self):
        self.id = None
        self.link = None
        self.post_date = None
        self.title = None
        self.author = None
        self.shares = 0
        self.channel = None
        self.type = None
        self.content = None
        self.timedelta = None
        self.n_tokens_title = 0
        self.n_tokens_content = 0
        self.num_hrefs = 0
        self.num_self_hrefs = 0
        self.num_imgs = 0
        self.num_videos = 0
        self.num_keywords = 0
        self.topics = None
        self.content_sentiment_polarity = None
        self.content_subjectivity = None
        self.title_sentiment_polarity = None
        self.title_subjectivity = None

    def __repr__(self):
        return "Article(" + str(self.id) + ')'
        
    def __eq__(self, other):
        if isinstance(other, Article):
            return (self.id == other.id)
        else:
            return False
        
    def __ne__(self, other):
        return (not self.__eq__(other))
    
    def __hash__(self):
        return hash(self.__repr__())

    def to_dict(self):
        #return dict([(k, v) for k, v in self.__dict__.iteritems() if not k.startswith('_')])
        return self.__dict__
    
    def __str__(self):
        my_str = ''
        
        for item in self.to_dict().iteritems():
            my_str += str(item) + '\n'
        
        return my_str
        
