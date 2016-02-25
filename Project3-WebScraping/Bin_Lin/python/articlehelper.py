'''
Created on Feb 21, 2016

@author: binlin
'''

from IPython.display import HTML
from bs4 import BeautifulSoup
import requests
import json
from datetime import datetime, timedelta
import time
import os
import re
import sys, traceback
from textblob import TextBlob
import ast

from articles import Article
import jsonhelper
import constants

def parseArticleSummary(article, articleType):
    '''
    @summary: : parse article summary in a dictionary. 
    @param article:
    @param articleType:  
    @return: Return information in dictionary
    '''

    articleDict = {}
    articleDict['id'] = article['_id']
    articleDict['link'] = article['link']
    articleDict['post_date'] = article['post_date']
    articleDict['title']= article['title']
    articleDict['author'] = article['author']
    #articleDict['sort_key'] = article['sort_key']
    articleDict['content'] = article['content']['plain']
    articleDict['shares'] = article['shares']['total']
    articleDict['channel'] = article['channel']
    articleDict['type'] = articleType
    return articleDict

def getRecentArticleList():
    '''
    @summary: : A function that retrieves a list of article through Mashable API. 
    @return: List of dictionaries that contain basic article information
    '''

    stopDate = datetime.now() - timedelta(days=14)
    types = ['new', 'hot', 'rising']
    requestUrlFormat = constants.MashableRequestUrlTemplate
  
    pageSizeDict = {'new': 100, 'hot': 50, 'rising': 50}; afterDict = {}
    for t in types:
        #pageSizeDict[t] = 100
        afterDict[t] = ''
        
    #pageSizeDict['new'] = 20

    results = []
    i = 0
    while (True):
        
        requestUrl = requestUrlFormat.format(pageSizeDict['hot'], pageSizeDict['new'], pageSizeDict['rising'], afterDict['new'], afterDict['hot'], afterDict['rising'])
        res = requests.get(requestUrl)
        contentJson = res.json()

        print 'Request made: ' + requestUrl
        
        last_sort_article_type = None
        smallest_sort_key = ''
        
        for articleType in types:
            articleList = contentJson[articleType]
            
            if(len(articleList) > 0):
                #parse the article list and add the article to the list
                for article in articleList:    
                    articleDict = parseArticleSummary(article, articleType)
                    results.append(articleDict)

                # get the post date of the last article for a certain type
                last_post_date = datetime.strptime(articleList[-1]['post_date'][0:19], '%Y-%m-%dT%H:%M:%S') 
                
                smallest_sort_key_temp = articleList[-1]['sort_key']
                if(smallest_sort_key_temp == ''): 
                    smallest_sort_key_temp = min([a for a in articleList if a != ''])

                # update the page size for each article type, set to 0 to stop receiving more
                if(last_post_date < stopDate):
                    pageSizeDict[articleType] = 0
                elif(smallest_sort_key == '' or smallest_sort_key  > smallest_sort_key_temp):
                    smallest_sort_key = articleList[-1]['sort_key']
                    last_sort_article_type = articleType
                 
        # Reset the sort key before next call
        for key, value in afterDict.iteritems():
            if(key == last_sort_article_type):
                afterDict[key] = smallest_sort_key
            else:
                afterDict[key] = ''
            
        # if page size for all type is 0, break the while loop
        if all(value == 0 for key,value in pageSizeDict.items()):
            print pageSizeDict
            print "all page size is 0, break the while loop"
            break
            
        # put a break here in case somethin wrong and cause infinite loop
        i += 1
        if(i > 100):
            break
        
        if(i % 5 == 0):
            print 'Sleeps for a few seconds...'
            time.sleep(2)
            
            
    return results

def saveArticleListToDisk(articleList):
    '''
    @summary: Save results into json file
    @param results: list of dictionary that contains basic information of a article/news 
    @return: the file path saved to local disk
    '''

    #print results
    
    timestamp = datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3].replace(':', '').replace('.', '').replace(' ', '-')

    filePath = constants.DataDir + constants.ArticleListFileName.format(timestamp)
    
    print 'total articles: ' + str(len(articleList))
    with open(filePath, 'w') as outfile:
        json.dump(articleList, outfile, sort_keys = True, indent=4, separators=(',', ': '))

    print "Finished saving json file: " + filePath
    
    return filePath


def crawlArticleHtmlPages(articleListFilePath):
    '''
    @summary: Crawl the html pages by following the links for each article in the article list file. 
    Html pages will be saved to pages.
    @param articleListFilePath: Path of the JSON file that contains the article list
    '''
    articleList = None
    
    # read in the json file that contains the list of articles
    with open(articleListFilePath) as data_file:    
        articleList = json.load(data_file)

    for i in xrange(len(articleList)):    
    #for i in xrange(50):   
        article = articleList[i]
        link = article['link']
        
        fileName = link[20:].replace('/', '-',3).replace('/', '') + '.html'
        
        #print fileName

        if(not os.path.isfile(constants.ArticleHtmlDir + fileName)):
            #'http://mashable.com/2016/02/18/crouching-tiger-new-trailer/'
            res = requests.get(link)
            text = res.text
            
            with open(constants.ArticleHtmlDir + fileName, 'w') as outfile:
                outfile.write(text.encode('utf8'))
                
                outfile.close()
                print link + ' is saved as: ' + fileName
        else:
            print fileName + ' exits, skipped.\n'

        if((i + 1) % 10 == 0):
            print 'Sleeps for a few seconds...'
            time.sleep(5)
            #break
            
    print "Finished saving article html files"
    
    


def parseArticleHtml(articleListFilePath):
    '''
    @summary: Parse the html page for each article by using BeautifufSoup and save them into JSON format.
    @return: Return a list of Article objects that contain details of each article.
    '''
    
    articleList = None
    with open(articleListFilePath) as data_file:    
        articleList = json.load(data_file)

    results = set()
    for i in xrange(len(articleList)):    
        article = Article()
        
        article = jsonhelper.simple_dict_to_object(articleList[i], article)
                        
        fileName = article.link[20:].replace('/', '-',3).replace('/', '') + '.html'
        #fileName = '2016-02-23-teens-marijuana-photos.html'
        filePath = constants.ArticleHtmlDir + fileName
        
        #filePath = constants.ArticleHtmlDir + '2016-02-18-amazon-studios-picks-up-untitled-woody-allen-movie.html'
                
        # parse html if file exists
        if(os.path.isfile(filePath)):
            articleFile = open(filePath)

            try: 
                bs = BeautifulSoup(articleFile, 'html.parser')
            
                # get total share
                shareNode = bs.find('div', {'class': 'total-shares'})
                if(shareNode):
                    article.shares = shareNode.get_text().replace('\n', '').replace('Shares', '')
                else:
                    shareNode = bs.find(lambda tag: tag.has_attr('data-shares'))
                    article.shares = shareNode.get('data-shares')
                    
                if(article.shares.endswith('k')):
                    article.shares = int(float(article.shares[:-1]) * 1000)
                
                article.shares = int(article.shares)

                # Get Number of Links
                links = bs.find_all('a')
                article.num_hrefs =  len(links)

                # Get links to other articles
                otherArticleLinks = bs.find_all('a', {'href': lambda value: value and re.search('mashable.com/\d{4}/\d{2}/\d{2}/', value)})
                article.num_self_hrefs = len(otherArticleLinks)

                # Get content tag
                contentTag = bs.find('section', {'class': lambda value: value and value.startswith('article-content')})
                                
                #video type article is different
                if(not contentTag):
                    contentTag = bs.find('section', {'id': 'video-matting'})
                    
                # now another type, seems post photos
                if(not contentTag):
                    contentTag = bs.find('div', {'id': 'skrollr-body'})
                    
                #also some article in iframe
                if(not contentTag): 
                    iframeDivTag = bs.find(lambda tag: tag.has_attr('data-url'))
                    
                    if(iframeDivTag):
                        iframeUrl = iframeDivTag.get('data-url')
                        
                        res = requests.get(iframeUrl)
                        iframeContent = res.text
                        
                        bsIframe = BeautifulSoup(iframeContent, 'html.parser')
                        contentTag = bsIframe.find('div', {'id': 'content'})

                # Get number of images in the article
                images = contentTag.find_all('img')
                if(images):
                    article.num_imgs = len(images)

                # Get number of videos in the article
                youtubeVideos = contentTag.find_all(lambda tag: tag.has_attr('src') and 'youtube.com' in tag.get('src'))
                ooyalaVideos = contentTag.find_all(lambda tag: tag.has_attr('data-video'))
                article.num_videos = len(youtubeVideos) + len(ooyalaVideos)
                
                # get topics
                footerTopicsTag = bs.find('footer', {'class': 'article-topics'})
                
                if(footerTopicsTag):
                    article.topics = footerTopicsTag.get_text().replace("Topics:", "").replace("\n", "")
                else:
                    # assume it is from iframe if not found in footer
                    jsTag  = bs.find("script", {'type': 'application/ld+json'})
                    scriptContent = jsTag.get_text()
                    dic = json.loads(scriptContent.decode('utf-8'))
                    #print dic
                    #dic = ast.literal_eval(scriptContent)
                    article.topics = dic['metadata']['omnitureData']['topics']

                # get Days between the article publication and the dataset created
                post_date = datetime.strptime(article.post_date[0:19], '%Y-%m-%dT%H:%M:%S')                 
                article.timedelta = (datetime.now() - post_date).days
                
                # get number of keywords from meta in head
                keywords = bs.head.find('meta', {'name': 'keywords'}).get('content')
                #print 'keywords: ' + keywords
                article.num_keywords = len(keywords.split(','))

                contentBlob = TextBlob(article.content)
                                
                # Number of words in the content
                article.n_tokens_content = len(contentBlob.words)
                
                # article sentiment
                article.content_sentiment_polarity = contentBlob.sentiment.polarity
                article.content_subjectivity = contentBlob.sentiment.subjectivity
                

                titleBlob = TextBlob(article.title)

                # Number of words in the title
                article.n_tokens_title = len(titleBlob.words)

                # title sentiment
                article.title_sentiment_polarity = titleBlob.sentiment.polarity
                article.title_subjectivity = titleBlob.sentiment.subjectivity
                
                #results.add(article.to_dict())
                results.add(article)
                
                #print article
                print 'Parsed: ' + fileName
            except Exception as ex: 
                print 'Error in: ', fileName
                traceback.print_exc()
            finally:
                articleFile.close()
        else:
            print 'File not found: ' + fileName
         
        '''
        i += 1
        if(i > 3):   
            break
        '''
            
    return results

    

def saveArticleDetailToDisk(articleDetailList):
    '''
    @summary: Save results into JSON file in local disk. Note, Result is a list of dictionary that contains article details.
    '''
    
    print 'total articles: ' + str(len(articleDetailList))
    
    results = [article.to_dict() for article in articleDetailList ]
    
    #print results
    
    timestamp = datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3].replace(':', '').replace('.', '').replace(' ', '-')
    
    filePath = constants.DataDir + constants.ArticleDetailFileName.format(timestamp)
    
    with open(filePath, 'w') as outfile:
        json.dump(results, outfile, sort_keys = True, indent=4, separators=(',', ': '))

    print "Finished saving json file" + filePath
    
