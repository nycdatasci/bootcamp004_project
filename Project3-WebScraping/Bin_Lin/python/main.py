'''
Created on Feb 21, 2016

@author: binlin
'''
import constants
import articlehelper
import os
import re


if __name__ == '__main__':
    
    articleList = articlehelper.getRecentArticleList()
    
    articleListFilePath = articlehelper.saveArticleListToDisk(articleList)
    
    articlehelper.crawlArticleHtmlPages(articleListFilePath)
    
    articleListFilePath = 'data/marshable_article_list_2016-02-24-095828259.json'
    results = articlehelper.parseArticleHtml(articleListFilePath)
    articlehelper.saveArticleDetailToDisk(results)
    