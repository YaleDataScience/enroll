'''
Created Jan 2015

@author: dcgreaves
'''

import BeautifulSoup
import re, requests

Soup = BeautifulSoup.BeautifulSoup

with open('subjects.txt', 'r') as f:
    subjects = f.read().split()

for s in subjects:
    url = 'https://ivy.yale.edu/course-stats/?termCode=201501&subjectCode='+s
    r = requests.get(url)
    soup = Soup(r.text)
    classes = soup.findAll('tr', attrs={'class': ['even highlight', 'odd highlight']})
    for c in classes:
        coursenum = c.find('a').text
        demand = c.find('table').text
        demand = re.sub('&nbsp;', '\t', demand)
        print coursenum+demand
