output_dir = "/Volumes/Trabalho/TESTE"
# output_dir = "/Users/guilhermejacob/Documents/TESTE"
datavault_dir = "/Volumes/DataVault/Censo Escolar"

# test if directory exists
import os
import shutil
if os.path.isdir( output_dir ) :
    shutil.rmtree( output_dir )

import pathlib
pathlib.Path(output_dir).mkdir(parents=True, exist_ok=True) 
#print(os.path.exists("/home/el/myfile.txt"))

# create catalog
import requests
from bs4 import BeautifulSoup
import re

url = 'http://portal.inep.gov.br/web/guest/microdados'
headers = {'user-agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36',
           'accept':'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8' ,
           'accept-encoding':'gzip, deflate'}
page = requests.get( url , headers=headers )

soup = BeautifulSoup(page.text, 'html.parser')

links = []
 
for link in soup.findAll('a', attrs={'href': re.compile("^http://")}):
    links.append(link.get('href'))

import pandas as pd
import numpy as np

links = pd.Series(links)[ pd.Series(links).str.contains('.zip$').tolist() ]

links = pd.Series(links)[ pd.Series(links).str.contains('censo_escolar').tolist() ]

links = np.array(links)

catalog = pd.DataFrame(data={'full_url': links})

catalog['full_url']

year = []

for x in catalog['full_url']:
    year.append( re.sub("(.*escolar_|.*escolar)|\\.zip$|_.*" , "" , x ) )
    #print( y )

catalog['year'] = pd.to_numeric( year )

catalog['output_folder'] = [output_dir + '/' + year[i] for i in range( 0 , len( year ) ) ]
catalog['dbfile'] = [output_dir + '/censo_escolar.sqlite' for i in range( 0 , len( year ) ) ]

# create datavault


# loop through catalog entries
import tempfile
tf= tempfile.TemporaryFile()

import sqlite3

# create and connect to database
conn = sqlite3.connect( catalog['dbfile'][ 1 ] )

import zipfile
