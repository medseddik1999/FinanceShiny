#!pip install newsapi-python 
from newsapi import NewsApiClient  
import datetime as dt 
import pandas as pd 
from datetime import datetime, timedelta

today = datetime.today()

# Calculate the start date of the last two weeks
two_weeks_ago = today - timedelta(weeks=2) 







newsapi=NewsApiClient('5f05f12b7eff4f62838aef95a197f887') 


data=newsapi.get_everything (q='stock market news' , language='en',from_param= two_weeks_ago ,
            sort_by='relevancy', to=today, page_size=100)
      
dita=newsapi.get_sources()

dita=dita['sources'] 

dita1=pd.DataFrame(dita)

dita1=dita1[dita1['language']=='en'] 

dita1=dita1[['id','name']] 

dita1.to_csv('source.csv' , index = False) 


