import numpy as np
import pandas as pd 
from sklearn.pipeline import Pipeline
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import LinearSVC
from sklearn.metrics import accuracy_score 


with open('modelSVC.pkl', 'rb') as file:
    model = pickle.load(file)  
    
    
    
def encode(senti): 
  if senti=='neutral': 
    return(0) 
  elif senti=='positive': 
    return(1) 
  else: 
    return(-1) 
    
    
    
    
def news201(title , source=None): 
  today = datetime.today()
  two_weeks_ago = today - timedelta(days=7) 
  data=newsapi.get_everything (q=title , sources=source , language='en', 
            sort_by='relevancy', to=today, page_size=100) 
  artic=data['articles']   
  news=pd.DataFrame(artic) 
  news=news.dropna()
  texot=news['description']
  predictions = model.predict(texot)
  news['sentiment']=predictions 
  news['sentiment']= news['sentiment'].apply(encode) 
   
  
  return(news) 












