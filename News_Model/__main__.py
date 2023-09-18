from newsapi import NewsApiClient  
import pandas as pd 
import numpy as np 
import tensorflow as ts
import text_hammer as th 
import spacy 
from keras.models import load_model 
from keras.preprocessing.text import Tokenizer
import pickle 
import json  
from datetime import datetime, timedelta
from keras.preprocessing.text import tokenizer_from_json
from keras.preprocessing.sequence import pad_sequences 
from newsapi import NewsApiClient   
import os
from dotenv import load_dotenv 

load_dotenv() 

my_token = os.getenv("NEWS_TOKEN") 


model=load_model('model_fanacial_setiment.h5') 
newsapi=NewsApiClient(my_token) 



with open("keras_tokenizer.json", "r") as json_file:
    tokenizer_json = json_file.read()


tokenizer = tokenizer_from_json(tokenizer_json)


def process_text(text):
  text=text.lower()
  text=th.cont_exp(text)
  text=th.remove_emails(text)
  text=th.remove_emails(text)
  text=th.remove_html_tags(text)
  text=th.remove_stopwords(text)
  text=th.remove_special_chars(text)
  text=th.remove_accented_chars(text) 
  text=th.make_base(text) 
  return(text) 

def convert_pred(pred): 
  if pred==0: 
    t=1 
  elif pred==1: 
    t=-1 
  else: 
    t=0 
  return(t)



model=load_model('model_fanacial_setiment.h5') 


def tok():
    with open("keras_tokenizer.json", "r") as json_file:
          tokenizer_json = json_file.read()
    tokenizer = tokenizer_from_json(tokenizer_json) 
    return(tokenizer)


tokenizer=tok()  


def convert_pred(pred): 
  if pred==0: 
    t=1 
  elif pred==1: 
    t=-1 
  else: 
    t=0 
  return(t)




def news20(): 
  today = datetime.today()
  two_weeks_ago = today - timedelta(days=7) 
  data=newsapi.get_everything (q='stock market news' , language='en',from_param= two_weeks_ago,
            sort_by='relevancy', to=today, page_size=100) 
  artic=data['articles']   
  news=pd.DataFrame(artic) 
  news=news.dropna()
  texot=news['description'].apply(process_text)    
  texot =tokenizer.texts_to_sequences(texot)
  texot=pad_sequences(texot,maxlen=81,padding='post') 
  predictions = model.predict(texot)
  pred=np.argmax(predictions , axis=1)  
  pred=[convert_pred(item) for item in pred] 
  news['sentiment']=pred  
  return(news)




def news201(title , source=None): 
  today = datetime.today()
  two_weeks_ago = today - timedelta(days=7) 
  data=newsapi.get_everything (q=title , sources=source , language='en', 
            sort_by='relevancy', to=today, page_size=100) 
  artic=data['articles']   
  news=pd.DataFrame(artic) 
  news=news.dropna()
  texot=news['description'].apply(process_text)    
  texot =tokenizer.texts_to_sequences(texot)
  texot=pad_sequences(texot,maxlen=81,padding='post') 
  predictions = model.predict(texot)
  pred=np.argmax(predictions , axis=1)  
  pred=[convert_pred(item) for item in pred] 
  news['sentiment']=pred  
  return(news)
  


#dota=news201('china' , source=None) 


#source=pd.read_csv('source.csv')   


#liso=[]
#for index,row in source.iterrows(): 
#   try: 
#    tak=news201('stock' , source=source['id'][index])  
#    liso.append(row) 
#   except: 
#     pass 
    




