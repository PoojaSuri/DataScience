#!/usr/bin/python

import re
import os
import pprint
import string
import operator
import csv
import unicodedata

import nltk
from nltk.corpus import stopwords
from nltk.tokenize import wordpunct_tokenize, word_tokenize
from nltk.stem.porter import PorterStemmer
from nltk.stem.snowball import EnglishStemmer
from nltk import bigrams, trigrams, ngrams

import pandas as pd
import numpy as np


def get_ngrams(text, n = "bi"):
    text = word_tokenize(text)
    if n == "bi":
        return bigrams(text)
    elif n == "tri":
        return trigrams(text)

def cleanup(text):
    text = str(text)
    text = lower(text)
    text = remove_accents(text)
    text = remove_special_chars(text)
    text = remove_extra_space(text)
    text = remove_stopwords(text)
    text = remove_punctuation(text)
    text = stemmed(text) 
    return text

def lower(text):
    """change everything to lowercase
    """
    text = text.lower()
    return text

def remove_accents(text):
    """Remove diacritics
    """
    nkfd_form = unicodedata.normalize('NFKD', text.decode('latin-1'))
    text = u"".join([c for c in nkfd_form if not unicodedata.combining(c)])

    text = re.sub(r'[\x80-\xFF]', '', text)

    return text

def remove_special_chars(text):
    """remove all special characters except the period (.)
       comma (,) and question mark (?)
       for instance, ">", "~", ", $, |, etc.
    """
    schars = ''.join([a for a in string.punctuation if a not in ".,?"])

    text = re.sub('[%s]' % re.escape(schars), '', text)
    return text

def remove_extra_space(text):
    """Remove multiple whitespaces
    """
    text = " ".join(text.split())
    return text


def remove_stopwords(text, swords=None):
    """Remove stopwords
    """
    if swords is None:
        swords = stopwords.words('english')
    words = wordpunct_tokenize(text)
    words = [w for w in words if w not in swords]
    text = ' '.join(words)
    return text


def remove_punctuation(text):
    """Replace punctuation mark with space
    """
    text = re.sub('[%s]' % re.escape(string.punctuation), '', text)
    return text


def stemmed(text, snowball=False):
    """Returns stemmed text
    """
    if snowball:
        st = EnglishStemmer()
    else:
        st = PorterStemmer()
    words = wordpunct_tokenize(text)
    words = [st.stem(w) for w in words]
    text = ' '.join(words)

    return text

def total_freq(ngrams):
    sum = 0
    for k, v in nltk.FreqDist(ngrams).items():
        sum += v
    return sum

def analyse_grams(ngrams_r, ngrams_d, ngrams_all):
    ngram_data = {}
    ngram_freqs_r = {}
    ngram_freqs_d = {}
    total_ngram_freq = 0
    ngram_chisq = {}

    for ngram, freq in nltk.FreqDist(ngrams_r).items():
        ngram_freqs_r[" ".join(list(ngram))] = freq

    for ngram, freq in nltk.FreqDist(ngrams_d).items():
        ngram_freqs_d[" ".join(list(ngram))] = freq

    for ngram, freq in nltk.FreqDist(ngrams_all).items():
        total_ngram_freq = total_ngram_freq + freq
        ngram_str = " ".join(list(ngram))
        ngram_data[ngram_str] = {}
        ngram_data[ngram_str]['all'] = freq

    for ngram_str in ngram_data.keys():
        ngram_data[ngram_str]['r'] = ngram_freqs_r[ngram_str] if ngram_str in ngram_freqs_r else 0
        ngram_data[ngram_str]['d'] = ngram_freqs_d[ngram_str] if ngram_str in ngram_freqs_d else 0
        ngram_data[ngram_str]['not_r'] = total_ngram_freq - ngram_data[ngram_str]['r']
        ngram_data[ngram_str]['not_d'] = total_ngram_freq - ngram_data[ngram_str]['d']

        if (ngram_data[ngram_str]['r'] == 0 and ngram_data[ngram_str]['d'] == 0):
            ngram_data[ngram_str]['chisq'] = 0
        else:
            ngram_data[ngram_str]['chisq'] = chisq(ngram_data[ngram_str]['r'], ngram_data[ngram_str]['d'], 
                                             ngram_data[ngram_str]['not_r'], ngram_data[ngram_str]['not_d'])

        ngram_chisq[ngram_str] = ngram_data[ngram_str]['chisq']

        #print "%s : %s :%s : %s : %s : %s : %s" % (ngram_str, ngram_data[ngram_str]['all'], ngram_data[ngram_str]['r'], 
        #                                 ngram_data[ngram_str]['d'], ngram_data[ngram_str]['not_r'],
        #                                 ngram_data[ngram_str]['not_d'], ngram_data[ngram_str]['chisq'] )


    return ngram_chisq

def chisq(fr_r, fr_d, not_fr_r, not_fr_d):
    chi = float(( (fr_r * not_fr_d) - (fr_d * not_fr_r) )^2) / float((fr_r+fr_d)*(fr_r+not_fr_r)*(fr_d+not_fr_d)*(not_fr_r+not_fr_d))
    return chi


# merge the two files first
file1 = "./capitolwords.xlsx"
file2 = "./voteview_combined.xlsx"

df1 = pd.read_excel(file1, parse_cols="A:D")
df2 = pd.read_excel(file2, parse_cols="A:E")
dfo = pd.merge(df1, df2, how="left", on="State.Name")

# init nltk first time
nltk.data.path.append("./nltk_data")
#nltk.download('stopwords', './nltk_data')
#nltk.download('punkt', './nltk_data')

# cleanup, tokenize, yada yada
dfo['Speech_clean'] = dfo.apply(lambda row: cleanup(row['Speech']), axis=1)

words_r = ""
words_d = ""
words_all = ""

# Combine all words spoken by everyone
for index, row in dfo.iterrows():
    if row['Party'] == 'R':
        words_r += row['Speech_clean']
    elif row['Party'] == 'D':
        words_d += row['Speech_clean']

words_all += words_r
words_all += words_d

# calculate bigrams and trigrams for all
bigrams_r = get_ngrams(words_r, "bi")
bigrams_d = get_ngrams(words_d, "bi")

trigrams_r = get_ngrams(words_r, "tri")
trigrams_d = get_ngrams(words_d, "tri")

bigrams_all = get_ngrams(words_all, "bi")
trigrams_all = get_ngrams(words_all, "tri")

# calculate chi square value for each bigram and trigram
bigrams_chisq = analyse_grams(bigrams_r, bigrams_d, bigrams_all)
trigrams_chisq = analyse_grams(trigrams_r, trigrams_d, trigrams_all)

sorted_bigrams_chisq = sorted(bigrams_chisq.items(), key=operator.itemgetter(1), reverse=True)
del sorted_bigrams_chisq[500:]

sorted_trigrams_chisq = sorted(trigrams_chisq.items(), key=operator.itemgetter(1), reverse=True)
del sorted_trigrams_chisq[500:]

bigramFile = open("bigrams.csv",'wb')
wr = csv.writer(bigramFile, dialect='excel')
wr.writerow(zip(*sorted_bigrams_chisq)[0])

trigramFile = open("trigrams.csv",'wb')
wr = csv.writer(trigramFile, dialect='excel')
wr.writerow(zip(*sorted_trigrams_chisq)[0])

#pp = pprint.PrettyPrinter(indent=4)
#pp.pprint(bigrams_data)

# write to CSV
dfo.to_csv("./merged.csv")