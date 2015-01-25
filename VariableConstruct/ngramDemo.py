#import codecs
import sys
import os
from operator import itemgetter
import re
import string


class ngram:

	def __init__(self):
		"""
		Initialization of the class
		"""


	def unigram(self,text):
		"""
		Function for generating unigrams(word) with frequency from a Unicode text
		Finds number of times occoures in the text and stores to a dictionery 'unifreq'
		Sorts the dictionery and prints it.
		"""

		self.text = text
		#self.unifreq = unifreq
		uniwords = self.__textReader(text)
		#print uniwords
		#global unifreq = {}

		global unifreq
		unifreq = {}

		for word in uniwords:
			unifreq[word] = uniwords.count(word)

		"""
		for unigram,frequency in sorted(unifreq.iteritems(), \
			key=lambda (key,value):(value,key),reverse=True):
			print unigram, frequency
		"""

		"""
		for word in uniwords:
			print word,"\t",uniwords.count(word)
			#print word,"\t",count(uniwords(word))
		"""
		return unifreq



	def bigram(self,text):
		"""
		Function to generate bigrams from a given text
		
		"""

		self.text = text
		uniwords = self.__textReader(text)
		bigrams = []

		for w in range(len(uniwords)-1):
			bigram = uniwords[w] + " " + uniwords[w+1]
			bigrams.append(bigram)
			#print bigram
			#print uniwords[w]," ",uniwords[w+1]

		global bigramfreq	
		bigramfreq = {}

		for bigram in bigrams:
			bigramfreq[bigram] = bigrams.count(bigram)


		"""
		for bigram,frequency in sorted(bigramfreq.iteritems(), \
			key = lambda (key,value):(value,key), reverse = True):
			print bigram," ", frequency
		"""
		"""
		for bigram in bigrams:
			print bigram
		"""
		return bigramfreq



	def trigram(self,text):
		"""
		Function to generate trigram from a given text.
		"""

		self.text = text
		uniwords = self.__textReader(text)
		trigrams = []
		
		for w in range(len(uniwords)-2):
			trigram = uniwords[w] + " " + uniwords[w+1] + " " +uniwords[w+2]
			trigrams.append(trigram)
			#print uniwords[w]," ",uniwords[w+1]," ",uniwords[w+2]

		global trigramfreq
		trigramfreq = {}

		for trigram in trigrams:
			trigramfreq[trigram] = trigrams.count(trigram)

		"""
		for trigram,frequency in sorted(trigramfreq.iteritems(), \
			key = lambda (key,value):(value,key), reverse = True):
			print trigram, " " , frequency
		"""
		"""
		for trigram in trigrams:
			print trigram

		"""
		return trigramfreq



	def saveAll(self, outf=None):
		"""
		Function to print unigram.
		The same function can be used to print the output to a file.
		It will print or save the output by frequency.
		The output file name should be given as parameter to the function.
		If no argument is passed to the function it will print the 
		unigram to the command line.
		"""

		self.unifreq = unifreq
		self.bigramfreq = bigramfreq
		self.trigramfreq = trigramfreq
		u1 = dict(unifreq, **bigramfreq)
		u2 = dict(u1, **trigramfreq)

		#out=codecs.open(outf, 'wb', 'utf8')
		#for unigram,frequency in sorted(unifreq.iteritems(), \
		#	key=lambda (key,value):(value,key),reverse=True):
		#	out.write(unigram+","+str(frequency)+"\n")
		#for bigram,frequency in sorted(bigramfreq.iteritems(), \
		#	key=lambda (key,value):(value,key),reverse=True):
		#	out.write(bigram+","+str(frequency)+"\n")
		#for trigram,frequency in sorted(trigramfreq.iteritems(), \
		#	key=lambda (key,value):(value,key),reverse=True):
		#	out.write(trigram+","+str(frequency)+"\n")
	
		return u2


	def __textReader(self,text):
		"""
		FUnction to read text and return a list of word.
		The function will read the text, removes the punctuation marks 
		if any and splits the text in to words. 
		This word list will be utilized by unigram() , 
		bigram() and trigram() functions.
		"""
		for p in string.punctuation:
			text = text.replace(p, " ")

		words=text.split()
		return words
