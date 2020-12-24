
from collections import Counter
import sys, csv
#from ggplot import *
#import numpy as np
from csv import DictReader
import os.path
from os import system, name,  path
from datetime import datetime
from datetime import date
import pandas as pd
import researchpy as rp
import requests
from zipfile import ZipFile
from bs4 import BeautifulSoup
import errno
import yaml
import fnmatch
from PIL import Image
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator
import matplotlib.pyplot as plt
import warnings
import string
import re
import nltk
#nltk.download('stopwords')
from nltk.corpus import stopwords
import re
from langdetect import detect
import time
from babel import Locale
import pycountry
from shutil import copyfile
RecordCounter=False
TargetFileNameStub = "kickstarter_filtered"
TargetAllFileNameStub = "kickstarter_all"
OutputString = "Kickstarter Autominer Filter Settings\n"
# pip install numpy==1.19.3
# pip install pandas
# pip install researchpy
# pip install requests
# pip install bs4
# pip install pyyaml
# pip install image
# pip install wordcloud
# pip install nltk
# pip install langdetect
# pip install babel
# pip install pycountry

def OutputFilters(ConfigFileItems,OutputString):
	# heres where we are going to output the data to a file
	OutputString += "GoBackDays:" +  str(ConfigFileItems['GoBackDays']) + " days\n"
	OutputString += "GoalGroupCount:" +  str(ConfigFileItems['GoalGroupCount']) + "\n"
	OutputString += "IncludeBlurb:" +  str(ConfigFileItems['IncludeBlurb']) + "\n"
	OutputString += "IncludeCancelled:" +  str(ConfigFileItems['IncludeCancelled']) +"\n"
	OutputString += "IncludeCountries:" +  str(	','.join(ConfigFileItems['IncludeCountries'])) +"\n"
	OutputString += "IncludeLanguages:" +  str(	','.join(ConfigFileItems['IncludeLanguages'])) +"\n"
	OutputString += "IncludeLive:" +  str(ConfigFileItems['IncludeLive']) +"\n"
	OutputString += "KeyWordsOne:" +  str(ConfigFileItems['KeyWordsOne']) +"\n"
	OutputString += "KeyWordsTwo:" +  str(ConfigFileItems['KeyWordsTwo']) +"\n"
	OutputString += "MaxGoal:" +  str(ConfigFileItems['MaxGoal']) +"\n"
	OutputString += "MaxPledge:" +  str(ConfigFileItems['MaxPledge'])+"\n"
	OutputString += "MinBackers:" +  str(ConfigFileItems['MinBackers'])+"\n"
	OutputString += "MinPledge:" +  str(ConfigFileItems['MinPledge'])+"\n"
	OutputString += "PledgedGroupCount:" +  str(ConfigFileItems['PledgedGroupCount'])+"\n"
	OutputString += "SourceDirectory:" +  str(ConfigFileItems['SourceDirectory'])+"\n"
	OutputString += "TargetDirectory:" +  str(ConfigFileItems['TargetDirectory'])+"\n"
	return OutputString



def addlanguages(FileName,ConfigFileContents):
	print('Adding languages for ' + FileName)
	if "all" in FileName:
		LangStartTime = datetime.now()
				#print('Doing languages for ALL dataset, started at:' +  AllLangStartTime.strftime("%H:%M:%S")   +'\n... might as well grab lunch...')
		print("Processing ALL Dataset ... this could take 30 minutes or more.\nStarted at:" +   LangStartTime.strftime("%H:%M:%S") )
	targetfiledf =  pd.read_csv(FileName, encoding="utf-8", error_bad_lines=False)
	for i in range(len(targetfiledf)):
	#	print(targetfiledf['language'].values[i] )
		
		TheBlurb = targetfiledf['blurb'].values[i]
		
		DetectedLanguage = detectlanguage(TheBlurb)
	#	input ('a' + targetfiledf['language'].values[i])
		#print (DetectedLanguage)
		targetfiledf['language'].values[i] = DetectedLanguage
	#	input ('b' + targetfiledf['language'].values[i])
		targetfiledf['languagename'].values[i] = getlanguagename(targetfiledf['language'].values[i])
	#	print(targetfiledf['language'].values[i] )
	targetfiledf.to_csv(FileName, index=False) 

def filterlanguages(FileName,ConfigFileContents):
	print('Filtering languages for ' + FileName)
	
	targetfiledf =  pd.read_csv(FileName, encoding="utf-8", error_bad_lines=False)
	StartCount = len(targetfiledf)
	filtereddf = targetfiledf[targetfiledf['language'].isin(ConfigFileContents['IncludeLanguages'])]
	EndCount = len(filtereddf)
	#input(filtereddf.to_string())
	filtereddf.to_csv(FileName, index=False)
	return StartCount - EndCount



def getlanguagename(langcode):
	try:
		langcode = langcode.lower()
		LanguageName = pycountry.languages.get(alpha_2=langcode)
		if LanguageName is None:
			return 'None'
		else:
			LanguageName = LanguageName.name
			LanguageName = str(LanguageName)
			#print (langcode + " converts to " + str(LanguageName))
			return LanguageName
			
	except:
		#input(langcode)
		return'Unknown'

def detectlanguage(sometext):
#	return True
	
	try:
		DetectedLanguage = detect(sometext)
		#DetectedLanguage = 'en'
		return DetectedLanguage

	except:
		return 0




def updateFileName(FileName):
	numberend=FileName.rfind(".csv")
	numberbegin=numberend -3
	# THIS UPDATES THE FILE NAME TO MOVE IT TO THE NEXT FILE IN THE LIST
# EXPECTING 'c:\\1\source\\kickstarter.csv' OR 'c:\\1\source\\kickstarter001.csv'

	number=FileName[numberbegin:numberend]
	if number.isnumeric():
	# ITS ALREADY A NUMBER FILE, INCRIMENT THE NUMBER
	# KICKSTARTER001.CSV SHOULD BECOME KICKSTARTER002.CSV
		number = int(number) + 1
		strnumber = str(number)
		if len(strnumber) == 1:
			strnumber = "0" + "0" + strnumber
		if len(strnumber) == 2:
			strnumber = "0" + strnumber
		FileName=FileName[0:numberbegin]
		FileName = FileName + strnumber + ".csv"
		while len(strnumber) < 3:
			strnumber = str(0) + str(number)
		
		
	else:
# ITS NOT NUMERIC, ADD THE 001
# KICKSTARTER.CSV BECOMES KICKSTARTER001.CSV
		FileName = FileName[0:numberend] + '001' + '.csv'
	
	return(FileName)
def extractstate(location):
# THIS EXTRACTS THE STATE VALUE FROM THE LOCATION FIELD.
# SAMPLE DATA:
# {"id":2468964,"name":"Pasadena","slug":"pasadena-ca-us","short_name":"Pasadena, CA","displayable_name":"Pasadena, CA","localized_name":"Pasadena","country":"US","state":"CA","type":"Town","is_root":false,"expanded_country":"United States","urls":{"web":{"discover":"https://www.kickstarter.com/discover/places/pasadena-ca-us","location":"https://www.kickstarter.com/locations/pasadena-ca-us"},"api":{"nearby_projects":"https://api.kickstarter.com/v1/discover?signature=1597368044.166c3e6ef698252cf3a1322fa346d6752d58dc1f&woe_id=2468964"}}}	
	outputloc = location.find('"state":"')
	if outputloc > 0:
		outputloc = outputloc +9
		output = location[outputloc:outputloc+2]
	else:
	# COULDNT FIND A STATE IN THE INCOMING STRING
		output = "XX"
	return(output.upper())

def extractcountry(location):
# THIS EXTRACTS THE STATE VALUE FROM THE LOCATION FIELD.
# SAMPLE DATA:
# {"id":2468964,"name":"Pasadena","slug":"pasadena-ca-us","short_name":"Pasadena, CA","displayable_name":"Pasadena, CA","localized_name":"Pasadena","country":"US","state":"CA","type":"Town","is_root":false,"expanded_country":"United States","urls":{"web":{"discover":"https://www.kickstarter.com/discover/places/pasadena-ca-us","location":"https://www.kickstarter.com/locations/pasadena-ca-us"},"api":{"nearby_projects":"https://api.kickstarter.com/v1/discover?signature=1597368044.166c3e6ef698252cf3a1322fa346d6752d58dc1f&woe_id=2468964"}}}	
	outputloc = location.find('"country":"')
	if outputloc > 0:
		outputloc = outputloc +11
		output = location[outputloc:outputloc+2]
	else:
	# COULDNT FIND A COUNTRY IN THE INCOMING STRING
		output = "XX"
	return(output.upper())

def extractcategory(category):
# This attempts to find the string "parent_name" in the incoming string and returns the category that follows it
# if there is no parent name it just looks for the name	
# SAMPLE DATA:
# {"id":23,"name":"Painting","slug":"art/painting","position":7,"parent_id":1,"parent_name":"Art","color":16760235,"urls":{"web":{"discover":"http://www.kickstarter.com/discover/categories/art/painting"}}}
# OR
# {"id":26,"name":"Crafts","slug":"crafts","position":3,"color":16744876,"urls":{"web":{"discover":"http://www.kickstarter.com/discover/categories/crafts"}}}
#	print(category)
	catbegin=category.find('"parent_name":') 
	if catbegin >1 :
	# it found a parent_name 
		catbegin = catbegin + 15
		catend=category.index('"',catbegin+1)
	else:
	# it did not find a parent name  so go get the name
		catbegin=category.index('"name":"') +8
		if catbegin > 1:
			# it found a Name
			catend=category.index('"',catbegin+1)
		else:
			# it couldnt find a name or a parent name
			catbegin = 0
			catend=6
			category="Unknown"
	categoryonly = category[catbegin:catend] 	
	return(categoryonly.upper())

def extractsubcategory(category):
# this finds the subcategory - stored as "name" in the category string
	cat1begin=category.index('"name":"') +8
	cat1end=category.index('"',cat1begin+1)
	subcategoryonly = category[cat1begin:cat1end]
	return(subcategoryonly.upper() )

def clear(): 
  
    # for windows 
    if name == 'nt': 
        _ = system('cls') 
  
    # for mac and linux(here, os.name is 'posix') 
    else: 
        _ = system('clear') 


def dedupe(TargetFile,ConfigFileContents,OutputString):
	targetfiledf =  pd.read_csv(TargetFile, encoding="utf-8", error_bad_lines=False)
	BeforeCount = len(targetfiledf)
	if RecordCounter: print('Count before:' + str(BeforeCount))
	targetfiledf.drop_duplicates(subset=['name','goal'],keep='last',inplace=True)
	AfterCount = len(targetfiledf)
	if RecordCounter: print('Count after:' + str(AfterCount))
	targetfiledf.to_csv(TargetFile, index=False)
	DupeCount = BeforeCount - AfterCount
	stubname = os.path.basename(TargetFile).replace(".csv","")
	OutputString +=  stubname + " Duplicates:" + str(DupeCount)
	return BeforeCount - AfterCount

def downloadnewsource(ConfigFileContents):
	UnzipDirectory = ConfigFileContents['SourceDirectory'] 
	url='https://webrobots.io/kickstarter-datasets/'
	print('Getting Source Data from:\n' + url)
	r = requests.get(url)
	soup = BeautifulSoup(r.text, 'html.parser')
	all_hrefs = soup.find_all('a')
	all_links = [link.get('href') for link in all_hrefs]
	zip_files = [dl for dl in all_links if dl and '.zip' in dl]
	# the first href they present is the newest zip...
	FullUrl = zip_files[0]
	print("Downloading from\n" + FullUrl)
	# the url should be something like this:
#	url = 'https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2020-09-17T03_20_18_143Z.zip'

	
	
	
	#url=full_url
	#print ('\nSource data URL:\n' + url)
	if FullUrl.find('/'):
		FileName=FullUrl.rsplit("/", 1)[1]
		#input(FileName)
	else:
	#nothing =input()
		FileName='kickstarter.zip'
	#print('\nUsing FileName:' +  FileName)
	FileName = ConfigFileContents['SourceDirectory']  + FileName
	
	# IF THE DIRECTORY DOESNT EXIST WE NEED TO MAKE IT
	if not os.path.exists(os.path.dirname(FileName)):
		print('Attempting to create the working directory (' +  ConfigFileContents['SourceDirectory']  + ')')
		try:
			os.makedirs(os.path.dirname(FileName))
#			os.makedirs(os.path.dirname(FileName "\\SuccessGrids"))
		except OSError as exc:
			if exc.errno != errno.EEXIST:
				raise
	
	print('\nSaving file ' + FileName + ' . . . ')
	r = requests.get(FullUrl, allow_redirects=True)
	open(FileName, 'wb').write(r.content)

	print ('\nUnzipping file ...')
	with ZipFile(FileName, 'r') as zipObj:
		zipObj.extractall(UnzipDirectory)
	print('\nFinished unzipping files...')
		
		
def runanova(ConfigFileContents):
	#if path.exists('KickStarterAutoMiner.yaml'):
	#	#input('found config file')
	#	with open('KickStarterAutoMiner.yaml') as ConfigFile:
	#		ConfigFileContents = yaml.load(ConfigFile, Loader=yaml.FullLoader)
	df = pd.read_csv(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'], encoding="utf-8")
	print('Running anova...')
	#subcat = df['fullcategory'] + " " + df['subcategory']
	anovaresultFileName  = ConfigFileContents['TargetDirectory'] + '\\kickstarter_anova_result.csv'
	anovaresultFileNameGoalFileName = ConfigFileContents['TargetDirectory'] + '\\kickstarter_anova_goal_result.csv'
	anovaresultFileName2  = ConfigFileContents['TargetDirectory'] + '\\kickstarter_anova_result2.csv'
	try:
		print('Anova result #1:\n')
		anovaresult = rp.summary_cont(df['pledged'])
		print("ANOVA result on Pledged field:\n")
		print(anovaresult)
		anovaresult.to_csv(anovaresultFileName, index=True)  
		anovaresultgoal = rp.summary_cont(df['goal'])
		print("ANOVA result on Goal field:\n")
		print(anovaresultgoal)
		anovaresultgoal.to_csv(anovaresultFileNameGoalFileName, index=True)  
	except:
		print("Error running anova #1")
	print('Attempting Category ANOVA:\n')
	#print(df['pledged'])
	#print(df.columns)
	try:
		anovaresult2 = rp.summary_cont(df['pledged'].groupby(df['subcategory']))
		print('Anova result #2:\n' )
		print(anovaresult2)
		anovaresult2.to_csv(anovaresultFileName2, index=True)  
	except:
		print("Error running anova #2")
	
		
		
def zsetupyamlfile():
	input("You shouldnt be here")
	if path.exists('KickStarterAutoMiner.yaml'):
		#input('found config file')
		with open('KickStarterAutoMiner.yaml') as ConfigFile:
			ConfigFileContents = yaml.load(ConfigFile, Loader=yaml.FullLoader)
	else:
	#CategoryBreakdown: ['TECHNOLOGY','DESIGN']
		ConfigFileItems = dict(GoBackDays= 365, IncludeBlurb= True,IncludeCancelled= False,DebugScript= False,RecordCounter= False,MaxGoal= 1000000,IncludeLive= True,MinBackers= 0,IncludeName= True,MinPledge= 0,IncludeCountries= ['US','CA'],IncludeLanguages = ['en','fr'], FileName= 'c:\\1\\source\\kickstarter.csv',GoalGroupCount=5,PledgedGroupCount=5, TargetDirectory= 'c:\\1\\output\\', TargetFileNameOnly = 'kickstarter_filtered.csv', CategoryBreakdown = ['TECHNOLOGY','DESIGN'])
		#print(ConfigFileItems['GoalGroupCount'])
		print('Creating config file...')
		with open('KickStarterAutoMiner.yaml', 'w') as ConfigFile:
			data = yaml.dump(ConfigFileItems,ConfigFile)
		with open('KickStarterAutoMiner.yaml') as ConfigFile:
			ConfigFileContents = yaml.load(ConfigFile, Loader=yaml.FullLoader)
			#input('go back days' + str(ConfigFileItems['GoBackDays']))
	
	dirname, filename = os.path.split(os.path.abspath(__file__))
	yamlname = dirname + '\'KickStarterAutoMiner.yaml'
			
	print('Type a letter to change the option.\n')
	print('"A"ge - to change how far back to gather data. (Currently: ' + str(ConfigFileContents['GoBackDays']) + ' days)')
	print('"B"lurb - to OMIT/INCLUDE the Blurb (Description). (IncludeBlurb Currently: ' + str(ConfigFileContents['IncludeBlurb']) + ')')
	print('"C"ancelled - to OMIT/INCLUDE projects marked as "Cancelled". (IncludeCancelled Currently: ' + str(ConfigFileContents['IncludeCancelled']) + ')')
	print('"D"ebug - to toggle DEBUG mode. (DebugScript Currently: ' + str(ConfigFileContents['DebugScript']) + ')')
	print('"G"oal - to include projects with a MAXIMUM goal below this amount. (MaxGoal Currently: ' + str(ConfigFileContents['MaxGoal']) + ')')
	print('"L"ive - to OMIT/INCLUDE projects marked as "Live". (IncludeLive Currently: ' + str(ConfigFileContents['IncludeLive']) + ')')
	print('"M"inimum - to include only projects with a minimum number of backers. (MinBackers Currently: ' + str(ConfigFileContents['MinBackers']) + ')')
	print('"N"ame - to OMIT/INCLUDE the Name. (IncludeName Currently: ' + str(ConfigFileContents['IncludeName']) + ')')
	print('"P"ledged - to include only projects with a minimum pledged amount. (MinPledge Currently: ' + str(ConfigFileContents['MinPledge']) + ')')
	print('"W"here - to change which locations to include. (IncludeCountries Currently: ' + str(ConfigFileContents['IncludeCountries']) + ')')
	print('Edit the following file to change default settings:\n' + yamlname)
	print('')
	useroption = input('Type letter(s) to change options or press Enter to continue using defaults:\n') 
	
	# HOW FAR BACK should it go?

	if "A" in useroption.upper():
		GoBackDays = input('Type in the number of days back the analysis should look. Default 365 to look back 1 year.\n')
		while not GoBackDays.isnumeric():
			GoBackDays = input('You must type in a number.\nType in the number of days back the analysis should look. Default 365 to look back 1 year.\n')
		#print('attempting write here ' + str(ConfigFileContents['GoBackDays']))
		ConfigFileContents['GoBackDays'] = GoBackDays	
		#print('wrote and read ' + str(ConfigFileContents['GoBackDays']))
	

	# SHOULD BE BLURB BE ADDED TO THE OUTPUT FILE
	if "B" in useroption.upper():
		print('Omitting blurb/description data')
		ConfigFileContents['IncludeBlurb'] = False


	# SHOULD STATE= CANCELLED BE INCLUDED OR OMITTED
	if "C" in useroption.upper():
		print('Toggling including "Cancelled" projects')
		if ConfigFileContents['IncludeCancelled'] == True:
			ConfigFileContents['IncludeCancelled'] = False
		else:
			ConfigFileContents['IncludeCancelled'] = True

		
	#DEBUG MODE TO OUTPUT DETAILS TO SCREEN
	# OUTPUT A RECORD COUNTER TO THE SCREEN
	if "D" in useroption.upper():
		print('Toggling DEBUG mode.')
		if ConfigFileContents['DebugScript'] == True:
			ConfigFileContents['DebugScript'] = False
			ConfigFileContents['RecordCounter'] = False
		else:
			ConfigFileContents['DebugScript'] = True
			ConfigFileContents['RecordCounter'] = True


	#SHOULD THERE BE A MAXIMUM GOAL AMOUNT 
	if "G" in useroption.upper():
		MaxGoal=input('Enter the maximum goal amount to be included (leave blank to include all goals)\n')
		if MaxGoal == '':
			print('Including all projects no matter the goal amount')
			ConfigFileContents['MaxGoal'] = ''
		else:
			while not MaxGoal.isnumeric():
				MaxGoal = input('You must type in a number.\nEnter the maximum goal amount to be included (leave blank to include all goals)\n')
			ConfigFileContents['MaxGoal'] = MaxGoal
	


	# SHOULD STATE= LIVE BE INCLUDED OR OMITTED
	if "L" in useroption.upper():
		print('Toggling including "Live" projects')
		if ConfigFileContents['IncludeLive'] == True:
			ConfigFileContents['IncludeLive'] = False
		else:
			ConfigFileContents['IncludeLive'] = True

		
	#SHOULD THERE BE A MINIMUM NUMBER OF BACKERS
	if "M" in useroption.upper():
		MinBackers=input('Enter the minimum number of backers\n')
		while not MinBackers.isnumeric():
			MinBackers = input('You must type in a number.\nEnter the minimum number of backers\n')	
		ConfigFileContents['MinBackers'] = MinBackers	




	#SHOULD THE NAME BE INCLUDED IN THE OUTPUT FILE
	if "N" in useroption.upper():
		print('Toggling including Name data')
		if ConfigFileContents['IncludeName'] == True:
			ConfigFileContents['IncludeName'] = False
		else:
			ConfigFileContents['IncludeName'] = True	

	if 'P' in useroption.upper():
		MinPledge=input('Enter the minimum pledged amount\n')
		while not MinPledge.isnumeric():
			MinPledge = input('You must type in a number.\nEnter the minimum pledged amount\n')	
		ConfigFileContents['MinPledge'] = MinPledge

		


	if "W" in useroption.upper():
		IncludeCountries = input('Type in the two letter abbreviation for the countries you want to include. Leave blank for all countries.\n (EX: "US GB AU CA" would include United States, Great Britain, Australia, and Canada.)\n')
		ConfigFileContents['IncludeCountries'] = IncludeCountries
		input('just set config file contents ' + ConfigFileContents['IncludeCountries'])
	
	return ConfigFileContents

def makecsv(ConfigFileItems,SourceFileName,FilterField,FilterValue,FilterOperator):
# Here we are going to export all the records in sourcefile where filterfield matches filtervalue
# save it to a file, the return for the function should be the file name it was saved to 
	df = pd.read_csv(SourceFileName, encoding="utf-8", error_bad_lines=False)
	#input ('Read source ' + str(len(df)) ) 
	OperationText = FilterField + '_' + FilterOperator + '_' + FilterValue
	stubname = os.path.basename(SourceFileName).replace(".csv","")
	print("Creating Subset for " + stubname + " ... " + OperationText)
	if FilterValue == "True":
		dftargets = df.loc[df[FilterField]]
	else:
		if FilterOperator == 'equals':
			dftargets = df.loc[df[FilterField] == FilterValue]
		if FilterOperator == "contains":
			dftargets = df[df[FilterField].str.contains(FilterValue)]
		if FilterOperator == "regex":
			test = df.columns
			#input(str(test))
			dftargets = df[df[FilterField].str.contains(FilterValue, regex = True ) ]
			fv = FilterValue.replace("|","")
			fv = fv.replace(" " ,".")
			fv = fv.replace(".." ,".")
			OperationText = FilterField + '_' + FilterOperator + '_' + fv
		else:
			OperationText = FilterField + '_' + FilterOperator + '_' + FilterValue
	#dftargets = df.loc[df['fullcategory'] == 'TECHNOLOGY GADGETS']
	#input ('Filtered ' + dftargets.to_string() ) 
	TargetFileName = os.path.basename(SourceFileName)
	TargetFileName = ConfigFileItems['TargetDirectory'] + "Subsets\\" + os.path.basename(SourceFileName).replace(".csv","_")  +  OperationText
	if not os.path.exists(os.path.dirname(TargetFileName)):
		print('Attempting to create the working directory (' + TargetFileName + ')')
		try:
			os.makedirs(os.path.dirname(TargetFileName))
		except OSError as exc:
			if exc.errno != errno.EEXIST:
				raise


	#input(TargetFileName)
	TargetFileName.replace('.csv','_')
	TargetFileName = TargetFileName + ".csv"
	#input(TargetFileName)
	
	dftargets.to_csv(TargetFileName, index=False) 
	return TargetFileName
	
	
	

def creategroups(DebugBool, FileName, GroupCount, GroupField):
	# WHAT WE ARE DOING HERE IS CREATING A NEW VARIABLE CALLED GOAL GROUPING THAT WILL BE ADDED TO EACH RECORD
	
	#FileName =  ConfigFileContents['TargetFileName']
	#GroupCount = ConfigFileContents['GoalGroupCount']
	
	print('Creating ' + str(GroupCount) + ' groups for ' + GroupField + ' in ' + FileName)
	df = pd.read_csv(FileName, encoding="utf-8", error_bad_lines=False)
	#input(df)
	if DebugBool: print(str(len(df.index)) + ' items')
	TotalProjectCount = len(df.index)
	EachGroupCount = TotalProjectCount//GroupCount
	LastGroupCount = TotalProjectCount - (EachGroupCount *(GroupCount-1))
	CurrentGoalGroupCounter = 1
	MyCounter= 1
	if DebugBool: print('There are ' + str(TotalProjectCount) + ' projects and ' + str(GroupCount) + ' groups\nEach group should contain ' + str(EachGroupCount) + ' projects, except the last which contains ' + str(LastGroupCount))
	final_df = df.sort_values(by=[GroupField], ascending=True)
	#print(' ')
	#input(final_df.to_string())
	for i, project in final_df.iterrows():
		#input('CGGC  * ' + str((CurrentGoalGroupCounter * EachGroupCount)))
		#if MyCounter < (CurrentGoalGroupCounter * EachGroupCount):
			#print ('goal group ' + str(CurrentGoalGroupCounter))
			
		if MyCounter == (CurrentGoalGroupCounter * EachGroupCount):
			if CurrentGoalGroupCounter < GroupCount:
				if project[GroupField] == lastgoal:
					# HERE WE HAVE FOUND ITS TIME TO MOVE TO THE NEW GOAL GROUP BUT THE GOAL IS THE SAME SO IT SHOULD BE IN THE LAST GROUP.
					if DebugBool: print('Extending group... :' + str(project[GroupField]) + '\nGroup: ' + str(CurrentGoalGroupCounter) + '\n' + str(lastgoal))
					MyCounter = MyCounter-1
				else:
					CurrentGoalGroupCounter = CurrentGoalGroupCounter + 1
				#	input ('here we are over the number of projects per group and not the same goal amount. \nnew goal group ' + str(CurrentGoalGroupCounter))
					
		if DebugBool: print(GroupField + str(project[GroupField]) + '\nGroup: ' + str(CurrentGoalGroupCounter))
		GroupFieldName = GroupField + 'group'
		final_df.at[i,GroupFieldName] = CurrentGoalGroupCounter
		#project['GoalGroup'] = CurrentGoalGroupCounter
		MyCounter = MyCounter + 1
		lastgoal = project[GroupField]
	final_df.to_csv(FileName, index=False) 
	
def daysdiff(date1,date2):
	dateone=date(date1)
	datetwo = date(date2) 
	delta = datetwo - dateone
	dd = delta.days
	return dd

def calccategorysuccess(AnalyzeFileName,OutputFileName,CategoryFieldName,SuccessFieldBool):	
# THIS FUNCTION CALCULATES A BUNCH OF STATISTICS 
# INCLUDING PERCENT SUCCESS BASED ON SUCCESSFIELDBOOL
 # CREATES AN OUTPUT FILE 

# WE ALSO DO OVERALL EVALUATION OF THE FILE 

	#input('Getting ready to read up ' + AnalyzeFileName)
	# First set up the dataframe objects 
	stubname = os.path.basename(AnalyzeFileName).replace(".csv","")
	print('Analyzing success by  '  + CategoryFieldName + ' in ' + stubname)
	analyzedf = pd.read_csv(AnalyzeFileName, encoding="utf-8",header =0, error_bad_lines=False)
#	analyzedf[['deadline','launched_at']] = analyzedf[['deadline','launched_at']].apply(pd.to_datetime)
#	analyzedf['duratondays'] =  (analyzedf['deadline']-analyzedf['launched_at']).dt.days 

	if len(analyzedf.index) ==0:
		print("There are no items to analyze in" +AnalyzeFileName)
	else:
		#if RecordCounter: print('Analyzing success by  '  + CategoryFieldName + ' in ' + stubname)
		#input('Analyzing success by  '  + CategoryFieldName + ' in ' + AnalyzeFileName)
	
		successdf = analyzedf.loc[analyzedf[SuccessFieldBool] ==True]
		faileddf = analyzedf.loc[analyzedf[SuccessFieldBool] ==False]
		if CategoryFieldName == 'year':
			analyzedf['year'] =  pd.to_datetime(analyzedf['state_changed_at']).dt.year
			allcategories = analyzedf.groupby(['year'], as_index=False)
		else:
			allcategories = analyzedf.groupby([CategoryFieldName], as_index=False)

		# populate the variables we're interested in for the overall file
	
		TotalRecords = len(analyzedf)
		#input('There are ' + str(TotalRecords) + ' total records in ' + AnalyzeFileName)
		OldestItem = analyzedf['state_changed_at'].min()
		# NOT USING THIS VARIABLE
		#print(analyzedf.to_string())
		#input()
		#print(successdf.to_string())
		#input()
		#print(faileddf.to_string())
		#input()
		#input('Oldest item :' + str(OldestItem))
	
	
		MeanPledged = round(analyzedf['pledged'].mean(axis=0),2)
		MeanPledgedSuccess = round(successdf['pledged'].mean(axis=0),2)
		MeanPledgedFailed = round(faileddf['pledged'].mean(axis=0),2)
		MedianPledged = round(analyzedf['pledged'].median(axis=0),2)
		MedianPledgedSuccess = round(successdf['pledged'].median(axis=0),2)
		MedianPledgedFailed = round(faileddf['pledged'].median(axis=0),2)
	
		MeanActualPledged = round(analyzedf['actualpledged'].mean(axis=0),2)
		MeanActualPledgedSuccess = round(successdf['actualpledged'].mean(axis=0),2)
		MeanActualPledgedFailed = round(faileddf['actualpledged'].mean(axis=0),2)
		MedianActualPledged = round(analyzedf['actualpledged'].median(axis=0),2)
		MedianActualPledgedSuccess = round(successdf['actualpledged'].median(axis=0),2)
		MedianActualPledgedFailed = round(faileddf['actualpledged'].median(axis=0),2)  # THIS SHOULD ALWAYS BE 0 !
	
		MeanGoal = round(analyzedf['goal'].mean(axis=0),2)
		MeanGoalSuccess = round(successdf['goal'].mean(axis=0),2)
		MeanGoalFailed = round(faileddf['goal'].mean(axis=0),2)
		MedianGoal = round(analyzedf['goal'].median(axis=0),2)
		MedianGoalSuccess = round(successdf['goal'].median(axis=0),2)
		MedianGoalFailed = round(faileddf['goal'].median(axis=0),2)
	
		MinGoal = round(min(analyzedf['goal'],default=0),2)
		MinGoalSuccess = round(min(successdf['goal'],default=0),2)
		MinGoalFailed = round(min(faileddf['goal'],default=0),2)
	
		MaxGoal = round(max(analyzedf['goal'],default=0),2)
		MaxGoalSuccess = round(max(successdf['goal'],default=0),2)
		MaxGoalFailed = round(max(faileddf['goal'],default=0),2)


		MeanBackers = round(analyzedf['backers_count'].mean(axis=0),0)
		MeanBackersSuccess = round(successdf['backers_count'].mean(axis=0),0)
		MeanBackersFailed = round(faileddf['backers_count'].mean(axis=0),0)
		MedianBackers = round(analyzedf['backers_count'].median(axis=0),2)
		MedianBackersSuccess =  round(successdf['backers_count'].median(axis=0),2)
		MedianBackersFailed = round(faileddf['backers_count'].median(axis=0),2)




		SuccessCount = len(successdf)
		FailCount = TotalRecords - SuccessCount
		OverallPCT = round((SuccessCount/TotalRecords)*100,2)
		#input('Total projects in the file: ' + str(TotalRecords))
		# SET UP THE DATA FRAME FOR THE CATEGORY
		categoryanalysisdf = pd.DataFrame(columns=[CategoryFieldName,'SuccessCount','FailCount','TotalCount','PercentSuccess',
													'MeanPledged', 'MeanPledgedSuccess', 'MeanPledgedFailed',
													'MedianPledged','MedianPledgedSuccess','MedianPledgedFailed',
													'MeanActualPledged', 'MeanActualPledgedSuccess', 'MeanActualPledgedFailed',
													'MedianActualPledged','MedianActualPledgedSuccess','MedianActualPledgedFailed',
													'MeanGoal','MeanGoalSuccess','MeanGoalFailed',
													'MedianGoal','MedianGoalSuccess','MedianGoalFailed',
													'MinGoal','MinGoalSuccess','MinGoalFailed',
													'MaxGoal','MaxGoalSuccess','MaxGoalFailed',
													'MeanBackers','MeanBackersSuccess','MeanBackersFailed',
													'MedianBackers','MedianBackersSuccess','MedianBackersFailed'],)
		# CREATE AND POPULATE UP THE 'this' DATAFRAME FOR THE OVERALL
		thiscategorydf = pd.DataFrame({CategoryFieldName : 'Overall', "SuccessCount" : [SuccessCount],"FailCount" : [FailCount],"TotalCount" : [TotalRecords],"PercentSuccess" : OverallPCT,
										"MeanPledged" : MeanPledged, "MeanPledgedSuccess" :MeanPledgedSuccess , "MeanPledgedFailed" :MeanPledgedFailed ,
										"MedianPledged" : MedianPledged, "MedianPledgedSuccess" : MedianPledgedSuccess,"MedianPledgedFailed" : MedianPledgedFailed,
										"MeanActualPledged" : MeanActualPledged, "MeanActualPledgedSuccess" :MeanActualPledgedSuccess , "MeanActualPledgedFailed" :MeanActualPledgedFailed ,
										"MedianActualPledged" : MedianActualPledged, "MedianActualPledgedSuccess" : MedianActualPledgedSuccess,"MedianActualPledgedFailed" : MedianActualPledgedFailed,
										"MeanGoal" : MeanGoal,"MeanGoalSuccess" : MeanGoalSuccess,"MeanGoalFailed" : MeanGoalFailed,
										"MedianGoal" : MedianGoal, "MedianGoalSuccess" : MedianGoalSuccess, "MedianGoalFailed": MedianGoalFailed,
										"MinGoal" : MinGoal,"MinGoalSuccess" : MinGoalSuccess,"MinGoalFailed" : MinGoalFailed,
										"MaxGoal" : MaxGoal,"MaxGoalSuccess" : MaxGoalSuccess,"MaxGoalFailed" : MaxGoalFailed,
										"MeanBackers" : MeanBackers ,"MeanBackersSuccess" : MeanBackersSuccess ,"MeanBackersFailed" : MeanBackersFailed,"MedianBackers" : MedianBackers, "MedianBackersSuccess" : MedianBackersSuccess, "MedianBackersFailed" : MedianBackersFailed })
									
		OverallResultsFileName = AnalyzeFileName.replace(".csv","_")  + SuccessFieldBool + '_Overall.csv'
		thiscategorydf.to_csv(OverallResultsFileName, index=False)
		if RecordCounter: print('Processing... ' + str(len(allcategories)) + ' categories in '  + CategoryFieldName + ' field ...\nOverall Results for ' + AnalyzeFileName + ' written to ' + OverallResultsFileName)
		for categorygroup in allcategories:
			ThisItem = categorygroup[0]
			groupdf = analyzedf.loc[analyzedf[CategoryFieldName]== categorygroup[0]]
			successgroupdf = groupdf.loc[groupdf[SuccessFieldBool]== True]
			failedgroupdf = groupdf.loc[groupdf[SuccessFieldBool]== False]
			GroupCount = len(groupdf)
		
			CategoryPledgedMean = round(groupdf['pledged'].mean(axis=0),2)
			SuccessPledgedMean = round(successgroupdf['pledged'].mean(axis=0),2)
			FailedPledgedMean = round(failedgroupdf['pledged'].mean(axis=0),2)
		
			CategoryPledgedMedian = round(groupdf['pledged'].median(axis=0),2)
			SuccessPledgedMedian = round(successgroupdf['pledged'].median(axis=0),2)
			FailedPledgedMedian = round(failedgroupdf['pledged'].median(axis=0),2)
		
			CategoryActualPledgedMean = round(groupdf['actualpledged'].mean(axis=0),2)
			SuccessActualPledgedMean = round(successgroupdf['actualpledged'].mean(axis=0),2)
			FailedActualPledgedMean = round(failedgroupdf['actualpledged'].mean(axis=0),2)
		
			CategoryActualPledgedMedian = round(groupdf['actualpledged'].median(axis=0),2)
			SuccessActualPledgedMedian = round(successgroupdf['actualpledged'].median(axis=0),2)
			FailedActualPledgedMedian = round(failedgroupdf['actualpledged'].median(axis=0),2)
		
			CategoryGoalMean = round(groupdf['goal'].mean(axis=0),2)
			SuccessGoalMean = round(successgroupdf['goal'].mean(axis=0),2)
			FailedGoalMean = round(failedgroupdf['goal'].mean(axis=0),2)
		
			CategoryGoalMin = round(min(groupdf['goal']),2)
			SuccessGoalMin = round(min(successgroupdf['goal'], default=0),2)
		

			FailedGoalMin = round(min(failedgroupdf['goal'], default=0),2)
		
			CategoryGoalMax = round(max(groupdf['goal']),2)
			SuccessGoalMax = round(max(successgroupdf['goal'], default=0),2)
			FailedGoalMax = round(max(failedgroupdf['goal'], default=0),2)


			CategoryGoalMedian = round(groupdf['goal'].median(axis=0),2)
			SuccessGoalMedian = round(successgroupdf['goal'].median(axis=0),2)
			FailedGoalMedian = round(failedgroupdf['goal'].median(axis=0),2)
		
			CategoryBackersMean = round(groupdf['backers_count'].mean(axis=0),0)
			SuccessBackersMean = round(successgroupdf['backers_count'].mean(axis=0),0)
			FailedBackersMean = round(failedgroupdf['backers_count'].mean(axis=0),0)

			MedianBackers = round(groupdf['backers_count'].median(axis=0),2)
			MedianBackersSuccess = round(successgroupdf['backers_count'].median(axis=0),2)
			MedianBackersFailed = round(failedgroupdf['backers_count'].median(axis=0),2)
		
			SuccessCount = len(successgroupdf)
			PercentSuccess = round(SuccessCount/(GroupCount)*100,2)
			FailCount = GroupCount - SuccessCount
			# SET UP THE DATAFRAME FOR THIS ITEM 
			thiscategorydf = pd.DataFrame({CategoryFieldName : [ThisItem], "SuccessCount" : [SuccessCount],"FailCount" : [FailCount],"TotalCount" : [GroupCount], "PercentSuccess" : [PercentSuccess],
											"MeanPledged" :  [CategoryPledgedMean], "MeanPledgedSuccess" : [SuccessPledgedMean],"MeanPledgedFailed" : [FailedPledgedMean],
											"MedianPledged" : [CategoryPledgedMedian], "MedianPledgedSuccess" : [SuccessPledgedMean],"MedianPledgedFailed" : [FailedPledgedMedian],
											"MeanActualPledged" :  [CategoryActualPledgedMean], "MeanActualPledgedSuccess" : [SuccessActualPledgedMean],"MeanActualPledgedFailed" : [FailedActualPledgedMean],
											"MedianActualPledged" : [CategoryActualPledgedMedian], "MedianActualPledgedSuccess" : [SuccessActualPledgedMean],"MedianActualPledgedFailed" : [FailedActualPledgedMedian],
											"MeanGoal" : [CategoryGoalMean],"MeanGoalSuccess" : [SuccessGoalMean],"MeanGoalFailed" : [FailedGoalMean] ,
											"MedianGoal" :[CategoryGoalMedian],"MedianGoalSuccess" :[SuccessGoalMedian] ,"MedianGoalFailed" :[FailedGoalMedian] ,
											"MinGoal" : [CategoryGoalMin],"MinGoalSuccess" : [SuccessGoalMin],"MinGoalFailed" : [FailedGoalMin],
											"MaxGoal" : [CategoryGoalMax],"MaxGoalSuccess" : [SuccessGoalMax],"MaxGoalFailed" : [FailedGoalMax],
											"MeanBackers" : [CategoryBackersMean],"MeanBackersSuccess" : [SuccessBackersMean],"MeanBackersFailed" : [FailedBackersMean] ,"MedianBackers" : [MedianBackers], "MedianBackersSuccess" : [MedianBackersSuccess], "MedianBackersFailed" : [MedianBackersFailed] })
		
		
			categoryanalysisdf = categoryanalysisdf.append(thiscategorydf)
	

		categoryanalysisdf.to_csv(OutputFileName, index=False)  

def cleantext(x):
	print("Preparing text for WordCloud ... " + x[:30] + " . . . ")
	

	x = x.lower()

	x.split() 	
	x.translate(str.maketrans('', '', string.punctuation))
	stop_words = set(stopwords.words('english'))
	xlist = x.split()
	xlist =  [word for word in xlist if word.lower() not in stopwords.words('english')]
	x = ' '.join(xlist)
	x = x.replace("-"," ")
	x = x.replace("enamel pin", "enamelpin")
	x = x.replace("enamel pins", "enamelpin")
	x = x.replace("hard enamelpin", "enamelpin") 
	x = x.replace("enamelpins", "enamelpin")
	x = x.replace("&", "")

	#re.sub('[\(\[].*?[\)\]]', ' ', x)
	pattern = r'[^a-zA-Z0-9\s]' #if not remove_digits else r'[^a-zA-Z\s]'    
	re.sub(pattern, '', x)
	return(x)

def makeblurbblobs(AnalyzeFullPath,ConfigFileContents):

# THIS WILL TAKE A PARTICULAR FILE< SEPERATE OUT THE BLURBS BASED ON SUCCESS O FAIL THEN MAKE WORD CLOUDS
	analyzedf = pd.read_csv(AnalyzeFullPath, encoding="utf-8",header =0, error_bad_lines=False)
	successdf = analyzedf.loc[analyzedf['projectsuccessb'] ==True]
	successdf = successdf.loc[successdf['language'] == 'en']
	faileddf = analyzedf.loc[analyzedf['projectsuccessb'] ==False]
	faileddf = faileddf.loc[faileddf['language'] == 'en']

	SuccessBlurbs = successdf['blurb']
	SuccessNames = successdf['name']
	FailedBlurbs =  faileddf['blurb']
	FailedNames =  faileddf['name']
	AllSuccessBlurbs = ""
	AllSuccessNames = ""
	AllFailedBlurbs = ""
	AllFailedNames = ""
	ThisBlurb = ""
	ThisName = ""
	FileNameOnly = os.path.basename(AnalyzeFullPath)
	FileNameOnly = FileNameOnly.replace(".csv","_")
	SuccessBlurbsFileName = ConfigFileContents['TargetDirectory'] + FileNameOnly + 'Success_Blurbs.txt'
	SuccessNamesFileName = ConfigFileContents['TargetDirectory'] + FileNameOnly + 'Success_Names.txt'
	FailedBlurbsFileName = ConfigFileContents['TargetDirectory'] + FileNameOnly + 'Failed_Blurbs.txt' 
	FailedNamesFileName = ConfigFileContents['TargetDirectory'] + FileNameOnly + 'Failed_Names.txt' 
	# DO SUCCESSFUL BLURBS
	for row in SuccessBlurbs:
		ThisBlurb = row
		AllSuccessBlurbs = AllSuccessBlurbs + " " + ThisBlurb
	AllSuccessBlurbs = cleantext(AllSuccessBlurbs)
	AllSuccessBlurbs += '\n'
	AllSuccessBlurbs = "Success\n" + AllSuccessBlurbs
	SuccessBlurbFile = open(SuccessBlurbsFileName,"w+", encoding="utf-8")
	SuccessBlurbFile.write(AllSuccessBlurbs)
	SuccessBlurbFile.close()
	SuccessBlurbWordCloud = WordCloud().generate(AllSuccessBlurbs)
	print("Ignore Tkinter exceptions here ...")
	plt.imshow(SuccessBlurbWordCloud, interpolation='bilinear')
	plt.axis("off")
	plt.savefig(ConfigFileContents['TargetDirectory'] + FileNameOnly + "Success_Blurbs.png", bbox_inches='tight')
# SHOW WILL CAUSE EVERYTHING TO STOP so it is commented for now
	#plt.show()
	
# SUCCESSFUL NAME BLURB
	for row in SuccessNames:
		ThisName = row
		AllSuccessNames = AllSuccessNames + " " + ThisName
	AllSuccessNames = cleantext(AllSuccessNames)
	AllSuccessNames += '\n'
	AllSuccessNames = "Success\n" + AllSuccessNames
	SuccessNamesFile = open(SuccessNamesFileName,"w+", encoding="utf-8")
	SuccessNamesFile.write(AllSuccessNames)
	SuccessNamesFile.close()
	SuccessNamesWordCloud = WordCloud().generate(AllSuccessNames)
	plt.imshow(SuccessNamesWordCloud, interpolation='bilinear')
	plt.axis("off")
	print("Ignore Tkinter exceptions here ...")
	plt.savefig(ConfigFileContents['TargetDirectory'] + FileNameOnly + "Success_Names.png", bbox_inches='tight')

	# NOW DO FAILED Blurbs
	for row in FailedBlurbs:
		ThisBlurb = row
		AllFailedBlurbs = AllFailedBlurbs + " " +  ThisBlurb
	AllFailedBlurbs = cleantext(AllFailedBlurbs)
	AllFailedBlurbs += '\n'
	AllFailedBlurbs = "Failed\n" + AllFailedBlurbs
	FailedBlurbFile = open(FailedBlurbsFileName,"w+", encoding="utf-8")
	FailedBlurbFile.write(AllFailedBlurbs)
	FailedBlurbFile.close()
	FailedBlurbWordCloud = WordCloud().generate(AllFailedBlurbs)
	plt.imshow(FailedBlurbWordCloud, interpolation='bilinear')
	plt.axis("off")
	plt.savefig(ConfigFileContents['TargetDirectory'] + FileNameOnly + "_Failed_Blurbs.png", bbox_inches='tight')
	#plt.show()
	
	# FAILED NAME BLURB
	for row in FailedNames:
		ThisName = row
		AllFailedNames = AllFailedNames + " " + ThisName
	AllFailedNames = cleantext(AllFailedNames)
	AllFailedNames += '\n'
	AllFailedNames = "Failed\n" + AllFailedNames
	FailedNamesFile = open(FailedNamesFileName,"w+", encoding="utf-8")
	FailedNamesFile.write(AllFailedNames)
	FailedNamesFile.close()
	FailedNamesWordCloud = WordCloud().generate(AllFailedNames)
	plt.imshow(FailedNamesWordCloud, interpolation='bilinear')
	plt.axis("off")
	plt.savefig(ConfigFileContents['TargetDirectory'] + FileNameOnly + "Failed_Names.png", bbox_inches='tight')
	# COMPARISON CLOUD IS DONE IN R

	# LETS LOOK AT the TOP 5 words
	#FileName = 'r:/1/output/kickstarter_clean_Success_Names.txt'
	#FileObject  = open(FileName,"r+", encoding="utf-8")
	data_set = AllSuccessBlurbs
	split_it = data_set.split() 
	Counterobj = Counter(split_it) 
	most_occur = Counterobj.most_common(5) 
	try:
		QueryStr = most_occur[0][0] + '|' + most_occur[1][0] + '|' + most_occur[2][0] + '|' + most_occur[3][0] + '|' + most_occur[4][0] 
	#TargetFileName
		df = pd.read_csv(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'], encoding="utf-8")
		dftargets = df[df['name'].str.contains(QueryStr , regex = True )]
		CommonFileName = ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'] .replace(".csv","_")  + 'FiveMostCommonWords.csv'
		dftargets.to_csv(CommonFileName, index=False) 
		calccategorysuccess(CommonFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(CommonFileName).replace(".csv","_") + 'by_goalgroup_success.csv','goalgroup','projectsuccessb')
	except:
		print("Error get top 5 words...")
	#CommonFileName
	
	
	
	
	
	

def calcfilestats(AnalyzeFullPath,ConfigFileContents):
# THIS FUNCTION WILL PERFOM SOME OF THE ANALYSIS
	if RecordCounter: print('Starting analysis on ' + AnalyzeFullPath)
	analyzedf = pd.read_csv(AnalyzeFullPath, encoding="utf-8",header =0, error_bad_lines=False)
	if len(analyzedf.index) ==0:
		print("There are no items to analyze in" +AnalyzeFullPath)
	else:
		successdf = analyzedf.loc[analyzedf['projectsuccessb'] ==True]
		faileddf = analyzedf.loc[analyzedf['projectsuccessb'] ==False]
	#	DescribeResult = analyzedf.describe()
	
	#	DescribeResult.to_csv(ConfigFileContents['TargetDirectory'] + os.path.basename(AnalyzeFullPath)  + '_describe.csv', index=False) 
		FileBaseName = ConfigFileContents['TargetDirectory'] + os.path.basename(AnalyzeFullPath).replace(".csv","_")
		analyzedf.describe().to_csv(FileBaseName  + 'describe.csv', index=False) 
	
	
		data_crosstab = pd.crosstab(analyzedf['features'], 
									analyzedf['projectsuccess'],  
									   margins = False) 
		data_crosstab.to_csv(FileBaseName  + 'features_success_crosstab.csv', index=True)   



		
def main():

	
	clear()
	print('Starting Kickstarter Data AutoMiner\n')
	print('This script will automatically seek out the latest KickStarter data files, download and analyze them.')
	if path.exists('KickStarterAutoMiner.yaml'):
		#input('found config file')
		with open('KickStarterAutoMiner.yaml') as ConfigFile:
			ConfigFileContents = yaml.load(ConfigFile, Loader=yaml.FullLoader)
	else:
		ConfigFileItems = dict(GoBackDays= 365, IncludeBlurb= True,IncludeCancelled= False,
								DebugScript= False,RecordCounter= False,MaxGoal= 1000000,IncludeLive= False,
								MinBackers= 5,IncludeName= True,MinPledge=100,MaxPledge = 1000000,
								IncludeCountries= ['US','CA'],IncludeLanguages = ['en','fr'],
								FileName= 'c:\\1\\source\\kickstarter.csv',TargetFileNameOnly= 'kickstarter_filtered.csv',TargetAllFileNameOnly='kickstarter_all.csv',
								GoalGroupCount=10,PledgedGroupCount=10, SourceDirectory= 'c:\\1\\source\\',TargetDirectory= 'c:\\1\output\\',
								KeyWordsOne = 'covid | pandemic | disinfect | mask | sanitize',
								KeyWordsTwo = '')
		with open('KickStarterAutoMiner.yaml', 'w') as ConfigFile:
			data = yaml.dump(ConfigFileItems,ConfigFile)
		if path.exists('KickStarterAutoMiner.yaml'):
			with open('KickStarterAutoMiner.yaml') as ConfigFile:
				ConfigFileContents = yaml.load(ConfigFile, Loader=yaml.FullLoader)
		else:
			print('Unable to create config file. This is a weird spot so trying something untested here.')
			ConfigFileContents=ConfigFileItems
	#input(ConfigFileContents['IncludeLanguages'])
#	RunOption = input('Press Enter to proceed with default options from KickStarterAutoMiner.yaml\n"C" to change miner options.\n"Q" to quit.\n')
#	if "C" in RunOption.upper():	
#		ConfigFileContents=setupyamlfile()
#		#print('finished setup ' + ConfigFileContents['IncludeCountries'])
#	if "Q" in RunOption.upper():	
#		sys.exit()
	
	



	#useroption = input('Type letter(s) to change options or press Enter to continue using defaults:\n') 

	FilesCount = 0
	TotalCount=0
	ThisFileCount=0
	WriteCount=0
	TotalWriteCount=0
	TotalSuccessCount=0
	TotalFailCount=0
	DropCount=0
	DropCountryCount = 0
	DropAgeCount = 0
	TotalDropCount=0
	DropMinCount = 0
	DropPledgedMin = 0
	DropPledgedMax = 0
	DropMaxGoal = 0
	DropLanguage = 0
	DropLiveCount = 0
	DropCancelCount = 0

	DebugScript = ConfigFileContents['DebugScript']
	RecordCounter = ConfigFileContents['DebugScript']
	ProjectSuccess=False
	StopEachFile = False
	PromptDownload = False
	WriteRow = True
	StartTime = time.time()
	OldestAll = date.today()
	OldestClean = date.today()
	OutputString = ""
	#SHOULD THE CURRENT ROW BE WRITTEN TO THE TARGET FILE

	RecordCounter = ConfigFileContents['RecordCounter']
	DebugScript = ConfigFileContents['DebugScript']

	



	IncludeHeader = True
	# INCLUDE FIELD NAMES AS HEADER IN THE OUTPUT FILE
	
	#FileName = 'c:\\1\\source\\kickstarter.csv'
	FileName =  ConfigFileContents['SourceDirectory'] + 'kickstarter.csv'
	# NAME OF THE FIRST FILE IN THE SET
	#TargetFileName = 'c:\\1\\output\\kickstarterclean.csv'
	TargetFileName = ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly']
	TargetAllFileName =  ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetAllFileNameOnly']
	TargetFileNameStub =   ConfigFileContents['TargetFileNameOnly'].replace('.csv','') 
	TargetAllFileNameStub =  ConfigFileContents['TargetAllFileNameOnly'].replace('.csv.','')
	TargetAllFileNameStub = TargetAllFileNameStub.replace(".csv","")
	#TargetSuccessFileName = ConfigFileContents['TargetSuccessFileName']
	#TargetFailFileName = ConfigFileContents['TargetFailFileName']
	# NAME OF THE FILE WHERE RESULTS WILL BE WRITTEN

	#CHECK IF THE SOURCE FILES EXIST
	if path.exists(FileName):
		print('Found SOURCE file:\n' + FileName)
		getnew = input('\nA source data file already exists, do you want to try to get a new set of files?\n[ENTER] to continue with the existing source files\n"N" to fetch new source files\n"Q" to quit.\n')
		if getnew.upper() == 'Q':
			sys.exit()
		if getnew.upper() == 'N':
			downloadnewsource(ConfigFileContents)
	else:
		if PromptDownload:
			Fetch = input('Could not find SOURCE file:\n' + FileName + '\n[ENTER] to fetch new files from the internet\n"Q" to quit.\n')
			if Fetch.upper() == 'Q':
				sys.exit()
			else:
				downloadnewsource(ConfigFileContents)
		else:
			downloadnewsource(ConfigFileContents)
	#OutputString = OutputFilters(ConfigFileContents,OutputString)
	OutputString = "Kickstarter Autominer Filter Settings\n" + OutputFilters(ConfigFileContents,OutputString)
	# SET UP TARGET FILE HEADER
	Header = "category,subcategory,state,backers_count,goal,pledged,spotlight,staff_pick,state_loc,state_change_at_unix,state_changed_at, launched_at_unix,launched_at,deadline_unix,deadline,durationdays,projectsuccessb,projectsuccess,features,country,fullcategory,actualpledged,country_displayable_name,language,languagename"
	if ConfigFileContents['IncludeName']:
		Header = Header + ",name"
	if ConfigFileContents['IncludeBlurb']:
		Header = Header + ",blurb"
	Header = Header + "\n"

	#CHECK IF THE TARGET FILES EXIST
	if path.exists(TargetFileName):
		print('Found target file:\n' + TargetFileName + '\n')
	else:
		print('Target file does not already exist.\n' + TargetFileName + '\nwill be created\n')
		

	#SET UP TARGET FILE
	targetexist=' '
	if path.exists(TargetFileName):
	# TARGET FILE ALREADY EXISTS, WHAT DO?
		targetexist = input('Target file already exists, \n[ENTER] to delete the existing ' + ConfigFileContents['TargetFileNameOnly']  +' file and re-import the source files\n"C" to recreate only the Clean dataset\n"G" to reset languages, groupings, de-dupe and analyze.\n"L" to skip processing languages (faster)\n"S" to skip to analysis.\n"Q" to quit.\n' )
		if targetexist.strip() == '':
			targetexist = 'D'
		if "D" in targetexist.upper():
		# User has typed in D to delete the existing file
			#input(Header)
			with open(TargetFileName, 'w+', encoding="utf-8") as csvFile:
				if IncludeHeader:
					csvFile.write(Header)
			with open(TargetAllFileName, 'w+', encoding="utf-8") as csvFile2:
					csvFile2.write(Header)
# TO CREATE FILES FOR SUCCESS ANF FAILED FILES UNCOMMENT NEXT 4 LINES			
			
#			with open(TargetSuccessFileName, 'w+', encoding="utf-8") as csvFile3:
#					csvFile3.write(Header)
#			with open(TargetFailFileName, 'w+', encoding="utf-8") as csvFile4:
#					csvFile4.write(Header)
			#input(Header)

		
		
			
	else:
	# TARGET FILE DOES NOT EXIST, CREATE IT AND WRITE HEADER
		#Targetexist = 'N'
		# FIRST CHECK THE PATH EXISTS AND CREATE IF NOT
		if not os.path.exists(os.path.dirname(TargetFileName)):
			print('Attempting to create the working directory (' + ConfigFileContents['TargetDirectory'] + ')')
			try:
				os.makedirs(os.path.dirname(TargetFileName))
			except OSError as exc:
				if exc.errno != errno.EEXIST:
					raise
		SuccessGridFileName = ConfigFileContents['TargetDirectory']  + "SuccessGrids\\"
		if not os.path.exists(os.path.dirname(SuccessGridFileName)):
			if DebugScript: print('Attempting to create the working directory (' + SuccessGridFileName + ')')
			try:
				os.makedirs(os.path.dirname(SuccessGridFileName))
			except OSError as exc:
				if exc.errno != errno.EEXIST:
					raise



		#		os.makedirs(os.path.dirname(FileName "\\SuccessGrid"))
		
		with open(TargetFileName, 'a+', encoding="utf-8") as csvFile1:
			csvFile1.write(Header)
		with open(TargetAllFileName, 'a+', encoding="utf-8") as csvFile2:
			csvFile2.write(Header)
# UNCOMMENT NEXT4 TO CREATE SUCCESS /FAIL FILES
	#	with open(TargetSuccessFileName, 'a+', encoding="utf-8") as csvFile3:
	#		csvFile3.write(Header)
		#input(TargetFailFileName)
	#	with open(TargetFailFileName, 'a+', encoding="utf-8") as csvFile4:
	#		csvFile4.write(Header)
		
	if "Q" in targetexist.upper():
		sys.exit()
	if "S" not in targetexist.upper() and "G" not in targetexist.upper():
		# THEY HAVE NOT CHOSEN TO SKIP THE IMPORT PHASE
		print('\nBeginning import phase...\n')
		list = os.listdir(os.path.dirname(FileName)) 
		#number_files = len(list)
		number_files = len(fnmatch.filter(os.listdir(os.path.dirname(FileName)), '*.csv'))
		print ('Processing ' + str((number_files)) + ' files...\n')
		
		while (path.exists(FileName)):
		#	print('Path exists')
			FilesCount = FilesCount + 1
			if StopEachFile:
				input ("Starting file " + str(FilesCount) + ": " + FileName + "\nPress ENTER to proceed\n")
			else:
				if RecordCounter: print("Starting file " + str(FilesCount) + ": " + FileName)
			# OPEN THE source file and read it up
			
			#with open(FileName, 'r',encoding="utf8", skipinitialspace = True) as f:
			# WHY IS skipinitialspace getting an error here?
			with open(FileName, 'r',encoding="utf8") as f:

				# OPENING CSV HERE
				# ADD UTF8 ENCODING
				if DebugScript:
					print('File Open')
				ThisFileCount = 0
				WriteCount = 0
				DropCount = 0
				
				csvreader = DictReader(f)
			
				for row in csvreader:
				#ITERATE THROUGH ALL THE ROWS IN THE CSV, FILTER AND OUTPUT
					ThisFileCount = ThisFileCount + 1
					TotalCount = TotalCount + 1
					outputrow = ''
					WriteRow = True
				#	print(row['country'])
					if DebugScript:	print(row['name'])
				# FILTERS 
				#filter based on COUNTRY = IncludeCountries
					ThisCountry = extractcountry(row['location'])
				   #row['country']
					if ConfigFileContents['IncludeCountries'] is None:
						WriteRow = WriteRow
						#print('all countries')
					else:
						#input('TC1\n' + row['country'] + '\n' + IncludeCountries)
						if ThisCountry not in ConfigFileContents['IncludeCountries']:
							WriteRow =False
							DropCountryCount = DropCountryCount + 1
						else:
							WriteRow = WriteRow


		
		# FILTER BASED ON DATE
					
					unixdate = row['state_changed_at']
					#input('UnixDate:' + unixdate)
					datestr = datetime.utcfromtimestamp(int(unixdate)).strftime('%Y-%m-%d %H:%M:%SZ')
					unixdatelaunch = row['launched_at']
					launchdatestr = datetime.utcfromtimestamp(int(unixdatelaunch)).strftime('%Y-%m-%d %H:%M:%SZ')
					unixdatedeadline = row['deadline']
					deadlinedatestr=datetime.utcfromtimestamp(int(unixdatedeadline)).strftime('%Y-%m-%d %H:%M:%SZ')
					humandate = datetime.strptime(datestr,'%Y-%m-%d %H:%M:%SZ')
					daysold = abs(datetime.now()-datetime.strptime(datestr,'%Y-%m-%d %H:%M:%SZ')).days
					if daysold > ConfigFileContents['GoBackDays']:
						WriteRow =False
						DropAgeCount = DropAgeCount +1
						if DebugScript:
							print("too old")
						
					
					if DebugScript:
						print (humandate)
					
					
					# FILTER BASED ON MINIMUM BACKERS
					
					if int(ConfigFileContents['MinBackers']) > 0:
						if int(ConfigFileContents['MinBackers']) > int(row['backers_count']):
							WriteRow =False
							DropMinCount = DropMinCount + 1
					
					# FILTER BASED ON MAXIMUM GOAL AMOUNT
					# SOME PEOPLE MAKE THESE OUTRAGEOUS GOALS THEY"RE UNSUCCESSFUL AND NOT WORHT LOOKING AT
					
					if float(ConfigFileContents['MaxGoal']) > 0:
						if float(ConfigFileContents['MaxGoal']) < float(row['goal']):
						# THE GOAL AMOUNT IS ABOVE THE SPECIFIED MAX GOAL
							WriteRow =False
							DropMaxGoal = DropMaxGoal  + 1
							
					# FILTER BASED ON MINIMUM PLEDGED AMOUNT
					if int(ConfigFileContents['MinPledge']) > 0:
						if float(ConfigFileContents['MinPledge']) > float(row['pledged']):
							WriteRow =False
							DropPledgedMin = DropPledgedMin + 1
					# FILTER BASED ON MAXIMUM PLEDGED AMOUNT
					
					if int(ConfigFileContents['MaxPledge']) > 0:
						if float(ConfigFileContents['MaxPledge']) < float(row['pledged']):
							WriteRow =False
							DropPledgedMax = DropPledgedMax + 1
					
		# FILTER BASED ON FINISHED PROJECTS ONLY, MAY OR MAY NOT INCLUDE CANCELLED PROJECTS
					if row['state'].lower()=='live':
						if ConfigFileContents['IncludeLive']:
							# SETTING INCLUDE LIVE PROJECTS< ONLY INCLUDE THEM IF THEY HAVE ALREADY SUCCESS
							if float(row['pledged']) < float(row['goal']):
								WriteRow =False
								DropLiveCount = DropLiveCount + 1
								
						else:
							WriteRow =False
							DropLiveCount = DropLiveCount + 1

						
					
					if 'cancel' in row['state'].lower():
						if not ConfigFileContents['IncludeCancelled']:
							WriteRow =False
							DropCancelCount = DropCancelCount + 1
						
					
					if WriteRow is False:
						DropCount = DropCount + 1
						TotalDropCount = TotalDropCount + 1
						if DebugScript:	print('Didnt write this row #' + str(DropCount))
							
							
# LETS SET UP THE FEATURES HERE
					if row['spotlight'] == 'true':
						#print('spotlighttrue1')
						if row['staff_pick'] =='true':
							#print('both true')
							#HERE WE ARE BOTH FEATURES
							Features='Both'
						else:
						# HERE WE ARE SPOTLIGHT ONLY
							#print('spotlightonly') 
							Features='Spotlight'
					else:
						if row['staff_pick'] == 'true':
						#HERE WE ARE STAFF PICK ONLY
							Features='StaffPick'
						else:
							# HERE WE ARE NEITHER
							Features='None'
					# SUCCESS OR FAIL
					
					if float(row['pledged']) >= float(row['goal']):
						ProjectSuccess = True
						successstr = 'Success'
						TotalSuccessCount = TotalSuccessCount + 1
						#input('Success Pledged:' + str(row['pledged']) + ' Goal ' + str(row['goal']))
					else:
						ProjectSuccess = False
						successstr = 'Failed'
						TotalFailCount = TotalFailCount + 1
						#input('Failed Pledged:' + str(row['pledged']) + ' Goal ' + str(row['goal']))	
					
					#if float(row['pledged']) > 1000000:
						#input ('Big one' + row['name'])
					
	# ASSEMBLE THE OUTPUT ROW
					Category = extractcategory(row['category'])
					SubCategory = extractsubcategory(row['category'])
					FullCategory = Category + " " + SubCategory
# here we are setting up actualpledge, which is 0 if the project state is failed or cancel.


# HERE we are setting up language field and filtering 
					LanguageCode =  row['country']
					LanguageName = 'zz'

#					LanguageCode = str(detectlanguage(row['blurb']))
#					LanguageName = str(getlanguagename(LanguageCode))
#					IncludeLanguages = ConfigFileContents['IncludeLanguages']
#					if IncludeLanguages is not None:
# Here there is something in the IncludeLanguages we must match it
#						if LanguageCode.lower() not in IncludeLanguages:
#							DropLanguage = DropLanguage +1
#							WriteRow =False
					#analyzedf[['deadline','launched_at']] = analyzedf[['deadline','launched_at']].apply(pd.to_datetime)
					#analyzedf['duratondays'] =  (analyzedf['deadline']-analyzedf['launched_at']).dt.days 
					deadlinedate = row['deadline']
					launcheddate = row['launched_at'] 
					durationdays = round((float(deadlinedate)-float(launcheddate))/(60*60*24),0)
					if row['static_usd_rate'] ==1:
						# US DOLLARS
						goalusd = row['goal']
						pledgedusd = row['pledged']
					else:
						goalusd = float(row['goal']) * float(row['static_usd_rate'])
						pledgedusd = float(row['pledged']) * float(row['static_usd_rate'])
					if 'CANCEL' in str(row['state']).upper() or  'FAIL' in str(row['state']).upper(): 
						ActualPledged = 0
					else:
						ActualPledged = pledgedusd

					outputrow = Category + "," + SubCategory + "," + str(row['state']).upper() + "," + row['backers_count'] + "," + str(goalusd) + "," + str(pledgedusd) + "," + row['spotlight'] + "," + row['staff_pick']  + "," + extractstate(row['location']) + "," + str(row['state_changed_at']) + "," + str(datetime.strptime(datestr,'%Y-%m-%d %H:%M:%SZ')) + "," + row['launched_at'] + "," + str(datetime.strptime(launchdatestr,'%Y-%m-%d %H:%M:%SZ')) + "," + row['deadline'] + "," +  str(datetime.strptime(deadlinedatestr,'%Y-%m-%d %H:%M:%SZ')) + "," + str(durationdays) + "," + str(ProjectSuccess) + "," + successstr + "," + Features + "," + extractcountry(row['location']) + "," + str(FullCategory) + "," + str(ActualPledged) + "," + row['country_displayable_name'] + "," + LanguageCode + "," + LanguageName
# ADD THE NAME AND BLURB TO THE OUTPUT ROW IF SPECIFIED
					if ConfigFileContents['IncludeName']:
						newname = row['name']
						bad_chars = [';', ':', '!', '*', '\\', '|', '@','\'','\"','.','?','#'] 
						for i in bad_chars:
							newname = newname.replace(i," ")
						newname = newname.replace(","," ")
						newname = newname.replace("\n"," ")
						outputrow= outputrow + "," + newname	
						
					if ConfigFileContents['IncludeBlurb']:
						blurb = row['blurb'] 
						bad_chars = [';', ':', '!', '*', '\\', '|', '@','\'','\"','.','?','#'] 
						for i in bad_chars:
							blurb = blurb.replace(i," ")
						blurb = blurb.replace(","," ")
						blurb = blurb.replace("\n"," ")
						outputrow = outputrow + "," + blurb
						
					
	
	
#  WRITE ALL RECORDS TO THEIR OWN FILE
					if "C" not in targetexist.upper():
						with open(TargetAllFileName, 'a', encoding="utf-8") as csvFileAll:
								try:
									if DebugScript:
										print('"All" file open, writing the row #' + str(TotalCount))
									csvFileAll.write(outputrow +"\n")
								except:
									pass

			
					if WriteRow is True:
			# WE WANT TO WRITE IT TO THE FILTERED TARGET FILE	
						WriteCount=WriteCount+1
						TotalWriteCount = TotalWriteCount + 1
			

					
						
						if DebugScript:	
							print(outputrow)
							print('write this row #' + str(WriteCount))
						if RecordCounter:
							print('Writing row #' + str(WriteCount))
						with open(TargetFileName, 'a', encoding="utf-8") as csvFile:
							try:
								if DebugScript:
									print('file open, writing the row #' + str(WriteCount))
					#	   rowwriter = csv.writer(csvfile, delimiter = ',')
								csvFile.write(outputrow +"\n")
				
					#	   csv.writer(csvFile).WriteRow(row['country']) 
							except:
								pass

				#	print('end for')
			f.close
			
			if DebugScript or RecordCounter:	
				print("Finished:" + FileName)
			if RecordCounter:
				print("Write count:" + str(WriteCount))
				print("Running Total Write count:" + str(TotalWriteCount))
				print("Drop count:" + str(DropCount))
				print("Running Total Drop count:" + str(TotalDropCount))
				print("File Count:" + str(ThisFileCount))
				print("Total so far:" + str(TotalCount))
				if int(DropMinCount) > 0:
					print ("Total below minimum backer count (" + str(ConfigFileContents['MinBackers']) +  "):" +  str(DropMinCount))
			
				if int(DropCountryCount) >0:
					print ("Total where Country (not in[" + ','.join(ConfigFileContents['IncludeCountries']) + "]:" + str(DropCountryCount))
		
				if int(DropAgeCount) >0:
					print ("Total older than " + str(ConfigFileContents['GoBackDays']) + " days:" +  str(DropAgeCount))
		
				if int(DropPledgedMin) > 0:
					print("Total below minimum pledged amount ($" + str(ConfigFileContents['MinPledge']) + "):" + str(DropPledgedMin))

				if int(DropPledgedMax) > 0:
					print("Total exceeding maximum pledged amount ($" +   str(ConfigFileContents['MaxPledge'])  + "):" + str(DropPledgedMax))

				if int(DropMaxGoal) > 0:
					print("Total exceeding maximum goal:" + str(DropMaxGoal))
				if int(DropCancelCount) > 0:
					print("Total cancelled projects:" + str(DropCancelCount))
				if int(DropLiveCount) > 0:
					print("Total Live Projects:" + str(DropLiveCount))


			FileName = updateFileName(FileName)
			if DebugScript:
				
				print("\nFinished processing " + str(FilesCount) + " input files.")
				print("Total projects in all files:" + str(TotalCount))
				print("Total successful projects in all files:" + str(TotalSuccessCount))
				print("Total failed projects in all files:" + str(TotalFailCount))
				print ("Total write count:" + str(TotalWriteCount))

				print ("written to: " + TargetFileName)
				print ("Total drop count:" + str(TotalDropCount))
				print("Note: Some records may be dropped appear to be dropped twice if two different filters would have dropped the record.")

		OutputString += "Total older than " + str(ConfigFileContents['GoBackDays']) + " days:" +  str(DropAgeCount) + "\n"	 
		OutputString += "Total below minimum backer count (" + str(ConfigFileContents['MinBackers']) +  "):" +  str(DropMinCount) + "\n"	
	#	print ("Total where Country (not in[" + ','.join(ConfigFileContents['IncludeCountries']) + "]:" + str(DropCountryCount))
		OutputString += "Total where Country (not in[" + ','.join(ConfigFileContents['IncludeCountries']) + "]:" + str(DropCountryCount)+ "\n"	
		OutputString += "Total below minimum pledged amount ($" + str(ConfigFileContents['MinPledge']) + "):" + str(DropPledgedMin) + "\n"
		OutputString += "Total exceeding maximum pledged amount ($" +   str(ConfigFileContents['MaxPledge'])  + "):" + str(DropPledgedMax) + "\n"
		OutputString += "Total exceeding maximum goal:" + str(DropMaxGoal) + "\n"
		OutputString += "Total cancelled projects:" + str(DropCancelCount) + "\n"
		OutputString += "Total live projects:" + str(DropLiveCount) + "\n"
		OutputString += "Records written: " + str(WriteCount) + "\n"
		OutputString += "File count: " + str(ThisFileCount) + "\n"
		OutputString += "Total successful projects in all files:" + str(TotalSuccessCount) + "\n"
		OutputString += "Total failed projects in all files:" + str(TotalFailCount) + "\n"
			

	if "S" not in targetexist.upper():
		print ('Deduping ' + ConfigFileContents['TargetFileNameOnly'] + '...')
		copyfile(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'], ConfigFileContents['TargetDirectory'] + '_Filtered_Pre_Dedupe.csv')
		DuplicateCountClean = dedupe(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'],ConfigFileContents,OutputString)
		if int(DuplicateCountClean) > 0:
			if RecordCounter: print(str(DuplicateCountClean) + " duplicate records in filtered file.")
		if "C" not in targetexist.upper():
			print ('Deduping ' + ConfigFileContents['TargetAllFileNameOnly'] + '...')
			DuplicateCountAll = dedupe(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetAllFileNameOnly'], ConfigFileContents,OutputString)
			if int(DuplicateCountAll) > 0:
				if RecordCounter: print(str(DuplicateCountAll) + " duplicate records in All file.")
		if "L" not in targetexist.upper():
			# user didnt enter L so we are doing languages
			if RecordCounter: print('Setting Up languages ...')
			addlanguages(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'],ConfigFileContents)
		
			DropLanguage = filterlanguages(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'],ConfigFileContents)
			
			if int(DropLanguage) > 0:
				TotalDropCount = TotalDropCount + DropLanguage
				OutputString += "Total where language not in [" + ','.join(ConfigFileContents['IncludeLanguages'])  + "]:" + str(DropLanguage)
				if ConfigFileContents['RecordCounter']: print("Total where language not in [" + ','.join(ConfigFileContents['IncludeLanguages'])  + "]:" + str(DropLanguage))
				if ConfigFileContents['DebugScript']: print("Total where language not in [" + ','.join(ConfigFileContents['IncludeLanguages'])  + "]:" + str(DropLanguage))
			if "C" not in targetexist.upper():
				AllLangStartTime = datetime.now()
				#print('Doing languages for ALL dataset, started at:' +  AllLangStartTime.strftime("%H:%M:%S")   +'\n... might as well grab lunch...')
				copyfile(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetAllFileNameOnly'], ConfigFileContents['TargetDirectory'] + '_All_Pre_Language.csv')
				addlanguages(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetAllFileNameOnly'],ConfigFileContents)
				FinishedAt=datetime.now()
				#print("Finished at " + FinishedAt.strftime("%H:%M:%S"))
				AllLangElapsed = FinishedAt - AllLangStartTime
				print('Took ' + str(AllLangElapsed) + ' to do languages...')
		if RecordCounter: print ("Total drop count:" + str(TotalDropCount))
		print (' ')

		print ('Creating Goal and Pledge Groups ...')
		creategroups(ConfigFileContents['DebugScript'],ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'], ConfigFileContents['GoalGroupCount'], 'goal')
		creategroups(ConfigFileContents['DebugScript'],ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'], ConfigFileContents['PledgedGroupCount'],'pledged')
		if "C" not in targetexist.upper():
			creategroups(ConfigFileContents['DebugScript'],ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetAllFileNameOnly'], ConfigFileContents['GoalGroupCount'], 'goal')
			creategroups(ConfigFileContents['DebugScript'],ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetAllFileNameOnly'], ConfigFileContents['PledgedGroupCount'],'pledged')
		print("Creating text blobs ...")
		makeblurbblobs(ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'],ConfigFileContents)
		
		FilterFileName = ConfigFileContents['TargetDirectory'] + ConfigFileContents['TargetFileNameOnly'].replace(".csv","_") + "FilterSettings.txt"
		
		#FilterFileContent = OutputFilters(ConfigFileContents,OutputString)
		open(FilterFileName, 'w').write(OutputString)
		
	
	print ('Beginning analysis ...\n')
#	groupgoals(ConfigFileContents)
	#grouppledged(ConfigFileContents)
	#calccategorysuccess(AnalyzeFileName,OutputFileName,CategoryFieldName,SuccessFieldBool):	
	#   TargetAllFileName
		# HERE WE ARE DOING SOME SPECIAL ANALYSIS ON PROJECTS IN THE CLEAN FILE THAT MATCH THE KEYWODS
	if ConfigFileContents['KeyWordsOne']:
		CreatedFile = makecsv(ConfigFileContents,TargetFileName,'blurb',ConfigFileContents['KeyWordsOne'], "regex")
		calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(CreatedFile).replace(".csv","_") + 'by_goalgroup_success.csv','goalgroup','projectsuccessb')
	#input(ConfigFileContents['KeyWordsTwo'])
	if ConfigFileContents['KeyWordsTwo']:
		CreatedFile = makecsv(ConfigFileContents,TargetFileName,'blurb',ConfigFileContents['KeyWordsTwo'], "regex")
		calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(CreatedFile).replace(".csv","_") + 'by_goalgroup_success.csv','goalgroup','projectsuccessb')


	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_durationdays_success.csv','durationdays','projectsuccessb')
# FOR SOME REASON THIS DOES NOT RUN AGAINST THE ALL FILE. GAVE UP TROUBLESHOOTING IT.
#	if "C" not in targetexist.upper():
#		if len(ConfigFileContents['KeyWords1']) > 0:
#			CreatedFile = makecsv(ConfigFileContents,TargetAllFileName,'blurb',ConfigFileContents['KeyWords1'], "regex")
#			calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(CreatedFile).replace(".csv","_") + 'by_goalgroup_success.csv','goalgroup','projectsuccessb')
#		if len(ConfigFileContents['KeyWords2']) > 0:
#			CreatedFile = makecsv(ConfigFileContents,TargetAllFileName,'blurb',ConfigFileContents['KeyWords2'], "regex")
#			calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(CreatedFile).replace(".csv","_") + 'by_goalgroup_success.csv','goalgroup','projectsuccessb')
	
	#grab CLEAN file, analyze every category
	analyzedf = pd.read_csv(TargetFileName, encoding="utf-8",header =0, error_bad_lines=False)
	allcategories = analyzedf.groupby(['category'], as_index=False)
	for Category in allcategories: #DOES calcategorysuccess for every category in CategoryBreakdown array defined in the YAML
	# THIS GOES THROUGH EVERY ITEM IN THE CategoryBreakdown array that is defined in the YAML file. It should be a list of categories.
		#input ('Category:' + Category[0])
		CreatedFile = makecsv(ConfigFileContents,TargetFileName,'category', Category[0],"equals" )
		calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_' + Category[0] + '_by_subcategory_success.csv','subcategory','projectsuccessb')
		calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_' + Category[0] + '_by_goalgroup_success.csv','goalgroup','projectsuccessb')
	
		# HERE  WE ARE ANALYZING THE SUCCESS OF certain SUBCATEGORIES
	CreatedFile = makecsv(ConfigFileContents,TargetFileName,'fullcategory', 'TECHNOLOGY GADGETS', "equals")
	calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_TECHNOLOGYGADGETS_by_goalgroup_success.csv','goalgroup','projectsuccessb')
	
	CreatedFile = makecsv(ConfigFileContents,TargetFileName,'staff_pick', 'True', "equals")
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_StaffPick_by_goalgroup_success.csv','goalgroup','projectsuccessb')

	#creategroups(ConfigFileContents['DebugScript'], ConfigFileContents['TargetDirectory'] + '' + TargetFileNameStub + '_TECHNOLOGY_by_goalgroup_success_target.csv', 5, 'goalgroup')
	CreatedFile = makecsv(ConfigFileContents,TargetFileName,'blurb', 'enamel', "contains")
	#calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\kickstarter_clean_' + Category[0] + '_by_subcategory_success.csv','subcategory','projectsuccessb')
	calccategorysuccess(CreatedFile, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(CreatedFile).replace(".csv","_") + 'by_goalgroup_success.csv','goalgroup','projectsuccessb')
	
	#LETS LOOK AT US ONLY STATES
	USRecordsOnlyCleanFileName = makecsv(ConfigFileContents,TargetFileName,'country', 'US', "equals")

	calccategorysuccess(USRecordsOnlyCleanFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(USRecordsOnlyCleanFileName).replace(".csv","_") +'by_state_loc_success.csv','state_loc','projectsuccessb')
	calccategorysuccess(USRecordsOnlyCleanFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(USRecordsOnlyCleanFileName).replace(".csv","_") +'by_goalgroup_success.csv','goalgroup','projectsuccessb')
	if "C" not in targetexist.upper():
		USRecordsOnlyAllFileName = makecsv(ConfigFileContents,TargetAllFileName,'country', 'US', "equals")
		calccategorysuccess(USRecordsOnlyAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + os.path.basename(USRecordsOnlyAllFileName).replace(".csv","_") +'by_state_loc_success.csv','state_loc','projectsuccessb')


	if DebugScript: print("Creating Success Grids for " + TargetFileNameStub)
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_fullcategory_success.csv','fullcategory','projectsuccessb')
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_projectsuccessb_success.csv','projectsuccessb','projectsuccessb')
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_category_success.csv','category','projectsuccessb')
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_goalgroup_success.csv','goalgroup','projectsuccessb')
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_staff_pick_success.csv','staff_pick','projectsuccessb')
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_features_success.csv','features','projectsuccessb')
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_goal_success.csv','goal','projectsuccessb')
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] +'SuccessGrids\\' + TargetFileNameStub + '_by_country_success.csv','country','projectsuccessb')
	calccategorysuccess(TargetFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetFileNameStub + '_by_language_success.csv','language','projectsuccessb')
	calcfilestats(TargetFileName,ConfigFileContents)
	

	if "C" not in targetexist.upper():
		if DebugScript: print("Creating Success Grids for " + TargetAllFileNameStub)
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_year_success.csv','year','projectsuccessb')
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_language_success.csv','language','projectsuccessb')
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_country_success.csv','country','projectsuccessb')
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_features_success.csv','features','projectsuccessb')
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_goalgroup_success.csv','goalgroup','projectsuccessb')
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_pledgedgroup_success.csv','pledgedgroup','projectsuccessb')
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_category_success.csv','category','projectsuccessb')
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_projectsuccessb_success.csv','projectsuccessb','projectsuccessb')
		calccategorysuccess(TargetAllFileName, ConfigFileContents['TargetDirectory'] + 'SuccessGrids\\' + TargetAllFileNameStub + '_by_fullcategory_success.csv','fullcategory','projectsuccessb')
		calcfilestats(TargetAllFileName,ConfigFileContents)







	EndTime = time.time()
	ElapsedTime = EndTime -  StartTime
	ElapsedTime = ElapsedTime/60
	time.strftime("%H:%M:%S", time.gmtime(ElapsedTime))
	print ("Total Run Time:" + str(round(ElapsedTime,1)) + " minutes")
	print("Program Complete.\nOutput Directory: " + ConfigFileContents['TargetDirectory'])

if __name__ == "__main__":
    main()


