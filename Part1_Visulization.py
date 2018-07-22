# -*- coding: utf-8 -*-
"""
Created on Mon Jul 16 18:56:02 2018

@author: Chayan
"""
import pandas as pd
import matplotlib.pyplot as plt
myfile = pd.read_csv("movie_metadata.csv")
myfile = pd.DataFrame(myfile)
myfile = myfile.dropna()
myfile.shape
myfile.head()
#How many movies with same Genere
myfile_genres = myfile["genres"].value_counts()

#How many movies directed by every director
myfile_directors = myfile["director_name"].value_counts()

#How many movies by each actor as lead

myfiles_actors = myfile["actor_1_name"].value_counts()

#Movies in diffrent Genres
myfile_movie_genres = myfile["movie_title"].groupby(myfile["genres"]).value_counts()
myfile_movie_genres = myfile.groupby(myfile["genres"])["movie_title"].sum()

#Gross collection of each Genres

myfile_gross_genre = myfile.groupby(myfile["genres"])["gross"].sum()
myfile_gross_genre.sort_values()

#Highest Grossing Directior

myfile_gross_genre = myfile.groupby(myfile["director_name"])["gross"].sum()
myfile_gross_genre.sort_values()


#Highest Grossing Actor

myfile_gross_genre = myfile.groupby(myfile["actor_1_name"])["gross"].sum()
myfile_gross_genre.sort_values()


plt.title("Histogram of IMDB Score")
plt.ylabel("No. of Ratings")
plt.xlabel("Rating out of 10")
plt.hist(myfile["imdb_score"])
plt.show()

plt.title("Bar graph of Movies rating and Facebook Likes")
plt.ylabel("FaceBook Like of Movie")
plt.xlabel("Movie Rating at IMDB")
plt.bar(myfile["imdb_score"],myfile["movie_facebook_likes"])

plt.title("Scatter plot of Movies rating and Facebook Likes")
plt.ylabel("FaceBook Like of Movie")
plt.xlabel("Movie Rating at IMDB")
plt.scatter(myfile["imdb_score"],myfile["movie_facebook_likes"])

plt.title("Scatter plot of Gross and Imdb Rating")
plt.ylabel("Gross of Movie")
plt.xlabel("Movie Rating at IMDB")
plt.scatter(myfile["imdb_score"],myfile["gross"])

plt.title("Bar Graph of Gross and Genre")
plt.ylabel("Gross of Movie")
plt.xlabel("Genres")
plt.plot(myfile["genres"],myfile["gross"])
