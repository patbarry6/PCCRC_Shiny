# PCCRC_Shiny
Shiny apps for PCCRC website
As the website developer for the PCCRC I made a few interactive ShinyApps to help showcase the funding history of the cooperative. This repo holds the R code for those apps so that in the future if the administrative assistant in charge needs help making changes I can easily help out. 

PCCRC_Map
https://www.pccrc.org/research-fellows-lite
This is an interactive graph with all of the students that have been funded by the PCCRC either on fellowship or through a research grant. It reads in the plain text file ‘Alumni.txt’ and plots the locations of all the students. Keeping track of all the students is a pain, but Gabrielle Hazelton has done a really good job of keeping tabs on all of the CFOS students.

PCCRC_Funding
https://www.pccrc.org/request-for-proposals
This is an interactive graph that allows the user to explore what the PCCRC has funded in the past. This app pulls data from the comma separated file ‘PCCRCHistoryMelt.csv’ which is just a melted version of the file PCCRCProjectHistoryShiny.csv.
