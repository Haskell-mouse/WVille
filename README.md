# WVille

Command-line utility that makes Wigner-Ville (or Pseudo Wigner-Ville) transformation. 

################################################

Utility opens all files with "txt" file extension in a given path, parse data and makes Wigner-Ville transformation. 

Data format in that files should be as given : 

<name_of_column_1>  <name_of_column_2>  etc....
1                   1
2                   2 
3                   3 
4                   4
5                   5
6                   6
7                   7
8                   8   
etc....             etc...

Columns should be divided by one or more spaces.

As result - there will be files with the result of Wigner-Willie transform, with name in format 
"<filename>-<name_of_column_n>-result.txt"

Columns mean time slices and rows are frequency.
Frequency range is from 0 to n/4, where n is a sampling rate.