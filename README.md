# The-Traveling-Salesman-Problem
For this assignment, you will write a program in FORTRAN 95 to solve the Minimum Tour problem (aka the Traveling Salesman Problem).  The problem may be described as follows:   A salesman must leave his home city and visit N cities after which he then returns home.  The order in which he visits the cities is unimportant, however he must visit each of the N cities, and he must start and end in his home city.   Your task is to find the minimum tour, the shortest possible route from home base through N cities and back home again.

Your project will consist of a single source code file,  p3.f95

You will find the shortest route using the brute force method.  You must sum the distance traveled for each possible route, retaining the shortest distance found as your answer.  Thus, your program will have to generate all possible routes.   This is an NP-complete problem with complexity O(n!).    The algorithm to generate all possible routes is somewhat complex.   Pseudocode  Download Pseudocodewill outline the algorithm.  A sample program to generate the permutations, written in java:  Permute.java  Download Permute.java.

Input:

Your program will first prompt the user to enter a filename.  This file name is a data file that conforms to the following specifications:  The first line of the file is the number of cities, including home base.   Following this count,  data consists of a two dimensional 'chart' giving the distance (in miles)  between each pair of cities.  The home base city is always the first city listed in the file.
  

Chart: distance between cities
San Diego	Phoenix	Denver	Dallas
San Diego	0	350	900	1100
Phoenix	350	0	560	604
Denver	900	560	0	389
Dallas	1100	604	389	0

 To make processing easier, there is only one item per line in the file.   The above data would appear in the file this way:

4
SanDiego
0
350
900
1100
Phoenix
350
0
560
604
Denver
900
560
0
389
Dallas
1100
604
389
0
 

Output:

Your program must print the shortest route found, including the name of each city in the order visited,  the distance traveled to get there, and the total number of miles traveled.  Sample output from the above data file:

 Enter filename: 
 data4.txt

 San Diego to Phoenix --  350  miles
 Phoenix to Dallas --  604  miles  
 Dallas to Denver --  389  miles
 Denver to San Diego --  900  miles

 Best distance is:  2243 miles

Grading:

Your program will be graded on correctness, efficiency, and good programming practices.   Your source code file will be compiled with the f90 compiler on Gradescope (Ubuntu) and edoras as follows:

gfortran -o p3 p3.f95

Programs that fail to compile will receive very little or no credit.   Your program will be tested with a data file not available to you. 

In this program you will use some or all of these Fortran features:

variable declarations
keyboard input
file input
output to screen
dynamic allocation
arrays
repetition
subroutine
recursion
