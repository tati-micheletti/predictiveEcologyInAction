# predictiveEcologyInAction
This repository was designed to demonstrate hands-on how different areas of ecology can be connected to answer interesting scientific questions.


#  Welcome the world of Predictive Ecology  #

Here you will find the instructions necessary to navigate the 
practical part of the course on Predictive Ecology. 

## GROUPS  
First, you will need to divide the whole class into 3 groups. 
These will be known at first as the TREE groups, the BIRDS group 
and the TURTLE group. Each group will have to bring one (AND ONLY 
ONE) computer to the course.

## NEEDED SOFTWARE  
The 3 computers should be preferably Windows based. They will have 
to have the following installed:
  2.1. Excel (preferably) or OpenOffice Calc.
  2.2. R
  2.3. RStudio (Desktop, not Server)

To install the last two, please use the follownig useful illustrated guide: 
https://techvidvan.com/tutorials/install-r/#:~:text=Step%20%E2%80%93%201%3A%20Go%20to%20CRAN,the%20latest%20version%20of%20R.

Alternatively, watch this YouTube Video: https://www.youtube.com/watch?v=9-RrkJQQYqY
with the accompanying link: https://rpubs.com/MohammadShadan/installrrstudio

Or you can even skip all that, and download the software directly here!
R: https://cran.r-project.org/bin/windows/base/R-4.1.3-win.exe
RStudio: https://download1.rstudio.org/desktop/windows/RStudio-2022.02.1-461.exe
Download and click on the .exe and follow instructions. Use all defaults.

## INSTALL LIBRARIES  
Once you have managed to install the needed software, you should install the 
libraries we will need for the exercise. It is important to try to do this 
BEFORE the course. Libraries can take time to download and install and we 
don't want to waste time doing it during the course.

Once you have installed R, you can open it and run the code between the lines below,
one at a time.  
```
if (!require("Require")) {install.packages("Require"); require("Require")}
Require("data.table")
Require("raster")
Require("googledrive")
Require("reproducible")
Require("ggplot2")
Require("gridExtra")
```
## DON'T PANIC  
I know it might be scary at the beginning, especially if something doesn't work. 
But we will go very slowly with the hands-on exercise, and I will guide you through it. 
There is no need to panic. Even if you were not successful installing the libraries, 
the important thing is to try! 

## OBJECTIVES  
The main objective of this course is to answer the following scientific question 
(with made up data, of course):

**Are birds good umbrellas for turtles now and in the future?**

in other words

**Do we have a positive relationship between bird abundance and turtle habitat suitability?**

## STARTING

To start, each group will open their respective files, either the .Rmd version in 
RStudio, or the .pdf version in Adobe. The advantage of using the .Rmd is that you 
can run the codes directly in RStudio, without the need to copy+paste. The choice is 
yours, though!  

*TREE GROUP:* `landscapeSimulation.Rmd` or `landscapeSimulation.pdf`  
*BIRD GROUP:* `birdSimulation.Rmd` or `birdSimulation.pdf`  
*TURTLE GROUP:* `turtleSimulation.Rmd` or `turtleSimulation.pdf`  

We will follow the instructions in these files and once all groups are done, we will 
work together on the integration of all this work with the file:  

*INTEGRATION:* `integratingSimulations.Rmd` or `integratingSimulations.pdf`  


See you soon!
