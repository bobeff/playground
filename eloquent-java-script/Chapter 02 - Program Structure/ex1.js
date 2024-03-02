/*
Looping a Triangle

Write a loop that makes seven calls to console.log to output the following
triangle:

#
##
###
####
#####
######
#######
*/

for (line = "#"; line.length <= 7; line += "#") {
  console.log(line);
}
