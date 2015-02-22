#!/usr/bin/python
#======================================================================================
# Pad records in a text file to 80 characters.
#
# Arguments:
# -i or --ifile path to input file
# -o or --ofile path to output file
#
# Version: 1.0.0
# Date: 06 Mar 2015
# Author: Dave Nicolette
#======================================================================================

import sys, getopt

def main(argv):
   inputfile = ''
   outputfile = ''
   try:
      opts, args = getopt.getopt(argv,"hi:o:",["help","ifile=","ofile="])
   except getopt.GetoptError:
      print 'test.py --ifile|-i <inputfile> --ofile|-o <outputfile>'
      sys.exit(2)
   for opt, arg in opts:
      if opt in ('-h', '--help'):
         print 'test.py --ifile|-i <inputfile> --ofile|-o <outputfile>'
         sys.exit()
      elif opt in ("-i", "--ifile"):
         inputfile = arg
      elif opt in ("-o", "--ofile"):
         outputfile = arg

   outfile = open(outputfile, 'w+')
   with open(inputfile, 'r') as infile:
       for line in infile:
           outfile.write(line.rstrip().ljust(80) + '\n')
   infile.close()
   outfile.close()
if __name__ == "__main__":
   main(sys.argv[1:])
