Title: DIKUGrade 1.1

Date: 2017/02/14

Authors: Christian Igel and Jon Sporring

Disclaimer: this software comes as is, is free to use, but we take no responsibility for the correctness of the end result. Send comments and wishes to sporring@di.ku.dk

This script copies grades from a grades file to an eksamensprotokol file. Example of its use is:

  python copyGrades.py emptyEksamensprotokol.pdf grades.txt filledEksamensprotokol /opt/X11/share/fonts/TTF/Vera.ttf

where grades.txt is a line-by-line listing of the student’s grade e.g.,

  ABC123 best.
  BCA231 7
  CAB312 udebl.

The script searches the emptyEksamensprotokol.pdf file for KUIDs from grades.txt and renders the whatever is on the remainder of the grades.txt file into the emptyEksamensprotokol.pdf and stores the result in filledEksamensprotokol.pdf. The font used is a parameter, and must be a Truetype font. If none is specified, then the useless minuscule font is used.

The script relies on a number of command-line tools:
  tesseract - an optical character recognition tool
  gs - a tool to render pdf files
  tiff2pdf - a tool to convert tiff images to pdf format
  python3 - a scripting tool. Note python version below 3 are not compatible with later
and a number of python packages:
  PIL
  subprocess
These must all be installed prior to the use of copyGrades.py.

