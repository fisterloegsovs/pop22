Title: copyGrade 1.2

Dates: 
2017/02/14 - Improved script so that it produces helper files itself.
2018/02/13 - Corrected error, that uzn-filename must not contain spaces. Added user-pattern to improve recognition rates of ku ids. Corrected confusion matrix for tesseract3

Authors: Christian Igel and Jon Sporring

Disclaimer: this software comes as is, is free to use, but we take no responsibility for the correctness of the end result. Send comments and wishes to sporring@di.ku.dk

Example: (macos v.10.13.3, python v.3.6.1 (from Anaconda), tesseract v.3.04.01 and tesseract-eng v.3.04_1 from macports)
  python3 copyGrades.py empty.pdf grades.csv filled.pdf

Description: 
This script copies grades from a grades file to an eksamensprotokol file. Example of its use is:

  python3 copyGrades.py empty.pdf grades.csv filled.pdf

where grades.txt is a line-by-line listing of the student’s grade e.g.,

  ABC123;best.
  BCA231;7
  CAB312;udebl.

IMPORTANT: The filename "emptyEksamensprotokol.pdf" must be a name without spaces, otherwise tesseract won't function properly.

The script searches the emptyEksamensprotokol.pdf file for KUIDs from grades.txt and renders the whatever is on the remainder of the grades.txt file into the emptyEksamensprotokol.pdf and stores the result in filledEksamensprotokol.pdf. A 4th argument may be added, e.g., '/opt/X11/share/fonts/TTF/Vera.ttf' specifying an alternative Truetype font to be used for printing into the 'filled.pdf' file.

The script relies on a number of command-line tools:
  tesseract - an optical character recognition tool
  gs - a tool to render pdf files
  tiff2pdf - a tool to convert tiff images to pdf format
  python3 - a scripting tool. Note python version below 3 are not compatible with later
and a number of python packages:
  PIL
  subprocess
These must all be installed prior to the use of copyGrades.py. Also, the script produces a number of temporary files copyGrades.*, which are deleted after use.
