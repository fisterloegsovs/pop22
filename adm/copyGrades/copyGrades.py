#! /usr/bin/env python
import sys, os
from PIL import Image, ImageFont, ImageDraw
from subprocess import call
from shutil import copyfile

# Parse input
if len(sys.argv) < 4:
    print("usage: ", sys.argv[0], "inputPdfFile gradesFile outputFile [trueTypeFont]")
    exit()
inputPdfFile = sys.argv[1]
#inputPdfFileBase, extension = os.path.splitext(inputPdfFile)
if os.path.splitext(inputPdfFile)[1].lower() != ".pdf":
    exit("error: first argument must have .pdf extension")
gradesFile = sys.argv[2]
outputFile = sys.argv[3]
if os.path.splitext(outputFile)[1].lower() != ".pdf":
    exit("error: third argument must have .pdf extension")
if len(sys.argv) > 4: # has a true type font be specified?
    trueTypeFont = sys.argv[4]
else :
    trueTypeFont = '/System/Library/Fonts/Keyboard.ttf'

# uzn filenames are assumed based on original name given as argument to tesseract. However, if name has spaces, then this goes wrong, so we copy to a temporary file with a usable filename
inputPdfFileBase, inputPdfFileExt = os.path.splitext(inputPdfFile)
inputPdfFileBase = "copyGrades"
inputPdfFileCopy = inputPdfFileBase + ".pdf"
copyfile(inputPdfFile,inputPdfFileCopy)

# generate tiff file.
tiffFileName = inputPdfFileBase + '.tiff'
call(["gs", "-o", tiffFileName, "-sDEVICE=tiffg4", "-r500",  inputPdfFileCopy])

# do ocr
# Recognition and speed are improved by use of an inputPdfFileBase.uzn for the UNLV system of psm4.
# This file specifies the bounding box of the kuids and depends on the resolution
# of the conversion. Here gs with '-r500' is assumed.
patFileName = inputPdfFileBase + ".pat"
with open(patFileName,"w") as file: file.write("\A\A\A\d\d\d")
uznFileName = inputPdfFileBase + ".uzn"
with open( uznFileName,"w") as file: file.write("350 1800 300 2800 Text/Latin")
cfgFileName = inputPdfFileBase+".cfg"
with open(cfgFileName,"w") as file:
  file.write("tessedit_char_whitelist 1234567890QWERTYUIOPASDFGHJKLZXCVBNM\n")
  file.write("tessedit_create_boxfile 1")
call(["tesseract", tiffFileName, "-psm", "4", "--user-patterns", patFileName, inputPdfFileBase, cfgFileName])

# read box file
boxl = []
boxFile = inputPdfFileBase + '.box'
with open(boxFile, 'r') as f:
    data = f.readlines()

for line in data:
    words = line.split()
    boxl.append(words)

# read grades file.
# The file must contain a line per student, where the first word is
# the KUID with capital letters, and the remainder of the line is the
# grade, e.g., 7 or "ej best."
gradesl = {}
with open(gradesFile, 'r') as f:
    data = f.readlines()

for line in data:
    words = line.split(';', 1)
    words[0] = words[0].upper()
    gradesl[words[0]] = words[1][:-1]

# extract students
studentl = []
for i in range(0, len(boxl)-5, 6):
    # confusion table. KUIDs are 3 letters followed by 3 digits
    for j in range(3):
        if boxl[i+j][0] == "2":
            boxl[i+j][0] = "Z"
            print("Fixed 2")
        if boxl[i+j][0] == "5":
            boxl[i+j][0] = "S"
            print("Fixed 5")
        if boxl[i+j][0] == "8":
            boxl[i+j][0] = "S"
            print("Fixed 8")
        if boxl[i+j][0] == "0":
            boxl[i+j][0] = "O"
            print("Fixed 0")
    for j in range(3, 6):
        if boxl[i+j][0] == "Z":
            boxl[i+j][0] = "2"
            print("Fixed Z")
        if boxl[i+j][0] == "S": # 5 and 8 are often confused with S, here a choice is made!
            boxl[i+j][0] = "5"
            print("Fixed S")
        if boxl[i+j][0] == "O":
            boxl[i+j][0] = "0"
            print("Fixed O")
    student = boxl[i][0] +  boxl[i+1][0] + boxl[i+2][0] + boxl[i+3][0] + boxl[i+4][0] + boxl[i+5][0]
    studentl.append([student, int(boxl[i][4]), int(boxl[i][5])])

# search, check and set font
try:
    font = ImageFont.truetype(trueTypeFont,56)
except:
    font = ImageFont.load_default()
    print("warning: did not find proper true type font")
    print("\tusing ugly default font instead")

# arguments for merging pdf files
arguments = ["gs", "-dBATCH", "-dNOPAUSE", "-q", "-sDEVICE=pdfwrite", "-dPDFSETTINGS=/prepress", '-sOutputFile=' + outputFile]

# read image file
im = Image.open(tiffFileName)
[imx, imy] =  im.size # get image dimensions
frames = im.n_frames
im = Image.open(tiffFileName) # What a mess! n_frames closes the file

# prepare drawing
page = 0 # page counter for multipage input file
try:
    im.seek(page)
except EOFError:
    exit('seek failed')
draw = ImageDraw.Draw(im)

# add grades, save each pages as single pdf. It is assumed that the students are ordered pagewise
outputFileBase, outputFileExt = os.path.splitext(outputFile)
for s in studentl:
    if(s[2]!=page):
        outputPageBase = outputFileBase + str(page)
        im.save(outputPageBase + '.tiff')
        call(['tiff2pdf', '-o', outputPageBase + outputFileExt, outputPageBase + '.tiff'])
        arguments.append(outputPageBase + outputFileExt)
        page += 1;
        try:
            im.seek(page)
        except EOFError:
            exit('seek failed')
        del draw
        draw = ImageDraw.Draw(im)
    if(s[0] in gradesl):
        draw.text((3475, imy - s[1]), gradesl[s[0]], 0, font);
        del gradesl[s[0]]
        #print(s[0], "on page ", page, ", expected on page ", im.tell())
outputPageBase = outputFileBase + str(page)
im.save(outputPageBase + '.tiff')
call(['tiff2pdf', '-o', outputPageBase + outputFileExt, outputPageBase + '.tiff'])
arguments.append(outputPageBase + outputFileExt)
im.close()

# merge pdfs
call(arguments)

# check whether some students were not listed on the sheet
if gradesl:
    print("warning: some graded students were not found in grading sheet:")
    print(gradesl)

# clean up
for page in range(frames):
    outputPageBase = outputFileBase + str(page)
    call(['rm', outputPageBase + '.tiff'])
    call(['rm', outputPageBase + '.pdf'])

call(['rm', uznFileName])
call(['rm', patFileName])
call(['rm', boxFile])
call(['rm', cfgFileName])
call(['rm', tiffFileName])
call(['rm', inputPdfFileCopy])
