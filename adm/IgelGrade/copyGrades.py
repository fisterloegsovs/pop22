#! /usr/bin/env python
import sys, os
from PIL import Image, ImageFont, ImageDraw
from subprocess import call 

if len(sys.argv) < 4:
    print("usage: ", sys.argv[0], "inputPdfFile gradesFile outputFilePrefix [trueTypeFont]")
    exit()
        
# generate tiff file.
# Original file is assumed to be an "Eksamensprotokol"
baseFileName, extension = os.path.splitext(sys.argv[1])

if extension !=".pdf":
    exit("error: first argument does not have .pdf extension")

tiffFileName = baseFileName + '.tiff'
call(["gs", "-o", tiffFileName, "-sDEVICE=tiffg4", "-r500",  sys.argv[1]])

# do ocr
# Christian did:
#    call(["tesseract", tiffFileName, baseFileName, "eksamen.cfg"])
# but for improved recognition and speed, we create a baseFileName.uzn
# with the bounding box of the kuids. This depends on the resolution
# of the conversion in the above case '-r500'.
call("echo '350 1800 300 2800 Text/Latin' > " + baseFileName + ".uzn", shell=True)
call("echo 'tessedit_char_whitelist 1234567890QWERTYUIOPASDFGHJKLZXCVBNM' > tesseract.cfg", shell=True)
call("echo 'tessedit_create_boxfile 1' >> tesseract.cfg", shell=True)
call(["tesseract", tiffFileName, "-psm", "4", baseFileName, "tesseract.cfg"])

# read box file
boxl = []
with open(baseFileName + '.box', 'r') as f:
        data = f.readlines()
# clean up
call(['rm', baseFileName + ".uzn"])
call(['rm', baseFileName + '.box'])
call(['rm', 'tesseract.cfg'])

for line in data:
        words = line.split()
        boxl.append(words)

# read grades file.
# The file must contain a line per student, where the first word is
# the KUID with capital letters, and the remainder of the line is the
# grade, e.g., 7 or "ej best."
gradesl = {}
with open(sys.argv[2], 'r') as f:
        data = f.readlines()

for line in data:
        words = line.split(' ', 1)
        words[0] = words[0].upper()
        gradesl[words[0]] = words[1][:-1]

#print(gradesl)
        
# extract students
studentl = []
for i in range(0, len(boxl)-5, 6):
    # confusion table. KUIDs are 3 letters followed by 3 digits
    for j in range(3):
        if boxl[i+j][0] == "2":
            boxl[i+j][0] = "Z"
        if boxl[i+j][0] == "8":
            boxl[i+j][0] = "S"
        if boxl[i+j][0] == "5":
            boxl[i+j][0] = "B"
        if boxl[i+j][0] == "0":
            boxl[i+j][0] = "O"
    for j in range(3, 6):
        if boxl[i+j][0] == "Z":
            boxl[i+j][0] = "2"
        if boxl[i+j][0] == "S":
            boxl[i+j][0] = "8"
        if boxl[i+j][0] == "B":
            boxl[i+j][0] = "5"
        if boxl[i+j][0] == "O":
            boxl[i+j][0] = "0"
    student = boxl[i][0] +  boxl[i+1][0] + boxl[i+2][0] + boxl[i+3][0] + boxl[i+4][0] + boxl[i+5][0] 
    studentl.append([student, int(boxl[i][4]), int(boxl[i][5])]) 

#print(studentl)

# search, check and set font 
fontname='/System/Library/Fonts/Keyboard.ttf'
if len(sys.argv) == 5: # has a true type font be specified?
    fontname = sys.argv[4]
try:
    font = ImageFont.truetype(fontname,56)
except:
    font = ImageFont.load_default()
    print("warning: did not find proper true type font")
    print("\tusing ugly default font instead")
                        
# arguments for merging pdf files
outputFileName, extension = os.path.splitext(sys.argv[3])
arguments = ["gs", "-dBATCH", "-dNOPAUSE", "-q", "-sDEVICE=pdfwrite", "-dPDFSETTINGS=/prepress", '-sOutputFile=' + outputFileName + '.pdf']

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

#print(im.n_frames)

# add grades, save each pages as single pdf. It is assumed that the students are ordered pagewise
for s in studentl:
    if(s[2]!=page):
        outputPageBase = outputFileName + str(page)
        im.save(outputPageBase + '.tiff')
        call(['tiff2pdf', '-o', outputPageBase + '.pdf', outputPageBase + '.tiff'])
        arguments.append(outputPageBase + '.pdf')
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
outputPageBase = outputFileName + str(page)
im.save(outputPageBase + '.tiff')
call(['tiff2pdf', '-o', outputPageBase + '.pdf', outputPageBase + '.tiff'])
arguments.append(outputPageBase + '.pdf')
im.close()

#print(arguments)

# merge pdfs
call(arguments)

# clean up
for page in range(frames):
    outputPageBase = outputFileName + str(page)
    call(['rm', outputPageBase + '.tiff'])
    call(['rm', outputPageBase + '.pdf'])
call(['rm', tiffFileName])

# check whether some students were not listed on the sheet
if gradesl:
    print("warning: some graded students were not found in grading sheet:")
    print(gradesl)
