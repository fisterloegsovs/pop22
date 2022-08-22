open System.Drawing

let bmp = new Bitmap(640, 480)

//do imgToBitmap bmp 10.0 (Img.boolToColor << Img.polarChecker 10)
do Img.imgToBitmap bmp 10.0 (Img.fracToColor << Img.wavDist)

do Img.toPngFile @"wav.png" bmp
