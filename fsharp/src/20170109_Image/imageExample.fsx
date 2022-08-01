let C = Image.fromFile "Barbara.jpg"
let gray = Image.toGray C
let col = Image.toColor gray
Image.toFile "test.jpg" col
