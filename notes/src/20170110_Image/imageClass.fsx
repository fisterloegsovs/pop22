type Image(width, height, c) =
  member val data = Collections.Array2D.create<float> width height c

  new(width, height) =
    Image(width, height, 0.0)
 
  new() =
    Image(256, 256, 0.0)

let F1 = new Image(3,5,0.0)
let F3 = new Image(3,5)
let F4 = new Image()
