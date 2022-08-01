type Image<'a>(width, height, c : 'a) =
  member val data = Collections.Array2D.create<'a> width height c

  new(width, height) =
    Image(width, height, Unchecked.defaultof<'a>)
 
  new() =
    Image(256, 256, Unchecked.defaultof<'a>)

let F1 = new Image<float>(3,5,0.0)
let F2 = new Image<float>(3,5)
let F3 = new Image<float>()

let c = System.Drawing.Color.FromArgb(0,0,0)
let C1 = new Image<System.Drawing.Color>(3,5,c)
let C2 = new Image<System.Drawing.Color>()
