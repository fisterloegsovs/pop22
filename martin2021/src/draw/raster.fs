module Raster

open System.Drawing
open System.Windows.Forms

let format = Imaging.PixelFormat.Format24bppRgb

let bitmap_of (f:Bitmap -> unit) (r:Rectangle) : Bitmap =
  let bitmap = new Bitmap (r.Width, r.Height, format)
  do f bitmap
  bitmap

let resize f (b:Bitmap ref) (w: #Form) _ =
  b := bitmap_of f w.ClientRectangle
  w.Invalidate()

let redraw f (b:Bitmap ref) (w: #Form) =
  do f (!b)
  w.Invalidate()

let paint (b:Bitmap ref) (v: #Form) (e:PaintEventArgs) =
  let r = e.ClipRectangle
  e.Graphics.DrawImage(!b,r,r,GraphicsUnit.Pixel)

let make_raster (t:string) (f:Bitmap -> unit) : Form =
  let form = new Form (Visible=true,Text=t)
  let bitmap = ref (bitmap_of f form.ClientRectangle)
  form.Resize.Add(resize f bitmap form)
  form.Paint.Add(paint bitmap form)
  form.KeyDown.Add(fun e -> if e.KeyCode = Keys.Escape then form.Close())
  form

let make_app (t:string)
             (f:'s -> Bitmap -> unit)
             (onKeyDown: 's -> KeyEventArgs -> 's option) (s:'s)  : Form =
  let form = new Form (Visible=true,Text=t)
  let state = ref s
  let g b = f (!state) b
  let bitmap = ref (bitmap_of g form.ClientRectangle)
  form.Resize.Add(resize g bitmap form)
  form.Paint.Add(paint bitmap form)
  form.KeyDown.Add(fun e -> if e.KeyCode = Keys.Escape then form.Close()
                            else match onKeyDown (!state) e with
                                   | None -> ()
                                   | Some s -> (state := s; redraw g bitmap form))
  form

//let f = make_raster "Hello" (fun _ -> ())

//do Application.Run f
