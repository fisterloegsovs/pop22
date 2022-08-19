open System
open System.Drawing
open System.Windows.Forms

// deserialize a bitmap file
let fromFile (name : string) = new Bitmap(name)

let f = new Form(Text = "Simple", MaximizeBox = true)

let a = ref 1
do f.Paint.Add (
    fun e -> let redPen : Pen = new Pen(Color.Red)
             let bluePen : Pen = new Pen(Color.Blue)
             let Rects : Rectangle array = [| new Rectangle(20,  20, 120, 20);
                                              new Rectangle(20,  50, 120, 30);
                                              new Rectangle(20,  90, 120, 40);
                                              new Rectangle(20, 140, 120, 60) |]
             let bmpPicture = fromFile @"apple.jpg"
             a := !a + 1
             e.Graphics.DrawImage(bmpPicture, 12, 12)
             e.Graphics.DrawRectangles(redPen, Rects)
             e.Graphics.DrawLine(bluePen, !a*10,20,120,45))

do Application.Run f



// do form.Show()

// do form.Refresh()
// do Application.DoEvents()
// do Application.DoEvents()

// form.Paint.Add (
//     fun e -> let penCurrent : Pen = new Pen(Color.Blue);
//              let Rect : Rectangle array = [| new Rectangle(30,  20, 120, 20);
//                                              new Rectangle(30,  50, 120, 30);
//                                              new Rectangle(30,  90, 120, 40);
//                                              new Rectangle(30, 140, 120, 60) |]
//              e.Graphics.DrawRectangles(penCurrent, Rect));;
// do form.Refresh()
// do Application.DoEvents()
