module Raster

val make_raster : string
                  -> (System.Drawing.Bitmap -> unit)
                  -> System.Windows.Forms.Form

val make_app : string
               -> ('s -> System.Drawing.Bitmap -> unit)
               -> ('s -> System.Windows.Forms.KeyEventArgs -> 's option)
               -> 's -> System.Windows.Forms.Form
