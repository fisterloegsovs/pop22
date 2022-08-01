open System  
open System.Drawing   
open System.Windows.Forms    
let gettimeform=new Form(Text="Show Current Time")  
gettimeform.BackColor<-Color.DarkGray
let lblmsg=new Label(Top=60,Left=60,Width=130,BorderStyle=BorderStyle.FixedSingle)
lblmsg.Text<-"Current Time of System"
gettimeform.Controls.Add(lblmsg)
let lbltime=new Label(Top=100,Left=60,Width=115,BorderStyle=BorderStyle.FixedSingle)  
let ffont=new Font("Arial", 14.5F) 
let exitbutton=new Button(Top=140,Left=80)   
exitbutton.Text<-"Exit"
exitbutton.BackColor<-Color.Ivory
let timeobject=new Timer(Interval=1000,Enabled=true)  
lbltime.Font<-ffont  
timeobject.Tick.Add(fun showtime->  
let dtobject=Convert.ToString(System.DateTime.Now)  
let timepart=dtobject.Substring(10)  
lbltime.Text<-timepart)  
gettimeform.Controls.Add(lbltime)  
gettimeform.Controls.Add(exitbutton)  
exitbutton.Click.Add(fun _->  
timeobject.Stop()   
gettimeform.Close())                     
gettimeform.Show()  
Application.Run(gettimeform)  
