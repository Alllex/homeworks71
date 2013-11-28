
namespace PatternBridge

type GraphicsSystem = WPF | WF // WindowsPresentationFoundation and WindowsForms

[<Interface>]
type BridgeWindowImp =
    abstract member ShowImp : unit -> unit
    abstract member DecreaseSize : unit -> unit
    abstract member IncreaseSize : unit -> unit
    abstract member SetTitle : string -> unit
    abstract member DrawLine : int * int * int * int -> unit
    
open System
open System.Windows.Forms
open System.Drawing

type WFWindow(width: int, height : int) as this = 
    class 
        inherit Form()
        
        do this.Text <- "WinForms Form"
           this.Size <- Size(width, height)

        interface BridgeWindowImp with
            member this.ShowImp() = do Application.Run(this)

            member this.DecreaseSize() = 
                this.Size <- System.Drawing.Size(this.Width * 2 / 3, this.Height * 2 / 3)

            member this.IncreaseSize() = 
                this.Size <- System.Drawing.Size(this.Width * 3 / 2, this.Height * 3 / 2)

            member this.SetTitle(s : string) = this.Text <- s

            member this.DrawLine(x1 : int, y1 : int, x2 : int, y2 : int) =
                let pen = new Pen(Color.FromArgb(255, 0, 0, 0))
                this.Paint.Add(fun e -> e.Graphics.DrawLine(pen, x1, y1, x2, y2))
    end

open System.Windows
open System.Windows.Shapes
open System.Windows.Controls

type WPFWindow(width: int, height : int) as this = 
    class         
        inherit Window()

        do this.Width <- (float) width
           this.Height <- (float) height
           this.Content <- new Canvas()
           this.Title <- "WindowsPresentationFoundation Window"

        interface BridgeWindowImp with
            member this.ShowImp() =
                this.Show()
                let app = new Application()
                app.Run() |> ignore

            member this.DecreaseSize() = 
                this.Width <- this.Width * 2.0 / 3.0
                this.Height <- this.Height * 2.0 / 3.0

            member this.IncreaseSize() = 
                this.Width <- this.Width * 3.0 / 2.0
                this.Height <- this.Height * 3.0 / 2.0

            member this.SetTitle(s : string) = this.Title <- s

            member this.DrawLine(x1 : int, y1 : int, x2 : int, y2 : int) =
                let myLine = new Line()
                myLine.Stroke <- System.Windows.Media.Brushes.LightSteelBlue
                myLine.X1 <- (float) x1
                myLine.X2 <- (float) x2
                myLine.Y1 <- (float) y1
                myLine.Y2 <- (float) y2
                myLine.StrokeThickness <- 1.0
                let sp = this.Content :?> Canvas
                sp.Children.Add(myLine) |> ignore
                this.Content <- sp
    end

type BridgeWindow(width: int, height : int) =

    let gs = 
            #if DEBUG 
                WF 
            #else 
                WPF 
            #endif
    
    let imp = match gs with
              | WPF -> new WPFWindow(width, height) :> BridgeWindowImp
              | WF  -> new WFWindow(width, height)  :> BridgeWindowImp

    member this.Show() = imp.ShowImp()
    member this.MakeBigger() = imp.IncreaseSize()
    member this.MakeSmaller() = imp.DecreaseSize()
    member this.DrawRect(x1 : int, y1 : int, x2 : int, y2 : int) =
        imp.DrawLine(x1, y1, x1, y2)
        imp.DrawLine(x1, y2, x2, y2)
        imp.DrawLine(x2, y2, x2, y1)
        imp.DrawLine(x2, y1, x1, y1)

type BigWindow() as this =
    class
        inherit BridgeWindow(500, 500)
        do this.DrawRect(20, 20, 460, 450)
    end

type SmallWindow() as this =
    class 
        inherit BridgeWindow(100, 100)
        do this.DrawRect(20, 20, 60, 50)        
    end
    


