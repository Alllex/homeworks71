open System
open System.Drawing
open System.Windows.Forms


let formW = 300
let formH = 300

type Orientation = Up | Down | Left | Right

let rand = new Random()
let randRotation() = 
    let t = rand.Next(100)
    if t > 75 then Left else if t > 50 then Up else if t > 25 then Right else Down

type Magnet(left : int, top : int, w : int, h : int, direction : Orientation) as this = 
    class
        inherit Control("", left, top, int (Math.Sqrt(float <| w*w + h*h)), int (Math.Sqrt(float <| w*w + h*h)))

        let W = int (Math.Sqrt(float <| w*w + h*h))
        let H = W
        let (centerX,centerY) = (W/2, H/2)
        let (w,h) = (w,h)
        let (halfW,halfH) = (w / 2, h / 2)
        let mutable dir = direction

        let angleFromDirection(dir' : Orientation) =
            match dir' with
            | Left  -> 0.0f
            | Up    -> 90.0f
            | Right -> 180.0f
            | Down  -> 270.0f

        let mutable angle = angleFromDirection(dir)

        let d'a = 1.0F

        let ERotateVert = new Event<_>()
        let ERotateHoriz = new Event<_>()

        
        do this.Paint.Add(fun e -> this.draw(e.Graphics))  
           this.MouseClick.Add(fun (e : MouseEventArgs) -> 
                match e.Button with
                | MouseButtons.Left -> this.rotateClockwise()
                | MouseButtons.Right -> this.rotateCounterClockwise()
                | _ -> ()
           )
           this.MouseDoubleClick.Add(fun (e : MouseEventArgs) -> 
               match e.Button with
               | MouseButtons.Left -> this.rotateClockwise()
               | MouseButtons.Right -> this.rotateCounterClockwise()
               | _ -> ()
           )
           this.Visible <- true
           this.DoubleBuffered <- true
          (* 
        let update = 
            let timer = new Timers.Timer(
                                            Interval = 40.0, 
                                            Enabled  = true                              
                                        )
            timer.Elapsed.Add(fun _ -> angle <- angle + d'a; this.Invalidate())
            *)


        new(left : int, top : int, w : int, h : int) = new Magnet(left, top, w, h, randRotation())

        member private x.updateAngle() =
            angle <- angleFromDirection(dir)
            x.Invalidate()

        member x.getDirect() = dir

        member x.getSide() = H

        member x.rotateClockwise() = 
            dir <- match dir with
                   | Up -> Right
                   | Right -> Down
                   | Down -> Left
                   | Left -> Up
            x.updateAngle()

        member x.rotateCounterClockwise() = 
            dir <- match dir with
                   | Up -> Left
                   | Left -> Down
                   | Down -> Right
                   | Right -> Up
            x.updateAngle()

        member private x.draw(g : Graphics) = 
            g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
            g.TranslateTransform(float32 centerX,float32 centerY)
            g.RotateTransform(angle)
            g.FillRectangle(Brushes.Red, -halfW, -halfH, halfW, h)
            g.FillRectangle(Brushes.Blue, 0, -halfH, halfW, h)

        [<CLIEvent>]
        member public x.RotateVert = ERotateVert.Publish
        [<CLIEvent>]
        member public x.RotateHoriz = ERotateHoriz.Publish

    end

let (mw, mh) = (100, 50)
let side = int (Math.Sqrt(float <| mw*mw + mh*mh))
let (rows,cols) = (3,4)

let magnets = Array2D.init cols rows (fun i j -> new Magnet(i * side, j * side, mw, mh))

let form = new Form(Text="Magnets",AutoSize=true)

Array2D.iteri (fun i j m -> 
                form.Controls.Add(m)
                for ii in [-1,1] do 
                    for jj in [-1,1] do
                    try
                        () // react on neigbours rotating
                    with | _ -> ()
) magnets

[<STAThreadAttribute>]
Application.Run(form)