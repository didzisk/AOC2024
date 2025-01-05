module GridForm

open System.Drawing
open System.Windows.Forms
open System

let mf=
    let mainForm = new Form(Width = 620, Height = 620, Text = "Visualizer")
    mainForm

let pp (mainForm:Form) =

    mainForm.Show()

    let myBrush = new System.Drawing.SolidBrush(System.Drawing.Color.Red);
    let formGraphics=mainForm.CreateGraphics();
    formGraphics.FillRectangle(myBrush, new Rectangle(0, 0, 200, 300));
    myBrush.Dispose();
    formGraphics.Dispose();
    mainForm

let rr (mainForm:Form) scale offsetX offsetY color x y =

    let localX = offsetX+x*scale
    let localY= offsetY-y*scale

    mainForm.Show()

    let myBrush = new System.Drawing.SolidBrush(color);
    let formGraphics=mainForm.CreateGraphics();
    formGraphics.FillRectangle(myBrush, new Rectangle(localX, localY, scale, scale));
    myBrush.Dispose();
    formGraphics.Dispose();
    ()

let SmallPoint (mainForm:Form) (scale:float) offsetX offsetY color x y =

    let pointSize = 
        if scale < 1.0 then
            1
        else
            int (Math.Floor scale)
    let localX = int (offsetX+x*scale)
    let localY= int (offsetY-y*scale)

    mainForm.Show()

    let myBrush = new System.Drawing.SolidBrush(color);
    let formGraphics=mainForm.CreateGraphics();
    formGraphics.FillRectangle(myBrush, new Rectangle(localX, localY, pointSize, pointSize));
    myBrush.Dispose();
    formGraphics.Dispose();
    ()
    
let PointRC (mainForm:Form) (scale:float) color x y =
    SmallPoint mainForm scale 0.0 0.0 color y -x

let SmallBall (mainForm:Form) (scale:float) offsetX offsetY color x y =

    let pointSize = 
        if scale < 1.0 then
            1
        else
            int (Math.Floor scale)
    let localX = int (offsetX+x*scale)
    let localY= int (offsetY-y*scale)

    mainForm.Show()

    let myBrush = new System.Drawing.SolidBrush(color);
    let formGraphics=mainForm.CreateGraphics();
    formGraphics.FillEllipse (myBrush, new Rectangle(localX, localY, pointSize, pointSize));
    myBrush.Dispose();
    formGraphics.Dispose();
    ()


let rainbowPoint (mainForm:Form) scale offsetX offsetY r angle =

    mainForm.Show()

    let formGraphics=mainForm.CreateGraphics();
    let rColors=    [
        System.Drawing.Color.Red;
        System.Drawing.Color.Orange;
        System.Drawing.Color.Yellow;
        System.Drawing.Color.Green;
        System.Drawing.Color.Blue;
        System.Drawing.Color.Indigo;
        System.Drawing.Color.Violet;]
    rColors
    |> List.indexed
    |> List.iter (fun (i, col)->
            let localX=offsetX+scale*(int ((50.0+float i)*Math.Cos angle))
            let localY=offsetY-scale*(int ((50.0+float i)*Math.Sin angle))
            let myBrush = new System.Drawing.SolidBrush(col);
            formGraphics.FillRectangle(myBrush, new Rectangle(localX, localY, scale, scale));
            myBrush.Dispose();
        )
    formGraphics.Dispose();
    ()