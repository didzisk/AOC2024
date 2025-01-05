// Learn more about F# at http://fsharp.org

open System


(*
[<STAThread>]
do
    Application.Run(mainForm)
*)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let f = GridForm.mf

    let mutable x=0-60

    while x<60 do
    (*
        GridForm.rr f 5 300 300 System.Drawing.Color.Red x x
        GridForm.rr f 5 300 305 System.Drawing.Color.Orange x x
        GridForm.rr f 5 300 310 System.Drawing.Color.Yellow x x
        GridForm.rr f 5 300 315 System.Drawing.Color.Green x x
        GridForm.rr f 5 300 320 System.Drawing.Color.Blue x x
        GridForm.rr f 5 300 325 System.Drawing.Color.Indigo x x
        GridForm.rr f 5 300 330 System.Drawing.Color.Violet x x
        *)
        //GridForm.rainbowPoint f 5 300 300 x x
        x<-x+1
        //(Console.ReadLine() |> int)
    for deg = 0 to 1800 do
        let rad = float deg / 1800.0 * Math.PI
        GridForm.rainbowPoint f 5 300 300 50 rad

    Console.ReadLine() |> ignore
    

    0 // return an integer exit code

(*
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Visualizer
{
    static class Program
    {
        /// <summary>
        ///  The main entry point for the application.
        /// </summary>
        [<STAThread>]
        static void Main()
        {
            Application.SetHighDpiMode(HighDpiMode.SystemAware);
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
        }
    }
}
*)