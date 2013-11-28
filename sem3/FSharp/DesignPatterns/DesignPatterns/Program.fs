
open System

open PatternBridge

[<STAThread>]
do
#if DEBUG
    let bigWindow = new BigWindow()
    bigWindow.Show()
#else
    let smallWindow = new SmallWindow()
    smallWindow.Show()
#endif
