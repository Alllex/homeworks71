open System.Net
open Microsoft.FSharp.Control.WebExtensions
open System.IO
open System.Text.RegularExpressions

let printAll = List.iter (printfn "[%s]")

let rmDup list = List.foldBack (fun x acc -> if List.tryFind ((=) x) acc <> None then acc else x::acc) list []

let linkRegex = new Regex("\"(?<addr>[^\s\"]+?)\"") 
let picRegex = Regex("(\"|\')(?<pic>[^\"\?=]+\.(png|jpe?g))(\'|\"|\?|=)+?")

let tillLastSlash (link : string) =
    link.Substring(0,link.LastIndexOf('/') + 1)

let processLink (dom : string) (link : string) = 
    if link.StartsWith("http") then link
    else if link.StartsWith("//") then "http:" + link
         else if dom.EndsWith("/") then
                if link.StartsWith("/") then dom + link.Substring(1) else dom + link
              else tillLastSlash dom + link

let folder = "pics"                         
if not <| Directory.Exists(folder) then Directory.CreateDirectory(folder) |> ignore

let localName name = 
    let rightChar c = (List.tryFind ((=) c) ['/';'\\';':';'?';'\"';'*';'<';'>']) <> None
    folder + "\\" +  (String.map (fun c -> if rightChar c then '_' else c) name)

let download link =
  async { 
    try
        let wc = new WebClient()
        wc.DownloadFile(new System.Uri(link), localName link) 
    with | ex -> printfn "%s" ex.Message
  }

let maxSize = 5000L * 1024L
let minSize = 500L * 1024L

let sizePredicate (url : string) = 
  try
    let wr = WebRequest.Create(url)
    let wres = wr.GetResponse()
    let size = wres.ContentLength
    wres.Close()
    size > minSize && size < maxSize
  with | ex -> false

(*
let downloadPicsTask (link : string) =
  async {
    try  
        printfn "Downloading from [%s]" link
        let webClient = new WebClient()
        let! html = webClient.AsyncDownloadString(new System.Uri(link))
        let pics = [for m in picRegex.Matches(html) -> m.Result("${pic}")]
                    |> List.map (processLink link)
                    |> rmDup
        //printAll pics
        pics
                    |> List.filter sizePredicate
                    |> Seq.map download
                    |> Async.Parallel 
                    |> Async.RunSynchronously
                    |> ignore
        ()
    with
        | ex -> ()
  }
*)

let denieded = [".css"; ".js"; ".ico"; ".jpg"; ".png"]

let filterSome = List.filter (fun (l:string) -> List.fold (&&) true <| List.map (fun d -> not (l.Contains(d))) denieded)

let onlyDeeper (dom : string) (link : string) =
  try
    let a = dom.IndexOf('/') + 2
    let b = dom.Substring(a).IndexOf('/') + a
    link.StartsWith(dom.Substring(0, b))
  with | ex -> false

let findLinks link =  
    try 
        let webClient = new WebClient()
        let html = webClient.DownloadString(new System.Uri(link))

        let links = [for m in linkRegex.Matches(html) -> m.Result("${addr}")]
                        |> List.map (processLink link)
                        //|> List.filter (onlyDeeper link)
                        |> List.filter (fun (l:string) -> l.StartsWith(link))
                        |> List.filter ((<>) link)
                        |> filterSome

        let pics = [for m in picRegex.Matches(html) -> m.Result("${pic}")]
                        |> List.map (processLink link)

        (links, pics)
    with 
        | ex -> ([],[])


let startUrl = "http://imgsrc.hubblesite.org/"
let startUrl' = "http://photography.nationalgeographic.com/photography/photos/extreme-earth/"

//let checked' = new ResizeArray<_>()

let rec picsLinks (url:string) = 
  async {
    printfn "New link:[%s]" url
    //checked'.Add(url)
    let (links, pics) = findLinks url
    let ls = links |> rmDup //|> List.filter (fun l -> not <| checked'.Contains l)
    pics
        |> rmDup 
        |> List.filter sizePredicate
        |> Seq.map download
        |> Async.Parallel 
        |> Async.StartAsTask
        |> ignore

    ls  |> Seq.map picsLinks
        |> Async.Parallel 
        |> Async.StartAsTask
        |> System.Threading.Tasks.Task.WaitAll
  }

let picsList = picsLinks startUrl'

Async.RunSynchronously picsList

(*
printfn "Pic list:"
printAll picsList*)
printfn "Start waiting..."
