open System.Net
open Microsoft.FSharp.Control.WebExtensions
open System.IO
open System.Text.RegularExpressions


let url' = "http://en.wikipedia.org/wiki/Bit"
let uri = new System.Uri(url')
let webClient = new WebClient()
let html = webClient.DownloadString(url')

let link = new Regex("href\s*=\s*\"(?<addr>.+?\/?)\"")

let links = [for m in link.Matches(html) -> m.Result("${addr}")] //|> List.iter (printfn "%s")

let process_link (link : string) = 
    if link.StartsWith("http") then link
    else if link.StartsWith("/") then url' + link.Substring(1)
         else ""

let check_site (link : string) =
    try 
        webClient.DownloadString(link) |> ignore
        true
    with
    | ex -> false

links 
    |> List.map process_link
    |> List.filter (fun l -> l <> "")
    |> List.filter check_site
    |> List.iter (fun l -> printfn "[%b] %s" (check_site l) l)


