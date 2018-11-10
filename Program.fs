open System
#light
// Language prediction based on letter frequencies
//
// This program analyzes text from various languages, and computes
// letter frequencies.  These letter frequencies serve as a "barcode"
// that potentially identify the language when written.  This approach,
// and assignment, is inspired by the students and professor of CS 141,
// Fall 2018, at the U. of Illinois, Chicago.  Kudos to Prof Reed.
//


//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
// I modified this to make all the characters lowercase
let explode s =
  [for c in s -> System.Char.ToLowerInvariant(c)]


//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string.
//
let implode L =
  let sb = System.Text.StringBuilder()
  List.iter (fun c -> ignore (sb.Append (c:char))) L
  sb.ToString()


//
// FileInput:
//
// Inputs text from a file line by line, returning this as a list
// of strings.  Each line is converted to lower-case.
//
let FileInput filename = 
  [ for line in System.IO.File.ReadLines(filename) -> line.ToLower() ]


//
// UserInput:
//
// This function reads from the keyboard, line by line, until 
// # appears on a line by itself.  The lines are returned as
// a list of strings; each line is converted to lower-case.
//
// NOTE: if the first line of input is blank (i.e. the user 
// presses ENTER), then input is read from the file 'input.txt'.
// Helpful when testing.
//
let rec _UserInput input =
  let line = System.Console.ReadLine()
  match line with
  | "#" -> List.rev input
  |  _  -> _UserInput (line.ToLower() :: input)

let UserInput() =
  let firstLine = System.Console.ReadLine()
  match firstLine with
  | "" -> FileInput @"./input.txt"
  | _  -> _UserInput [firstLine.ToLower()]
  
//Function recursively goes through the text and returns the number of occurences a given letter has. 
let rec returnFreq alphabet trainer =
  match trainer with 
  | head::tail ->
      if head = alphabet then
        1 + returnFreq alphabet tail 
      else
        returnFreq alphabet tail 
  | _ -> 0

//Function that recursively goes through the Frequency sorted alphabet and returns a list with tuples of [(occurences, letter)]
let rec findFreq alphabet trainer =
  match alphabet with
  | head::tail ->
    ((returnFreq head trainer), head)::(findFreq tail trainer)
  | _ -> []
  
  
//***********************************************************************//
//
// Main:
//
[<EntryPoint>]
let main argv =
  printfn "** Training... **"
  printfn ""
  
  // Find a way to automate the list making, this will not work
  // You cannot expect or assume any of the training files, they can and will differ.
  // 
  // Created a list of used languages, as well as a list of used characters...Standard English Characters a-z
  let Languages = ["english"; "finnish"; "french"; "german"; "hungarian"; "italian"; "portuguese"; "spanish"]
  // These are the only characters we are checking for
  let alphabet  = ['a' .. 'z']
  
  let files = [ for filename in System.IO.Directory.GetFiles(@"./training") -> filename]
  
  // For each of the training files, read in one string and create a list of characters using the explode function
  let englishTrainer    = explode (IO.File.ReadAllText (@"./training/MacbethEnglish.txt"   ))  
  let finnishTrainer    = explode (IO.File.ReadAllText (@"./training/MacbethFinnish.txt"   ))
  let frenchTrainer     = explode (IO.File.ReadAllText (@"./training/MacbethFrench.txt"    ))
  let germanTrainer     = explode (IO.File.ReadAllText (@"./training/MacbethGerman.txt"    )) 
  let hungarianTrainer  = explode (IO.File.ReadAllText (@"./training/MacbethHungarian.txt" ))
  let italianTrainer    = explode (IO.File.ReadAllText (@"./training/MacbethItalian.txt"   ))
  let portugueseTrainer = explode (IO.File.ReadAllText (@"./training/MacbethPortuguese.txt"))
  let spanishTrainer    = explode (IO.File.ReadAllText (@"./training/MacbethSpanish.txt"   ))
  
  // Get frequency in the tuple form of [(frequency, letter)] 
  // F# allows you to skip to a different start points of a list
  // I took advantage of this feature so I can skip the names of the languages
  // I also sent the alphabet list so I can filter out any non standard letters
  let englishList    = (findFreq alphabet englishTrainer.[7..]    ) 
  let finnishList    = (findFreq alphabet finnishTrainer.[7..]    )
  let frenchList     = (findFreq alphabet frenchTrainer.[6..]     )
  let germanList     = (findFreq alphabet germanTrainer.[6..]     )
  let hungarianList  = (findFreq alphabet hungarianTrainer.[9..]  )
  let italianList    = (findFreq alphabet italianTrainer.[7..]    )
  let portugueseList = (findFreq alphabet portugueseTrainer.[10..])
  let spanishList    = (findFreq alphabet spanishTrainer.[7..]    )
  
  // Like the last step, but instead we are sorting by tuple with HIGHEST frequency
  // I could have modified the already made lists but professor H, doesn't like mutable lists.
  let sortEnglishList    = List.sortDescending (findFreq alphabet englishTrainer.[7..]    ) 
  let sortFinnishList    = List.sortDescending (findFreq alphabet finnishTrainer.[7..]    )
  let sortFrenchList     = List.sortDescending (findFreq alphabet frenchTrainer.[6..]     )
  let sortGermanList     = List.sortDescending (findFreq alphabet germanTrainer.[6..]     )
  let sortHungarianList  = List.sortDescending (findFreq alphabet hungarianTrainer.[9..]  )
  let sortItalianList    = List.sortDescending (findFreq alphabet italianTrainer.[7..]    )
  let sortPortugueseList = List.sortDescending (findFreq alphabet portugueseTrainer.[10..])
  let sortSpanishList    = List.sortDescending (findFreq alphabet spanishTrainer.[7..]    )
  
  // Now we can start to print out the frequency counts
  // We can access the information of the list of tuples 
  // Using List.map, fst for first, and snd for second
  printfn "** Letter Frequency Counts (A->Z) **"
  // Print English letter frequency
  let t1 = englishList |> List.map fst
  printf "%A" "english" 
  printf ": "
  t1 |> List.iter (fun x -> printf "%d " x)
  printfn ""
  //Print Finnish letter frequency
  let t2 = finnishList |> List.map fst
  printf "%A" "finnish" 
  printf ": "
  t2 |> List.iter (fun x -> printf "%d " x)
  printfn ""
  // Print French letter frequency
  let t3 = frenchList |> List.map fst
  printf "%A" "french" 
  printf ": "
  t3 |> List.iter (fun x -> printf "%d " x)
  printfn ""
  // Print German letter frequency
  let t4 = germanList |> List.map fst
  printf "%A" "german" 
  printf ": "
  t4 |> List.iter (fun x -> printf "%d " x)
  printfn ""
  // Print Hungarian letter frequency
  let t5 = hungarianList |> List.map fst
  printf "%A" "hungarian" 
  printf ": "
  t5 |> List.iter (fun x -> printf "%d " x)
  printfn ""
  // Print Italian letter frequency
  let t6 = italianList |> List.map fst
  printf "%A" "italian" 
  printf ": "
  t6 |> List.iter (fun x -> printf "%d " x)
  printfn ""
  // Print Portuguese letter frequency
  let t7 = portugueseList |> List.map fst
  printf "%A" "portuguese" 
  printf ": "
  t7 |> List.iter (fun x -> printf "%d " x)
  printfn ""
  // Print Spanish letter frequency
  let t8 = spanishList |> List.map fst
  printf "%A" "spanish" 
  printf ": "
  t8 |> List.iter (fun x -> printf "%d " x)
  printfn ""
  printfn ""
  
  printfn "** Letter Frequency Order (High->Low) **"
  // Print English letter frequency order
  let t9 = sortEnglishList |> List.map snd
  printf "%A" "english" 
  printf ": "
  t9 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  // Print Finnish letter frequency order
  let t10 = sortFinnishList |> List.map snd
  printf "%A" "finnish" 
  printf ": "
  t10 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  // Print French letter frequency order
  let t11 = sortFrenchList |> List.map snd
  printf "%A" "french" 
  printf ": "
  t11 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  // Print German letter frequency order
  let t12 = sortGermanList |> List.map snd
  printf "%A" "german" 
  printf ": "
  t12 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  // Print Hungarian letter frequency order
  let t13 = sortHungarianList |> List.map snd
  printf "%A" "hungarian" 
  printf ": "
  t13 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  // Print Italian letter frequency order
  let t14 = sortItalianList |> List.map snd
  printf "%A" "italian" 
  printf ": "
  t14 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  // Print Portuguese letter frequency order
  let t15 = sortPortugueseList |> List.map snd
  printf "%A" "portuguese" 
  printf ": "
  t15 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  // Print Spanish letter frequency order
  let t16 = sortSpanishList |> List.map snd
  printf "%A" "spanish" 
  printf ": "
  t16 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  printfn ""
  //
  // Here we get text from the user, analyze, and guess the language:
  //
  printfn "Please enter text, followed by # (default = 'input.txt')> "
  let defaultTrainer  = explode (IO.File.ReadAllText (@"./input.txt"   ))
  let defaultList     = (findFreq alphabet defaultTrainer    ) 
  let defaultListSort = List.sortDescending (findFreq alphabet defaultTrainer )
  
  printfn ""
  //
  printf "Enter difference threshold (default = 4)> "
  let s = System.Console.ReadLine()
  let threshold = if s = "" then 4 else int(s)
  printfn ""
  //
  //
  //

  let t17 = defaultListSort |> List.map snd
  printf "%A" "input" 
  printf ": "
  t17 |> List.iter (fun x -> printf "%c" x)
  printfn ""
  printfn ""
  
  
  let prediction = "?"
  printfn "** Input language: %A" prediction
  printfn ""
  //
  0