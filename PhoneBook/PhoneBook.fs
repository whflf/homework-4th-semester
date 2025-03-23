module PhoneBook

open System.IO

type Entry =
    { Name: string;
      PhoneNumber: string; }

let entryExists name phoneNumber phoneBook = 
    List.exists (fun x -> x.Name = name || x.PhoneNumber = phoneNumber) phoneBook

let addEntry name phoneNumber phoneBook = 
    if entryExists name phoneNumber phoneBook then 
        phoneBook
    else
        let entry = { Name = name; PhoneNumber = phoneNumber }
        entry :: phoneBook

let rec findNumberByName name phoneBook =
    match phoneBook with
    | [] -> None
    | head :: tail -> if head.Name = name then Some head.PhoneNumber else findNumberByName name tail

let rec findNameByNumber number phoneBook =
    match phoneBook with
    | [] -> None
    | head :: tail -> if head.PhoneNumber = number then Some head.Name else findNameByNumber number tail

let rec printPhoneBook phoneBook =
    match phoneBook with
    | [] -> ()
    | head :: tail -> 
        printfn "%s — %s" head.Name head.PhoneNumber
        printPhoneBook tail

let rec writePhoneBookToFile (filePath: string) phoneBook =
    use writer = new StreamWriter(filePath)

    let rec writeLines phoneBook =
        match phoneBook with
        | [] -> ()
        | head :: tail -> 
            sprintf "%s %s" head.Name head.PhoneNumber |> writer.WriteLine
            writeLines tail

    writeLines phoneBook

let rec readPhoneBookFromFile (filePath: string) phoneBook =
    use reader = new StreamReader(filePath)

    let rec readLines phoneBook =
        let line = reader.ReadLine()

        match line with
        | null -> List.rev phoneBook
        | _ -> 
            let splitLine = line.Split(' ')
            let entry = { Name = splitLine[0]; PhoneNumber = splitLine[1] }
            let newPhoneBook = 
                if entryExists entry.Name entry.PhoneNumber phoneBook then 
                    phoneBook 
                else 
                    entry :: phoneBook
            readLines newPhoneBook

    readLines phoneBook
