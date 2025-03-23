open System

open PhoneBook

let info = """Choose an action to continue:
        0 - exit
        1 - add entry
        2 - find phone number by name
        3 - find name by phone number
        4 - print all entries
        5 - save entries to file
        6 - import entries from file
    """

let rec waitForEnter () =
    let keyInfo = Console.ReadKey(true)
    if keyInfo.Key <> ConsoleKey.Enter then
        waitForEnter()

let rec programLoop phoneBook = 
    Console.Clear()
    printfn "%s" info

    let keyInfo = Console.ReadKey()
    waitForEnter ()
    match keyInfo.Key with
    | ConsoleKey.D0 -> ()
    | ConsoleKey.D1 -> 
        printfn "\nName: "
        let name = Console.ReadLine()

        printfn "\nPhone number: "
        let phoneNumber = Console.ReadLine()

        let newPhoneBook = addEntry name phoneNumber phoneBook
        if newPhoneBook <> phoneBook then
            printfn "The entry has been added"
        else
            printfn "The entry with such name or phone number already exists"

        reenterLoop newPhoneBook
    | ConsoleKey.D2 -> 
        printfn "\nName: "
        let name = Console.ReadLine()

        match findNumberByName name phoneBook with
        | Some phoneNumber ->
            printfn "\n%s's phone number is %s" name phoneNumber
        | None -> 
            printfn "\nNo entry with such name was found"

        reenterLoop phoneBook
    | ConsoleKey.D3 -> 
        printfn "\nPhone number: "
        let phoneNumber = Console.ReadLine()

        match findNameByNumber phoneNumber phoneBook with
        | Some name ->
            printfn "\n%s is %s's phone number" phoneNumber name
        | None -> 
            printfn "\nNo entry with such phone number was found"

        reenterLoop phoneBook
    | ConsoleKey.D4 -> 
        printfn "\n"
        printPhoneBook phoneBook

        reenterLoop phoneBook
    | ConsoleKey.D5 -> 
        printfn "\nFile path to save entries to:"
        let filePath = Console.ReadLine()
        writePhoneBookToFile filePath phoneBook 
        printfn "\nAll entries have been saved to %s" filePath

        reenterLoop phoneBook
    | ConsoleKey.D6 -> 
        printfn "\nFile path to import entries from:"
        let filePath = Console.ReadLine()
        let newPhoneBook = readPhoneBookFromFile filePath phoneBook
        printfn "\nAll entries have been imported from %s" filePath

        reenterLoop newPhoneBook
    | _ -> printfn "\nUnsupported action"

and reenterLoop phoneBook =
    printfn "\nPress Enter to continue..."
    waitForEnter ()
    programLoop phoneBook

programLoop []
