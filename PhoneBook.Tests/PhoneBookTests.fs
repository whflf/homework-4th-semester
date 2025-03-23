module PhoneBook.Tests

open System.IO
open NUnit.Framework
open FsUnit
open FsCheck.NUnit

open PhoneBook

let getPhoneBook () = [
    { Name = "Alice"; PhoneNumber = "123" }
    { Name = "Bob"; PhoneNumber = "456" }
]

[<Test>]
let ``addEntry should add new entry if it does not exist`` () =
    let newPhoneBook = addEntry "Charlie" "999" (getPhoneBook())
    newPhoneBook |> should haveLength 3
    newPhoneBook |> should contain { Name = "Charlie"; PhoneNumber = "999" }

[<Test>]
let ``findNumberByName should return phone number if name exists`` () =
    findNumberByName "Alice" (getPhoneBook()) |> should equal (Some "123")

[<Test>]
let ``findNameByNumber should return name if phone number exists`` () =
    findNameByNumber "123" (getPhoneBook()) |> should equal (Some "Alice")

[<Test>]
let ``writePhoneBookToFile and readPhoneBookFromFile should work correctly`` () =
    let filePath = Path.GetTempFileName()
    let phoneBook = (getPhoneBook())

    writePhoneBookToFile filePath phoneBook
    let readPhoneBook = readPhoneBookFromFile filePath []
    readPhoneBook |> should equal phoneBook
    
    File.Delete(filePath)

[<Property>]
let ``addEntry should increase the length of the phone book if entry does not exist`` 
    (name: string) (phoneNumber: string) (phoneBook: Entry list) =
    let newPhoneBook = addEntry name phoneNumber phoneBook
    if entryExists name phoneNumber phoneBook then
        newPhoneBook.Length = phoneBook.Length
    else
        newPhoneBook.Length = phoneBook.Length + 1

[<Property>]
let ``addEntry should not add duplicate entries`` 
    (name: string) (phoneNumber: string) (phoneBook: Entry list) =
    let phoneBookWithEntry = addEntry name phoneNumber phoneBook
    let newPhoneBook = addEntry name phoneNumber phoneBookWithEntry
    newPhoneBook.Length = phoneBookWithEntry.Length

[<Property>]
let ``findNumberByName should return the correct phone number if name exists`` 
    (name: string) (phoneNumber: string) (phoneBook: Entry list) =
    let newPhoneBook = addEntry name phoneNumber phoneBook
    if newPhoneBook <> phoneBook then
        findNumberByName name newPhoneBook = Some phoneNumber
    else
        findNumberByName name phoneBook <> None || findNameByNumber phoneNumber phoneBook <> None

[<Property>]
let ``findNumberByName should return None if name does not exist`` 
    (name: string) (phoneNumber: string) (phoneBook: Entry list) =
    let newPhoneBook = addEntry name phoneNumber phoneBook
    let nonExistentName = name + "_non_existent"
    findNumberByName nonExistentName newPhoneBook = None

[<Property>]
let ``findNameByNumber should return the correct name if phone number exists`` 
    (name: string) (phoneNumber: string) (phoneBook: Entry list) =
    let newPhoneBook = addEntry name phoneNumber phoneBook
    if newPhoneBook <> phoneBook then
        findNameByNumber phoneNumber newPhoneBook = Some name
    else
        findNameByNumber phoneNumber phoneBook <> None || findNumberByName name phoneBook <> None

[<Property>]
let ``findNameByNumber should return None if phone number does not exist`` 
    (name: string) (phoneNumber: string) (phoneBook: Entry list) =
    let newPhoneBook = addEntry name phoneNumber phoneBook
    let nonExistentNumber = phoneNumber + "_non_existent"
    findNameByNumber nonExistentNumber newPhoneBook = None
