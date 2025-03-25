module LocalNetwork.Tests

open NUnit.Framework
open FsUnit

open LocalNetwork

type MockRandom(values: float list) =
    let mutable queue = System.Collections.Generic.Queue(values)
    interface IRandomProvider with
        member _.NextDouble() = 
            if queue.Count > 0 then queue.Dequeue()
            else failwith "No more values"

[<Test>]
let ``Infection should spread through chain with deterministic probabilities`` () =
    let computers = [|
        Computer(0, Windows, true)
        Computer(1, Linux, false)
        Computer(2, MacOS, false)
    |]
    let adjacency = array2D [| 
                                [| false; true; false |]; 
                                [| true; false; true |]; 
                                [| false; true; false |] 
                            |]
    let probabilities = { Windows = 0.9; Linux = 0.5; MacOS = 0.1 }

    let mockRandom = MockRandom([0.4; 0.05]) :> IRandomProvider
    
    let network = LocalNetwork(computers, probabilities, adjacency, mockRandom)

    network.Step() |> should be True
    computers.[1].IsInfected |> should be True
    computers.[2].IsInfected |> should be False
    
    network.Step() |> should be True
    computers.[2].IsInfected |> should be True
    
    network.Step() |> should be False

[<Test>]
let ``Should partially infect network based on OS vulnerabilities`` () =
    let computers = [|
        Computer(0, Windows, true)
        Computer(1, Linux, false)
        Computer(2, MacOS, false)
    |]
    let adjacency = array2D [| 
                                [| false; true; true |]; 
                                [| true; false; false |]; 
                                [| true; false; false |] 
                            |]
    let probabilities = { Windows = 0.9; Linux = 0.1; MacOS = 0.8 }

    let mockRandom = MockRandom([0.15; 0.05]) :> IRandomProvider
    
    let network = LocalNetwork(computers, probabilities, adjacency, mockRandom)

    network.Step() |> should be True

    computers.[1].IsInfected |> should be False
    computers.[2].IsInfected |> should be True

[<Test>]
let ``Should stop when no more possible infections despite random values`` () =
    let computers = [| Computer(0, Windows, true) |]
    let adjacency = array2D [| [| false |] |]
    let mockRandom = MockRandom([0.1; 0.2; 0.3]) :> IRandomProvider
    
    let network = LocalNetwork(computers, { Windows = 0.5; Linux = 0.5; MacOS = 0.5 }, adjacency, mockRandom)

    let result = network.Step()

    result |> should be False

[<Test>]
let ``Should return false when all computers are infected`` () =
    let computers = [| Computer(0, Windows, true); Computer(1, Linux, true) |]
    let adjacency = array2D [| [| false; true |]; [| true; false |] |]
    let mockRandom = MockRandom([]) :> IRandomProvider
    
    let network = LocalNetwork(computers, { Windows = 1.0; Linux = 1.0; MacOS = 1.0 }, adjacency, mockRandom)

    network.Step() |> should be False

[<Test>]
let ``Should never infect with zero probabilities regardless of random values`` () =
    let computers = [| Computer(0, Windows, true); Computer(1, Linux, false) |]
    let adjacency = array2D [| [| false; true |]; [| true; false |] |]
    let mockRandom = MockRandom([0.0; 0.0]) :> IRandomProvider
    
    let network = LocalNetwork(computers, { Windows = 0.0; Linux = 0.0; MacOS = 0.0 }, adjacency, mockRandom)

    network.Step() |> should be False
    computers.[1].IsInfected |> should be False

[<Test>]
let ``Virus should spread like BFS when probability is 1`` () =
    let computers = [|
        Computer(0, Windows, true)
        Computer(1, Linux, false)
        Computer(2, MacOS, false)
        Computer(3, Windows, false)
        Computer(4, Linux, false)
    |]

    let adjacency = array2D [|
        [| false; true; false; true; false |]
        [| true; false; true; false; false |]
        [| false; true; false; true; false |]
        [| true; false; true; false; true  |]
        [| false; false; false; true; false |]
    |]

    let probabilities = { Windows = 1.0; Linux = 1.0; MacOS = 1.0 }

    let mockRandom = MockRandom(List.replicate 10 0.0) :> IRandomProvider
    
    let network = LocalNetwork(computers, probabilities, adjacency, mockRandom)

    network.Step() |> should be True
    computers.[1].IsInfected |> should be True
    computers.[3].IsInfected |> should be True
    computers.[2].IsInfected |> should be False
    computers.[4].IsInfected |> should be False

    network.Step() |> should be True
    computers.[2].IsInfected |> should be True
    computers.[4].IsInfected |> should be True

    network.Step() |> should be False

    computers |> Array.forall (fun c -> c.IsInfected) |> should be True
