open LocalNetwork

let computers = [|
        Computer(0, OS.Windows, true)
        Computer(1, OS.Linux, false)
        Computer(2, OS.MacOS, false)
        Computer(3, OS.Windows, false)
    |]

let adjacencyMatrix = array2D [|
    [| false; true;  false; true |]
    [| true;  false; true;  false |]
    [| false; true;  false; false |]
    [| true;  false; false; false |]
|]

let probabilities = {
    Windows = 0.7
    Linux = 0.4
    MacOS = 0.3
}

let network = LocalNetwork(computers, probabilities, adjacencyMatrix, DefaultRandom())

network.Simulate()
