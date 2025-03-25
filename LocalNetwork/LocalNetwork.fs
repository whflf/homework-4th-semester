module LocalNetwork

type OS = Windows | Linux | MacOS

type Probabilities = { 
    Windows: float;
    Linux: float;
    MacOS: float; 
}
 type Computer(id: int, os: OS, isInfected: bool) =
    member this.Id = id
    member this.OS = os
    member val IsInfected = isInfected with get, set

type IRandomProvider =
    abstract NextDouble: unit -> float

type DefaultRandom() =
    interface IRandomProvider with
        member _.NextDouble() = System.Random().NextDouble()

type LocalNetwork(computers: Computer[], probabilities: Probabilities, adjacencyMatrix: bool[,], random: IRandomProvider) =
    
    member this.Computers = computers

    member this.Step() =
        let mutable anyPossibleInfection = false
        let newInfections = ResizeArray<int>()
        
        for i in 0 .. computers.Length - 1 do
            if computers.[i].IsInfected then
                for j in 0 .. computers.Length - 1 do
                    if adjacencyMatrix.[i, j] && not computers.[j].IsInfected then
                        let infectionProbability =
                            match computers.[j].OS with
                            | Windows -> probabilities.Windows
                            | Linux -> probabilities.Linux
                            | MacOS -> probabilities.MacOS
                        
                        if infectionProbability <> 0 then
                            anyPossibleInfection <- true

                        if random.NextDouble() < infectionProbability then
                            newInfections.Add(j)
        
        newInfections |> Seq.iter (fun idx -> computers.[idx].IsInfected <- true)
        anyPossibleInfection

    member this.Simulate() =
        let mutable step = 0
        let mutable isInfectionPossible = true
        
        while isInfectionPossible do
            printfn $" Step {step}:\n{this.GetState()}"
            isInfectionPossible <- this.Step()
            step <- step + 1

    member private this.GetState() =
        let infected = "Infected"
        let clean = "Clean"
        computers 
        |> Array.map (fun c -> $"Computer {c.Id} ({c.OS}): {if c.IsInfected then infected else clean}")
        |> String.concat "\n"
