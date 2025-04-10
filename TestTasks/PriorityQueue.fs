module PriorityQueue

type PriorityQueueItem<'T> = int * 'T

type PriorityQueue<'T>() =
    let mutable queue = []

    member this.Enqueue(priority, value) =
        let newItem = (priority, value)
        let rec insert list =
            match list with
            | [] -> [newItem]
            | ((p, _) as head) :: tail when p <= priority -> head :: insert tail
            | _ -> newItem :: list
        queue <- insert queue

    member this.Dequeue() = 
        match queue with
        | [] -> failwith "The queue was empty"
        | head :: tail -> 
            queue <- tail
            head
