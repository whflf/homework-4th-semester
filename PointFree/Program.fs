﻿module PointFree

let func x l = List.map (fun y -> y * x) l

let func'1 x l = List.map ((*) x) l

let func'2 x : int list -> int list = List.map ((*) x)

let func'3 : int -> int list -> int list = (*) >> List.map
