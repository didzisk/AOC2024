module ArrayUtils

let NW (arr:'a array array) row col =
    arr[row-1][col-1]
    
let NE (arr:'a array array) row col =
    arr[row-1][col+1]
    
let SW (arr:'a array array) row col =
    arr[row+1][col-1]
    
let SE (arr:'a array array) row col =
    arr[row+1][col+1]
    
let stringToLines (s:string) =
    s.Split("\r\n")
    
let findWhere predicate (arr:'a array array) =
    [
    for r in 0..arr.Length-1 do
        for c in 0..arr[r].Length-1 do
            if predicate(arr[r][c]) then
                yield (r,c)
                ]
    |> List.head

let validPos (world:'a array array) (r,c) =     
    let validRow = r>=0 && r<world.Length
    let validCol = c>=0 && c<world[0].Length
    validRow && validCol