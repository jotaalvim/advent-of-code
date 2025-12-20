package main

import "fmt"
import "os"
import "strings"
import "strconv"

func FloorDiv(a int, b int) int {
    q := a / b
    if a % b < 0 {
        q--
    }
    return q
}

type Rotation int 

func parse ( path string) [] Rotation {
    i, err  := os.ReadFile ( path )

    if (err != nil) {
        fmt.Print("ERROR:",err)
        return nil
    }

    lines := strings.Split ( string(i) , "\n" )

    result := [] Rotation {}

    var newV int
    for _,v:= range lines {
        if (v != "") {

            if v[0] == 'L' {
                newV, _ = strconv.Atoi("-"+v[1:])
            }
            if v[0] == 'R' {
                newV, _ = strconv.Atoi(    v[1:])
            }
            result = append(result, Rotation( newV ) )
            // := -> create
            // =  -> update
        }
    }
    return result
}

func countsZero ( listR [] Rotation ) int {
    total := 0
    cur   := 50
    for _,v := range listR {
        cur = (cur + int(v)) % 100
        if (cur == 0) {
            total = total + 1
        }
    }
    return total
}

func countsZeroChanges ( listR [] Rotation ) int {
    cur    := 50
    total  := 0
    before := cur // stores the last value 
    laps   := 0

    for _,v := range listR {
        cur  = (((before + int(v)) % 100) + 100) % 100

        laps = FloorDiv( before + int(v) , 100)
        if laps < 0 { laps = -laps }

        if ( int(v) < 0 && before == 0 ) { // L
            laps = laps - 1
        }
        if ( int(v) > 0 && cur == 0 ) { // R
            laps = laps - 1
        }
        total = total + laps
        before = cur
    }
    return total
}

func main () {
    //path   := "inputExample.txt"
    path   := "input.txt"
    result := parse (path)

    part1  := countsZero(result)
    part2  := countsZeroChanges(result)

    fmt.Println("part 1:",part1)
    fmt.Println("part 2:",part2 + part1)
}
