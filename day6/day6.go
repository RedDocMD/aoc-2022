package main

import (
	"fmt"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "Expected: <filename>")
		os.Exit(1)
	}
	filename := os.Args[1]
	contentBytes, err := os.ReadFile(filename)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	content := string(contentBytes)
    markerOff := findMarker(content, 4)
    fmt.Println("Marker offset:", markerOff)
    messageOff := findMarker(content, 14)
    fmt.Println("Message offset:", messageOff)
}

func findMarker(content string, cnt int) int {
	cntr := make(map[byte]int)
	if len(content) < cnt {
		fmt.Fprintln(os.Stderr, "Too short string:", content)
		os.Exit(1)
	}
    uniqCnt := 0
    markerOff := 0
	for i := 0; i < cnt; i++ {
        bt := content[i]
        if _, ok := cntr[bt]; !ok {
            uniqCnt += 1
            cntr[bt] = 1
        } else {
            cntr[bt] += 1
        }
        markerOff += 1
	}
    for i := cnt; i < len(content); i++ {
        if uniqCnt == cnt {
            break
        }
        rem := content[i - cnt]
        if cntr[rem] == 1 {
            delete(cntr, rem)
            uniqCnt -= 1
        } else {
            cntr[rem] -= 1
        }
        add := content[i]
        if _, ok := cntr[add]; !ok {
            uniqCnt += 1
            cntr[add] = 1
        } else {
            cntr[add] += 1
        }
        markerOff += 1
    }
    return markerOff
}
