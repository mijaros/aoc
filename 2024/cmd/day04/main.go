package main

import (
	"fmt"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

const xmas = "XMAS"

const (
	HOR int = iota
	VERT
	DIAGH
	DIAGV
)

func extractOverlayXMAS(dst *[][]byte, trg []string, i, j, t int) {
	for k := range xmas {
		switch t {
		case HOR:
			(*dst)[i][j+k] = trg[i][j+k]
		case VERT:
			(*dst)[i+k][j] = trg[i+k][j]
		case DIAGH:
			(*dst)[i+k][j+k] = trg[i+k][j+k]
		case DIAGV:
			(*dst)[i+k][j-k] = trg[i+k][j-k]
		}
	}
}

func extractOverlayX_MAS(dst *[][]byte, trg []string, i, j int) {
	(*dst)[i][j] = trg[i][j]
	(*dst)[i+1][j+1] = trg[i+1][j+1]
	(*dst)[i-1][j-1] = trg[i-1][j-1]
	(*dst)[i+1][j-1] = trg[i+1][j-1]
	(*dst)[i-1][j+1] = trg[i-1][j+1]
}

func mergeOverlays(left, right [][]byte) [][]byte {
	result := make([][]byte, len(left))
	for k := range result {
		result[k] = make([]byte, len(left[k]))
		for j := range result[k] {
			result[k][j] = left[k][j]
			if result[k][j] == '.' && right[k][j] != '.' {
				result[k][j] = right[k][j]
			}
		}
	}
	return result
}

func makeOvrelay(l, h int) [][]byte {
	overlay := make([][]byte, l)
	for k := range overlay {
		overlay[k] = []byte(strings.Repeat(".", h))
	}
	return overlay

}
func findXMAS(s []string, i, j int) (int, [][]byte) {
	if len(s) == 0 {
		return 0, [][]byte{}
	}
	mx, my := len(s), len(s[0])
	overlay := makeOvrelay(mx, my)
	result := 0
	directions := []bool{true, true, true, true, true, true, true, true}
	for k := range xmas {
		if !(directions[0] && k+i < mx && s[i+k][j] == xmas[k]) {
			directions[0] = false
		}
		if !(directions[1] && k+j < my && s[i][j+k] == xmas[k]) {
			directions[1] = false
		}
		if !(directions[2] && k+i < mx && k+j < my && s[i+k][j+k] == xmas[k]) {
			directions[2] = false
		}
		if !(directions[3] && i-k >= 0 && s[i-k][j] == xmas[k]) {
			directions[3] = false
		}
		if !(directions[4] && j-k >= 0 && s[i][j-k] == xmas[k]) {
			directions[4] = false
		}
		if !(directions[5] && i-k >= 0 && j-k >= 0 && s[i-k][j-k] == xmas[k]) {
			directions[5] = false
		}
		if !(directions[6] && i-k >= 0 && j+k < my && s[i-k][j+k] == xmas[k]) {
			directions[6] = false
		}
		if !(directions[7] && i+k < mx && j-k >= 0 && s[i+k][j-k] == xmas[k]) {
			directions[7] = false
		}
	}
	offset := len(xmas) - 1
	for p, d := range directions {
		if !d {
			continue
		}
		result++
		switch p {
		case 0:
			extractOverlayXMAS(&overlay, s, i, j, VERT)
		case 1:
			extractOverlayXMAS(&overlay, s, i, j, HOR)
		case 2:
			extractOverlayXMAS(&overlay, s, i, j, DIAGH)
		case 3:
			extractOverlayXMAS(&overlay, s, i-offset, j, VERT)
		case 4:
			extractOverlayXMAS(&overlay, s, i, j-offset, HOR)
		case 5:
			extractOverlayXMAS(&overlay, s, i-offset, j-offset, DIAGH)
		case 6:
			extractOverlayXMAS(&overlay, s, i-offset, j+offset, DIAGV)
		case 7:
			extractOverlayXMAS(&overlay, s, i, j, DIAGV)

		}
	}
	return result, overlay
}

func findX_MAS(s []string, i, j int) bool {
	mx, my := len(s), len(s[0])
	if i == 0 || j == 0 || i+1 == mx || j+1 == my || s[i][j] != 'A' {
		return false
	}
	diag := []bool{false, false}

	if (s[i-1][j-1] == 'M' && s[i+1][j+1] == 'S') || (s[i-1][j-1] == 'S' && s[i+1][j+1] == 'M') {
		diag[0] = true
	}
	if (s[i-1][j+1] == 'M' && s[i+1][j-1] == 'S') || (s[i-1][j+1] == 'S' && s[i+1][j-1] == 'M') {
		diag[1] = true
	}
	return diag[0] && diag[1]
}

func findAllXmass(s []string) (int, int) {
	result := 0
	resultP2 := 0
	overlay := makeOvrelay(len(s), len(s[0]))
	overlay2 := makeOvrelay(len(s), len(s[0]))
	for i := range s {
		for j := range s[i] {
			r1, it := findXMAS(s, i, j)
			overlay = mergeOverlays(overlay, it)
			result += r1
			if findX_MAS(s, i, j) {
				extractOverlayX_MAS(&overlay2, s, i, j)
				resultP2 += 1
			}

		}
	}
	fmt.Println("Part I visualisation")
	for k := range overlay {
		fmt.Println(string(overlay[k]))
	}
	fmt.Println("Part II visualisation")
	for k := range overlay2 {
		fmt.Println(string(overlay2[k]))
	}
	return result, resultP2
}

func main() {
	in := util.InputSlice()
	p1, p2 := findAllXmass(in)
	fmt.Printf("Number of XMAS: %d\n", p1)
	fmt.Printf("Number of X-MAS: %d\n", p2)
}
