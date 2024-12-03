package main

import (
	"fmt"
	"github.com/mijaros/adventofcode/v2024/pkg/util"
	"strconv"
	"strings"
)

const prefix = "mul("
const dos = "do()"
const donts = "don't()"

func Digit(b byte) bool {
	return b >= '0' && b <= '9'
}

func getInt(s string, index int) (r, n int, err error) {
	r, n = 0, index
	var i = 0
	it := s[index:]
	for i = range it {
		if i > 0 && it[i] == ',' {
			break
		}
		if !Digit(it[i]) || i >= 3 {
			break
		}

	}
	r, err = strconv.Atoi(it[:i])
	if err != nil {
		return
	}
	n = index + i
	return
}

func ReadMult(s string) (int, int) {
	i := 0
	r := 0
	r2 := 0
	do := true
	for i < len(s) {
		it := s[i:]
		l := strings.Index(it, prefix)
		if l == -1 {
			return r, r2
		}

		doi := strings.Index(it, dos)
		donti := strings.Index(it, donts)
		if doi != -1 && doi < l && (doi < donti || donti == -1) {
			do = true
			i += doi + 1
			continue
		}
		if donti != -1 && donti < l && (donti < doi || doi == -1) {
			do = false
			i += donti + 1
			continue
		}
		fn, n, err := getInt(it, l+len(prefix))
		if err != nil || n-(l+len(prefix)) > 4 || it[n] != ',' {
			i += 1
			continue
		}
		fn2, n2, err := getInt(it, n+1)
		if err != nil || n2-n > 4 || it[n2] != ')' {
			i += 1
			continue
		}
		r += fn * fn2
		if do {
			r2 += fn * fn2
		}
		i += n2 + 1
	}
	return r, r2
}

func main() {
	in := util.GetInput()
	partI, partII := ReadMult(in)
	fmt.Printf("Result of computation with mult: %d\n", partI)
	fmt.Printf("Result of computation with mult and do: %d\n", partII)
}
