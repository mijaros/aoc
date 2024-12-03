package util

import (
	"flag"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

var inFile *string

func init() {
	inFile = flag.String("iFile", "", "Ahoj Svete")

}

func GetInput() string {
	if !flag.Parsed() {
		flag.Parse()
	}
	dat, err := os.Open(*inFile)
	if err != nil {
		log.Panicf("Couldn't open %s: %s", *inFile, err.Error())
	}
	defer dat.Close()
	rdr, err := io.ReadAll(dat)
	if err != nil {
		log.Panicf("Couldn't read file: %s", err.Error())
	}
	return string(rdr)

}

func InputSlice() []string {
	input := GetInput()
	r := strings.Split(input, "\n")
	l := len(r)
	if l > 0 && r[l-1] == "" {
		return r[:l-1]
	}
	return r
}

func InputMatStr() [][]string {
	lines := InputSlice()
	res := make([][]string, len(lines))
	for i, v := range lines {
		res[i] = strings.Fields(v)
	}
	return res
}

func InputInts() []int {
	slc := InputSlice()
	var err error
	res := make([]int, len(slc))
	for i, v := range slc {
		res[i], err = strconv.Atoi(v)
		if err != nil {
			log.Panicf("Couldn't parse %s: %s", v, err.Error())
		}
	}
	return res
}
func InputMatInt() [][]int {
	inMat := InputMatStr()
	result := make([][]int, len(inMat))
	for i, v := range inMat {
		result[i] = make([]int, len(v))
		for j, m := range v {
			d, err := strconv.Atoi(m)
			if err != nil {
				log.Panic(err)
			}
			result[i][j] = d
		}
	}
	return result

}

func Abs(i int) int {
	if i < 0 {
		return -i
	}
	return i
}
