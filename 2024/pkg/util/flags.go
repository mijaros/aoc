package util

import (
	"flag"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

var (
	inFile  *string
	Verbose bool
)

func init() {
	inFile = flag.String("iFile", "", "Ahoj Svete")
	flag.BoolVar(&Verbose, "verbose", false, "Toggle verbose output")
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

func InputBytes() [][]byte {
	slc := InputSlice()
	return ToBytes(slc)
}

func ToBytes(s []string) [][]byte {
	res := make([][]byte, len(s))
	for k := range s {
		res[k] = []byte(s[k])
	}
	return res
}

func Abs(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

func CopyMat[K any](in [][]K) [][]K {
	res := make([][]K, len(in))
	for i := range res {
		res[i] = append([]K{}, in[i]...)
	}
	return res
}
func Last[T any](v []T) *T {
	if len(v) == 0 {
		return nil
	}
	return &v[len(v)-1]
}

func Transform[K, V any](in []K, trans func(K) V) []V {
	res := make([]V, 0, len(in))
	for _, v := range in {
		res = append(res, trans(v))
	}
	return res
}
