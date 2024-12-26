package main

import (
	"flag"
	"fmt"
	"maps"
	"slices"
	"strconv"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

var (
	runTest bool
)

func mix(in, s int) int {
	return in ^ s
}

func prune(in int) int {
	return in % 16777216
}

func NextSecretNumber(in int) int {
	res := in * 64
	res = prune(mix(res, in))
	res = prune(mix(res/32, res))
	return prune(mix(res*2048, res))
}

func init() {
	flag.BoolVar(&runTest, "tests", false, "Run only secret generator test")
}

func getSeedNumbers() []int {
	inp := util.InputSlice()
	res := make([]int, len(inp))
	for k := range inp {
		r, err := strconv.Atoi(inp[k])
		if err != nil {
			panic(err.Error())
		}
		res[k] = r
	}
	return res
}

func Calc2000th(in int) int {
	r := in
	for i := 0; i < 2000; i++ {
		r = NextSecretNumber(r)
	}
	return r
}

func BuildSequence2000(in int) [][2]int8 {
	res := make([][2]int8, 2001)
	res[0] = [2]int8{int8(in % 10), int8(in % 10)}
	for i := 0; i < 2000; i++ {
		ni := NextSecretNumber(in)
		pd := int8(ni%10) - res[i][0]
		res[i+1] = [2]int8{int8(ni % 10), pd}
		in = ni
	}
	return res
}

func TransformToSequence(in []int) [][][2]int8 {
	res := make([][][2]int8, len(in))
	for i := range res {
		res[i] = BuildSequence2000(in[i])
	}
	return res
}

func FindSequenceIn2000th(seq [4]int, in int) int {
	curr := 1
	subseq := [4]int{in % 10, -11, -11, -11}
	np := in % 10
	for i := 0; i < 2000; i++ {
		in2 := NextSecretNumber(in)
		if curr < 4 {
			subseq[curr] = (in2 % 10) - np
			curr++
		} else {
			for i := 0; i < curr-1; i++ {
				subseq[i] = subseq[i+1]
			}
			subseq[curr-1] = (in2 % 10) - np
		}
		if subseq == seq {
			return in2 % 10
		}
		np = in2 % 10
		in = in2
	}
	return 11
}

func FindSequenceInCalc(seq [4]int, dat [][2]int8) int {
	for i := 0; i < len(dat)-len(seq); i++ {
		found := true
		for j := 0; j < len(seq); j++ {
			if seq[j] != int(dat[i][1]) {
				found = false
			}
		}
		if found {
			return int(dat[i+3][0])
		}
	}
	return 11
}

func GetAllForSequence(seq [4]int, dat [][][2]int8) int {
	sum := 0
	for i := range dat {
		res := FindSequenceInCalc(seq, dat[i])
		if res != 11 {
			sum += res
		}
	}
	return sum
}

func CalcBestSequenceII(in []int) int {
	allCombs := genAllCombinations()
	allSeqs := TransformToSequence(in)
	maximum := 0
	for _, c := range allCombs {
		r := GetAllForSequence(c, allSeqs)
		if r > maximum {
			maximum = r
		}
	}
	return maximum
}

func RecalcSeed(in []int) []int {
	res := make([]int, len(in))
	for k := range res {
		res[k] = Calc2000th(in[k])
	}
	return res
}

func genAllCombinations() [][4]int {
	res := make([][4]int, 0)
	fres := make(map[[4]int]bool)
	for i := 0; i < 10; i++ {
		for j := 0; j < 10; j++ {
			for k := 0; k < 10; k++ {
				for l := 0; l < 10; l++ {
					for m := 0; m < 10; m++ {
						fres[[4]int{i - j, j - k, k - l, l - m}] = true
					}
				}
			}
		}
	}
	res = slices.Collect(maps.Keys(fres))
	return res
}

func genAllCombinationsOld() [][4]int {
	res := make([][4]int, 0)
	for i := -9; i < 10; i++ {
		for j := -9; j < 10; j++ {
			for k := -9; k < 10; k++ {
				for l := -9; l < 10; l++ {
					res = append(res, [4]int{i, j, k, l})
				}
			}
		}
	}
	return res
}

func gentTest() {
	sn := 123
	for i := 0; i < 10; i++ {
		sn = NextSecretNumber(sn)
		fmt.Println(sn)
	}
	testSeed := 3
	testSeq := [4]int{-2, 1, -1, 3}
	fmt.Println(FindSequenceIn2000th(testSeq, testSeed))
	allCombsOld := genAllCombinationsOld()
	allCombs := genAllCombinations()
	fmt.Println(len(allCombsOld), len(allCombs), len(allCombsOld)-len(allCombs))
}

func ValidateSingleSequence(nums []int, seq [4]int) int {
	sum := 0
	for _, k := range nums {
		p := FindSequenceIn2000th(seq, k)
		if p != 11 {
			sum += p
		}
	}
	return sum
}

func CalcBestSequence(nums []int) int {
	best := 0
	sequences := genAllCombinations()
	for _, s := range sequences {
		l := ValidateSingleSequence(nums, s)
		if l > best {
			best = l
		}
	}
	return best
}

func main() {
	if !flag.Parsed() {
		flag.Parse()
	}
	if runTest {
		gentTest()
		return
	}

	seedNumbers := getSeedNumbers()
	secretAfter2K := RecalcSeed(seedNumbers)
	sum := 0
	for i, v := range secretAfter2K {
		if util.Verbose {
			fmt.Printf("%d: %d\n", seedNumbers[i], v)
		}
		sum += v
	}
	fmt.Printf("Sum of 2000th secret numbers is %d\n", sum)
	largestBananas := CalcBestSequence(seedNumbers)
	fmt.Printf("The biggest number of bananas is %d\n", largestBananas)
}
