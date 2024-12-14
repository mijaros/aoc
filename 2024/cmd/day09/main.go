package main

import (
	"errors"
	"fmt"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

func sum(s []int8) int64 {
	var res int64 = 0
	for _, k := range s {
		res += int64(k)
	}
	return res

}

func genSlice(i, p int) []int {
	res := make([]int, i)
	for k := range res {
		res[k] = p
	}
	return res
}

func buildDisk(d []int8) []int {
	res := []int{}
	for i, d := range d {
		res = append(res, genSlice(int(d), i)...)
	}
	for k := range res {
		if res[k]%2 == 0 {
			res[k] /= 2
		} else {
			res[k] = -1
		}
	}
	return res
}

func toDisk(s string) (res []int8, err error) {
	res = make([]int8, len(s))
	k := 0
	for _, b := range s {
		if b == '\n' {
			return
		}
		if b < '0' || b > '9' {
			err = errors.New("Invalid byte")
			return
		}
		d := b - '0'
		res[k] = int8(d)
		k++
	}
	res = res[:k]
	return

}

func findNext(i int, disk []int) int {
	for k := i; k < len(disk); k++ {
		if disk[k] == -1 {
			return k
		}
	}
	return -1
}

func findPrev(i int, disk []int) int {
	for k := i; k >= 0; k-- {
		if disk[k] != -1 {
			return k
		}
	}
	return -1
}

func defragDisk(d []int) []int {
	fst, last := findNext(0, d), findPrev(len(d)-1, d)
	for last > fst {
		d[fst], d[last] = d[last], d[fst]
		fst, last = findNext(fst+1, d), findPrev(last-1, d)
	}
	return d
}

func lastFile(d []int, i int) (int, int) {
	r := -1
	p := 0
	for k := range d {
		if d[k] == i {
			if r == -1 {
				r = k
			}
			p++
		}
	}
	return r, p
}

func findHole(d []int, i int) (int, int) {
	r := -1
	p := 0
	for k := i; k < len(d); k++ {
		if d[k] != -1 && r != -1 {
			return r, p
		}
		if d[k] == -1 {
			if r == -1 {
				r = k
			}
			p++
		}
	}
	return r, p
}

func defragDiskII(d []int, h int) []int {
	lf := h
	for lf > 0 {
		index, length := lastFile(d, lf)
		hi, hl := findHole(d, 0)
		for hi < index && length > hl {
			hi, hl = findHole(d, hi+hl)
		}
		if hi < index && length <= hl {
			for k := 0; k < length; k++ {
				d[index+k], d[hi+k] = d[hi+k], d[index+k]
			}
		}
		lf--
	}
	return d
}

func checkSum(d []int) int {
	res := 0
	for i, l := range d {
		if l == -1 {
			continue
		}
		res += i * l
	}
	return res
}

func main() {
	disk, err := toDisk(util.GetInput())
	if err != nil {
		fmt.Println(err)
		return
	}
	d := buildDisk(disk)
	highestId := d[len(d)-1]
	fmt.Println("Highest id ", highestId)
	partI := make([]int, len(d))
	copy(partI, d)
	def := defragDisk(partI)
	fmt.Printf("The checksum of the disk is %d\n", checkSum(def))

	partII := make([]int, len(d))
	copy(partII, d)
	defII := defragDiskII(partII, highestId)
	fmt.Printf("The checksum of new fragmentation is %d\n", checkSum(defII))
}
