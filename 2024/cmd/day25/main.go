package main

import (
	"fmt"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type KeyLock struct {
	columns [5]int
	key     bool
}

func ParseKeyLock(in []string, start int) (*KeyLock, int) {
	res := [5]int{}
	valid := true
	key := false
	if in[start] == "#####" && in[start+6] == "....." {
		valid = true
		key = false
	}
	if in[start] == "....." && in[start+6] == "#####" {
		valid = true
		key = true
	}
	if !valid {
		return nil, start + 7
	}
	for i := start + 1; i < start+6; i++ {
		if in[i] == "" {
			break
		}
		for j, b := range []byte(in[i]) {
			if b == '#' {
				res[j]++
			}
		}
	}
	return &KeyLock{columns: res, key: key}, start + 7
}

func ParseKeyLocks(in []string) []KeyLock {
	index := 0
	result := make([]KeyLock, 0)
	for index < len(in) {
		var kl *KeyLock
		kl, index = ParseKeyLock(in, index)
		result = append(result, *kl)
		index++
	}
	return result
}

func FilterFunc[K any](s []K, f func(K) bool) []K {
	res := make([]K, 0)
	for _, v := range s {
		if f(v) {
			res = append(res, v)
		}
	}
	return res
}

func TryAllKeys(kl []KeyLock) int {
	fitting := make(map[[2]KeyLock]bool)
	keys := FilterFunc(kl, func(v KeyLock) bool { return v.key })
	locks := FilterFunc(kl, func(v KeyLock) bool { return !v.key })
	fmt.Printf("keys: %+v\n", keys)
	fmt.Printf("locks: %+v\n", locks)
	for _, k := range keys {
		for _, l := range locks {
			fit := true
			for i := 0; i < 5; i++ {
				if k.columns[i]+l.columns[i] > 5 {
					fmt.Printf("Not fitting %+v %+v, %d\n", l, k, k.columns[i]+l.columns[i])
					fit = false
					break
				}
			}
			if fit {
				fitting[[2]KeyLock{k, l}] = true
			}
		}
	}
	return len(fitting)
}

func main() {
	inp := util.InputSlice()
	keyLocks := ParseKeyLocks(inp)
	fmt.Printf("%+v\n", keyLocks)
	fitting := TryAllKeys(keyLocks)
	fmt.Printf("Number of all fitting key/lock pairs is %d\n", fitting)
}
