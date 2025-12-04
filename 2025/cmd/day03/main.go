// Copyright (c) 2025 Miroslav Jaros
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// SPDX-License-Identifier: MIT

package main

import (
	"fmt"
	"strconv"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(3)
}

func findBiggestPartI(b []byte) []byte {
	bInd, sInd := 0, 1
	biggest := int(b[0])<<8 | int(b[1])
	for i := 0; i < len(b); i++ {
		for j := i + 1; j < len(b); j++ {
			if i == j || (i == 0 && j == 1) {
				continue
			}
			curr := int(b[i])<<8 | int(b[j])
			if curr > biggest {
				biggest = curr
				bInd, sInd = i, j
			}
		}
	}
	return []byte{b[bInd], b[sInd]}
}

func findBiggestPartII(b []byte) int64 {
	bytesExtracted := 0
	highestByte := -1
	var currNum int64 = 0
	for bytesExtracted < 12 {
		cur := b[highestByte+1]
		var i int
		bi := highestByte + 1
		for i = highestByte + 2; i < len(b)-(11-bytesExtracted); i++ {
			if b[i] > cur {
				cur = b[i]
				bi = i
			}
		}
		currNum = currNum*10 + int64(cur-'0')
		highestByte = bi
		bytesExtracted++
	}
	return currNum
}

func main() {
	data := util.InputBytes()
	sum := 0
	var sum2 int64
	for _, k := range data {
		s := string(findBiggestPartI(k))
		if util.Verbose {
			fmt.Println(string(k), s)
		}
		n, err := strconv.Atoi(s)
		if err != nil {
			panic(err.Error())
		}
		sum += n
	}

	for _, k := range data {
		p2n := findBiggestPartII(k)
		if util.Verbose {
			fmt.Println(p2n)
		}
		sum2 += p2n
	}
	fmt.Println("Maximal jolts (part I answer) is:", sum)
	fmt.Println("Maximal jolts (part II answer) is:", sum2)
}
