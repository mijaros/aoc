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
	"strings"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(2)
}

type Range struct {
	from, to int
}

func StrToRange(s string) Range {
	nums := strings.Split(s, "-")
	var res Range
	for i := range nums {
		if i == 2 {
			panic("Invalid number of numbers in range")
		}
		n, err := strconv.Atoi(nums[i])
		if err != nil {
			panic(err.Error())
		}
		if i == 0 {
			res.from = n
		} else {
			res.to = n
		}
	}
	return res
}

func ParseRanges(s []string) []Range {
	sRanges := make([]string, 0)
	for _, v := range s {
		r := strings.Split(v, ",")
		sRanges = append(sRanges, r...)
	}
	result := make([]Range, len(sRanges))
	for i := range sRanges {
		it := StrToRange(sRanges[i])
		result[i] = it
	}
	return result
}

func (r Range) SumInvalidIdsP1() int64 {
	var res int64
	for i := r.from; i <= r.to; i++ {
		sRep := strconv.Itoa(i)
		if len(sRep)%2 == 1 {
			continue
		}
		if sRep[:len(sRep)/2] == sRep[len(sRep)/2:] {
			res += int64(i)
		}

	}
	return res
}

func IsInvalid(s string) bool {
	lenght := len(s)
OUT:
	for i := 1; i <= lenght/2; i++ {
		if lenght%i != 0 {
			continue
		}
		splitted := make([]string, lenght/i)
		for k := range splitted {
			splitted[k] = s[k*i : (k+1)*i]
		}

		for j := 0; j < len(splitted)-1; j++ {
			if splitted[j] != splitted[j+1] {
				continue OUT
			}
		}
		if util.Verbose {
			fmt.Println(splitted)
		}
		return true
	}
	return false
}
func (r Range) SumInvalidIdsP2() int64 {
	var res int64
	for i := r.from; i <= r.to; i++ {
		sRep := strconv.Itoa(i)
		if IsInvalid(sRep) {
			res += int64(i)
		}

	}
	return res
}

func main() {
	data := util.InputSlice()
	ranges := ParseRanges(data)
	var sum int64
	var sum2 int64
	for _, r := range ranges {
		sum += r.SumInvalidIdsP1()
		sum2 += r.SumInvalidIdsP2()
	}
	fmt.Println("Sum of all invalid ids is (Part I answer)", sum)
	fmt.Println("Sum of all invalid ids is (Part II answer)", sum2)
}
