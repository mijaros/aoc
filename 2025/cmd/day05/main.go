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
	"slices"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(5)
}

type Range struct {
	from, to int
}

func ParseRange(s string) Range {
	numS := strings.Split(s, "-")
	num := make([]int, len(numS))
	for i := range numS {
		n, err := strconv.Atoi(numS[i])
		if err != nil {
			panic(err.Error())
		}
		num[i] = n
	}
	return Range{from: num[0], to: num[1]}
}

func ParseRanges(inp []string) ([]Range, int) {
	res := make([]Range, 0)
	for k, v := range inp {
		if v == "" {
			return res, k
		}
		res = append(res, ParseRange(v))
	}
	return res, len(res)
}

func (r Range) IsValidId(id int) bool {
	return id >= r.from && id <= r.to
}

func (r Range) Size() int {
	return (r.to - r.from) + 1
}

func VerifyId(id int, ranges []Range) bool {
	for _, r := range ranges {
		if r.IsValidId(id) {
			return true
		}
	}
	return false
}

func GetFreshIds(ranges []Range, ids []int) []int {
	result := make([]int, 0)

	for _, id := range ids {
		if VerifyId(id, ranges) {
			result = append(result, id)
		}
	}
	return result
}

func ParseInventory(d []string) ([]Range, []int) {
	var ranges []Range
	ids := make([]int, 0)
	var rangeOff int
	ranges, rangeOff = ParseRanges(d)

	for _, id := range d[rangeOff+1:] {
		num, err := strconv.Atoi(id)
		if err != nil {
			panic(err.Error())
		}
		ids = append(ids, num)
	}
	return ranges, ids
}

func compareRanges(a, b Range) int {
	if a.from < b.from {
		return -1
	}
	if b.from < a.from {
		return 1
	}
	if a.to < b.from {
		return 1
	}
	return 0
}

func CountFreshIds(ranges []Range) int {
	slices.SortFunc(ranges, compareRanges)
	unifiedRanges := make([]Range, 0)
	top := ranges[0]
	for _, r := range ranges[1:] {
		if top.IsValidId(r.from) {
			if r.to > top.to {
				top.to = r.to
			}
			continue
		}
		unifiedRanges = append(unifiedRanges, top)
		top = r
	}
	unifiedRanges = append(unifiedRanges, top)
	if util.Verbose {
		fmt.Println(unifiedRanges)
	}

	lenght := 0
	for _, r := range unifiedRanges {
		lenght += r.Size()
	}
	return lenght
}

func main() {
	dat := util.InputSlice()
	r, i := ParseInventory(dat)
	freshIds := GetFreshIds(r, i)
	fmt.Println("Number of fresh Ids (Part I answer) is", len(freshIds))
	fmt.Println("Numbef of all fresh Ids (Part II answer) is", CountFreshIds(r))
}
