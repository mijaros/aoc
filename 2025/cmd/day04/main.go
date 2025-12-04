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

	"github.com/mijaros/adventofcode/v2025/pkg/graphs"
	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

func init() {
	util.SetIdentifier(4)
}

func CountAvailable(graph [][]byte) [][2]int {
	result := make([][2]int, 0)
	for i := range graph {
		for j := range graph[i] {
			if graph[i][j] != '@' {
				continue
			}
			paperRolls := 0
			neighbors := graphs.GenPointsDiag([2]int{i, j}, len(graph), len(graph[0]))
			for _, p := range neighbors {
				if graph[p[0]][p[1]] == '@' {
					paperRolls++
				}
			}
			if paperRolls < 4 {
				result = append(result, [2]int{i, j})
			}
		}
	}
	return result
}

func RemoveAll(graph [][]byte) int {
	removed := 0
	for {
		removable := CountAvailable(graph)
		if len(removable) == 0 {
			return removed
		}
		for _, r := range removable {
			graph[r[0]][r[1]] = '.'
		}
		removed += len(removable)
	}
	return removed
}

func main() {
	mat := util.InputBytes()
	availableRolls := CountAvailable(mat)
	fmt.Println("Number of available paper rolls (Part I answer) is", len(availableRolls))
	allRemoved := RemoveAll(mat)
	fmt.Println("Number of all removable paper rolls is (Part II answer)", allRemoved)

}
