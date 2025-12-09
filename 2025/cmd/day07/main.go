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
	util.SetIdentifier(7)
}

func TraverseGraph(gr [][]byte) (int, int) {
	splits := 0
	max := len(gr[0])
	start := graphs.FindNode(gr, 'S')
	currLevel := start[0]
	levelNodes := map[int]int{start[1]: 1}

	for currLevel < len(gr) {
		nextLevel := make(map[int]int)
		for node := range levelNodes {
			if gr[currLevel][node] == '^' {
				splits++
				left, right := node-1, node+1
				if left >= 0 {
					nextLevel[left] += levelNodes[node]
				}
				if right < max {
					nextLevel[right] += levelNodes[node]
				}
				continue
			}
			nextLevel[node] += levelNodes[node]
		}
		levelNodes = nextLevel
		for node := range levelNodes {
			if gr[currLevel][node] == '.' {
				gr[currLevel][node] = '|'
			}

		}
		currLevel++
	}
	sum := 0
	for _, v := range levelNodes {
		sum += v
	}
	return splits, sum
}

func main() {
	graph := util.InputBytes()
	splits, paths := TraverseGraph(graph)
	graphs.PrintMat(graph)
	fmt.Println("Number of splits of tachyon beam (Part I answer) is", splits)
	fmt.Println("Number of different tachyon beams (Part II answer) is", paths)
}
