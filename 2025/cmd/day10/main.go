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
	"flag"
	"fmt"
	"strconv"
	"strings"

	"github.com/mijaros/adventofcode/v2025/pkg/util"
)

var (
	maxDist int
)

func init() {
	util.SetIdentifier(10)
	flag.IntVar(&maxDist, "maxDist", 200, "Set maximal parameters vector 1-norm - algorithm will all vectors from <0,maxDist>")
}

func gcd(a, b int) int {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	if b > a {
		return gcd(b, a)
	}

	for b != 0 {
		a, b = b, a%b
	}
	return a
}

type Rational struct {
	numerator, denominator int
}

func (r Rational) Minimize() Rational {
	if r.numerator == 0 {
		r.denominator = 0
		return r
	}
	g := gcd(r.numerator, r.denominator)
	if g > 1 {
		return Rational{r.numerator / g, r.denominator / g}
	}
	return r
}

func Whole(number int) Rational {
	return Rational{number, 1}.Minimize()
}

func (r Rational) Add(o Rational) Rational {
	if o.denominator == 0 {
		return r
	}
	if r.denominator == 0 {
		return o
	}
	if r.denominator == o.denominator {
		res := r
		res.numerator += o.numerator
		return res.Minimize()
	}
	num := r.numerator*o.denominator + o.numerator*r.denominator
	den := r.denominator * o.denominator
	return Rational{numerator: num, denominator: den}.Minimize()
}

func (r Rational) Neg() Rational {
	res := r
	res.numerator = -res.numerator
	return res
}

func (r Rational) Mult(o Rational) Rational {
	return Rational{r.numerator * o.numerator, r.denominator * o.denominator}.Minimize()
}

func (r Rational) Inverse() Rational {
	if r.numerator < 0 {
		return Rational{-r.denominator, -r.numerator}
	}
	return Rational{r.denominator, r.numerator}.Minimize()
}

func (r Rational) Div(o Rational) Rational {
	return r.Mult(o.Inverse()).Minimize()
}

func (r Rational) IsZero() bool {
	return r.numerator == 0
}

func (r Rational) IsWhole() bool {
	return r.denominator == 1 || r.denominator == 0
}

func (r Rational) IsOne() bool {
	return r.numerator == 1 && r.denominator == 1
}

func (r Rational) String() string {
	if r.IsWhole() {
		return fmt.Sprint(r.numerator)
	}
	return fmt.Sprintf("%d/%d", r.numerator, r.denominator)
}

type VectorParameter struct {
	index      int
	valueDist  *int
	multiplier Rational
}

type Variable struct {
	scalar     Rational
	parameters map[int]VectorParameter
	opened     bool
	parameter  bool
}

type Matrix struct {
	values [][]Rational
}

type Solution struct {
	variables  []Variable
	parameters []VectorParameter
}

func (v *Variable) IsOpen() bool {
	return v.opened
}

func (v Variable) Evaluate() Rational {
	res := v.scalar
	for _, p := range v.parameters {
		res = res.Add(p.Evaluate())
	}
	return res.Minimize()
}

func (v *Variable) Add(r Rational, other Variable) {
	v.scalar = v.scalar.Add(other.scalar.Mult(r))
	for k, va := range other.parameters {
		if curr, ok := v.parameters[k]; ok {
			curr.multiplier = curr.multiplier.Add(va.multiplier.Mult(r))
			v.parameters[k] = curr
		} else {
			v.parameters[k] = va.Derive(r)
		}
	}
}

func (v VectorParameter) Evaluate() Rational {
	return v.multiplier.Mult(Whole(*v.valueDist))
}

func (v VectorParameter) Mult(s Rational) VectorParameter {
	res := v
	res.multiplier = res.multiplier.Mult(s)
	return res
}

func (v VectorParameter) Derive(r Rational) VectorParameter {
	return VectorParameter{index: v.index, multiplier: r.Mult(v.multiplier), valueDist: v.valueDist}
}

func NewParameter(index int) VectorParameter {
	holder := 0
	return VectorParameter{index: index, multiplier: Whole(1), valueDist: &holder}
}

type Device struct {
	target  string
	buttons [][]int
	joltage []int
}

func ParseList(s string) []int {
	it := strings.SplitSeq(s, ",")
	res := make([]int, 0)
	for lit := range it {
		n, err := strconv.Atoi(lit)
		if err != nil {
			panic(err)
		}
		res = append(res, n)
	}
	return res
}

func ParseDevice(s string) Device {
	items := strings.Fields(s)
	target := strings.Trim(items[0], "[]")
	joltage := ParseList(strings.Trim(items[len(items)-1], "{}"))
	buttons := make([][]int, 0)

	for _, l := range items[1 : len(items)-1] {
		trimmed := strings.Trim(l, "()")
		buttons = append(buttons, ParseList(trimmed))
	}
	return Device{target: target, buttons: buttons, joltage: joltage}
}

func Gen1NormVectors(params, width int) [][]int {
	if params == 1 {
		return [][]int{{width}}
	}
	result := make([][]int, 0)
	for i := 0; i <= width; i++ {
		particulars := Gen1NormVectors(params-1, width-i)
		for _, k := range particulars {
			r := append([]int{i}, k...)
			result = append(result, r)
		}
	}
	return result
}

func ParseDevices(s []string) []Device {
	res := make([]Device, len(s))
	for i := range s {
		res[i] = ParseDevice(s[i])
	}
	return res
}

func Switch(s string, ind []int) string {
	sb := []byte(s)
	for _, i := range ind {
		if sb[i] == '#' {
			sb[i] = '.'
		} else {
			sb[i] = '#'
		}
	}
	return string(sb)
}

func buildEmpty(length int) string {
	res := make([]byte, length)
	for i := 0; i < length; i++ {
		res[i] = '.'
	}
	return string(res)
}

func (d Device) LightItUp() int {
	current := map[string]bool{buildEmpty(len(d.target)): true}
	depth := 0
	mapper := make(map[string][]string)
	found := false
	for !found {
		depth++
		next := make(map[string]bool)
		for c := range current {
			for _, b := range d.buttons {
				res := Switch(c, b)
				mapper[c] = append(mapper[c], Switch(c, b))
				next[res] = true
				if res == d.target {
					return depth
				}
			}
		}
		current = next
	}
	return -1
}

func (d Device) BuildLinearEquations() Matrix {
	linOp := make([][]Rational, len(d.joltage))
	for a := range d.joltage {
		linOp[a] = make([]Rational, len(d.buttons)+1)
		for b := range d.buttons {
			linOp[a][b] = Whole(0)
			for _, k := range d.buttons[b] {
				if a == k {
					linOp[a][b] = Whole(1)
				}
			}
		}
		linOp[a][len(d.buttons)] = Whole(d.joltage[a])
	}

	return Matrix{values: linOp}
}

func (m *Matrix) FindLeadingRow(col, row int) {
	mat := m.values
	for i := row; i < len(mat); i++ {
		if !mat[i][col].IsZero() {
			if i != row {
				mat[row], mat[i] = mat[i], mat[row]
			}
			break
		}
	}
	if mat[row][col].IsZero() || mat[row][col].IsOne() {
		return
	}
	mult := mat[row][col].Inverse()
	for j := col; j < len(mat[row]); j++ {
		mat[row][j] = mat[row][j].Mult(mult)
	}
}

func (m *Matrix) ElimZeros() {
	var next [][]Rational
LINE:
	for _, l := range m.values {
		for _, j := range l {
			if !j.IsZero() {
				next = append(next, l)
				continue LINE
			}
		}
	}
	m.values = next
}

func (m *Matrix) GEM() {
	col, row := 0, 0
	for col < len(m.values[0])-1 && row < len(m.values) {
		m.FindLeadingRow(col, row)
		if m.values[row][col].IsZero() {
			col++
			continue
		}
		for j := row + 1; j < len(m.values); j++ {
			if m.values[j][col].IsZero() {
				continue
			}
			mul := m.values[j][col]
			for k := col; k < len(m.values[j]); k++ {
				m.values[j][k] = m.values[j][k].Add(mul.Mult(m.values[row][k]).Neg())
			}

		}
		row++
		col++
		m.ElimZeros()
	}
}

func SolveEquation(equation []Rational, solution *Solution) {
	columns := []int{}
	value := equation[len(equation)-1]
	mainCol := -1
	for i := 0; i < len(equation)-1; i++ {
		if equation[i].numerator != 0 {
			if mainCol == -1 {
				mainCol = i
			} else {
				columns = append(columns, i)
			}
		}
	}
	solution.variables[mainCol].opened = false
	solution.variables[mainCol].parameter = false
	solution.variables[mainCol].scalar = value
	if len(columns) == 0 {
		return
	}

	for _, c := range columns {
		v := solution.variables[c]
		if v.IsOpen() {
			param := NewParameter(c)
			solution.parameters = append(solution.parameters, param)
			v.scalar = Whole(0)
			v.parameters = map[int]VectorParameter{c: param}
			v.opened = false
			v.parameter = true
			solution.variables[c] = v
		}
		solution.variables[mainCol].Add(equation[c].Neg(), v)
	}
}

func NewVariable() Variable {
	return Variable{scalar: Whole(0), opened: true, parameter: false, parameters: make(map[int]VectorParameter)}
}

func (m *Matrix) SolveEquations() Solution {
	result := Solution{
		variables:  make([]Variable, len(m.values[0])-1),
		parameters: make([]VectorParameter, 0)}
	for k := range result.variables {
		result.variables[k] = NewVariable()
	}
	for i := len(m.values) - 1; i >= 0; i-- {
		SolveEquation(m.values[i], &result)
	}
	return result
}

func (s *Solution) Solve(maxDist int) int {
	resFound := false
	res := Whole(0)
	if len(s.parameters) == 0 {
		for _, v := range s.variables {
			res = res.Add(v.Evaluate())
		}
		return res.numerator
	}
	for dist := 0; dist < maxDist; dist++ {
		pVal := Gen1NormVectors(len(s.parameters), dist)
		currMin := Whole(0)
		res2Found := false
	INNER:
		for _, pVec := range pVal {
			inRes := Whole(0)
			for i := 0; i < len(s.parameters); i++ {
				*(s.parameters[i].valueDist) = pVec[i]
			}
			for _, v := range s.variables {
				cv := v.Evaluate()
				if cv.denominator > 1 || cv.numerator < 0 {
					continue INNER
				}
				inRes = inRes.Add(cv)
			}
			if inRes.numerator != 0 && (!res2Found || currMin.numerator > inRes.numerator) {
				currMin = inRes
				res2Found = true
			}

		}
		if currMin.numerator == 0 && !resFound {
			continue
		}
		if !resFound && currMin.numerator != 0 {
			res = currMin
			resFound = true
		} else if res.numerator > currMin.numerator && currMin.numerator != 0 {
			res = currMin
		}

	}
	return res.numerator

}

func PrintMatrix(l [][]Rational) {
	for _, line := range l {
		for x, d := range line {
			split := ""
			if x == len(line)-1 {
				split = "\n"
			}
			fmt.Printf("%4v%s", d, split)
		}
	}
}

func main() {
	inp := util.InputSlice()
	devices := ParseDevices(inp)
	sum1, sum2 := 0, 0

	for i, d := range devices {
		sum1 += d.LightItUp()
		mat := d.BuildLinearEquations()
		if util.Verbose {
			fmt.Println("Matrix no.", i)
			PrintMatrix(mat.values)
		}
		mat.GEM()
		if util.Verbose {
			fmt.Println("After GEM")
			PrintMatrix(mat.values)
		}
		sol := mat.SolveEquations()
		part := sol.Solve(maxDist)
		if part == 0 {
			fmt.Println("Invalid solution for", i)
		}
		if util.Verbose {
			fmt.Println("Solution for", i, "is", sol)
			fmt.Println("Evaluated", part)
		}
		sum2 += part
	}

	fmt.Println("The fewest number of keypresses required (part I answer) is", sum1)
	fmt.Println("The fewest number of keypresses required (part II answer) is", sum2)
}
