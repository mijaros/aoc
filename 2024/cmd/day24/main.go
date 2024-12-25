package main

import (
	"flag"
	"fmt"
	"io"
	"maps"
	"os"
	"slices"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type Function int8

const (
	AND Function = iota
	OR
	XOR
	NIL
)

var (
	swaps   string
	outFile string
)

func (f Function) Apply(left, right bool) bool {
	switch f {
	case AND:
		return left && right
	case OR:
		return left || right
	case XOR:
		return !(left && right) && !(!left && !right)
	}
	return false
}

func (f Function) String() string {
	switch f {
	case AND:
		return "AND"
	case OR:
		return "OR"
	case XOR:
		return "XOR"
	}
	return "NIL"
}

func ReadFun(s string) Function {
	switch s {
	case "AND":
		return AND
	case "OR":
		return OR
	case "XOR":
		return XOR
	}
	return NIL
}

type Wire struct {
	name  string
	value bool
	set   bool
}

type Gate struct {
	in1, in2 *Wire
	f        Function
	out      *Wire
	stable   bool
	state    bool
}

type Device struct {
	gates      []*Gate
	gateMap    map[string][]*Gate
	revGateMap map[string][]*Gate
	wiresMap   map[string]*Wire
	inputWires map[string]*Wire
}

func (d *Device) ReadWires(in []string) int {
	var res int
	for res = range in {
		if in[res] == "" {
			return res
		}
		var name string
		var value bool
		parts := strings.Split(in[res], ": ")
		name = parts[0]
		if parts[1] == "1" {
			value = true
		}
		newWire := &Wire{name: name, value: value, set: true}
		d.inputWires[name] = newWire
		d.wiresMap[name] = newWire
		d.gateMap[name] = make([]*Gate, 0)
		d.revGateMap[name] = make([]*Gate, 0)
	}
	return res
}

func (d *Device) GetWire(name string) *Wire {
	if v, ok := d.wiresMap[name]; ok {
		return v
	}
	newWire := &Wire{name: name, set: false}
	d.wiresMap[name] = newWire
	d.gateMap[name] = make([]*Gate, 0)
	d.revGateMap[name] = make([]*Gate, 0)
	return newWire
}

func (d *Device) ResetCircut() {
	for k, v := range d.wiresMap {
		if _, ok := d.inputWires[k]; !ok {
			v.set = false
		}
	}
}

func (d *Device) SetValues(x, y int) {
	keys := slices.Sorted(maps.Keys(d.inputWires))
	for _, k := range keys {
		fmt.Printf("setting %s ", k)
		if strings.HasPrefix(k, "x") {
			d.inputWires[k].value = (x % 2) == 1
			fmt.Println("with x value", (x%2) == 1)
			x >>= 1
		} else {
			d.inputWires[k].value = (y % 2) == 1
			fmt.Println("with y value", (y%2) == 1)
			y >>= 1
		}
	}
}

func (d *Device) ReadGates(in []string, beg int) {
	for i := beg + 1; i < len(in); i++ {
		line := in[i]
		var froml, fromr, to, op string
		fmt.Sscanf(line, "%s %s %s -> %s", &froml, &op, &fromr, &to)
		wLeft, wRight, wTo := d.GetWire(froml), d.GetWire(fromr), d.GetWire(to)
		fun := ReadFun(op)
		newGate := &Gate{in1: wLeft, in2: wRight, f: fun, out: wTo, stable: false}
		d.gates = append(d.gates, newGate)
		d.gateMap[wLeft.name] = append(d.gateMap[wLeft.name], newGate)
		d.gateMap[wRight.name] = append(d.gateMap[wRight.name], newGate)
		d.revGateMap[wTo.name] = append(d.revGateMap[wTo.name], newGate)

	}
}

func (d *Device) Setup(in []string) {
	wiresEnd := d.ReadWires(in)
	d.ReadGates(in, wiresEnd)
}

func NewDevice() *Device {
	return &Device{gates: make([]*Gate, 0), wiresMap: make(map[string]*Wire), gateMap: make(map[string][]*Gate), revGateMap: make(map[string][]*Gate), inputWires: make(map[string]*Wire)}
}

func FilterFunc[T any](in []T, functor func(T) bool) []T {
	res := make([]T, 0)
	for _, v := range in {
		if functor(v) {
			res = append(res, v)
		}
	}
	return res
}

func FilterMapValueFunc[K comparable, V any](in map[K]V, functor func(V) bool) map[K]V {
	res := make(map[K]V)
	for k, v := range in {
		if functor(v) {
			res[k] = v
		}
	}
	return res
}

func FilterMapFunc[K comparable, V any](in map[K]V, functor func(K, V) bool) map[K]V {
	res := make(map[K]V)
	for k, v := range in {
		if functor(k, v) {
			res[k] = v
		}
	}
	return res

}

func (d *Device) StabilizeCircut() {
	stableWires := FilterMapValueFunc(d.wiresMap, func(w *Wire) bool {
		return w.set
	})
	unStableWires := FilterMapValueFunc(d.wiresMap, func(w *Wire) bool {
		return !w.set
	})

	for len(unStableWires) != 0 {
		resolvable := FilterFunc(d.gates, func(value *Gate) bool {
			if _, ok := stableWires[value.out.name]; ok {
				return false
			}
			if _, ok := stableWires[value.in1.name]; !ok {
				return false
			}
			if _, ok := stableWires[value.in2.name]; !ok {
				return false
			}
			return true
		})
		for _, g := range resolvable {
			g.out.value = g.f.Apply(g.in1.value, g.in2.value)
			g.out.set = true
			delete(unStableWires, g.out.name)
			stableWires[g.out.name] = g.out
		}
	}
}

func (d *Device) BuildNumber() int {
	nums := make(map[string]bool)
	for k, v := range d.wiresMap {
		num, ok := strings.CutPrefix(k, "z")
		if !ok {
			continue
		}
		nums[num] = v.value
	}
	keys := slices.Sorted(maps.Keys(nums))
	slices.Reverse(keys)
	result := 0
	for _, k := range keys {
		b := 0
		if nums[k] {
			b = 1
		}
		result = (result << 1) | b
	}
	return result
}

func (d *Device) CheckAddition(i, j int) bool {
	d.ResetCircut()
	d.SetValues(i, j)
	d.StabilizeCircut()
	res := d.BuildNumber()
	return i+j == res
}

func (d *Device) GetBits() (int, int, int) {
	xMax, yMax, zMax := 0, 0, 0
	for k := range d.wiresMap {
		var t byte
		var ind int
		fmt.Sscanf(k, "%c%d", &t, &ind)
		if t == 'x' {
			if ind > xMax {
				xMax = ind
			}
		}
		if t == 'y' {
			if ind > yMax {
				yMax = ind
			}
		}
		if t == 'z' {
			if ind > zMax {
				zMax = ind
			}
		}
	}
	return xMax, yMax, zMax
}

func (d *Device) BuildExpressions(g *Gate) string {
	exps := []string{g.in1.name, g.in2.name}
	flat := []bool{true, true}
	for i := range exps {
		if _, ok := d.inputWires[exps[i]]; !ok {
			flat[i] = false
			invGate := d.revGateMap[exps[i]]
			if len(invGate) != 1 {
				panic("Invalid gates for" + exps[i])
			}
			exps[i] = d.BuildExpressions(invGate[0])
		}
	}
	res := strings.Builder{}
	if !flat[0] {
		res.WriteByte('(')
	}
	res.WriteString(exps[0])
	if !flat[0] {
		res.WriteByte(')')
	}
	res.WriteByte(' ')
	res.WriteString(g.f.String())
	res.WriteByte(' ')
	if !flat[1] {
		res.WriteByte('(')
	}
	res.WriteString(exps[1])
	if !flat[1] {
		res.WriteByte(')')
	}
	return res.String()
}

func (d *Device) AllExpressions() []string {
	zOuts := slices.Sorted(maps.Keys(FilterMapFunc(d.wiresMap, func(k string, v *Wire) bool {
		return strings.HasPrefix(k, "z")
	})))
	res := make([]string, len(zOuts))
	for i, z := range zOuts {
		g := d.revGateMap[z]
		if len(g) != 1 {
			panic("Invalid state for " + z)
		}
		res[i] = z + ": " + d.BuildExpressions(g[0])
	}
	return res
}

func (d *Device) BuildDigraph(output io.Writer) {
	io.WriteString(output, "digraph {\n")
	for k, v := range d.revGateMap {
		stringBuilder := strings.Builder{}
		if len(v) == 1 {
			stringBuilder.WriteString("\t" + k)
			stringBuilder.WriteString("[label=\"")
			stringBuilder.WriteString(k)
			stringBuilder.WriteString("[")
			stringBuilder.WriteString(v[0].f.String())
			stringBuilder.WriteString("]\"]\n")
		}
		io.WriteString(output, stringBuilder.String())
	}
	for k, v := range d.gateMap {
		if len(v) == 0 {
			continue
		}
		stringBuilder := strings.Builder{}

		stringBuilder.WriteString("\t" + k + " -> {")
		for i, n := range v {
			if i != 0 {
				stringBuilder.WriteByte(',')
			}
			stringBuilder.WriteString(n.out.name)
		}
		stringBuilder.WriteString("}\n")
		io.WriteString(output, stringBuilder.String())
	}
	io.WriteString(output, "{ rank = same;")
	for s := range d.inputWires {
		io.WriteString(output, s+"; ")
	}
	io.WriteString(output, "}\n")
	io.WriteString(output, "{ rank = same; ")
	for s := range d.wiresMap {
		if strings.HasPrefix(s, "z") {
			io.WriteString(output, s+"; ")
		}
	}
	io.WriteString(output, "}\n}\n")
}

func init() {
	flag.StringVar(&swaps, "swaps", "", "list of swap tuples")
	flag.StringVar(&outFile, "oFile", "", "output file for dot file")
}

func (d *Device) PerformSwap(dat string) string {
	if len(dat) == 0 {
		return ""
	}
	result := make([]string, 0)
	tuples := strings.Split(dat, ",")
	for _, t := range tuples {
		swaps := strings.Split(t, "-")
		if len(swaps) != 2 {
			panic("Invalid format")
		}
		l1, l2 := d.revGateMap[swaps[0]], d.revGateMap[swaps[1]]
		l1[0].out, l2[0].out = l2[0].out, l1[0].out
		d.revGateMap[swaps[0]] = l2
		d.revGateMap[swaps[1]] = l1
		result = append(result, swaps[0], swaps[1])
	}
	slices.Sort(result)
	return strings.Join(result, ",")
}

func main() {
	circutDesc := util.InputSlice()
	dev := NewDevice()
	dev.Setup(circutDesc)
	dev.StabilizeCircut()
	res := dev.BuildNumber()

	fmt.Printf("The number of all Z outptus is %d\n", res)
	fd, err := os.Create(outFile)
	if err != nil {
		panic(err)
	}
	defer fd.Close()
	outSring := dev.PerformSwap(swaps)
	dev.BuildDigraph(fd)
	fmt.Printf("Resulting sequence of swaps is \"%s\"\n", outSring)
}
