package main

import (
	"fmt"
	"math"
	"strings"

	"github.com/mijaros/adventofcode/v2024/pkg/util"
)

type State int8
type MoveState int8

// +---+---+---+
// | 7 | 8 | 9 | 0 1 2
// +---+---+---+
// | 4 | 5 | 6 | 3 4 5
// +---+---+---+
// | 1 | 2 | 3 | 6 7 8
// +---+---+---+
//
//	| 0 | A | 9 10 11
//	+---+---+
const (
	NUM1 State = 6
	NUM2       = 7
	NUM3       = 8
	NUM4       = 3
	NUM5       = 4
	NUM6       = 5
	NUM7       = 0
	NUM8       = 1
	NUM9       = 2
	NUM0       = 10
	ACT        = 11
	NIL        = 9
)

func (s State) String() string {
	switch s {
	case NUM1:
		return "1"
	case NUM2:
		return "2"
	case NUM3:
		return "3"
	case NUM4:
		return "4"
	case NUM5:
		return "5"
	case NUM6:
		return "6"
	case NUM7:
		return "7"
	case NUM8:
		return "8"
	case NUM9:
		return "9"
	case NUM0:
		return "0"
	case ACT:
		return "A"
	case NIL:
		return "N"
	}
	return ""
}

//	+---+---+
//	| ^ | A | 0 1 2
//
// +---+---+---+
// | < | v | > | 3 4 5
// +---+---+---+
const (
	MNIL   MoveState = 0
	MUP              = 1
	MACT             = 2
	MLEFT            = 3
	MDOWN            = 4
	MRIGHT           = 5
)

func (m MoveState) String() string {
	switch m {
	case MNIL:
		return "N"
	case MUP:
		return "^"
	case MACT:
		return "A"
	case MLEFT:
		return "<"
	case MDOWN:
		return "v"
	case MRIGHT:
		return ">"
	}
	return ""
}

func ToState(b byte) State {
	switch b {
	case '0':
		return NUM0
	case '1':
		return NUM1
	case '2':
		return NUM2
	case '3':
		return NUM3
	case '4':
		return NUM4
	case '5':
		return NUM5
	case '6':
		return NUM6
	case '7':
		return NUM7
	case '8':
		return NUM8
	case '9':
		return NUM9
	case 'A':
		return ACT
	}
	return NIL
}

func ToMoveState(b byte) MoveState {
	switch b {
	case '<':
		return MLEFT
	case '>':
		return MRIGHT
	case 'v':
		return MDOWN
	case '^':
		return MUP
	case 'A':
		return MACT
	}
	return MNIL
}

type iterBuilder struct {
	stat State
	path string
}

func BuildKeypadMap() map[State]map[State][]string {
	result := make(map[State]map[State][]string)
	for i := State(0); i < 12; i++ {
		result[i] = make(map[State][]string)
		for j := State(0); j < 12; j++ {
			result[i][j] = make([]string, 0)
			hf, vf := i%3, i/3
			ht, vt := j%3, j/3
			md := util.Abs(int(hf-ht)) + util.Abs(int(vf-vt))
			curr := []iterBuilder{{stat: i, path: ""}}
			for k := 0; k <= md; k++ {
				next := []iterBuilder{}
				for _, c := range curr {
					if c.stat == NIL {
						continue
					}
					if c.stat == j {
						result[i][j] = append(result[i][j], c.path+"A")
					}
					hc, vc := c.stat%3, c.stat/3
					hd, vd := hc-ht, vc-vt
					if hd < 0 {
						next = append(next, iterBuilder{stat: State(vc*3 + hc + 1), path: c.path + ">"})
					}
					if hd > 0 {
						next = append(next, iterBuilder{stat: State(vc*3 + hc - 1), path: c.path + "<"})
					}
					if vd < 0 {
						next = append(next, iterBuilder{stat: State((vc+1)*3 + hc), path: c.path + "v"})
					}
					if vd > 0 {
						next = append(next, iterBuilder{stat: State((vc-1)*3 + hc), path: c.path + "^"})
					}
				}
				curr = next
			}
		}
	}
	return result
}

type iterBuilderII struct {
	stat MoveState
	path string
}

func BuildMovepadMap() map[MoveState]map[MoveState][]string {
	result := make(map[MoveState]map[MoveState][]string)
	for i := MoveState(0); i < 6; i++ {
		result[i] = make(map[MoveState][]string)
		for j := MoveState(0); j < 6; j++ {
			result[i][j] = []string{}
			hf, vf := i%3, i/3
			ht, vt := j%3, j/3
			md := util.Abs(int(hf-ht)) + util.Abs(int(vf-vt))
			curr := []iterBuilderII{{stat: i, path: ""}}
			for k := 0; k <= md; k++ {
				next := []iterBuilderII{}
				for _, c := range curr {
					if c.stat == MNIL {
						continue
					}
					if c.stat == j {
						result[i][j] = append(result[i][j], c.path+"A")
						continue
					}
					hc, vc := c.stat%3, c.stat/3
					hd, vd := hc-ht, vc-vt
					if hd < 0 {
						next = append(next, iterBuilderII{stat: MoveState(vc*3 + hc + 1), path: c.path + ">"})
					}
					if hd > 0 {
						next = append(next, iterBuilderII{stat: MoveState(vc*3 + hc - 1), path: c.path + "<"})
					}
					if vd < 0 {
						next = append(next, iterBuilderII{stat: MoveState((vc+1)*3 + hc), path: c.path + "v"})
					}
					if vd > 0 {
						next = append(next, iterBuilderII{stat: MoveState((vc-1)*3 + hc), path: c.path + "^"})
					}
				}
				curr = next
			}
		}
	}
	return result
}

type NumPad struct {
	state State
	m     *MovePad
}

type MovePad struct {
	state MoveState
	sub   *MovePad
	l     int
	cache map[[2]MoveState]int
}

func (n *NumPad) MoveState(next State) string {
	res := moves[n.state][next]
	minLen := math.MaxInt
	minStr := ""
	for _, alt := range res {
		moves := n.m.ProcessString(alt)
		if len(moves) < minLen {
			minLen = len(moves)
			minStr = moves
		}
	}
	n.state = next
	return minStr
}
func (n *NumPad) MoveStateCount(next State) int {
	res := moves[n.state][next]
	minLen := math.MaxInt
	for _, alt := range res {
		moves := n.m.ProcessCount(alt)
		if moves < minLen {
			minLen = moves
		}
	}
	n.state = next
	if minLen == math.MaxInt {
		return 0
	}
	return minLen
}

func (n *NumPad) ProcessPasscode(passcode string) string {
	builder := strings.Builder{}
	for _, c := range []byte(passcode) {
		ns := ToState(c)
		builder.WriteString(n.MoveState(ns))
	}
	return builder.String()

}
func (n *NumPad) ProcessPasscodeCount(passcode string) int {
	res := 0
	for _, c := range []byte(passcode) {
		ns := ToState(c)
		res += n.MoveStateCount(ns)
	}
	return res

}

func (m *MovePad) MoveState(next MoveState) []string {
	res := keyMoves[m.state][next]
	m.state = next
	return res
}

func (m *MovePad) ProcessString(moves string) string {
	builder := strings.Builder{}
	for _, c := range []byte(moves) {
		ns := ToMoveState(c)
		all := m.MoveState(ns)
		mRes := ""
		mLen := math.MaxInt
		for _, r := range all {
			if m.sub != nil {
				r = m.sub.ProcessString(r)
			}
			if len(r) < mLen {
				mRes = r
				mLen = len(r)
			}
		}
		builder.WriteString(mRes)
	}
	return builder.String()
}

func (m *MovePad) ProcessCount(moves string) int {
	res := 0
	for _, c := range []byte(moves) {
		ns := ToMoveState(c)
		stateDesc := [2]MoveState{m.state, ns}
		all := m.MoveState(ns)               // Two days to find out, that you really can't do the continue without moving state
		if d, ok := m.cache[stateDesc]; ok { // Bloody hell!
			res += d
			continue
		}
		mLen := math.MaxInt
		for _, move := range all {
			tLen := len(move)
			if m.sub != nil {
				m.sub.state = MACT
				tLen = m.sub.ProcessCount(move)
			}
			if tLen < mLen {
				mLen = tLen
			}
		}
		if mLen == math.MaxInt {
			continue
		}
		m.cache[stateDesc] = mLen
		res += mLen
	}
	return res
}

func NewNumPad() NumPad {
	return NumPad{state: ACT, m: nil}
}

func NewMovePad() MovePad {
	return MovePad{state: MACT, sub: nil, cache: make(map[[2]MoveState]int)}
}

type moveHolder struct {
	f, t MoveState
	l    int
}

var (
	moves    map[State]map[State][]string
	keyMoves map[MoveState]map[MoveState][]string
)

func init() {
	moves = BuildKeypadMap()
	keyMoves = BuildMovepadMap()
}

func PartI(codes []string) int {
	np := NewNumPad()
	k1 := NewMovePad()
	k2 := NewMovePad()
	k1.sub = &k2
	np.m = &k1
	result := 0

	for _, k := range codes {
		v := 0
		fmt.Sscanf(k, "%dA", &v)
		lvl1 := np.ProcessPasscode(k)
		result += v * len(lvl1)
	}
	return result
}

func PartII(codes []string, l int) (int, map[string]int) {
	resM := make(map[string]int)
	np := NewNumPad()
	mps := make([]MovePad, l)
	mps[0] = NewMovePad()
	mps[0].l = l
	for i := 1; i < len(mps); i++ {
		mps[i] = NewMovePad()
		mps[i].l = l - i
		mps[i].sub = &mps[i-1]
	}
	np.m = &mps[len(mps)-1]
	result := 0
	for _, k := range codes {
		v := 0
		fmt.Sscanf(k, "%dA", &v)
		lvl1 := np.ProcessPasscodeCount(k)
		resM[k] = lvl1
		result += v * lvl1
	}
	return result, resM

}

func main() {
	codes := util.InputSlice()
	result := PartI(codes)

	fmt.Printf("The complexity of code is %d\n", result)
	resultI, _ := PartII(codes, 2)
	fmt.Printf("The sanity check for counting algorithm got complexity %d\n", resultI)
	resultII, _ := PartII(codes, 25)
	fmt.Printf("The complexity of code for 25 dirPads is %d\n", resultII)

}
