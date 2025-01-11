package util

import (
	"fmt"
	"io"
	"net/http"
	"os"
)

const (
	AOC_COOKIE_ENV string = "AOC_AUTH"
	AOC_BACKEND    string = "https://adventofcode.com/2024/day/%d/input"
	AOC_COOKIE     string = "session=%s"
	AOC_INPUT_FILE string = "inputs/day%02d/input.txt"
)

func resolveInput(file string) {
	out, err := os.Create(file)
	if err != nil {
		panic(err)
	}
	defer out.Close()
	cookie := os.Getenv(AOC_COOKIE_ENV)
	if cookie == "" {
		panic("Cannot connect to AOC backend without AOC_AUTH envvar")
	}
	r, err := http.NewRequest(http.MethodGet, fmt.Sprintf(AOC_BACKEND, GetIdentifier()), nil)
	if err != nil {
		panic(err.Error())
	}
	cookies, err := http.ParseCookie(fmt.Sprintf(AOC_COOKIE, cookie))
	r.AddCookie(cookies[0])
	res, err := http.DefaultClient.Do(r)
	if err != nil || res.StatusCode != http.StatusOK {
		panic(fmt.Sprintf("%s %v %v", "Invalid response", err, res.StatusCode))
	}
	defer res.Body.Close()
	_, err = io.Copy(out, res.Body)
	if err != nil {
		panic(err)
	}

}

func GetOfficialInput() string {
	file := fmt.Sprintf(AOC_INPUT_FILE, GetIdentifier())
	f, err := os.Open(file)
	if err != nil {
		fmt.Printf("Input file not found, resolving it from AOC... ")
		resolveInput(file)
		f, _ = os.Open(file)
		fmt.Print("Resolved\n")
	}
	defer f.Close()
	res, err := io.ReadAll(f)
	if err != nil {
		panic(err)
	}
	return string(res)
}
