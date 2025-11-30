package util

var dayIdentifier *int

func GetIdentifier() int {
	return *dayIdentifier
}

func SetIdentifier(code int) {
	if dayIdentifier != nil {
		panic("Trying to reset static id")
	}
	dayIdentifier = &code
}
