package main

import (
	"fmt"
	"regexp"
)

const text = `
My email is ccmouse@gmail.com
email1 is abc@126.com
email3 is cdfer@qq.com
`

func main() {
	re := regexp.MustCompile(
		`([a-zA-z0-9]+)@([a-zA-Z0-9]+)(\.[a-zA-Z0-9.]+)`)
	match := re.FindAllStringSubmatch(text, -1)
	for _, m := range match {
		fmt.Println(m)
	}
}
