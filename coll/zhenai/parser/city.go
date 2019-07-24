package parser

import (
	"regexp"

	"github.com/jhsygg/mycode/coll/engine"
)

//const cityRe = `<a href="(http://album.zhenai.com/u/[0-9]+)"[^>]*>([^<]+)</a>`

const cityRe = `<p align="" style="margin-top: 0px; margin-bottom: 0px; text-indent: 2em; text-align: justify; font-family: 宋体;"><span style="font-weight: bold;">([^<]+)</span>([^<]+)</p>`

// ParseCity ...
func ParseCity(
	contents []byte) engine.ParseResult {
	re := regexp.MustCompile(cityRe)
	matches := re.FindAllSubmatch(contents, -1)

	result := engine.ParseResult{}
	for _, m := range matches {
		result.Items = append(
			result.Items, "User "+string(m[2]))
		result.Requests = append(
			result.Requests, engine.Request{
				Url: string(m[1]),
				ParserFunc: func(
					c []byte) engine.ParseResult {
					return ParseProfile(c, string(m[2]))
				},
			})
	}

	return result
}
