package parser

import (
	"regexp"

	"github.com/jhsygg/mycode/coll/engine"
)

//const cityListRe = `<a href="(http://www.zhenai.com/zhenghun/[0-9a-z]+)"[^>]*>([^<]+)</a>`

const cityListRe = `<a href="(http://www.gov.cn/zhengce/content/\d{4}-\d{2}/\d{2}/content_\d{7}.htm)" target="_blank"  >([^<]+)</a>`

func ParseCityList(
	contents []byte) engine.ParseResult {

	re := regexp.MustCompile(cityListRe)
	matches := re.FindAllSubmatch(contents, -1)

	result := engine.ParseResult{}

	for _, m := range matches {
		result.Items = append(
			result.Items, "City "+string(m[2]))
		result.Requests = append(
			result.Requests, engine.Request{
				Url:        string(m[1]),
				ParserFunc: ParseCity,
			})
	}
	return result
}
