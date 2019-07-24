package main

import (
	"github.com/jhsygg/mycode/coll/engine"
	"github.com/jhsygg/mycode/coll/zhenai/parser"
)

func main() {
	engine.Run(engine.Request{
		Url:        "http://www.gov.cn/zhengce/zuixin.htm",
		ParserFunc: parser.ParseCityList,
	})
}
