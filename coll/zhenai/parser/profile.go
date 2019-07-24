package parser

import (
	"regexp"

	"github.com/jhsygg/mycode/coll/engine"
	"github.com/jhsygg/mycode/coll/model"
)

var basicInfoRe = regexp.MustCompile(
	`<div class="des f-cl" data-v-[0-9a-zA-Z]>([^<]+)</div>`)

// ParseProfile ...
func ParseProfile(
	contents []byte,
	name string) engine.ParseResult {
	profile := model.Profile{}
	profile.Name = name

	profile.Marriage = extractString(
		contents, basicInfoRe)

	result := engine.ParseResult{
		Items: []interface{}{profile},
	}

	return result
}

func extractString(
	contents []byte, re *regexp.Regexp) string {
	match := re.FindSubmatch(contents)

	if len(match) >= 2 {
		return string(match[1])
	} else {
		return ""
	}
}
