package ballerina_test

import (
	"fmt"
	"testing"

	ballerina "ballerina.com/core"
)

func TestSum2Serialization(t *testing.T) {
	t.Parallel()
	testCases := []ballerina.Sum2[int, string]{
		ballerina.Case1Of2[int, string](10),
		ballerina.Case2Of2[int, string]("abc"),
	}

	for caseIndex, testCase := range testCases {
		t.Run(fmt.Sprintf("case %d of %d", caseIndex+1, len(testCases)), func(t *testing.T) {
			assertBackAndForthFromJsonYieldsSameValue(t, testCase)
		})
	}
}
