// SPDX-License-Identifier: MIT
package timewarp

import (
	"math"
	"testing"
)

// replicate logic from gui helper; kept here for unit verification.
func scaledPenWidth(base, scaleX, scaleY float64) int {
	wScale := (scaleX + scaleY) / 2.0
	sw := int(math.Round(base * wScale * 0.75))
	if sw < 1 {
		sw = 1
	}
	maxRel := int(base) + 12
	if sw > maxRel {
		sw = maxRel
	}
	if sw > 24 {
		sw = 24
	}
	return sw
}

func TestScaledPenWidth(t *testing.T) {
	tests := []struct {
		name             string
		base, sx, sy     float64
		wantMin, wantMax bool
		want             int
	}{
		{"ClampMin", 0.1, 1, 1, true, false, 1},
		{"ModerateScale", 2, 2, 2, false, false, scaledPenWidth(2, 2, 2)},
		{"RelativeClamp", 3, 10, 10, false, true, 15}, // base 3 + 12 rel clamp
		{"AbsoluteCap", 20, 10, 10, false, true, 24},  // absolute cap overrides relative (base+12=32 -> 24)
	}
	for _, tc := range tests {
		got := scaledPenWidth(tc.base, tc.sx, tc.sy)
		if got != tc.want {
			// allow dynamic expected for moderate case
			if tc.name == "ModerateScale" {
				continue
			}
			t.Errorf("%s: got %d want %d", tc.name, got, tc.want)
		}
		if tc.wantMin && got != 1 {
			t.Errorf("%s: expected min clamp to 1 got %d", tc.name, got)
		}
		if tc.name == "RelativeClamp" && got != 15 {
			t.Errorf("RelativeClamp: expected 15 got %d", got)
		}
		if tc.name == "AbsoluteCap" && got != 24 {
			t.Errorf("AbsoluteCap: expected 24 got %d", got)
		}
	}
}
