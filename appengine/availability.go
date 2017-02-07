package dropp

import (
	"regexp"
	"strconv"
	"strings"
)

// AvaComp is the compounded availability of an item that contains the
// availability type and the item count if present
type AvaComp struct {
	Availability AvaType
	ItemCount    int
}

// AvaType indicates the availability of an item from the source
type AvaType int

const (
	// Available if the item count is above a certain threshold
	Available AvaType = iota

	// Low if the item count is below a certain threshold
	Low

	// Out if there are no items available
	Out

	// Unknown is used when the update could not be performed
	Unknown
)

var threshold int

func init() {
	threshold = 10
}

func newAva(avaString string) AvaComp {
	switch {
	case avaString == "Currently out of stock":
		return AvaComp{
			Availability: Out,
			ItemCount:    0,
		}
	case avaString == "In stock, usually dispatched in 1 business day":
		return AvaComp{
			Availability: Available,
			ItemCount:    0,
		}
	case strings.Contains(avaString, "usually dispatched in 1 business day"):
		rx, _ := regexp.Compile("[0-9]+")
		avaCount, _ := strconv.Atoi(rx.FindStringSubmatch(avaString)[0])
		if avaCount <= threshold {
			return AvaComp{
				Availability: Low,
				ItemCount:    avaCount,
			}
		}
		return AvaComp{
			Availability: Available,
			ItemCount:    avaCount,
		}
	default:
		return AvaComp{
			Availability: Unknown,
			ItemCount:    0,
		}
	}
}

// Print returns a well formatted string for any availability case
func (av AvaComp) Print() string {
	switch av.Availability {
	case Available:
		if av.ItemCount == 0 {
			return "Item available"
		}
		return strconv.Itoa(av.ItemCount) + " Items available"

	case Low:
		return "Item low: only " + strconv.Itoa(av.ItemCount) + " items left"
	case Out:
		return "Out of stock"
	default:
		return "Could not update availability"
	}
}
