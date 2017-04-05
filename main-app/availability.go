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

// NewAva returns a new compounded availability structure starting from a
// line scraped from the website page
func NewAva(avaString string) AvaComp {
	outPredicate := avaString == "Currently out of stock" ||
		avaString == "Usually dispatched in 6-9 business days" ||
		avaString == "Under restocking. It will be available soon" ||
		avaString == "Sold Out Currently!" ||
		strings.Contains(avaString, "Expected restock on")

	avaPredicate := avaString == "In stock, usually dispatched in 1 business day" ||
		avaString == "In stock, usually dispatched in 1-3 business days" ||
		avaString == "In stock , usually dispatched in 1-3 business days"

	switch {
	case outPredicate:
		return AvaComp{
			Availability: Out,
			ItemCount:    0,
		}
	case avaPredicate:
		return AvaComp{
			Availability: Available,
			ItemCount:    0,
		}
	case strings.Contains(avaString, "Only"):
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

// String returns a well formatted string for any availability case
func (av AvaComp) String() string {
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

// AvaColor returns the HTML color with witch to display the availility
func AvaColor(av AvaComp) string {
	switch av.Availability {
	case Available:
		return "green"
	case Low:
		return "orange"
	case Out:
		return "red"
	default:
		return "blue"
	}
}
